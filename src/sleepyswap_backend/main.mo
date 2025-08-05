import Principal "mo:base/Principal";
import Nat "mo:base/Nat";
import Blob "mo:base/Blob";
import Nat8 "mo:base/Nat8";
import Nat64 "mo:base/Nat64";
import Sleepy "Sleepy";

import RBTree "../util/motoko/StableCollections/RedBlackTree/RBTree";
import Time64 "../util/motoko/Time64";
import Value "../util/motoko/Value";
import Error "../util/motoko/Error";
import Account "../util/motoko/ICRC-1/Account";
import ICRC_1_Types "../util/motoko/ICRC-1/Types";

shared (install) persistent actor class Canister(
  // deploy : {
  //   #Init : {  };
  //   #Upgrade;
  // }
) = Self {
  func self() : Principal = Principal.fromActor(Self);

  var order_id = 0;
  var orders = RBTree.empty<Nat, Sleepy.Order>();
  var closed_orders = RBTree.empty<Nat, ()>(); // to trim

  var sell_book : Sleepy.Book = RBTree.empty();
  var buy_book : Sleepy.Book = RBTree.empty();

  var trade_id = 0;
  var trades = RBTree.empty<Nat, Sleepy.Trade>();

  var user_ids = RBTree.empty<Nat, Principal>();
  var users = RBTree.empty<Principal, Sleepy.User>();

  var place_dedupes = RBTree.empty<Sleepy.PlaceArg, Sleepy.PlaceOk>();

  var metadata : Value.Metadata = RBTree.empty();

  public shared query func sleepyswap_metadata() : async [(Text, Value.Type)] = async RBTree.array(metadata);

  public shared ({ caller }) func sleepyswap_place(arg : Sleepy.PlaceArg) : async Sleepy.PlaceResult = async try {
    // if (not Canister.isAvailable(metadata)) return Error.text("Unavailable");
    let user_account = { owner = caller; subaccount = arg.subaccount };
    if (not Account.validate(user_account)) return Error.text("Caller account is not valid");

    let batch_size = arg.buy_orders.size() + arg.sell_orders.size();
    if (batch_size == 0) return Error.text("Orders cannot be empty");
    let max_batch = Value.getNat(metadata, Sleepy.MAX_ORDER_BATCH, 0);
    if (max_batch > 0 and batch_size > max_batch) return #Err(#BatchTooLarge { current_batch_size = batch_size; maximum_batch_size = max_batch });

    let sell_token_id = switch (Value.metaPrincipal(metadata, Sleepy.SELL_TOKEN)) {
      case (?found) found;
      case _ return Error.text("Metadata `" # Sleepy.SELL_TOKEN # "` is not set");
    };
    let buy_token_id = switch (Value.metaPrincipal(metadata, Sleepy.BUY_TOKEN)) {
      case (?found) found;
      case _ return Error.text("Metadata `" # Sleepy.BUY_TOKEN # "` is not set");
    };
    let (sell_token, buy_token) = (ICRC_1_Types.genActor(sell_token_id), ICRC_1_Types.genActor(buy_token_id));

    let (sell_decimals_res, buy_decimals_res, sell_fee_res, buy_fee_res) = (sell_token.icrc1_decimals(), buy_token.icrc1_decimals(), sell_token.icrc1_fee(), buy_token.icrc1_fee());
    let (sell_power, buy_power, sell_token_fee, buy_token_fee) = (10 ** Nat8.toNat(await sell_decimals_res), 10 ** Nat8.toNat(await buy_decimals_res), await sell_fee_res, await buy_fee_res);

    var amount_tick = Value.getNat(metadata, Sleepy.AMOUNT_TICK, 0);
    if (amount_tick < sell_token_fee) {
      amount_tick := sell_token_fee;
      metadata := Value.setNat(metadata, Sleepy.AMOUNT_TICK, ?amount_tick);
    };
    var price_tick = Value.getNat(metadata, Sleepy.PRICE_TICK, 0);
    if (price_tick < buy_token_fee) {
      price_tick := buy_token_fee;
      metadata := Value.setNat(metadata, Sleepy.PRICE_TICK, ?price_tick);
    };
    var fee_denom = Value.getNat(metadata, Sleepy.TRADING_FEE_DENOM, 0);
    if (fee_denom < 100) {
      fee_denom := 100;
      metadata := Value.setNat(metadata, Sleepy.TRADING_FEE_DENOM, ?fee_denom);
    };
    var maker_fee_numer = Value.getNat(metadata, Sleepy.MAKER_FEE_NUMER, 0);
    let max_fee_denom = fee_denom / 10; // max at most 10%
    if (maker_fee_numer > max_fee_denom) {
      maker_fee_numer := max_fee_denom;
      metadata := Value.setNat(metadata, Sleepy.MAKER_FEE_NUMER, ?maker_fee_numer);
    };
    var taker_fee_numer = Value.getNat(metadata, Sleepy.TAKER_FEE_NUMER, 0);
    if (taker_fee_numer > max_fee_denom) {
      taker_fee_numer := max_fee_denom;
      metadata := Value.setNat(metadata, Sleepy.TAKER_FEE_NUMER, ?taker_fee_numer);
    };
    let min_fee_numer = Nat.max(1, Nat.min(maker_fee_numer, taker_fee_numer));
    // (tokenfee * 2) for amount + future transfer of amount
    let lowest_sell_amount = sell_token_fee * 2 * fee_denom / min_fee_numer;
    var min_sell_amount = Value.getNat(metadata, Sleepy.MIN_SELL_AMOUNT, 0);
    if (min_sell_amount < lowest_sell_amount) {
      min_sell_amount := lowest_sell_amount;
      metadata := Value.setNat(metadata, Sleepy.MIN_SELL_AMOUNT, ?min_sell_amount);
    };
    let lowest_buy_amount = buy_token_fee * 2 * fee_denom / min_fee_numer;
    var min_buy_amount = Value.getNat(metadata, Sleepy.MIN_BUY_AMOUNT, 0);
    if (min_buy_amount < lowest_buy_amount) {
      min_buy_amount := lowest_buy_amount;
      metadata := Value.setNat(metadata, Sleepy.MIN_BUY_AMOUNT, ?min_buy_amount);
    };
    let lowest_price = lowest_buy_amount / lowest_sell_amount;
    var min_price = Value.getNat(metadata, Sleepy.MIN_PRICE, 0);
    if (min_price < lowest_price) {
      min_price := lowest_price;
      metadata := Value.setNat(metadata, Sleepy.MIN_PRICE, ?min_price);
    };

    var total_incoming_sell_amount = 0;
    var incoming_sells = RBTree.empty<(price : Nat), { index : Nat; amount : Nat }>();
    for (incoming in arg.sell_orders.vals()) {
      let incoming_index = RBTree.size(incoming_sells);
      if (incoming.price < min_price) return #Err(#SellPriceTooLow { incoming with index = incoming_index; minimum_price = min_price });

      let nearest_price = Sleepy.nearTick(incoming.price, price_tick);
      if (incoming.price != nearest_price) return #Err(#SellPriceTooFar { incoming with index = incoming_index; nearest_price });

      let min_amount = Nat.max(min_sell_amount, min_buy_amount / incoming.price);
      if (incoming.amount < min_amount) return #Err(#SellAmountTooLow { incoming with index = incoming_index; minimum_amount = min_amount });

      let nearest_amount = Sleepy.nearTick(incoming.amount, amount_tick);
      if (incoming.amount != nearest_amount) return #Err(#SellAmountTooFar { incoming with index = incoming_index; nearest_amount });

      total_incoming_sell_amount += incoming.amount;
      switch (RBTree.get(incoming_sells, Nat.compare, incoming.price)) {
        case (?found) return #Err(#DuplicateSellPrice { incoming with indexes = [found.index, incoming_index] });
        case _ ();
      };
      incoming_sells := RBTree.insert(incoming_sells, Nat.compare, incoming.price, { incoming with index = incoming_index });
    };

    var total_incoming_buy_amount = 0;
    var incoming_buys = RBTree.empty<(price : Nat), { index : Nat; amount : Nat }>();
    for (incoming in arg.buy_orders.vals()) {
      let incoming_index = RBTree.size(incoming_buys);
      if (incoming.price < min_price) return #Err(#BuyPriceTooLow { incoming with index = incoming_index; minimum_price = min_price });

      let nearest_price = Sleepy.nearTick(incoming.price, price_tick);
      if (incoming.price != nearest_price) return #Err(#BuyPriceTooFar { incoming with index = incoming_index; nearest_price });

      let min_amount = Nat.max(min_sell_amount, min_buy_amount / incoming.price);
      if (incoming.amount < min_amount) return #Err(#BuyAmountTooLow { incoming with index = incoming_index; minimum_amount = min_amount });

      let nearest_amount = Sleepy.nearTick(incoming.amount, amount_tick);
      if (incoming.amount != nearest_amount) return #Err(#BuyAmountTooFar { incoming with index = incoming_index; nearest_amount });

      total_incoming_buy_amount += incoming.amount;
      switch (RBTree.get(incoming_buys, Nat.compare, incoming.price)) {
        case (?found) return #Err(#DuplicateBuyPrice { incoming with indexes = [found.index, incoming_index] });
        case _ ();
      };
      incoming_buys := RBTree.insert(incoming_buys, Nat.compare, incoming.price, { incoming with index = incoming_index });
    };
    let min_incoming_sell = RBTree.min(incoming_sells);
    let max_incoming_buy = RBTree.max(incoming_buys);
    switch (min_incoming_sell, max_incoming_buy) {
      case (?(min_sell_price, min_sell), ?(max_buy_price, max_buy)) if (max_buy_price >= min_sell_price) {
        return #Err(#OrdersOverlap { buy_price = max_buy_price; buy_index = max_buy.index; sell_price = min_sell_price; sell_index = min_sell.index });
      };
      case _ ();
    };

    var user = switch (RBTree.get(users, Principal.compare, caller)) {
      case (?found) found;
      case _ Sleepy.newUser(user_ids);
    };
    let arg_subaccount = Account.denull(arg.subaccount);
    var subaccount = switch (RBTree.get(user.subaccounts, Blob.compare, arg_subaccount)) {
      case (?found) found;
      case _ Sleepy.newSubaccount(user.subaccount_ids);
    };
    let min_own_sell = RBTree.min(subaccount.sells);
    let max_own_buy = RBTree.max(subaccount.buys);
    switch (min_incoming_sell, max_own_buy) {
      case (?(min_incoming_sell_price, min_incoming_sell_detail), ?(max_own_buy_price, _)) if (min_incoming_sell_price <= max_own_buy_price) return #Err(#SellPriceTooLow { min_incoming_sell_detail with price = min_incoming_sell_price; minimum_price = max_own_buy_price });
      case _ ();
    };
    switch (max_incoming_buy, min_own_sell) {
      case (?(max_incoming_buy_price, max_incoming_buy_detail), ?(min_own_sell_price, _)) if (max_incoming_buy_price >= min_own_sell_price) return #Err(#BuyPriceTooHigh { max_incoming_buy_detail with price = max_incoming_buy_price; maximum_price = min_own_sell_price });
      case _ ();
    };
    for ((incoming_price, incoming_detail) in RBTree.entries(incoming_sells)) switch (RBTree.get(subaccount.sells, Nat.compare, incoming_price)) {
      case (?found) return #Err(#SellPriceOccupied { incoming_detail with price = incoming_price; order_id = found });
      case _ ();
    };
    for ((incoming_price, incoming_detail) in RBTree.entries(incoming_buys)) switch (RBTree.get(subaccount.buys, Nat.compare, incoming_price)) {
      case (?found) return #Err(#BuyPriceOccupied { incoming_detail with price = incoming_price; order_id = found });
      case _ ();
    };

    var tx_window = Nat64.fromNat(Value.getNat(metadata, Sleepy.TX_WINDOW, 0));
    let min_tx_window = Time64.MINUTES(15);
    if (tx_window < min_tx_window) {
      tx_window := min_tx_window;
      metadata := Value.setNat(metadata, Sleepy.TX_WINDOW, ?(Nat64.toNat(tx_window)));
    };

    var permitted_drift = Nat64.fromNat(Value.getNat(metadata, Sleepy.PERMITTED_DRIFT, 0));
    let min_permitted_drift = Time64.SECONDS(5);
    if (permitted_drift < min_permitted_drift) {
      permitted_drift := min_permitted_drift;
      metadata := Value.setNat(metadata, Sleepy.PERMITTED_DRIFT, ?(Nat64.toNat(permitted_drift)));
    };

    let now = Time64.nanos();
    switch (arg.created_at_time) {
      case (?created_time) {
        let start_time = now - tx_window - permitted_drift;
        if (created_time < start_time) return #Err(#TooOld);
        let end_time = now + permitted_drift;
        if (created_time > end_time) return #Err(#CreatedInFuture { ledger_time = now });
        switch (RBTree.get(place_dedupes, Sleepy.placeCompare, arg)) {
          case (?duplicate_of) return #Err(#Duplicate { duplicate_of });
          case _ ();
        };
      };
      case _ ();
    };
    #Ok([]);
  } catch e Error.error(e);

  public shared ({ caller }) func sleepyswap_cancel(arg : Sleepy.CancelArg) : async Sleepy.CancelResult {
    [];
  };

  public shared ({ caller }) func sleepyswap_match() : async () {

  };
};
