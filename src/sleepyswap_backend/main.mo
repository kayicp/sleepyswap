import Principal "mo:base/Principal";
import Nat "mo:base/Nat";
import Blob "mo:base/Blob";
import Nat8 "mo:base/Nat8";
import Nat64 "mo:base/Nat64";
import Buffer "mo:base/Buffer";
import Text "mo:base/Text";
import Option "mo:base/Option";
import Sleepy "Sleepy";

import RBTree "../util/motoko/StableCollections/RedBlackTree/RBTree";
import Time64 "../util/motoko/Time64";
import Value "../util/motoko/Value";
import Error "../util/motoko/Error";
import Account "../util/motoko/ICRC-1/Account";
import ICRC_1_Types "../util/motoko/ICRC-1/Types";
import Management "../util/motoko/Management";

shared (install) persistent actor class Canister(
  // deploy : {
  //   #Init : {  };
  //   #Upgrade;
  // }
) = Self {
  // func self() : Principal = Principal.fromActor(Self);

  var order_id = 0;
  var orders = RBTree.empty<Nat, Sleepy.Order>();
  var closed_orders = RBTree.empty<Nat, ()>(); // to trim

  var sell_book : Sleepy.Book = RBTree.empty();
  var buy_book : Sleepy.Book = RBTree.empty();

  var trade_id = 0;
  var trades = RBTree.empty<Nat, Sleepy.Trade>();

  var user_ids = RBTree.empty<Nat, Principal>();
  var users = RBTree.empty<Principal, Sleepy.User>();

  var last_placed = 0 : Nat64;
  var last_placer = Management.principal();

  var place_dedupes = RBTree.empty<Sleepy.PlaceArg, Sleepy.PlaceOk>();

  var metadata : Value.Metadata = RBTree.empty();

  public shared query func sleepyswap_metadata() : async [(Text, Value.Type)] = async RBTree.array(metadata);

  public shared ({ caller }) func sleepyswap_place(arg : Sleepy.PlaceArg) : async Sleepy.PlaceResult {
    // if (not Canister.isAvailable(metadata)) return Error.text("Unavailable");
    let user_account = { owner = caller; subaccount = arg.subaccount };
    if (not Account.validate(user_account)) return Error.text("Caller account is not valid");

    let batch_size = arg.buy_orders.size() + arg.sell_orders.size();
    if (batch_size == 0) return Error.text("Orders cannot be empty");
    let max_batch = Value.getNat(metadata, Sleepy.MAX_ORDER_BATCH, 0);
    if (max_batch > 0 and batch_size > max_batch) return #Err(#BatchTooLarge { batch_size = batch_size; maximum_batch_size = max_batch });

    var is_locker = false;
    var user = getUser(caller);
    let arg_subaccount = Account.denull(arg.subaccount);
    var subaccount = Sleepy.getSubaccount(user, arg_subaccount);

    try {
      let sell_token_id = switch (Value.metaPrincipal(metadata, Sleepy.SELL_TOKEN)) {
        case (?found) found;
        case _ return Error.text("Metadata `" # Sleepy.SELL_TOKEN # "` is not properly set");
      };
      let buy_token_id = switch (Value.metaPrincipal(metadata, Sleepy.BUY_TOKEN)) {
        case (?found) found;
        case _ return Error.text("Metadata `" # Sleepy.BUY_TOKEN # "` is not properly set");
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
      let lowest_price = min_buy_amount / min_sell_amount;
      var min_price = Value.getNat(metadata, Sleepy.MIN_PRICE, 0);
      if (min_price < lowest_price) {
        min_price := lowest_price;
        metadata := Value.setNat(metadata, Sleepy.MIN_PRICE, ?min_price);
      };

      var max_expiry = Time64.SECONDS(Nat64.fromNat(Value.getNat(metadata, Sleepy.MAX_ORDER_EXPIRY, 0)));
      let lowest_max_expiry = Time64.HOURS(24);
      let highest_max_expiry = lowest_max_expiry * 30;
      if (max_expiry < lowest_max_expiry) {
        max_expiry := lowest_max_expiry;
        metadata := Value.setNat(metadata, Sleepy.MAX_ORDER_EXPIRY, ?(Nat64.toNat(lowest_max_expiry / 1_000_000_000)));
      } else if (max_expiry > highest_max_expiry) {
        max_expiry := highest_max_expiry;
        metadata := Value.setNat(metadata, Sleepy.MAX_ORDER_EXPIRY, ?(Nat64.toNat(highest_max_expiry / 1_000_000_000)));
      };

      var min_expiry = Time64.SECONDS(Nat64.fromNat(Value.getNat(metadata, Sleepy.MIN_ORDER_EXPIRY, 0)));
      let lowest_min_expiry = Time64.HOURS(1);
      let max_expiry_seconds = Nat64.toNat(max_expiry / 1_000_000_000);
      if (min_expiry < lowest_min_expiry) {
        min_expiry := lowest_min_expiry;
        metadata := Value.setNat(metadata, Sleepy.MIN_ORDER_EXPIRY, ?(Nat64.toNat(min_expiry / 1_000_000_000)));
      } else if (min_expiry > max_expiry) {
        min_expiry := max_expiry;
        metadata := Value.setNat(metadata, Sleepy.DEFAULT_ORDER_EXPIRY, ?max_expiry_seconds);
      };

      var default_expiry = Time64.SECONDS(Nat64.fromNat(Value.getNat(metadata, Sleepy.DEFAULT_ORDER_EXPIRY, 0)));
      if (default_expiry < min_expiry or default_expiry > max_expiry) {
        default_expiry := (min_expiry + max_expiry) / 2;
        metadata := Value.setNat(metadata, Sleepy.DEFAULT_ORDER_EXPIRY, ?(Nat64.toNat(default_expiry / 1_000_000_000)));
      };

      let now = Time64.nanos();
      let max_expires_at = now + max_expiry;
      let min_expires_at = now + min_expiry;
      let default_expires_at = now + default_expiry;
      var total_incoming_sell_amount = 0;
      var incoming_sells = RBTree.empty<(price : Nat), { index : Nat; amount : Nat }>();
      for (incoming in arg.sell_orders.vals()) {
        let incoming_index = RBTree.size(incoming_sells);

        let incoming_expires_at = Option.get(incoming.expires_at, default_expires_at);
        if (incoming_expires_at < min_expires_at) return #Err(#ExpiresTooSoon { expires_at = incoming_expires_at; index = incoming_index; minimum_expires_at = min_expires_at });
        if (incoming_expires_at > max_expires_at) return #Err(#ExpiresTooLate { expires_at = incoming_expires_at; index = incoming_index; maximum_expires_at = max_expires_at });

        if (incoming.price < min_price) return #Err(#SellPriceTooLow { incoming with index = incoming_index; minimum_price = min_price });

        let nearest_price = Sleepy.nearTick(incoming.price, price_tick);
        if (incoming.price != nearest_price) return #Err(#SellPriceTooFar { incoming with index = incoming_index; nearest_price });

        switch (RBTree.get(incoming_sells, Nat.compare, incoming.price)) {
          case (?found) return #Err(#DuplicateSellPrice { incoming with indexes = [found.index, incoming_index] });
          case _ ();
        };

        let min_amount = Nat.max(min_sell_amount, min_buy_amount / incoming.price);
        if (incoming.amount < min_amount) return #Err(#SellAmountTooLow { incoming with index = incoming_index; minimum_amount = min_amount });

        let nearest_amount = Sleepy.nearTick(incoming.amount, amount_tick);
        if (incoming.amount != nearest_amount) return #Err(#SellAmountTooFar { incoming with index = incoming_index; nearest_amount });

        total_incoming_sell_amount += incoming.amount;
        incoming_sells := RBTree.insert(incoming_sells, Nat.compare, incoming.price, { incoming with index = incoming_index });
      };

      var total_incoming_buy_amount = 0;
      var incoming_buys = RBTree.empty<(price : Nat), { index : Nat; amount : Nat }>();
      for (incoming in arg.buy_orders.vals()) {
        let incoming_index = RBTree.size(incoming_buys);
        if (incoming.price < min_price) return #Err(#BuyPriceTooLow { incoming with index = incoming_index; minimum_price = min_price });

        let nearest_price = Sleepy.nearTick(incoming.price, price_tick);
        if (incoming.price != nearest_price) return #Err(#BuyPriceTooFar { incoming with index = incoming_index; nearest_price });

        switch (RBTree.get(incoming_buys, Nat.compare, incoming.price)) {
          case (?found) return #Err(#DuplicateBuyPrice { incoming with indexes = [found.index, incoming_index] });
          case _ ();
        };

        let min_amount = Nat.max(min_sell_amount, min_buy_amount / incoming.price);
        if (incoming.amount < min_amount) return #Err(#BuyAmountTooLow { incoming with index = incoming_index; minimum_amount = min_amount });

        let nearest_amount = Sleepy.nearTick(incoming.amount, amount_tick);
        if (incoming.amount != nearest_amount) return #Err(#BuyAmountTooFar { incoming with index = incoming_index; nearest_amount });

        total_incoming_buy_amount += incoming.amount * incoming.price;
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

      user := getUser(caller);
      subaccount := Sleepy.getSubaccount(user, arg_subaccount);

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
        case (?found) return #Err(#SellPriceUnavailable { incoming_detail with price = incoming_price; order_id = found });
        case _ ();
      };
      for ((incoming_price, incoming_detail) in RBTree.entries(incoming_buys)) switch (RBTree.get(subaccount.buys, Nat.compare, incoming_price)) {
        case (?found) return #Err(#BuyPriceUnavailable { incoming_detail with price = incoming_price; order_id = found });
        case _ ();
      };

      let min_rate_limit = Time64.MILLI(10);
      var none_rate_limit = Time64.MILLI(Nat64.fromNat(Value.getNat(metadata, Sleepy.AUTH_NONE_RATE_LIMIT, 0)));
      if (none_rate_limit < min_rate_limit) {
        none_rate_limit := min_rate_limit;
        metadata := Value.setNat(metadata, Sleepy.AUTH_NONE_RATE_LIMIT, ?10);
      };
      var credit_rate_limit = Time64.MILLI(Nat64.fromNat(Value.getNat(metadata, Sleepy.AUTH_CREDIT_RATE_LIMIT, 0)));
      if (credit_rate_limit < min_rate_limit) {
        credit_rate_limit := min_rate_limit;
        metadata := Value.setNat(metadata, Sleepy.AUTH_CREDIT_RATE_LIMIT, ?10);
      };

      let icrc2_rates = Option.get(Value.metaValueMapArray(metadata, Sleepy.AUTH_ICRC2_RATES), []);

      let none_available_time = last_placed + none_rate_limit;
      let credit_available_time = user.credit_last_updated + credit_rate_limit;
      let auto_errors = Buffer.Buffer<Sleepy.Auto_Failure>(3 + icrc2_rates.size()); // none, credit, eepy, icp, ckbtc

      let self = Principal.fromActor(Self);
      let valid_auth = switch (arg.authorization) {
        case (?#None) switch (Sleepy.authNoneCheck(now, none_available_time, last_placer)) {
          case (#Err err) return #Err(#Unauthorized err);
          case _ #None;
        };
        case (?#Credit) switch (Sleepy.authCreditCheck(user, now, credit_available_time)) {
          case (#Err err) return #Err(#Unauthorized err);
          case _ #Credit;
        };
        case (?#ICRC2 auth) switch (await* Sleepy.authIcrc2Check(icrc2_rates, auth, user_account, self)) {
          case (#Err(#ICRC2 err)) return #Err(#Unauthorized(#ICRC2 err));
          case (#Err(#Text err)) return Error.text(err);
          case (#Ok payment) #ICRC2 payment;
        };
        case _ switch (await* Sleepy.authAutoCheck(auto_errors, now, none_available_time, last_placer, user, credit_available_time, icrc2_rates, user_account, self)) {
          case (#Ok selected) selected;
          case _ return #Err(#Unauthorized(#Automatic { failures = Buffer.toArray(auto_errors) }));
        };
        // note: we dont refresh user+subaccount because we're gonna do it later after the next await
      };

      // all arg has been validated, now to dedupe it
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

      switch (user.place_locked) {
        case (?locked_at) return #Err(#Locked { timestamp = locked_at });
        case _ ();
      };
      is_locker := true;
      user := saveUser(caller, { Sleepy.saveSubaccount(user, arg_subaccount, subaccount) with place_locked = ?now });
      func unlock<Return>(ret : Return) : Return {
        ignore saveUser(caller, { user with place_locked = null });
        ret;
      };

      let self_account = { owner = self; subaccount = null };
      let self_approval = { account = user_account; spender = self_account };

      let (sell_balance_res, sell_approval_res, buy_balance_res, buy_approval_res) = (sell_token.icrc1_balance_of(user_account), sell_token.icrc2_allowance(self_approval), buy_token.icrc1_balance_of(user_account), buy_token.icrc2_allowance(self_approval));

      let (sell_balance, sell_approval, buy_balance, buy_approval) = (await sell_balance_res, await sell_approval_res, await buy_balance_res, await buy_approval_res);

      user := getUser(caller);
      subaccount := Sleepy.getSubaccount(user, arg_subaccount);

      let sell_amount = subaccount.sell_amount;
      let min_sell_balance_approval = (sell_amount.initial - sell_amount.filled) + total_incoming_sell_amount;

      if (sell_balance < min_sell_balance_approval) return unlock(#Err(#InsufficientSellFunds { balance = sell_balance; minimum_balance = min_sell_balance_approval }));
      if (sell_approval.allowance < min_sell_balance_approval) return unlock(#Err(#InsufficientSellAllowance { sell_approval with minimum_allowance = min_sell_balance_approval }));

      let buy_amount = subaccount.buy_amount;
      let min_buy_balance_approval = (buy_amount.initial - buy_amount.filled) + total_incoming_buy_amount;

      if (buy_balance < min_buy_balance_approval) return unlock(#Err(#InsufficientBuyFunds { balance = buy_balance; minimum_balance = min_buy_balance_approval }));
      if (buy_approval.allowance < min_buy_balance_approval) return unlock(#Err(#InsufficientBuyAllowance { buy_approval with minimum_allowance = min_buy_balance_approval }));

      // writing time
      let authorized = switch valid_auth {
        case (#None) {
          last_placed := now;
          last_placer := caller;
          #None;
        };
        case (#Credit) {
          user := {
            user with credit = user.credit - 1;
            credit_last_updated = now;
          };
          #Credit;
        };
        case (#ICRC2 payment) {
          let token = ICRC_1_Types.genActor(payment.canister_id);
          let xfer_arg = {
            to = self_account;
            fee = ?payment.fee;
            spender_subaccount = self_account.subaccount;
            from = user_account;
            memo = null;
            created_at_time = null;
            amount = payment.amount;
          };
          let xfer = switch (await token.icrc2_transfer_from(xfer_arg)) {
            case (#Err err) return #Err(#Unauthorized(#ICRC2(#TransferFromFailed err)));
            case (#Ok block_id) block_id;
          };
          user := getUser(caller);
          subaccount := Sleepy.getSubaccount(user, arg_subaccount);
          #ICRC2 { payment with xfer };
        };
      };

      for ((incoming_price, incoming_detail) in RBTree.entries(incoming_sells)) {

      };
      for ((incoming_price, incoming_detail) in RBTree.entries(incoming_buys)) {

      };

      // todo: unlock user
      // todo: trim users
      // todo: trim dedupes
      #Ok([]);
    } catch e {
      if (is_locker) ignore saveUser(caller, { user with place_locked = null });
      Error.error(e);
    };
  };

  public shared ({ caller }) func sleepyswap_cancel(arg : Sleepy.CancelArg) : async Sleepy.CancelResult {
    [];
  };

  public shared ({ caller }) func sleepyswap_match() : async () {

  };

  func getUser(p : Principal) : Sleepy.User = switch (RBTree.get(users, Principal.compare, p)) {
    case (?found) found;
    case _ ({
      id = Sleepy.recycleId(user_ids);
      credit = 0;
      credit_last_updated = 0;
      place_locked = null;
      subaccounts = RBTree.empty();
      subaccount_ids = RBTree.empty();
    });
  };
  func saveUser(p : Principal, user : Sleepy.User) : Sleepy.User {
    users := RBTree.insert(users, Principal.compare, p, user);
    user_ids := RBTree.insert(user_ids, Nat.compare, user.id, p);
    user;
  };
};
