import Principal "mo:base/Principal";
import Nat "mo:base/Nat";
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
  var orders_by_expiry : Sleepy.Expiries = RBTree.empty();
  // 1st round - expiry
  // 2nd round - deletion by ttl

  var sell_book : Sleepy.Book = RBTree.empty();
  var buy_book : Sleepy.Book = RBTree.empty();

  var sell_amount = Sleepy.newAmount(0);
  var buy_amount = Sleepy.newAmount(0); // in buy unit

  var trade_id = 0;
  var trades = RBTree.empty<Nat, Sleepy.Trade>();

  var user_ids = RBTree.empty<Nat, Principal>();
  var users = RBTree.empty<Principal, Sleepy.User>();

  var last_none_placed = 0 : Nat64;
  var last_none_placer = Management.principal();

  var credit_id = 0;
  var credits = RBTree.empty<Nat, Sleepy.Credit>();
  var credits_by_expiry : Sleepy.Expiries = RBTree.empty();

  var place_dedupes = RBTree.empty<Sleepy.PlaceArg, Sleepy.PlaceOk>();

  var block_id = 0;
  var blocks = RBTree.empty<Nat, Value.Type>();

  func trim() {
    // trim all stable vars (orders, trades, users, credits, dedupes, blocks)
  };

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

    func unlock<Return>(ret : Return) : Return {
      ignore saveUser(caller, { user with place_locked = null });
      ret;
    };
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
      var incoming_sells : Sleepy.Incoming = RBTree.empty();
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
        incoming_sells := RBTree.insert(incoming_sells, Nat.compare, incoming.price, { incoming with index = incoming_index; expires_at = incoming_expires_at });
      };

      var total_incoming_buy_amount = 0;
      var incoming_buys : Sleepy.Incoming = RBTree.empty();
      for (incoming in arg.buy_orders.vals()) {
        let incoming_index = RBTree.size(incoming_buys);

        let incoming_expires_at = Option.get(incoming.expires_at, default_expires_at);
        if (incoming_expires_at < min_expires_at) return #Err(#ExpiresTooSoon { expires_at = incoming_expires_at; index = incoming_index; minimum_expires_at = min_expires_at });
        if (incoming_expires_at > max_expires_at) return #Err(#ExpiresTooLate { expires_at = incoming_expires_at; index = incoming_index; maximum_expires_at = max_expires_at });

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
        incoming_buys := RBTree.insert(incoming_buys, Nat.compare, incoming.price, { incoming with index = incoming_index; expires_at = incoming_expires_at });
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

      let min_rate_limit = Time64.SECONDS(1);
      var none_rate_limit = Time64.SECONDS(Nat64.fromNat(Value.getNat(metadata, Sleepy.AUTH_NONE_RATE_LIMIT, 0)));
      if (none_rate_limit < min_rate_limit) {
        none_rate_limit := min_rate_limit;
        metadata := Value.setNat(metadata, Sleepy.AUTH_NONE_RATE_LIMIT, ?1);
      };

      let icrc2_rates = Option.get(Value.metaValueMapArray(metadata, Sleepy.AUTH_ICRC2_RATES), []);

      let none_available_time = last_none_placed + none_rate_limit;
      let auto_errors = Buffer.Buffer<Sleepy.Auto_Failure>(3 + icrc2_rates.size()); // none, credit, eepy, icp, ckbtc

      let self = Principal.fromActor(Self);
      let valid_auth = switch (arg.authorization) {
        case (?#None) switch (Sleepy.authNoneCheck(now, none_available_time, last_none_placer)) {
          case (#Err err) return #Err(#Unauthorized err);
          case _ #None;
        };
        case (?#Credit) switch (Sleepy.authCreditCheck(user, now, credits)) {
          case (#Err err) return #Err(#Unauthorized err);
          case _ #Credit;
        };
        case (?#ICRC2 auth) switch (await* Sleepy.authIcrc2Check(icrc2_rates, auth, user_account, self)) {
          case (#Err(#ICRC2 err)) return #Err(#Unauthorized(#ICRC2 err));
          case (#Err(#Text err)) return Error.text(err);
          case (#Ok payment) #ICRC2 payment;
        };
        case _ switch (await* Sleepy.authAutoCheck(auto_errors, now, none_available_time, last_none_placer, user, credits, icrc2_rates, user_account, self)) {
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

      let self_account = { owner = self; subaccount = null };
      let self_approval = { account = user_account; spender = self_account };

      let (sell_balance_res, sell_approval_res, buy_balance_res, buy_approval_res) = (sell_token.icrc1_balance_of(user_account), sell_token.icrc2_allowance(self_approval), buy_token.icrc1_balance_of(user_account), buy_token.icrc2_allowance(self_approval));

      let (sell_balance, sell_approval, buy_balance, buy_approval) = (await sell_balance_res, await sell_approval_res, await buy_balance_res, await buy_approval_res);

      user := getUser(caller);
      subaccount := Sleepy.getSubaccount(user, arg_subaccount);

      let min_sell_balance_approval = (subaccount.sell_amount.initial - subaccount.sell_amount.filled) + total_incoming_sell_amount;

      if (sell_balance < min_sell_balance_approval) return unlock(#Err(#InsufficientSellFunds { balance = sell_balance; minimum_balance = min_sell_balance_approval }));
      if (sell_approval.allowance < min_sell_balance_approval) return unlock(#Err(#InsufficientSellAllowance { sell_approval with minimum_allowance = min_sell_balance_approval }));

      let min_buy_balance_approval = (subaccount.buy_amount.initial - subaccount.buy_amount.filled) + total_incoming_buy_amount;

      if (buy_balance < min_buy_balance_approval) return unlock(#Err(#InsufficientBuyFunds { balance = buy_balance; minimum_balance = min_buy_balance_approval }));
      if (buy_approval.allowance < min_buy_balance_approval) return unlock(#Err(#InsufficientBuyAllowance { buy_approval with minimum_allowance = min_buy_balance_approval }));

      // write mode
      let authorized = switch valid_auth {
        case (#None) {
          last_none_placed := now;
          last_none_placer := caller;
          let min_reward = 1;
          var reward = Value.getNat(metadata, Sleepy.AUTH_NONE_CREDIT_REWARD, 0);
          if (reward < min_reward) {
            reward := min_reward;
            metadata := Value.setNat(metadata, Sleepy.AUTH_NONE_CREDIT_REWARD, ?reward);
          };
          let min_reward_expiry = Time64.DAYS(1);
          var reward_expiry = Time64.SECONDS(Nat64.fromNat(Value.getNat(metadata, Sleepy.AUTH_NONE_CREDIT_REWARD_EXPIRY, 0)));
          if (reward_expiry < min_reward_expiry) {
            reward_expiry := min_reward_expiry;
            metadata := Value.setNat(metadata, Sleepy.AUTH_NONE_CREDIT_REWARD_EXPIRY, ?Nat64.toNat(reward_expiry / 1_000_000_000));
          };
          let new_cid = credit_id;
          credits := RBTree.insert(credits, Nat.compare, new_cid, { owner = user.id; amount = reward; used = 0 });
          credits_by_expiry := Sleepy.insertExpiry(credits_by_expiry, reward_expiry, new_cid);
          user := {
            user with credits_by_expiry = Sleepy.insertExpiry(user.credits_by_expiry, reward_expiry, new_cid)
          };
          #None;
        };
        case (#Credit) switch (Sleepy.findActiveCredit(user, now, credits)) {
          case (#Err) return #Err(#Unauthorized(#Credit(#OutOfCredit)));
          case (#Ok(cr_id, cr)) {
            credits := RBTree.insert(credits, Nat.compare, cr_id, { cr with used = cr.used + 1 });
            #Credit;
          };
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
            case (#Ok xfer_id) xfer_id;
          };
          user := getUser(caller);
          subaccount := Sleepy.getSubaccount(user, arg_subaccount);
          let min_reward = 3;
          var reward = Value.getNat(metadata, Sleepy.AUTH_ICRC2_CREDIT_REWARD, 0);
          if (reward < min_reward) {
            reward := min_reward;
            metadata := Value.setNat(metadata, Sleepy.AUTH_ICRC2_CREDIT_REWARD, ?reward);
          };
          let min_reward_expiry = Time64.DAYS(4);
          var reward_expiry = Time64.SECONDS(Nat64.fromNat(Value.getNat(metadata, Sleepy.AUTH_NONE_CREDIT_REWARD_EXPIRY, 0)));
          if (reward_expiry < min_reward_expiry) {
            reward_expiry := min_reward_expiry;
            metadata := Value.setNat(metadata, Sleepy.AUTH_NONE_CREDIT_REWARD_EXPIRY, ?Nat64.toNat(reward_expiry / 1_000_000_000));
          };
          let new_cid = credit_id;
          credits := RBTree.insert(credits, Nat.compare, new_cid, { owner = user.id; amount = reward; used = 0 });
          credits_by_expiry := Sleepy.insertExpiry(credits_by_expiry, reward_expiry, new_cid);
          user := {
            user with credits_by_expiry = Sleepy.insertExpiry(user.credits_by_expiry, reward_expiry, new_cid)
          };
          #ICRC2 { payment with xfer };
        };
      };

      let res_buff = Buffer.Buffer<Nat>(RBTree.size(incoming_sells) + RBTree.size(incoming_buys));
      for ((incoming_price, incoming_detail) in RBTree.entries(incoming_sells)) {
        let new_oid = order_id;
        let new_order = Sleepy.newOrder(now, user.id, subaccount.id, false, incoming_price, incoming_detail, authorized);
        orders := RBTree.insert(orders, Nat.compare, new_oid, new_order);
        orders_by_expiry := Sleepy.insertExpiry(orders_by_expiry, incoming_detail.expires_at, new_oid);
        order_id += 1;

        subaccount := Sleepy.subaccountNewOrder(subaccount, new_oid, new_order);

        sell_book := Sleepy.insertPrice(sell_book, new_oid, new_order);

        res_buff.add(new_oid);
      };
      sell_amount := {
        sell_amount with initial = sell_amount.initial + total_incoming_sell_amount
      };

      for ((incoming_price, incoming_detail) in RBTree.entries(incoming_buys)) {
        let new_oid = order_id;
        let new_order = Sleepy.newOrder(now, user.id, subaccount.id, true, incoming_price, incoming_detail, authorized);
        orders := RBTree.insert(orders, Nat.compare, new_oid, new_order);
        orders_by_expiry := Sleepy.insertExpiry(orders_by_expiry, incoming_detail.expires_at, new_oid);
        order_id += 1;

        subaccount := Sleepy.subaccountNewOrder(subaccount, new_oid, new_order);

        buy_book := Sleepy.insertPrice(buy_book, new_oid, new_order);

        res_buff.add(new_oid);
      };
      buy_amount := {
        buy_amount with initial = buy_amount.initial + total_incoming_buy_amount
      };
      user := Sleepy.saveSubaccount(user, arg_subaccount, subaccount);
      unlock();

      let phash = switch (RBTree.max(blocks)) {
        case (?(_, max_block)) ?Value.hash(max_block);
        case _ null;
      };
      let new_block = Sleepy.genValue(caller, now, phash, #Place { arg; authorized; incoming_buys; incoming_sells });
      let new_block_id = block_id;
      blocks := RBTree.insert(blocks, Nat.compare, new_block_id, new_block);
      block_id += 1;

      #Ok(Buffer.toArray(res_buff));
    } catch e {
      if (is_locker) unlock();
      Error.error(e);
    };
  };
  // todo: include credit reward in block
  // todo: include credit reward in return(?)

  public shared ({ caller }) func sleepyswap_cancel(arg : Sleepy.CancelArg) : async Sleepy.CancelResult {
    let user_account = { owner = caller; subaccount = arg.subaccount };
    if (not Account.validate(user_account)) return Error.text("Caller account is not valid");

    let batch_size = arg.orders.size();
    if (batch_size == 0) return Error.text("Orders cannot be empty");
    let max_batch = Value.getNat(metadata, Sleepy.MAX_ORDER_BATCH, 0);
    if (max_batch > 0 and batch_size > max_batch) return #Err(#BatchTooLarge { batch_size = batch_size; maximum_batch_size = max_batch });

    var user = switch (RBTree.get(users, Principal.compare, caller)) {
      case (?found) found;
      case _ return Error.text("Caller is not a user");
    };
    let arg_subaccount = Account.denull(arg.subaccount);
    var subaccount = Sleepy.getSubaccount(user, arg_subaccount);

    let res_buff = Buffer.Buffer<Sleepy.OrderCancelResult>(batch_size);
    var closed_set = RBTree.empty<Nat, ()>();
    var expired_set = RBTree.empty<Nat, ()>();
    let def_p = Management.principal();
    let now = Time64.nanos();
    // todo: remove the order from the book
    label looping for (oid in arg.orders.vals()) {
      var order = switch (RBTree.get(orders, Nat.compare, oid)) {
        case (?found) found;
        case _ {
          res_buff.add(#NotFound);
          continue looping;
        };
      };
      if (order.owner != user.id) {
        let p = Option.get(RBTree.get(user_ids, Nat.compare, order.owner), def_p);
        res_buff.add(#NotOwner { owner = p; caller });
        continue looping;
      };
      if (order.subaccount != subaccount.id) {
        let s = Option.get(RBTree.get(user.subaccount_ids, Nat.compare, order.subaccount), "" : Blob);
        res_buff.add(#NotSubaccount { subaccount = s; caller_subaccount = arg_subaccount });
        continue looping;
      };
      switch (order.closed) {
        case (?found) {
          res_buff.add(#Closed found);
          continue looping;
        };
        case _ ();
      };
      switch (RBTree.maxKey(order.trades)) {
        case (?max) switch (RBTree.get(trades, Nat.compare, max)) {
          case (?trade) switch (Sleepy.tradeStory(trade)) {
            case (#MakerToEscrow(#Pending)) ();
            case (#MakerToEscrow(#Failed _)) ();
            case (#EscrowRefundMaker(#Ok _)) ();
            case (#EscrowToTaker(#Ok _)) ();
            case _ {
              res_buff.add(#Locked { trade = max });
              continue looping;
            };
          };
          case _ ();
        };
        case _ ();
      };
      let reason = if (order.expires_at < now) {
        expired_set := RBTree.insert(expired_set, Nat.compare, oid, ());
        #Expired;
      } else {
        closed_set := RBTree.insert(closed_set, Nat.compare, oid, ());
        #Canceled;
      };
      order := { order with closed = ?{ at = now; reason } };
      orders := RBTree.insert(orders, Nat.compare, oid, order);
      orders_by_expiry := Sleepy.deleteExpiry(orders_by_expiry, order.expires_at, oid);
      // orders_by_expiry := Sleepy.insertExpiry(orders_by_expiry, order.expires_at + ttl, oid); // todo: finish this

      if (order.is_buy) {
        buy_book := Sleepy.deletePrice(buy_book, oid, order);
        buy_amount := {
          buy_amount with initial = buy_amount.initial - (order.amount.initial * order.price);
          filled = buy_amount.filled - (order.amount.filled * order.price);
        };
        subaccount := {
          subaccount with buys = RBTree.delete(subaccount.buys, Nat.compare, order.price);
          buy_amount = {
            subaccount.buy_amount with initial = buy_amount.initial - (order.amount.initial * order.price);
            filled = buy_amount.filled - (order.amount.filled * order.price);
          };
        };
      } else {
        sell_book := Sleepy.deletePrice(sell_book, oid, order);
        sell_amount := {
          sell_amount with initial = sell_amount.initial - order.amount.initial;
          filled = sell_amount.filled - order.amount.filled;
        };
        subaccount := {
          subaccount with sells = RBTree.delete(subaccount.sells, Nat.compare, order.price);
          sell_amount = {
            subaccount.sell_amount with initial = sell_amount.initial - order.amount.initial;
            filled = sell_amount.filled - order.amount.filled;
          };
        };
      };
      res_buff.add(#Ok);
    };
    user := saveUser(caller, Sleepy.saveSubaccount(user, arg_subaccount, subaccount));

    let closes = RBTree.arrayKey(closed_set);
    if (closes.size() > 0) {
      // todo: blockify
    };
    let expires = RBTree.arrayKey(expired_set);
    if (expires.size() > 0) {
      // todo: blockify
    };
    #Ok(Buffer.toArray(res_buff));
  };
  // todo: set max capacity to dedupes
  // todo: set max capacity to orders?
  // todo: rename sleepyswap to something else
  public shared ({ caller }) func sleepyswap_match() : async () {

  };

  public shared ({ caller }) func sleepyswap_settle() : async () {

  };

  public shared ({ caller }) func sleepyswap_archive() : async () {

  };

  public shared ({ caller }) func sleepyswap_trim_orders() : async () {

  };

  public shared ({ caller }) func sleepyswap_trim_dedupes() : async () {

  };

  public shared ({ caller }) func sleepyswap_trim_credits() : async () {

  };

  func getUser(p : Principal) : Sleepy.User = switch (RBTree.get(users, Principal.compare, p)) {
    case (?found) found;
    case _ ({
      id = Sleepy.recycleId(user_ids);
      place_locked = null;
      subaccounts = RBTree.empty();
      subaccount_ids = RBTree.empty();
      credits_unused = 0;
      credits_by_expiry = RBTree.empty();
    });
  };
  func saveUser(p : Principal, user : Sleepy.User) : Sleepy.User {
    users := RBTree.insert(users, Principal.compare, p, user);
    user_ids := RBTree.insert(user_ids, Nat.compare, user.id, p);
    user;
  };
};
