import RBTree "../util/motoko/StableCollections/RedBlackTree/RBTree";
import Result "../util/motoko/Result";
import ICRC_1_Types "../util/motoko/ICRC-1/Types";
import Error "../util/motoko/Error";
import Order "mo:base/Order";
import Blob "mo:base/Blob";
import Nat64 "mo:base/Nat64";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Principal "mo:base/Principal";
import Text "mo:base/Text";
import Buffer "mo:base/Buffer";
import Option "mo:base/Option";
import OptionX "../util/motoko/Option";
import Value "../util/motoko/Value";
import Queue "../util/motoko/StableCollections/Queue";
import Account "../util/motoko/ICRC-1/Account";
import Management "../util/motoko/Management";

module {

  public let SELL_TOKEN = "sleepyswap:sell_icrc2_canister_id";
  public let BUY_TOKEN = "sleepyswap:buy_icrc2_canister_id";
  public let MAX_ORDER_BATCH = "sleepyswap:max_order_batch_size";

  public let AMOUNT_TICK = "sleepyswap:amount_tick";
  public let PRICE_TICK = "sleepyswap:price_tick";
  public let MAKER_FEE_NUMER = "sleepyswap:maker_trading_fee_numerator";
  public let TAKER_FEE_NUMER = "sleepyswap:taker_trading_fee_numerator";
  public let TRADING_FEE_DENOM = "sleepyswap:trading_fee_denomination";
  public let MIN_BUY_AMOUNT = "sleepyswap:minimum_buy_amount";
  public let MIN_SELL_AMOUNT = "sleepyswap:minimum_sell_amount";
  public let MIN_PRICE = "sleepyswap:minimum_price";
  public let TTL = "sleepyswap:time_to_live";
  public let DEFAULT_EXPIRY = "sleepyswap:default_expiry";
  public let MAX_EXPIRY = "sleepyswap:max_expiry";
  public let MIN_EXPIRY = "sleepyswap:min_expiry";
  public let AUTH_NONE_PLACE_GLOBAL_RATE_LIMIT = "sleepyswap:auth_none_place_global_rate_limit"; // millisecond
  public let AUTH_CREDIT_PLACE_RATE_LIMIT = "sleepyswap:auth_credit_place_rate_limit"; // millisecond
  public let AUTH_ICRC2_RATES = "sleepyswap:auth_icrc2_rates"; // map(canisterid, map("place", amount))

  public let TX_WINDOW = "sleepyswap:tx_window";
  public let PERMITTED_DRIFT = "sleepyswap:permitted_drift";

  // todo: memo size

  public type Order = {
    created_at_time : Nat64;
    owner : Nat; // user id
    subaccount : Nat; // user's subaccount id
    is_buy : Bool;
    price : Nat;
    amount : Nat; // in sell unit
    locked : Nat;
    filled : Nat;
    trades : RBTree.Type<Nat, ()>; // trade ids
    close : ?{
      #Canceled : { timestamp : Nat64 };
      #InsufficientFunds : {
        balance : Nat;
        minimum_balance : Nat;
        timestamp : Nat64;
      };
      #InsufficientAllowance : {
        allowance : Nat;
        minimum_allowance : Nat;
        timestamp : Nat64;
      };
      #Filled : { timestamp : Nat64 };
    };
  };
  public type Book = RBTree.Type<Nat, RBTree.Type<Nat, ()>>; // price to order ids
  type Call<OkT, ErrT> = {
    #Calling : { caller : Principal; timestamp : Nat64 };
    #Called : Result.Type<OkT, ErrT>;
  }; // attempts = calls / 2
  public type Trade = {
    maker : { id : Nat; fee : Nat }; // track amount and account too?
    taker : { id : Nat; fee : Nat };

    maker_to_escrow : RBTree.Type<Nat64, Call<Nat, ICRC_1_Types.TransferFromError>>;
    taker_to_maker : RBTree.Type<Nat64, Call<Nat, ICRC_1_Types.TransferFromError>>;
    escrow_to : {
      receiver : Nat; // order id of maker (if taker_to_maker fails), or order id of taker (if taker_to_maker succeeds)
      transfer : RBTree.Type<Nat64, Call<Nat, ICRC_1_Types.TransferError>>;
    };
  };
  type Amount = { initial : Nat; locked : Nat; filled : Nat };
  func newAmount() : Amount = { initial = 0; locked = 0; filled = 0 };
  type Subaccount = {
    id : Nat;
    orders : RBTree.Type<(id : Nat), ()>;
    // note: cant place order on same price
    sells : RBTree.Type<(price : Nat), (order : Nat)>;
    sell_amount : Amount; // in sell unit
    buys : RBTree.Type<(price : Nat), (order : Nat)>;
    buy_amount : Amount; // in buy unit
    trades : RBTree.Type<(id : Nat), ()>;
  };
  type Subaccounts = RBTree.Type<Blob, Subaccount>;
  public func getSubaccount<K>(user : User, subaccount : Blob) : Subaccount = switch (RBTree.get(user.subaccounts, Blob.compare, subaccount)) {
    case (?found) found;
    case _ ({
      id = recycleId(user.subaccount_ids);
      orders = RBTree.empty();
      sells = RBTree.empty();
      sell_amount = newAmount();
      buys = RBTree.empty();
      buy_amount = newAmount();
      trades = RBTree.empty();
    });
  };
  public func saveSubaccount(user : User, subaccount_b : Blob, subaccount : Subaccount) : User = {
    user with
    subaccounts = RBTree.insert(user.subaccounts, Blob.compare, subaccount_b, subaccount);
    subaccount_ids = RBTree.insert(user.subaccount_ids, Nat.compare, subaccount.id, subaccount_b);
  };
  public type User = {
    id : Nat;
    credit : Nat; // todo: credit last updated
    credit_last_updated : Nat64;
    place_locked : ?Nat64;
    subaccounts : RBTree.Type<Blob, Subaccount>;
    subaccount_ids : RBTree.Type<Nat, Blob>;
  };
  public type Users = RBTree.Type<Principal, User>;

  type OrderArg = { price : Nat; amount : Nat; expires_at : ?Nat64 };
  public type PlaceArg = {
    // todo: payment, add to placeCompare
    subaccount : ?Blob;
    created_at_time : ?Nat64;
    memo : ?Blob; // todo: check memo
    authorization : ?Authorization; // if not set, it's auto
    buy_orders : [OrderArg];
    sell_orders : [OrderArg];
  };
  public type PlaceOk = [Nat];
  public type PlaceError = {
    #GenericError : Error.Type;
    #BatchTooLarge : { batch_size : Nat; maximum_batch_size : Nat };
    #ExpiresTooSoon : {
      expires_at : Nat64;
      index : Nat;
      minimum_expires_at : Nat64;
    };
    #ExpiresTooLate : {
      expires_at : Nat64;
      index : Nat;
      maximum_expires_at : Nat64;
    };
    #BuyAmountTooLow : { amount : Nat; index : Nat; minimum_amount : Nat };
    #SellAmountTooLow : { amount : Nat; index : Nat; minimum_amount : Nat };
    #BuyPriceTooFar : { price : Nat; index : Nat; nearest_price : Nat };
    #SellPriceTooFar : { price : Nat; index : Nat; nearest_price : Nat };
    #BuyAmountTooFar : { price : Nat; index : Nat; nearest_amount : Nat };
    #SellAmountTooFar : { price : Nat; index : Nat; nearest_amount : Nat };
    #DuplicateSellPrice : { price : Nat; indexes : [Nat] };
    #DuplicateBuyPrice : { price : Nat; indexes : [Nat] };
    #OrdersOverlap : {
      sell_index : Nat;
      sell_price : Nat;
      buy_index : Nat;
      buy_price : Nat;
    };
    #BuyPriceTooHigh : { price : Nat; index : Nat; maximum_price : Nat };
    #SellPriceTooHigh : { price : Nat; index : Nat; maximum_price : Nat };
    #BuyPriceTooLow : { price : Nat; index : Nat; minimum_price : Nat };
    #SellPriceTooLow : { price : Nat; index : Nat; minimum_price : Nat };
    #SellPriceUnavailable : { price : Nat; index : Nat; order_id : Nat };
    #BuyPriceUnavailable : { price : Nat; index : Nat; order_id : Nat };
    #Unauthorized : Unauthorized;
    #Locked : { timestamp : Nat64 };
    #InsufficientBuyFunds : { balance : Nat; minimum_balance : Nat };
    #InsufficientSellFunds : { balance : Nat; minimum_balance : Nat };
    #InsufficientBuyAllowance : {
      allowance : Nat;
      minimum_allowance : Nat;
    };
    #InsufficientSellAllowance : {
      allowance : Nat;
      minimum_allowance : Nat;
    };
    #Duplicate : { duplicate_of : PlaceOk };
    #CreatedInFuture : { ledger_time : Nat64 };
    #TooOld;
  };
  public type PlaceResult = Result.Type<PlaceOk, PlaceError>;

  func orderCompare(a : OrderArg, b : OrderArg) : Order.Order {
    var c = Nat.compare(a.price, b.price);
    switch c {
      case (#equal) ();
      case _ return c;
    };
    c := Nat.compare(a.amount, b.amount);
    switch c {
      case (#equal) ();
      case _ return c;
    };
    #equal;
  };
  public func placeCompare(a : PlaceArg, b : PlaceArg) : Order.Order {
    // todo: add caller
    // todo: add new fields from arg
    // todo: should created_at_time be first check for easier trim
    var c = OptionX.compare(a.subaccount, b.subaccount, Blob.compare);
    switch c {
      case (#equal) ();
      case _ return c;
    };
    c := OptionX.compare(a.created_at_time, b.created_at_time, Nat64.compare);
    switch c {
      case (#equal) ();
      case _ return c;
    };
    c := OptionX.compare(a.memo, b.memo, Blob.compare);
    switch c {
      case (#equal) ();
      case _ return c;
    };
    c := Nat.compare(a.buy_orders.size(), b.buy_orders.size());
    switch c {
      case (#equal) ();
      case _ return c;
    };
    c := Nat.compare(a.sell_orders.size(), b.sell_orders.size());
    switch c {
      case (#equal) ();
      case _ return c;
    };
    for (i in Iter.range(0, a.buy_orders.size() - 1)) {
      c := orderCompare(a.buy_orders[i], b.buy_orders[i]);
      switch c {
        case (#equal) ();
        case _ return c;
      };
    };
    for (i in Iter.range(0, a.sell_orders.size() - 1)) {
      c := orderCompare(a.sell_orders[i], b.sell_orders[i]);
      switch c {
        case (#equal) ();
        case _ return c;
      };
    };
    #equal;
  };

  public type CancelArg = [{ id : Nat; subaccount : ?Blob }];
  public type CancelError = {
    #GenericError : Error.Type;
    #GenericBatchError : Error.Type;
  };
  public type CancelResult = [?Result.Type<(), CancelError>];

  public func recycleId<K>(ids : RBTree.Type<Nat, K>) : Nat = switch (RBTree.minKey(ids), RBTree.maxKey(ids)) {
    case (?min_id, ?max_id) if (min_id > 0) min_id - 1 else max_id + 1;
    case _ 0;
  };

  public func nearTick(n : Nat, tick : Nat) : Nat {
    let lower = (n / tick) * tick;
    let upper = lower + tick;
    if (n - lower <= upper - n) lower else upper;
  };

  type ICRC2_Auth = { canister_id : Principal; amount : ?Nat };
  public type Authorization = {
    #None;
    #Credit;
    #ICRC2 : ICRC2_Auth;
    // #Custom : { options : [{ #None; #Credit; #ICRC2 : ICRC2_Auth }] };
  };
  public type Authorized = {
    #None;
    #Credit;
    #ICRC2 : { canister_id : Principal; xfer : Nat };
  };
  type None_Failure = {
    #TemporarilyUnavailable : {
      time : Nat64;
      available_time : Nat64;
      used_by : Principal;
    };
  };
  type Credit_Failure = {
    #TemporarilyUnavailable : { time : Nat64; available_time : Nat64 };
    #OutOfCredit;
  };
  type ICRC2_Failure = {
    #BadCanister : {
      canister_id : Principal;
      expected_canister_ids : [Principal];
    };
    #BadAmount : { amount : Nat; expected_amount : Nat };
    #TransferFromFailed : ICRC_1_Types.TransferFromError;
  };
  public type Auto_Failure = {
    #None : None_Failure;
    #Credit : Credit_Failure;
    #ICRC2 : ICRC2_Failure;
  };
  public type Unauthorized = {
    #None : None_Failure;
    #Credit : Credit_Failure;
    #ICRC2 : ICRC2_Failure;
    #Automatic : { failures : [Auto_Failure] };
    // #Custom : Auto_Failure;
  };

  public func authNoneCheck(now : Nat64, none_available_time : Nat64, last_placer : Principal) : Result.Type<(), { #None : None_Failure }> = if (now < none_available_time) return #Err(#None(#TemporarilyUnavailable { time = now; available_time = none_available_time; used_by = last_placer })) else #Ok;

  public func authCreditCheck(user : User, now : Nat64, credit_available_time : Nat64) : Result.Type<(), { #Credit : Credit_Failure }> {
    if (user.credit == 0) return #Err(#Credit(#OutOfCredit));
    if (now < credit_available_time) return #Err(#Credit(#TemporarilyUnavailable { time = now; available_time = credit_available_time }));
    #Ok;
  };

  public func authIcrc2Check(icrc2_rates : [(Value.Type, Value.Type)], auth : ICRC2_Auth) : Result.Type<(), { #ICRC2 : ICRC2_Failure; #Text : Text }> {
    if (icrc2_rates.size() == 0) return #Err(#ICRC2(#BadCanister { auth with expected_canister_ids = [] }));
    let icrc2_rates_map = Value.mapArrayToPrincipalMap(icrc2_rates);
    let rates = switch (RBTree.get(icrc2_rates_map, Principal.compare, auth.canister_id)) {
      case (?#Map found) RBTree.fromArray(found, Text.compare);
      case _ return #Err(#ICRC2(#BadCanister { auth with expected_canister_ids = RBTree.arrayKey(icrc2_rates_map) }));
    };
    let expected_amount = Value.getNat(rates, "place", 0);
    if (expected_amount == 0) return #Err(#Text("Metadata `" # AUTH_ICRC2_RATES # "." # Principal.toText(auth.canister_id) # ".place` is not properly set"));

    switch (auth.amount) {
      case (?amount) if (amount != expected_amount) return #Err(#ICRC2(#BadAmount { amount; expected_amount }));
      case _ ();
    };
    #Ok;
  };

  public func authAutoCheck(failures : Buffer.Buffer<Auto_Failure>, now : Nat64, none_available_time : Nat64, last_placer : Principal, user : User, credit_available_time : Nat64, icrc2_rates : [(Value.Type, Value.Type)], user_account : Account.Pair, self : Principal) : async* Result.Type<Authorization, ()> {
    switch (authNoneCheck(now, none_available_time, last_placer)) {
      case (#Err(#None err)) failures.add(#None err);
      case _ return #Ok(#None);
    };
    switch (authCreditCheck(user, now, credit_available_time)) {
      case (#Err(#Credit err)) failures.add(#Credit err);
      case _ return #Ok(#Credit);
    };
    if (icrc2_rates.size() == 0) {
      failures.add(#ICRC2(#BadCanister { canister_id = Management.principal(); expected_canister_ids = [] }));
      return #Err;
    };
    var q = Queue.empty<(token : Principal, amount : Nat, fee : async Nat, bal_res : async Nat, apr_res : async ICRC_1_Types.Allowance)>();
    let self_account = { owner = self; subaccount = null };
    let self_approval = { account = user_account; spender = self_account };
    label rating for ((principalv, ratev) in icrc2_rates.vals()) {
      let rate = switch ratev {
        case (#Map map) RBTree.fromArray<Text, Value.Type>(map, Text.compare);
        case _ continue rating;
      };
      let amt = Option.get(Value.metaNat(rate, "place"), 0);
      if (amt == 0) continue rating;
      let p = switch (Value.toPrincipal(principalv)) {
        case (?found) found;
        case _ continue rating;
      };
      let token = ICRC_1_Types.genActor(p);
      let fee = token.icrc1_fee();
      let balance = token.icrc1_balance_of(user_account);
      let approval = token.icrc2_allowance(self_approval);
      q := Queue.insertHead(q, (p, amt, fee, balance, approval));
    };
    if (Queue.size(q) == 0) {
      failures.add(#ICRC2(#BadCanister { canister_id = Management.principal(); expected_canister_ids = [] }));
      return #Err;
    };
    for ((p, amt, fee_res, bal_res, apr_res) in Queue.iterTail(q)) {
      let fee = await fee_res;
      let bal = await bal_res;
      let apr = await apr_res;
      let min = amt + fee;
      if (bal < min) {
        failures.add(#ICRC2(#TransferFromFailed(#InsufficientFunds { balance = bal })));
      } else if (apr.allowance < min) {
        failures.add(#ICRC2(#TransferFromFailed(#InsufficientAllowance apr)));
      } else return #Ok(#ICRC2 { canister_id = p; amount = ?amt });
    };
    #Err;
  };
};
