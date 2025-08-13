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
  public let DEFAULT_ORDER_EXPIRY = "sleepyswap:default_order_expiry";
  public let MAX_ORDER_EXPIRY = "sleepyswap:max_order_expiry";
  public let MIN_ORDER_EXPIRY = "sleepyswap:min_order_expiry";
  public let AUTH_NONE_RATE_LIMIT = "sleepyswap:auth_none_rate_limit"; // millisecond
  public let AUTH_NONE_CREDIT_REWARD = "sleepyswap:auth_none_credit_reward"; // 1
  // todo: add when credit expiry
  // public let AUTH_CREDIT_PLACE_EXPIRY = "sleepyswap:auth_credit_place_expiry";
  // public let AUTH_CREDIT_MATCH_EXPIRY = "sleepyswap:auth_credit_match_expiry";
  public let AUTH_ICRC2_RATES = "sleepyswap:auth_icrc2_fee_rates"; // map(canisterid, amount)
  public let AUTH_ICRC2_CREDIT_REWARD = "sleepyswap:auth_icrc2_credit_reward"; // 2

  public let TX_WINDOW = "sleepyswap:tx_window";
  public let PERMITTED_DRIFT = "sleepyswap:permitted_drift";

  // todo: add max memo size

  public type Order = {
    created_at : Nat64;
    owner : Nat; // user id
    subaccount : Nat; // user's subaccount id
    is_buy : Bool;
    price : Nat;
    amount : Amount; // in sell unit
    expires_at : Nat64;
    trades : RBTree.Type<Nat, ()>; // trade ids
    authorization : Authorized;
    closed : ?{
      at : Nat64;
      reason : {
        #Filled;
        #Expired;
        #Canceled;
        #InsufficientFunds : {
          balance : Nat;
          minimum_balance : Nat;
        };
        #InsufficientAllowance : {
          allowance : Nat;
          minimum_allowance : Nat;
        };
      };
    };
  };
  public func newOrder(now : Nat64, user_id : Nat, subacc_id : Nat, is_buy : Bool, price : Nat, { amount : Nat; expires_at : Nat64 }, auth : Authorized) : Order = {
    created_at = now;
    owner = user_id;
    subaccount = subacc_id;
    authorization = auth;
    is_buy;
    price;
    amount = newAmount(amount);
    expires_at;
    trades = RBTree.empty();
    closed = null;
  };
  public type Price = {
    amount : Amount;
    orders : RBTree.Type<Nat, ()>;
  };
  public func getPrice(book : RBTree.Type<Nat, Price>, price : Nat) : Price = switch (RBTree.get(book, Nat.compare, price)) {
    case (?found) found;
    case _ ({
      amount = newAmount(0);
      orders = RBTree.empty();
    });
  };
  public func priceNewOrder(p : Price, oid : Nat, o : Order) : Price = ({
    amount = { p.amount with initial = p.amount.initial + o.amount.initial };
    orders = RBTree.insert(p.orders, Nat.compare, oid, ());
  });
  public func savePrice(book : RBTree.Type<Nat, Price>, o : Order, p : Price) : RBTree.Type<Nat, Price> = if (p.amount.initial > 0) {
    RBTree.insert(book, Nat.compare, o.price, p);
  } else RBTree.delete(book, Nat.compare, o.price);
  type Call<OkT, ErrT> = {
    #Calling : { caller : Principal; timestamp : Nat64 };
    #Called : Result.Type<OkT, ErrT>;
  };
  public type Trade = {
    maker : { id : Nat; fee : Nat }; // track amount and account too?
    taker : { id : Nat; fee : Nat };

    maker_to_escrow : RBTree.Type<Nat64, Call<Nat, ICRC_1_Types.TransferFromError>>; // attempts = calls / 2
    taker_to_maker : RBTree.Type<Nat64, Call<Nat, ICRC_1_Types.TransferFromError>>;
    escrow_to : {
      receiver : Nat; // order id of maker (if taker_to_maker fails), or order id of taker (if taker_to_maker succeeds)
      transfer : RBTree.Type<Nat64, Call<Nat, ICRC_1_Types.TransferError>>;
    };
  };
  type Amount = { initial : Nat; locked : Nat; filled : Nat };
  public func newAmount(initial : Nat) : Amount = {
    initial;
    locked = 0;
    filled = 0;
  };
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
      sell_amount = newAmount(0);
      buys = RBTree.empty();
      buy_amount = newAmount(0);
      trades = RBTree.empty();
    });
  };
  public func saveSubaccount(user : User, subaccount_b : Blob, subaccount : Subaccount) : User = {
    user with
    subaccounts = RBTree.insert(user.subaccounts, Blob.compare, subaccount_b, subaccount);
    subaccount_ids = RBTree.insert(user.subaccount_ids, Nat.compare, subaccount.id, subaccount_b);
  };
  public func subaccountNewOrder(_sa : Subaccount, oid : Nat, o : Order) : Subaccount {
    let sa = if (o.is_buy) ({
      _sa with buys = RBTree.insert(_sa.buys, Nat.compare, o.price, oid);
      buy_amount = {
        _sa.buy_amount with initial = _sa.buy_amount.initial + (o.amount.initial * o.price)
      };
    }) else ({
      _sa with sells = RBTree.insert(_sa.sells, Nat.compare, o.price, oid);
      sell_amount = {
        _sa.sell_amount with initial = _sa.sell_amount.initial + o.amount.initial
      };
    });
    {
      sa with orders = RBTree.insert(sa.orders, Nat.compare, oid, ());
    };
  };
  public type Credit = {
    owner : Nat; // user id
    amount : Nat;
    used : Nat;
  };
  public type Credits = RBTree.Type<Nat, Credit>;
  public type User = {
    id : Nat;
    place_locked : ?Nat64;
    subaccounts : RBTree.Type<Blob, Subaccount>;
    subaccount_ids : RBTree.Type<Nat, Blob>;
    credits_unused : Nat;
    credits_by_expiry : RBTree.Type<Nat64, RBTree.Type<Nat, ()>>;
  };
  public type Users = RBTree.Type<Principal, User>;
  public func findActiveCredit(u : User, now : Nat64, cs : Credits) : Result.Type<(Nat, Credit), ()> {
    label finding_active for ((expiry, cids) in RBTree.entries(u.credits_by_expiry)) {
      if (now > expiry) continue finding_active;
      for ((cid, _) in RBTree.entries(cids)) switch (RBTree.get(cs, Nat.compare, cid)) {
        case (?credit) if (credit.used < credit.amount) return #Ok(cid, credit);
        case _ ();
      };
    };
    #Err;
  };

  type OrderArg = { price : Nat; amount : Nat; expires_at : ?Nat64 };
  public type PlaceArg = {
    // todo: add auth to placeCompare
    subaccount : ?Blob;
    memo : ?Blob; // todo: check memo
    buy_orders : [OrderArg];
    sell_orders : [OrderArg];
    authorization : ?Authorization;
    created_at_time : ?Nat64;
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

  type ICRC2_Authorization = { canister_id : Principal; amount : ?Nat };
  public type Authorization = {
    #None;
    #Credit;
    #ICRC2 : ICRC2_Authorization;
    // #Custom : { options : [{ #None; #Credit; #ICRC2 : ICRC2_Authorization }] };
  };
  public type ICRC2_Authorized = { canister_id : Principal; xfer : Nat };
  public type Authorized = {
    #None;
    #Credit;
    #ICRC2 : ICRC2_Authorized;
  };
  type None_Failure = {
    #TemporarilyUnavailable : {
      time : Nat64;
      available_time : Nat64;
      used_by : Principal;
    };
  };
  type Credit_Failure = {
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

  public func authCreditCheck(user : User, now : Nat64, credits : Credits) : Result.Type<(), { #Credit : Credit_Failure }> = switch (findActiveCredit(user, now, credits)) {
    case (#Ok _) #Ok;
    case (#Err) #Err(#Credit(#OutOfCredit));
  };

  type ICRC2_Selected = { canister_id : Principal; amount : Nat; fee : Nat };
  public func authIcrc2Check(icrc2_rates : [(Value.Type, Value.Type)], auth : ICRC2_Authorization, user_account : Account.Pair, self : Principal) : async* Result.Type<ICRC2_Selected, { #ICRC2 : ICRC2_Failure; #Text : Text }> {
    if (icrc2_rates.size() == 0) return #Err(#ICRC2(#BadCanister { auth with expected_canister_ids = [] }));
    let icrc2_rates_map = Value.mapArrayToPrincipalMap(icrc2_rates);
    let expected_amount = switch (RBTree.get(icrc2_rates_map, Principal.compare, auth.canister_id)) {
      case (?#Nat found) found;
      case _ return #Err(#ICRC2(#BadCanister { auth with expected_canister_ids = RBTree.arrayKey(icrc2_rates_map) }));
    };
    if (expected_amount == 0) return #Err(#Text("Metadata `" # AUTH_ICRC2_RATES # "." # Principal.toText(auth.canister_id) # ".place` is not properly set"));

    switch (auth.amount) {
      case (?amount) if (amount != expected_amount) return #Err(#ICRC2(#BadAmount { amount; expected_amount }));
      case _ ();
    };
    let self_account = { owner = self; subaccount = null };
    let self_approval = { account = user_account; spender = self_account };
    let token = ICRC_1_Types.genActor(auth.canister_id);
    let fee_res = token.icrc1_fee();
    let bal_res = token.icrc1_balance_of(user_account);
    let apr_res = token.icrc2_allowance(self_approval);
    let fee = await fee_res;
    let bal = await bal_res;
    let apr = await apr_res;
    let min = expected_amount + fee;
    if (bal < min) {
      #Err(#ICRC2(#TransferFromFailed(#InsufficientFunds { balance = bal })));
    } else if (apr.allowance < min) {
      #Err(#ICRC2(#TransferFromFailed(#InsufficientAllowance apr)));
    } else #Ok { auth with amount = expected_amount; fee };
  };

  public func authAutoCheck(failures : Buffer.Buffer<Auto_Failure>, now : Nat64, none_available_time : Nat64, last_placer : Principal, user : User, credits : Credits, icrc2_rates : [(Value.Type, Value.Type)], user_account : Account.Pair, self : Principal) : async* Result.Type<{ #None; #Credit; #ICRC2 : ICRC2_Selected }, ()> {
    switch (authNoneCheck(now, none_available_time, last_placer)) {
      case (#Err(#None err)) failures.add(#None err);
      case _ return #Ok(#None);
    };
    switch (authCreditCheck(user, now, credits)) {
      case (#Err(#Credit err)) failures.add(#Credit err);
      case (#Ok) return #Ok(#Credit);
    };
    if (icrc2_rates.size() == 0) {
      failures.add(#ICRC2(#BadCanister { canister_id = Management.principal(); expected_canister_ids = [] }));
      return #Err;
    };
    var q = Queue.empty<(token : Principal, amount : Nat, fee : async Nat, bal_res : async Nat, apr_res : async ICRC_1_Types.Allowance)>();
    let self_account = { owner = self; subaccount = null };
    let self_approval = { account = user_account; spender = self_account };
    label rating for ((principalv, amtv) in icrc2_rates.vals()) {
      let amt = switch amtv {
        case (#Nat n) n;
        case _ continue rating;
      };
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
      } else return #Ok(#ICRC2 { canister_id = p; amount = amt; fee });
    };
    #Err;
  };
};
