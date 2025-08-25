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
  public let TTL = "sleepyswap:time_to_live"; // seconds
  public let DEFAULT_ORDER_EXPIRY = "sleepyswap:default_order_expiry";
  public let MAX_ORDER_EXPIRY = "sleepyswap:max_order_expiry";
  public let MIN_ORDER_EXPIRY = "sleepyswap:min_order_expiry";
  public let AUTH_NONE_PLACE_RATE_LIMIT = "sleepyswap:auth_none_place_rate_limit"; // millisecond
  public let AUTH_NONE_CREDIT_REWARD = "sleepyswap:auth_none_credit_reward"; // 1 (waiting)
  public let AUTH_NONE_CREDIT_REWARD_EXPIRY = "sleepyswap:auth_none_credit_reward_expiry"; // 3 days
  public let AUTH_ICRC2_RATES = "sleepyswap:auth_icrc2_fee_rates"; // map(canisterid, amount)
  public let AUTH_ICRC2_CREDIT_REWARD = "sleepyswap:auth_icrc2_credit_reward"; // 3 (pay, transfer fee, waiting)
  public let AUTH_ICRC2_CREDIT_REWARD_EXPIRY = "sleepyswap:auth_icrc2_credit_reward_expiry"; // 3 days

  public let TX_WINDOW = "sleepyswap:tx_window";
  public let PERMITTED_DRIFT = "sleepyswap:permitted_drift";

  public let MIN_MEMO = "sleepyswap:min_memo_size";
  public let MAX_MEMO = "sleepyswap:max_memo_size";

  public let CANCEL_RATE_LIMIT = "sleepyswap:cancel_rate_limit"; // millisecond
  public let WORK_RATE_LIMIT = "sleepyswap:work_rate_limit"; // millisecond

  /* ratelimits for free calls
    place:
      global: n/a
      user: 1s after last_none_placed = 10 max orders
      subaccount = n/a
    cancel:
      global: 3s after free order creation (because token transfers take at most 3s)
      user: n/a
      subaccount: n/a
    work:
      global: 10ms per job (this way same caller cant spam call or the same job, but can retry later)
      user: n/a
      subaccount: n/a
  */

  type OrderClosed = {
    at : Nat64;
    reason : {
      #Filled;
      #Expired;
      #Canceled;
      #Failed : { trade : Nat };
    };
  };
  public type Order = {
    created_at : Nat64;
    owner : Nat; // user id
    subaccount : Nat; // user's subaccount id
    is_buy : Bool;
    price : Nat;
    amount : Amount; // in sell unit
    expires_at : Nat64;
    trades : IDs;
    authorization : Authorized;
    closed : ?OrderClosed;
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
  public type Price = { amount : Amount; orders : IDs };
  public type Book = RBTree.Type<Nat, Price>;
  public func getPrice(book : Book, price : Nat) : Price = switch (RBTree.get(book, Nat.compare, price)) {
    case (?found) found;
    case _ ({
      amount = newAmount(0);
      orders = RBTree.empty();
    });
  };
  public func savePrice(book : Book, o : Order, p : Price) : Book = if (RBTree.size(p.orders) > 0) {
    RBTree.insert(book, Nat.compare, o.price, p);
  } else RBTree.delete(book, Nat.compare, o.price);
  public func insertPrice(book : Book, oid : Nat, o : Order) : Book {
    var p = getPrice(book, o.price);
    let amount = addAmount(p.amount, o.amount);
    p := { amount; orders = RBTree.insert(p.orders, Nat.compare, oid, ()) };
    savePrice(book, o, p);
  };
  public func deletePrice(book : Book, oid : Nat, o : Order) : Book {
    var p = getPrice(book, o.price);
    let amount = minusAmount(p.amount, o.amount);
    p := { amount; orders = RBTree.delete(p.orders, Nat.compare, oid) };
    savePrice(book, o, p);
  };

  type Call<OkT, ErrT> = {
    #Calling : { caller : Principal };
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
  type XferOk = { time : Nat64; xfer : Nat };
  type XferErr = { time : Nat64; err : ICRC_1_Types.TransferFromError };
  type TradeStory = {
    #MakerToEscrow : {
      #Pending;
      #Transferring : { time : Nat64; caller : Principal };
      #Failed : XferErr;
    };
    #TakerToMaker : {
      #Pending : { maker_to_escrow : XferOk };
      #Transferring : {
        time : Nat64;
        caller : Principal;
        maker_to_escrow : XferOk;
      };
    };
    #EscrowRefundMaker : {
      #Pending : { maker_to_escrow : XferOk; taker_to_maker : XferErr };
      #Transferring : {
        time : Nat64;
        caller : Principal;
        maker_to_escrow : XferOk;
        taker_to_maker : XferErr;
      };
      #Failed : {
        time : Nat64;
        err : ICRC_1_Types.TransferError;
        maker_to_escrow : XferOk;
        taker_to_maker : XferErr;
      };
      #Ok : {
        time : Nat64;
        xfer : Nat;
        maker_to_escrow : XferOk;
        taker_to_maker : XferErr;
      };
    };
    #EscrowToTaker : {
      #Pending : { maker_to_escrow : XferOk; taker_to_maker : XferOk };
      #Transferring : {
        time : Nat64;
        caller : Principal;
        maker_to_escrow : XferOk;
        taker_to_maker : XferOk;
      };
      #Failed : {
        time : Nat64;
        err : ICRC_1_Types.TransferError;
        maker_to_escrow : XferOk;
        taker_to_maker : XferOk;
      };
      #Ok : {
        time : Nat64;
        xfer : Nat;
        maker_to_escrow : XferOk;
        taker_to_maker : XferOk;
      };
    };
  };
  public func tradeStory(t : Trade) : TradeStory {
    let maker_to_escrow = switch (RBTree.max(t.maker_to_escrow)) {
      case null return #MakerToEscrow(#Pending);
      case (?(time, #Calling { caller })) return #MakerToEscrow(#Transferring { time; caller });
      case (?(time, #Called(#Err err))) return #MakerToEscrow(#Failed { time; err });
      case (?(time, #Called(#Ok xfer))) ({ time; xfer });
    };
    let t2m = switch (RBTree.max(t.taker_to_maker)) {
      case null return #TakerToMaker(#Pending { maker_to_escrow });
      case (?(time, #Calling { caller })) return #TakerToMaker(#Transferring { time; caller; maker_to_escrow });
      case (?(time, #Called res)) ({ time; res });
    };
    switch (t2m.res, RBTree.max(t.escrow_to.transfer)) {
      case (#Err err, null) #EscrowRefundMaker(#Pending { maker_to_escrow; taker_to_maker = { t2m with err } });
      case (#Err err, ?(time, #Calling { caller })) #EscrowRefundMaker(#Transferring { time; caller; maker_to_escrow; taker_to_maker = { t2m with err } });
      case (#Err errZ, ?(time, #Called(#Err err))) #EscrowRefundMaker(#Failed { time; err; maker_to_escrow; taker_to_maker = { t2m with err = errZ } });
      case (#Err err, ?(time, #Called(#Ok xfer))) #EscrowRefundMaker(#Ok { time; xfer; maker_to_escrow; taker_to_maker = { t2m with err } });
      case (#Ok xfer, null) #EscrowToTaker(#Pending { maker_to_escrow; taker_to_maker = { t2m with xfer } });
      case (#Ok xfer, ?(time, #Calling { caller })) #EscrowToTaker(#Transferring { time; caller; maker_to_escrow; taker_to_maker = { t2m with xfer } });
      case (#Ok xfer, ?(time, #Called(#Err err))) #EscrowToTaker(#Failed { time; err; maker_to_escrow; taker_to_maker = { t2m with xfer } });
      case (#Ok xferZ, ?(time, #Called(#Ok xfer))) #EscrowToTaker(#Ok { time; xfer; maker_to_escrow; taker_to_maker = { t2m with xfer = xferZ } });
    };
  };
  public func tradeLocker() {
    switch (RBTree.maxKey(order.trades)) {
      case (?max) switch (RBTree.get(trades, Nat.compare, max)) {
        case (?trade) switch (Sleepy.tradeStory(trade)) {
          case (#MakerToEscrow(#Pending)) ();
          case (#MakerToEscrow(#Failed _)) ();
          case (#EscrowRefundMaker(#Ok _)) ();
          case (#EscrowToTaker(#Ok _)) ();
          case _ {
            res_buff.add(#Err(#Locked { trade = max }));
            continue looping;
          };
        };
        case _ ();
      };
      case _ ();
    };
  };
  type Amount = { initial : Nat; locked : Nat; filled : Nat };
  public func newAmount(initial : Nat) : Amount = {
    initial;
    locked = 0;
    filled = 0;
  };
  public func priceAmount(a : Amount, price : Nat) : Amount = ({
    initial = a.initial * price;
    filled = a.filled * price;
    locked = a.locked * price;
  });
  public func addAmount(a : Amount, add : Amount) : Amount = ({
    initial = a.initial + add.initial;
    filled = a.filled + add.filled;
    locked = a.locked + add.locked;
  });
  public func minusAmount(a : Amount, minus : Amount) : Amount = ({
    initial = if (a.initial >= minus.initial) a.initial - minus.initial else 0;
    filled = if (a.filled >= minus.filled) a.filled - minus.filled else 0;
    locked = if (a.locked >= minus.locked) a.locked - minus.locked else 0;
  });
  type Subaccount = {
    id : Nat;
    orders : IDs;
    // note: cant place order on same price
    sells : RBTree.Type<(price : Nat), (order : Nat)>;
    sell_amount : Amount; // in sell unit
    buys : RBTree.Type<(price : Nat), (order : Nat)>;
    buy_amount : Amount; // in buy unit
    trades : IDs; // ongoing/finished
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
      buy_amount = addAmount(_sa.buy_amount, priceAmount(o.amount, o.price));
    }) else ({
      _sa with sells = RBTree.insert(_sa.sells, Nat.compare, o.price, oid);
      sell_amount = addAmount(_sa.sell_amount, o.amount);
    });
    let orders = RBTree.insert(sa.orders, Nat.compare, oid, ());
    { sa with orders };
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
    last_none_placed : Nat64;
    subaccounts : RBTree.Type<Blob, Subaccount>;
    subaccount_ids : RBTree.Type<Nat, Blob>;
    credits_by_expiry : Expiries;
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

  public type CancelArg = {
    subaccount : ?Blob;
    orders : [Nat];
  };
  public type CancelError = {
    #GenericBatchError : Error.Type;
    #BatchTooLarge : { batch_size : Nat; maximum_batch_size : Nat };

    #GenericError : Error.Type;
    #NotFound;
    #Closed : OrderClosed;
    #Locked : { trade : Nat };
    #TemporarilyUnavailable : { time : Nat64; available_time : Nat64 };
  };
  public type CancelResult = Result.Type<(), CancelError>;

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
    #TemporarilyUnavailable : { time : Nat64; available_time : Nat64 };
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

  public func authNoneCheck(now : Nat64, none_available_time : Nat64) : Result.Type<(), { #None : None_Failure }> = if (now < none_available_time) return #Err(#None(#TemporarilyUnavailable { time = now; available_time = none_available_time })) else #Ok;

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

  public func authAutoCheck(failures : Buffer.Buffer<Auto_Failure>, now : Nat64, none_available_time : Nat64, user : User, credits : Credits, icrc2_rates : [(Value.Type, Value.Type)], user_account : Account.Pair, self : Principal) : async* Result.Type<{ #None; #Credit; #ICRC2 : ICRC2_Selected }, ()> {
    switch (authNoneCheck(now, none_available_time)) {
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

  public type Incoming = RBTree.Type<(price : Nat), { index : Nat; amount : Nat; expires_at : Nat64 }>;
  type Arg = {
    #Place : {
      arg : PlaceArg;
      authorized : Authorized;
      incoming_buys : Incoming;
      incoming_sells : Incoming;
    };
    #Cancel : ();
    #Match : ();
  };
  func authMap(auth : Authorized) : Value.Metadata {
    var map : Value.Metadata = RBTree.empty();
    switch auth {
      case (#None) map := Value.setText(map, "via", ?"none");
      case (#Credit) map := Value.setText(map, "via", ?"credit");
      case (#ICRC2 token) {
        map := Value.setText(map, "via", ?"icrc2");
        map := Value.setPrincipal(map, "token", ?token.canister_id);
        map := Value.setNat(map, "xfer", ?token.xfer);
      };
    };
    map;
  };

  func orderValues(incoming : Incoming) : [Value.Type] {
    let buff = Buffer.Buffer<Value.Type>(RBTree.size(incoming));
    for ((price, { amount; expires_at }) in RBTree.entries(incoming)) {
      var map : Value.Metadata = RBTree.empty();
      map := Value.setNat(map, "price", ?price);
      map := Value.setNat(map, "amount", ?amount);
      map := Value.setNat(map, "expires_at", ?(Nat64.toNat(expires_at)));
      buff.add(#Map(RBTree.array(map)));
    };
    Buffer.toArray(buff);
  };

  public func genValue(
    caller : Principal,
    now : Nat64,
    phash : ?Blob,
    arg : Arg,
  ) : Value.Type {
    var tx : Value.Metadata = RBTree.empty();
    var map : Value.Metadata = RBTree.empty();
    map := Value.setNat(map, "ts", ?Nat64.toNat(now));
    switch arg {
      case (#Place place) {
        switch (place.arg.authorization) {
          case (?_found) tx := Value.setMap(tx, "auth", authMap(place.authorized));
          case _ map := Value.setMap(map, "auth", authMap(place.authorized));
        };
        map := Value.setText(map, "op", ?"place");
        tx := Value.setAccountP(tx, "from", ?{ place.arg with owner = caller });
        tx := Value.setBlob(tx, "memo", place.arg.memo);
        switch (place.arg.created_at_time) {
          case (?found) tx := Value.setNat(tx, "ts", ?Nat64.toNat(found));
          case _ ();
        };
        tx := Value.setArray(tx, "sells", orderValues(place.incoming_sells));
        tx := Value.setArray(tx, "buys", orderValues(place.incoming_buys));
      };
      case _ (); // reject
    };
    map := Value.setMap(map, "tx", tx);
    switch phash {
      case (?found) map := Value.setBlob(map, "phash", ?found);
      case _ ();
    };
    #Map(RBTree.array(map));
  };

  type IDs = RBTree.Type<Nat, ()>;
  public type Expiries = RBTree.Type<Nat64, IDs>;
  public func getExpiry(expiries : Expiries, expiry : Nat64) : IDs = switch (RBTree.get(expiries, Nat64.compare, expiry)) {
    case (?found) found;
    case _ RBTree.empty();
  };
  public func saveExpiry(expiries : Expiries, expiry : Nat64, ids : IDs) : Expiries = if (RBTree.size(ids) > 0) RBTree.insert(expiries, Nat64.compare, expiry, ids) else RBTree.delete(expiries, Nat64.compare, expiry);
  public func insertExpiry(expiries : Expiries, expiry : Nat64, id : Nat) : Expiries {
    var expiring_ids = getExpiry(expiries, expiry);
    expiring_ids := RBTree.insert(expiring_ids, Nat.compare, id, ());
    saveExpiry(expiries, expiry, expiring_ids);
  };
  public func deleteExpiry(expiries : Expiries, expiry : Nat64, id : Nat) : Expiries {
    var expiring_ids = getExpiry(expiries, expiry);
    expiring_ids := RBTree.delete(expiring_ids, Nat.compare, id);
    saveExpiry(expiries, expiry, expiring_ids);
  };

  public type WorkArg = { subaccount : ?Blob };
  public type WorkErr = { #GenericError : Error.Type };
  public type WorkResult = Result.Type<Nat, WorkErr>; // token reward minting

  public func canWork(caller : Principal, last_caller : Principal, available_time : Nat64, now : Nat64) : Bool = if (caller == last_caller) available_time < now else true;
};
