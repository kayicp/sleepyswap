import RBTree "../util/motoko/StableCollections/RedBlackTree/RBTree";
import Result "../util/motoko/Result";
import ICRC_1_Types "../util/motoko/ICRC-1/Types";
import Error "../util/motoko/Error";
import Order "mo:base/Order";
import Blob "mo:base/Blob";
import Nat64 "mo:base/Nat64";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Option "../util/motoko/Option";

module {

  public let SELL_TOKEN = "sleepyswap:sell_token_canister_id";
  public let BUY_TOKEN = "sleepyswap:buy_token_canister_id";
  public let MAX_ORDER_BATCH = "sleepyswap:max_order_batch_size";

  public let AMOUNT_TICK = "sleepyswap:amount_tick";
  public let PRICE_TICK = "sleepyswap:price_tick";
  public let MAKER_FEE_NUMER = "sleepyswap:maker_trading_fee_numerator";
  public let TAKER_FEE_NUMER = "sleepyswap:taker_trading_fee_numerator";
  public let TRADING_FEE_DENOM = "sleepyswap:trading_fee_denomination";
  public let MIN_BUY_AMOUNT = "sleepyswap:minimum_buy_amount";
  public let MIN_SELL_AMOUNT = "sleepyswap:minimum_sell_amount";
  public let MIN_PRICE = "sleepyswap:minimum_price";

  public let TX_WINDOW = "sleepyswap:tx_window";
  public let PERMITTED_DRIFT = "sleepyswap:permitted_drift";

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
  public func newSubaccount<K>(ids : RBTree.Type<Nat, K>) : Subaccount = {
    id = recycleId(ids);
    orders = RBTree.empty();
    sells = RBTree.empty();
    sell_amount = newAmount();
    buys = RBTree.empty();
    buy_amount = newAmount();
    trades = RBTree.empty();
  };
  public type User = {
    id : Nat;
    credit : Nat;
    subaccounts : RBTree.Type<Blob, Subaccount>;
    subaccount_ids : RBTree.Type<Nat, Blob>;
  };
  public func newUser<K>(ids : RBTree.Type<Nat, K>) : User = {
    id = recycleId(ids);
    credit = 0;
    subaccounts = RBTree.empty();
    subaccount_ids = RBTree.empty();
  };
  type OrderArg = { price : Nat; amount : Nat };
  public type PlaceArg = {
    // todo: payment, add to placeCompare
    subaccount : ?Blob;
    created_at_time : ?Nat64;
    memo : ?Blob;
    buy_orders : [OrderArg];
    sell_orders : [OrderArg];
  };
  public type PlaceOk = [Nat];
  public type PlaceError = {
    #GenericError : Error.Type;
    #BatchTooLarge : { batch_size : Nat; maximum_batch_size : Nat };
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
    #SellPriceOccupied : { price : Nat; index : Nat; order_id : Nat };
    #BuyPriceOccupied : { price : Nat; index : Nat; order_id : Nat };
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
    var c = Option.compare(a.subaccount, b.subaccount, Blob.compare);
    switch c {
      case (#equal) ();
      case _ return c;
    };
    c := Option.compare(a.created_at_time, b.created_at_time, Nat64.compare);
    switch c {
      case (#equal) ();
      case _ return c;
    };
    c := Option.compare(a.memo, b.memo, Blob.compare);
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

};
