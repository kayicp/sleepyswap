import RBTree "../util/motoko/StableCollections/RedBlackTree/RBTree";
import Result "../util/motoko/Result";
import ICRC_1_Types "../util/motoko/ICRC-1/Types";
import Error "../util/motoko/Error";

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

  public type Order = {
    created_at_time : Nat64;
    owner : Nat; // user id
    subaccount : Nat; // user's subaccount id
    is_buy : Bool;
    price : Nat;
    amount : Nat;
    remaining : Nat;
    available : Nat;
    trades : RBTree.Type<Nat, ()>; // trade ids
    close : ?{
      #Canceled : { timestamp : Nat64 };
      #InsufficientFunds : {
        canister_id : Principal;
        subaccount : ?Blob;
        current_balance : Nat;
        minimum_balance : Nat;
        timestamp : Nat64;
      };
      #InsufficientAllowance : {
        canister_id : Principal;
        subaccount : ?Blob;
        current_allowance : Nat;
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
  type Subaccount = {
    id : Nat;
    orders : RBTree.Type<(id : Nat), ()>;
    // note: cant place order on same price
    sells : RBTree.Type<(price : Nat), (order : Nat)>;
    buys : RBTree.Type<(price : Nat), (order : Nat)>;
    trades : RBTree.Type<(id : Nat), ()>;
  };
  public func newSubaccount<K>(ids : RBTree.Type<Nat, K>) : Subaccount = {
    id = recycleId(ids);
    orders = RBTree.empty();
    sells = RBTree.empty();
    buys = RBTree.empty();
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
    // payment
    subaccount : ?Blob;
    created_at_time : ?Nat64;
    buy_orders : [OrderArg];
    sell_orders : [OrderArg];
  };
  public type PlaceError = {
    #GenericError : Error.Type;
    #BatchTooLarge : { current_batch_size : Nat; maximum_batch_size : Nat };
    #BuyAmountTooLow : { amount : Nat; index : Nat; minimum_amount : Nat };
    #SellAmountTooLow : { amount : Nat; index : Nat; minimum_amount : Nat };
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
    #ExistingSellPrice : { price : Nat; index : Nat; order_id : Nat };
    #ExistingBuyPrice : { price : Nat; index : Nat; order_id : Nat };
    #InsufficientBuyFunds : { current_balance : Nat; minimum_balance : Nat };
    #InsufficienSellFunds : { current_balance : Nat; minimum_balance : Nat };
    #InsufficientBuyAllowance : {
      current_allowance : Nat;
      minimum_allowance : Nat;
    };
    #InsufficientSellAllowance : {
      current_allowance : Nat;
      minimum_allowance : Nat;
    };
    #Duplicate : { duplicate_of : Nat };
    #CreatedInFuture : { ledger_time : Nat64 };
    #TooOld;
  };
  public type PlaceResult = Result.Type<[Nat], PlaceError>;

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
};
