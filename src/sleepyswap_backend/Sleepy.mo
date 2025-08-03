import RBTree "../util/motoko/StableCollections/RedBlackTree/RBTree";
import Result "../util/motoko/Result";
import ICRC_1_Types "../util/motoko/ICRC-1/Types";
import Error "../util/motoko/Error";

module {

  public let SELL_TOKEN = "sleepyswap:sell_token_canister_id";
  public let BUY_TOKEN = "sleepyswap:buy_token_canister_id";
  public let MAX_ORDER_BATCH = "sleepyswap:max_order_batch_size";

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
    orders : RBTree.Type<Nat, ()>; // order ids by time
    // order ids by price
    // note: cant place order on same price
    sell_prices : RBTree.Type<Nat, Nat>;
    buy_prices : RBTree.Type<Nat, Nat>;
    trades : RBTree.Type<Nat, ()>; // trade ids
  };
  public type User = {
    id : Nat;
    credit : Nat;
    subaccounts : RBTree.Type<Blob, Subaccount>;
    subaccount_ids : RBTree.Type<Nat, Blob>;
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
    #DuplicateSellPrice : { price : Nat; indexes : [Nat] };
    #DuplicateBuyPrice : { price : Nat; indexes : [Nat] };
    #OrdersOverlap : {
      sell_index : Nat;
      sell_price : Nat;
      buy_index : Nat;
      buy_price : Nat;
    };
    #ExistingSellPrice : { price : Nat; order_id : Nat };
    #ExistingBuyPrice : { price : Nat; order_id : Nat };
    #BuyPriceTooHigh : { price : Nat; maximum_price : Nat };
    #SellPriceTooHigh : { price : Nat; maximum_price : Nat };
    #BuyPriceTooLow : { price : Nat; minimum_price : Nat };
    #SellPriceTooLow : { price : Nat; minimum_price : Nat };
    #InsufficientFunds : {
      canister_id : Principal;
      subaccount : ?Blob;
      current_balance : Nat;
      minimum_balance : Nat;
    };
    #InsufficientAllowance : {
      canister_id : Principal;
      subaccount : ?Blob;
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
};
