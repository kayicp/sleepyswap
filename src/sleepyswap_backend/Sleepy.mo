import RBTree "../util/motoko/StableCollections/RedBlackTree/RBTree";
import Result "../util/motoko/Result";
import ICRC_1_Types "../util/motoko/ICRC-1/Types";
import Error "../util/motoko/Error";

module {
  public type Order = {
    created_at_time : Nat64;
    owner : Principal;
    subaccount : ?Blob;
    is_buy : Bool;
    price : Nat;
    amount : Nat;
    remaining : Nat;
    available : Nat;
    trades : RBTree.Type<Nat, ()>; // trade ids
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
  public type User = {
    credit : Nat;
    // order ids by price
    // note: cant place order on same price
    sell_prices : RBTree.Type<Nat, Nat>;
    buy_prices : RBTree.Type<Nat, Nat>;
    orders : RBTree.Type<Nat, ()>; // order ids by time
    trades : RBTree.Type<Nat, ()>; // trade ids
  };
  public type Users = RBTree.Type<Principal, RBTree.Type<Blob, User>>;
  public type PlaceArg = [{
    subaccount : ?Blob;
    price : Nat;
    amount : Nat;
    is_buy : Bool;
    created_at_time : ?Nat64;
  }];
  public type PlaceError = {
    #GenericError : Error.Type;
    #GenericBatchError : Error.Type;
  };
  public type PlaceResult = [?Result.Type<Nat, PlaceError>];

  public type CancelArg = [{ id : Nat; subaccount : ?Nat }];
  public type CancelError = {
    #GenericError : Error.Type;
    #GenericBatchError : Error.Type;
  };
  public type CancelResult = [?Result.Type<(), CancelError>];
};
