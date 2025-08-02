import RBTree "../util/motoko/StableCollections/RedBlackTree/RBTree";

module {
  public type Order = {
    created_at : Nat64;
    price : Nat;
    amount : Nat;
    trades : RBTree.Type<Nat, ()>; // trade ids
  };
  public type Price = {
    orders : RBTree.Type<Nat, ()>;
  };
  public type Book = RBTree.Type<Nat, Price>; // price to order ids
  public type Trade = {
    timestamp : Nat64;
    xfer : Nat; // transfer id
  };
  public type User = {
    credit : Nat;
    orders : RBTree.Type<Nat, ()>; // order ids by time
    // note: cant place order on same price
    order_prices : RBTree.Type<Nat, Nat>; // order ids by price
    trades : RBTree.Type<Nat, ()>; // trade ids
  };
  public type PlaceArg = {
    created_at : ?Nat64;
    subaccount : ?Blob;
    orders : [{
      price : Nat;
      amount : Nat;
    }];
  };
  public type CancelArg = {
    subaccount : ?Nat;
    orders : [Nat];
  };

};
