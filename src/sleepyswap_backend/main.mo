import Principal "mo:base/Principal";
import Sleepy "Sleepy";

import RBTree "../util/motoko/StableCollections/RedBlackTree/RBTree";

shared (install) persistent actor class Canister(
  // deploy : {
  //   #Init : {  };
  //   #Upgrade;
  // }
) = Self {
  func self() : Principal = Principal.fromActor(Self);

  var order_id = 0;
  var orders = RBTree.empty<Nat, Sleepy.Order>();

  var sell_book : Sleepy.Book = RBTree.empty();
  var buy_book : Sleepy.Book = RBTree.empty();

  var trade_id = 0;
  var trades = RBTree.empty<Nat, Sleepy.Trade>();

  var users : Sleepy.Users = RBTree.empty();

  public shared ({ caller }) func sleepyswap_place(batch : [Sleepy.PlaceArg]) : async Sleepy.PlaceResult {

    [];
  };

  public shared ({ caller }) func sleepyswap_cancel(batch : [Sleepy.CancelArg]) : async Sleepy.CancelResult {
    [];
  };

  public shared ({ caller }) func sleepyswap_match() : async () {

  };
};
