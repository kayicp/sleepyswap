import Principal "mo:base/Principal";
import Nat "mo:base/Nat";
import Sleepy "Sleepy";

import RBTree "../util/motoko/StableCollections/RedBlackTree/RBTree";
import Time64 "../util/motoko/Time64";
import Value "../util/motoko/Value";
import Error "../util/motoko/Error";
import Account "../util/motoko/ICRC-1/Account";

shared (install) persistent actor class Canister(
  // deploy : {
  //   #Init : {  };
  //   #Upgrade;
  // }
) = Self {
  func self() : Principal = Principal.fromActor(Self);

  var order_id = 0;
  var orders = RBTree.empty<Nat, Sleepy.Order>();
  var closed_orders = RBTree.empty<Nat, ()>(); // to trim

  var sell_book : Sleepy.Book = RBTree.empty();
  var buy_book : Sleepy.Book = RBTree.empty();

  var trade_id = 0;
  var trades = RBTree.empty<Nat, Sleepy.Trade>();

  var user_id = 0;
  var user_ids = RBTree.empty<Nat, Principal>();
  var users = RBTree.empty<Principal, Sleepy.User>();

  var metadata : Value.Metadata = RBTree.empty();

  public shared query func sleepyswap_metadata() : async [(Text, Value.Type)] = async RBTree.array(metadata);

  public shared ({ caller }) func sleepyswap_place(arg : Sleepy.PlaceArg) : async Sleepy.PlaceResult = async try {
    // if (not Canister.isAvailable(metadata)) return Error.text("Unavailable");
    let user_account = { owner = caller; subaccount = arg.subaccount };
    if (not Account.validate(user_account)) return Error.text("Caller account is not valid");

    let batch_size = arg.buy_orders.size() + arg.sell_orders.size();
    if (batch_size == 0) return Error.text("Orders cannot be empty");
    let max_batch = Value.getNat(metadata, Sleepy.MAX_ORDER_BATCH, 0);
    if (max_batch > 0 and batch_size > max_batch) return #Err(#BatchTooLarge { current_batch_size = batch_size; maximum_batch_size = max_batch });

    var user = switch (RBTree.get(users, Principal.compare, caller)) {
      case (?found) found;
      case _ ({
        id = user_id;
        credit = 0;
        subaccounts = RBTree.empty();
        subaccount_ids = RBTree.empty();
      });
    };
    var incoming_buys = RBTree.empty<(price : Nat), { index : Nat; amount : Nat }>();
    for (incoming in arg.buy_orders.vals()) {
      let incoming_index = RBTree.size(incoming_buys);
      switch (RBTree.get(incoming_buys, Nat.compare, incoming.price)) {
        case (?found) return #Err(#DuplicateBuyPrice { incoming with indexes = [found.index, incoming_index] });
        case _ ();
      };
      incoming_buys := RBTree.insert(incoming_buys, Nat.compare, incoming.price, { incoming with index = incoming_index });
    };
    var incoming_sells = RBTree.empty<(price : Nat), { index : Nat; amount : Nat }>();
    for (incoming in arg.buy_orders.vals()) {
      let incoming_index = RBTree.size(incoming_sells);
      switch (RBTree.get(incoming_sells, Nat.compare, incoming.price)) {
        case (?found) return #Err(#DuplicateSellPrice { incoming with indexes = [found.index, incoming_index] });
        case _ ();
      };
      incoming_sells := RBTree.insert(incoming_sells, Nat.compare, incoming.price, { incoming with index = incoming_index });
    };
    switch (RBTree.max(incoming_buys), RBTree.min(incoming_sells)) {
      case (?(max_buy_price, max_buy), ?(min_sell_price, min_sell)) if (max_buy_price >= min_sell_price) return #Err(#OrdersOverlap { buy_price = max_buy_price; buy_index = max_buy.index; sell_price = min_sell_price; sell_index = min_sell.index });
      case _ ();
    };

    let now = Time64.nanos();

    switch (arg.created_at_time) {
      case (?defined) (); // deduplication
      case _ ();
    };
    #Ok([]);
  } catch e Error.error(e);

  public shared ({ caller }) func sleepyswap_cancel(arg : Sleepy.CancelArg) : async Sleepy.CancelResult {
    [];
  };

  public shared ({ caller }) func sleepyswap_match() : async () {

  };
};
