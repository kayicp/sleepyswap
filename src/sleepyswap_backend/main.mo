import Principal "mo:base/Principal";

shared (install) persistent actor class Canister(
  // deploy : {
  //   #Init : {  };
  //   #Upgrade;
  // }
) = Self {
  func self() : Principal = Principal.fromActor(Self);
};
