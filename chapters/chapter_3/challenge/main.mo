import Result "mo:base/Result";
import Principal "mo:base/Principal";
import HashMap "mo:base/HashMap";
import Option "mo:base/Option";
import Text "mo:base/Text";
import Nat "mo:base/Nat";
import Types "types";
actor {

    type Result<Ok, Err> = Types.Result<Ok, Err>;
    type HashMap<K, V> = Types.HashMap<K, V>;

    type Value = Nat;
    let ledger = HashMap.HashMap<Principal, Value>(0, Principal.equal, Principal.hash);

    public query func tokenName() : async Text {
        return "DarkPetal";
    };

    public query func tokenSymbol() : async Text {
        return "@DP";
    };

    public func mint(owner : Principal, amount : Nat) : async Result<(), Text> {
        let currentBalance = Option.get(ledger.get(owner), 0);
        ledger.put(owner, currentBalance + amount);
        return #ok();
    };

    public func burn(owner : Principal, amount : Nat) : async Result<(), Text> {
        let currentBalance = Option.get(ledger.get(owner), 0);
        if(currentBalance < amount) {
            return #err("Not enough balance!");
        };
        ledger.put(owner, currentBalance - amount);
        return #ok();
    };

    public shared ({ caller }) func transfer(from : Principal, to : Principal, amount : Nat) : async Result<(), Text> {
        if(from == to) {
            return #err("Not possible to transfer to self!");
        };
        
        let fromBalance = Option.get(ledger.get(from), 0);
        let toBalance = Option.get(ledger.get(to), 0);

        if(fromBalance < amount) {
            return #err("Not enough balance!");
        };
        
        ledger.put(from, fromBalance - amount);
        ledger.put(to, toBalance + amount);
        return #ok();
    };

    public query func balanceOf(account : Principal) : async Nat {
        return Option.get(ledger.get(account), 0);
    };

    public query func totalSupply() : async Nat {
        var suply : Nat = 0;
        for(balance in ledger.vals()){
            suply += balance;
        };
        return suply;
    };

};