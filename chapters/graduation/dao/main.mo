import Result "mo:base/Result";
import Text "mo:base/Text";
import Principal "mo:base/Principal";
import Nat "mo:base/Nat";
import HashMap "mo:base/HashMap";
import Time "mo:base/Time";
import Iter "mo:base/Iter";
import Array "mo:base/Array";
import Buffer "mo:base/Buffer";
import Int "mo:base/Int";
import Hash "mo:base/Hash";
import Types "types";
actor {

        type Result<A, B> = Result.Result<A, B>;
        type Member = Types.Member;
        type ProposalContent = Types.ProposalContent;
        type ProposalId = Types.ProposalId;
        type Proposal = Types.Proposal;
        type Vote = Types.Vote;
        type HttpRequest = Types.HttpRequest;
        type HttpResponse = Types.HttpResponse;

        // The principal of the Webpage canister associated with this DAO canister (needs to be updated with the ID of your Webpage canister)
        stable let canisterIdWebpage : Principal = Principal.fromText("2gsgt-vyaaa-aaaab-qacia-cai");
        stable var manifesto = "Your manifesto";
        stable let name = "Your DAO";
        stable var goals : [Text] = [];

        let members = HashMap.HashMap<Principal, Member>(0, Principal.equal, Principal.hash);

        //Inicial setup
        let firstMentor = {
                name = "motoko_bootcamp";
                role = #Mentor;
        };
        let id : Principal = Principal.fromText("nkqop-siaaa-aaaaj-qa3qq-cai");
        members.put(id, firstMentor);

        var nextProposalId : Nat = 0;
        let proposals = HashMap.HashMap<ProposalId, Proposal>(0, Nat.equal, Hash.hash);

        let tokenCanister = actor("jaamb-mqaaa-aaaaj-qa3ka-cai") : actor {
                burn : shared (owner : Principal, amount : Nat) -> async ();
                mint : shared (owner : Principal, amount : Nat) -> async ();
                balanceOf : shared (owner : Principal) -> async Nat;
        };

        // Returns the name of the DAO
        public query func getName() : async Text {
                return name;
        };

        // Returns the manifesto of the DAO
        public query func getManifesto() : async Text {
                return manifesto;
        };

        // Returns the goals of the DAO
        public query func getGoals() : async [Text] {
                return goals;
        };

        // Register a new member in the DAO with the given name and principal of the caller
        // Airdrop 10 MBC tokens to the new member
        // New members are always Student
        // Returns an error if the member already exists
        public shared ({ caller }) func registerMember(member : Member) : async Result<(), Text> {
                switch(members.get(caller)) {
                        case(?member) {
                                return #err("This member already exists!");
                        };
                        case(null) {
                                let newMember : Member = {
                                        name = member.name;
                                        role = #Student;
                                };
                                members.put(caller, newMember);
                                await tokenCanister.mint(caller, 10);
                                return #ok();
                        };
                };
        };

        // Get the member with the given principal
        // Returns an error if the member does not exist
        public query func getMember(p : Principal) : async Result<Member, Text> {
                switch(members.get(p)) {
                        case(null) {
                                return #err("This member does not exist!");
                        };
                        case(?member) {
                                return #ok(member);
                        };
                };
        };

        // Graduate the student with the given principal
        // Returns an error if the student does not exist or is not a student
        // Returns an error if the caller is not a mentor
        public shared ({ caller }) func graduate(student : Principal) : async Result<(), Text> {
                switch(members.get(caller)) {
                        case(null) {
                                return #err("The caller is not a member!");
                        };
                        case(?callerMember) {
                                let role = callerMember.role;
                                if(role != #Mentor) {
                                        return #err("The caller is not a mentor!");
                                };
                                switch(members.get(student)) {
                                        case(null) {
                                                return #err("This student does not exist!");
                                        };
                                        case(?member) {
                                                let studentRole = member.role;
                                                if(studentRole != #Student) {
                                                        return #err("This member is not a student!");
                                                };
                                                let newGraduate = {
                                                        name = member.name;
                                                        role = #Graduate;
                                                };
                                                members.put(student, newGraduate);
                                                return #ok();
                                        };
                                };
                        };
                };
        };

        // Create a new proposal and returns its id
        // Returns an error if the caller is not a mentor or doesn't own at least 1 MBC token
        public shared ({ caller }) func createProposal(content : ProposalContent) : async Result<ProposalId, Text> {
                switch(members.get(caller)) {
                        case(null) {
                                return #err("This caller is not a member!");
                        };
                        case(?member) {
                                let role = member.role;
                                if(role != #Mentor) {
                                        return #err("This member is not a mentor!");
                                };
                                let balance = await tokenCanister.balanceOf(caller);
                                if(balance <= 0) {
                                        return #err("Not enough tokens to create a proposal!");
                                };
                                switch(content) {
                                        case(#AddMentor(id)) {
                                                switch(members.get(id)) {
                                                        case(null) {
                                                                return #err("Invalid content!");
                                                        };
                                                        case(?graduate) {
                                                                if(graduate.role != #Graduate) {
                                                                        return #err("Invalid content!");
                                                                };
                                                        };
                                                };
                                        };
                                        case(_) {};
                                };
                                // Create the proposal and burn the tokens
                                let proposal : Proposal = {
                                id = nextProposalId;
                                content;
                                creator = caller;
                                created = Time.now();
                                executed = null;
                                votes = [];
                                voteScore = 0;
                                status = #Open;
                                };
                                proposals.put(nextProposalId, proposal);
                                nextProposalId += 1;
                                await tokenCanister.burn(caller, 1);
                                return #ok(nextProposalId - 1);
                        };
                };
        };

        // Get the proposal with the given id
        // Returns an error if the proposal does not exist
        public query func getProposal(id : ProposalId) : async Result<Proposal, Text> {
                switch(proposals.get(id)) {
                        case(null) {
                                return #err("Proposal does not exist!");
                        };
                        case(?proposal) {
                                return #ok(proposal);
                        };
                };
        };

        // Returns all the proposals
        public query func getAllProposal() : async [Proposal] {
                return Iter.toArray(proposals.vals());
        };

        // Vote for the given proposal
        // Returns an error if the proposal does not exist or the member is not allowed to vote
        public shared ({ caller }) func voteProposal(proposalId : ProposalId, yesOrNo : Bool) : async Result<(), Text> {
                switch (members.get(caller)) {
                        case (null) {
                                 return #err("The caller is not a member - cannot vote one proposal");
                        };
                        case (?member) {
                                // Check if the proposal exists
                                switch (proposals.get(proposalId)) {
                                        case (null) {
                                                return #err("The proposal does not exist");
                                        };
                                        case (?proposal) {
                                                // Check if the proposal is open for voting
                                                if (proposal.status != #Open) {
                                                        return #err("The proposal is not open for voting");
                                                };
                                                // Check if the caller has already voted
                                                if (_hasVoted(proposal, caller)) {
                                                        return #err("The caller has already voted on this proposal");
                                                };
                                                let balance = await tokenCanister.balanceOf(caller);
                                                let multiplierVote = switch (yesOrNo) {
                                                        case (true) { 1 };
                                                        case (false) { -1 };
                                                };
                                                var votingPower : Nat = 0;
                                                switch(member.role) {
                                                        case(#Student) {
                                                                votingPower := 0;
                                                        };
                                                        case(#Graduate) {
                                                                votingPower := balance;
                                                        };
                                                        case(#Mentor) {
                                                                votingPower := balance * 5;
                                                        };
                                                };
                                                let newVoteScore : Int = proposal.voteScore + votingPower * multiplierVote;
                                                var newExecuted : ?Time.Time = null;
                                                let newVotes = Buffer.fromArray<Vote>(proposal.votes);
                                                let vote = {
                                                        member = caller;
                                                        votingPower;
                                                        yesOrNo;
                                                };
                                                newVotes.add(vote);
                                                let newStatus = if (newVoteScore >= 100) {
                                                        #Accepted;
                                                } else if (newVoteScore <= -100) {
                                                        #Rejected;
                                                } else {
                                                        #Open;
                                                };
                                                switch (newStatus) {
                                                        case (#Accepted) {
                                                                _executeProposal(proposal.content);
                                                                newExecuted := ?Time.now();
                                                        };
                                                        case (_) {};
                                                };
                                                let newProposal : Proposal = {
                                                        id = proposal.id;
                                                        content = proposal.content;
                                                        creator = proposal.creator;
                                                        created = proposal.created;
                                                        executed = newExecuted;
                                                        votes = Buffer.toArray(newVotes);
                                                        voteScore = newVoteScore;
                                                        status = newStatus;
                                                };
                                                proposals.put(proposal.id, newProposal);
                                                return #ok();
                                        };
                                };
                        };
                };
        };

        func _hasVoted(proposal : Proposal, member : Principal) : Bool {
                return Array.find<Vote>(
                proposal.votes,
                func(vote : Vote) {
                        return vote.member == member;
                },
                ) != null;
        };

        func _executeProposal(content : ProposalContent) : () {
                switch (content) {
                        case (#ChangeManifesto(newManifesto)) {
                                manifesto := newManifesto;
                        };
                        case (#AddGoal(newGoal)) {
                                let newGoals = Buffer.fromArray<Text>(goals);
                                newGoals.add(newGoal);
                                goals := Buffer.toArray<Text>(newGoals);
                        };
                        case(#AddMentor(id)) {
                                switch(members.get(id)) {
                                        case(null) {
                                        };
                                        case(?member) {
                                                let newMentor = {
                                                        name = member.name;
                                                        role = #Mentor;
                                                };
                                                members.put(id, newMentor);
                                        };
                                };
                        };
                };
                return;
        };

        // Returns the Principal ID of the Webpage canister associated with this DAO canister
        public query func getIdWebpage() : async Principal {
                return canisterIdWebpage;
        };

};
