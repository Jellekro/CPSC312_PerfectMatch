% Project 2 - Prolog
% Sam Fraser - h0c2b
% Andy Worth - k8m1b
% Prolog representation of a dating matching algorithm
% Builds a list of stable matches based on compatibility scores, 
% and provides the best match given a person X and a KB of profiles.

%% Test Cases for Main Function: perfect_match
%?- perfect_match(p1). % Perfect Match Example!
%?- perfect_match(p2). % No Match Example.
%?- perfect_match(p3). % Decent Match Example.

%% IO/Messaging for User Queries and Responses
% given a MaleID, finds the corresponding match if there is one, and prints a response message
perfect_match(MaleID) :-
	call_GS(Engaged),
	dict_pairs(Engaged, engaged, Matches),
	matches_contains_male(MaleID, Matches, FemaleID, Score) ->
		yes_match_handler(MaleID, FemaleID, Score);
		no_match_message(MaleID).

% true if MaleID is in Matches
matches_contains_male(MaleID, [_-(MaleID, [(FemaleID, Score)|_])|_], FemaleID, Score).
matches_contains_male(MaleID, [_|Rest], FemaleID, Score) :-
	matches_contains_male(MaleID, Rest, FemaleID, Score).

% finds the rank of the FemaleID and prints out corresponding response message (different for rank==1)
yes_match_handler(MaleID, FemaleID, Score) :-
	men_prefs_sorted(_, MaleID, Ranking),
	prefs_position(FemaleID, Ranking, 0, Rank),
	match_message(MaleID, FemaleID, Score, Rank).

% finds the index of FemaleID in a list of Female information, and returns as Rank1
prefs_position(FemaleID, [(FemaleID, _)|_], Rank, Rank1) :- 
	Rank1 is Rank + 1.
prefs_position(FemaleID, [_|Rest], Rank, Rank1) :-
	RankTemp is Rank + 1,
	prefs_position(FemaleID, Rest, RankTemp, Rank1).

% message response for a perfect match, where rank==1
match_message(MaleID, FemaleID, Score, 1) :-
	profile(MaleID, Name1, _, _, (_,_), _, (_,_), _, _),
	profile(FemaleID, Name2, _, _, (_,_), _, (_,_), _, _),
	write("ʕ •ᴥ•ʔ < Congratulations "),
	write(Name1),
	write("! You and "),
	write(Name2),
	write(" preferred each other more than anyone else - and are destined to be together!\n"),
	write("✿      < The closer your compatibility is to zero, the more aligned you are...\n"),
	write("✿      < Your compatibility score was: "),
	write(Score),
	write("\n"),
	write("✿      < Thank you for using perfect_match!\n").

% message response for a decent match, where rank!=1 but they were a match
match_message(MaleID, FemaleID, Score, Rank) :-
	profile(MaleID, Name1, _, _, (_,_), _, (_,_), _, _),
	profile(FemaleID, Name2, _, _, (_,_), _, (_,_), _, _),
	write("ʕ •ᴥ•ʔ < Congratulations "),
	write(Name1),
	write("! You have been matched with "),
	write(Name2),
	write(" which was your preference: #"),
	write(Rank),
	write("\n"),
	write("✿      < The closer your compatibility is to zero, the more aligned you are...\n"),
	write("✿      < Your compatibility score was: "),
	write(Score),
	write("\n"),
	write("✿      < Thank you for using perfect_match!\n").

% message response for a case of no match
no_match_message(MaleID) :-
		profile(MaleID, Name, _, _, (_,_), _, (_,_), _, _),
		write("ʕ •ᴥ•ʔ < Hey there "),
		write(Name),
		write(", sorry to break it to you... \n"),
		write("✿      < We were unable to find your perfect_match...\n"),
		write("✿      < Don't be discouraged! You'll find someone one day.\n").



%% Running GS Algorithm to Create Matches
% true if parameters for GS are obtained and GS algorithm is run, returns resulting matched set as Engaged1
call_GS(Engaged1) :-
	get_males_info(Males),
	dict_create(Engaged, engaged, []),
	gale_shapley(Males, Engaged, Engaged1).

% true if GS algorithm is run, returns resulting matched set as Engaged1 (or Engaged in base case)
gale_shapley([], Engaged, Engaged).
gale_shapley(Males, Engaged, Engaged1) :- 
	propose(Males, Engaged, Males1, EngagedTemp), 
	gale_shapley(Males1, EngagedTemp, Engaged1).

% MaleID proposes to his most preferred FemaleID and contests any existing engagement, returns result in Males1 and Engaged1
propose([(MaleID, [(FemaleID, Score)|Rest]) | RestMales], Engaged, Males1, Engaged1) :-
	dict_contains(FemaleID, Engaged, DoesContain),
	(DoesContain) ->  
		contested_proposal([(MaleID, [(FemaleID, Score)|Rest]) | RestMales], Engaged, Males1, Engaged1);
		Males1 = RestMales, 
		Engaged1 = Engaged.put(FemaleID, (MaleID, [(FemaleID, Score)|Rest])).

% contains function for dicts, because it did not exist!
dict_contains(Key,Dict,DoesContain) :-
	DoesContain = del_dict(Key, Dict, _, _).

% MaleID contests existing engagement and succeeds if he is better, other rejected; returns result in Males1 and Engaged1
contested_proposal([(MaleID, [(FemaleID, Score)|Rest]) | RestMales], Engaged, Males1, Engaged1) :-
	(_, [(FemaleID, OtherScore)|_]) = (Engaged.get(FemaleID)),
	% we are minimizing score, therefore successful proposals come from lower scores
	(Score < OtherScore) ->
		successful_proposal([(MaleID, [(FemaleID, Score)|Rest]) | RestMales], Engaged, Males1, Engaged1);
		Engaged1 = Engaged, 
		rejected_male([(MaleID, [(FemaleID, Score)|Rest]) | RestMales], Males1).

% MaleID becomes engaged to FemaleID and the previous suitor is rejected; returns result in Males1 and Engaged1
successful_proposal([(MaleID, [(FemaleID, Score)|Rest]) | RestMales], Engaged, Males1, Engaged1) :-
	OtherGuy = Engaged.get(FemaleID),
	del_dict(FemaleID, Engaged, _, EngagedTemp),
	MalesTemp = [OtherGuy | RestMales],
	rejected_male(MalesTemp, Males1),
	Engaged1 = EngagedTemp.put(FemaleID, (MaleID, [(FemaleID, Score)|Rest])).

% rejected MaleID removes top preferred FemaleID from his preferences and is removed if he has no more proposals
rejected_male([(_, [_|[]]) | RestMales], RestMales).
rejected_male([(MaleID, [_|Rest]) | RestMales], [(MaleID, Rest) | RestMales]).



%% Preparing Males for GS Algorithm
% retrieves male IDs from knowledge base and attaches preference lists of all females to their corresponding MaleID
get_males_info(Males) :-
	findall(ID, male(ID), MaleIDs), 
	attach_info_to_ID(men_prefs_sorted, _, MaleIDs, Males).

% sorts preference list of MaleID by score
men_prefs_sorted(_, MaleID, SortedPrefs) :-
	men_prefs(MaleID, Prefs),
	insertion_sort(Prefs, SortedPrefs).

%%% Citation for Insertion Sort, modified to sort by our score: https://stackoverflow.com/questions/12715293/prolog-insertion-sort
insert(X, [], [X]):- !.
insert((FemaleID, Score), [(FemaleID1, Score1)|L1], [(FemaleID, Score), (FemaleID1, Score1)|L1]):-
	Score=<Score1, !.
insert((FemaleID, Score), [(FemaleID1, Score1)|L1], [(FemaleID1, Score1)|L]):-
	insert((FemaleID, Score), L1, L).

insertion_sort([], []):- !.
insertion_sort([(FemaleID, Score)|L], S):-
	insertion_sort(L, S1), insert((FemaleID, Score), S1, S).
%%%

% retrieves female IDs from knowledge base and generates pair of MaleID and his preference list of FemaleIDs
men_prefs(MaleID, Prefs) :-
	findall(ID, female(ID), Females), 
	attach_info_to_ID(score, MaleID, Females, Prefs).

% abstract function to attach information to IDs based on the given mapping Predicate
attach_info_to_ID(_, _, [], []).
attach_info_to_ID(Predicate, OptArg, [A|As], [B|Bs]) :-
	pair_with_ID_info(Predicate, OptArg, A, B),
	attach_info_to_ID(Predicate, OptArg, As, Bs).

% runs mapping Predicate and pairs ID with obtained information
pair_with_ID_info(Predicate, OptArg, ID, Pair) :-
	call(Predicate, OptArg, ID, Info),
	Pair = (ID, Info).

% true if ID belongs to a male profile
male(ID) :-
	profile(ID, _, male, _, (_,_), _, (_,_), _, _).

% true if ID belongs to a female profile
female(ID) :-
	profile(ID, _, female, _, (_,_), _, (_,_), _, _).



%% Compatibility Scoring Functions (Soft & Hard Constraints)
% true if Result is the soft_score Result or 999999999 if the hard constraints failed (a large number to fit our minimizer goal).
score(MaleID, FemaleID, Result) :-
	hard_score(MaleID, FemaleID) ->
		soft_score(MaleID, FemaleID, Result);
		Result is 999999999.

% true if the two profiles meet the hard constraints (same city, opposite gender, age within partner range)
hard_score(MaleID, FemaleID) :-
	profile(MaleID, _, male, Age1, (MinAge1, MaxAge1), City, (_,_), _, _),
	profile(FemaleID, _, female, Age2, (MinAge2, MaxAge2), City, (_,_), _, _),
	MinAge2=<Age1, Age1=<MaxAge2,
	MinAge1=<Age2, Age2=<MaxAge1.

% true if Result is the sum of the difference soft score aspect results
soft_score(MaleID, FemaleID, Result) :-
	profile(MaleID, _, _, Age1, (_,_), _, (Lat1, Lon1), _, StarSign1),
	profile(FemaleID, _, _, Age2, (_,_), _, (Lat2, Lon2), _, StarSign2),
	score_age(Age1, Age2, ResultAge), 
	score_dist((Lat1, Lon1), (Lat2, Lon2), ResultDist),
   %Bio soft score unimplemented in this version
   %score_bio(Bio1, Bio2, ResultBio), 
	score_star(StarSign1, StarSign2, ResultStar), 
	Result is ResultAge + ResultDist + ResultStar.

% true if ResultAge is the difference between Age1 and Age2
score_age(Age1, Age2, ResultAge) :-
	ResultAge is abs(Age1 - Age2).

% true if ResultDist is the euclidean distance between (Lat1, Lon1) and (Lat2, Lon2)
score_dist((Lat1, Lon1), (Lat2, Lon2), ResultDist) :-
	ResultDist is sqrt((Lat1-Lat2)**2 + (Lon1-Lon2)**2).

% true if ResultBio is equal to the shared number of keywords between Bio1 and Bio2, multiplied by -1 (to fit our minimizer goal)
% score_bio(Bio1, Bio2, ResultBio) :- false.

% true if ResultStar is -1 for compatible (to fit our minimizer goal), and 0 otherwise
score_star(aries, leo, -1).
score_star(aries, libra, -1).
score_star(taurus, scorpio, -1).
score_star(taurus, cancer, -1).
score_star(gemini, sagittarius, -1).
score_star(gemini, aquarius, -1).
score_star(cancer, capricorn, -1).
score_star(leo, aquarius, -1).
score_star(leo, gemini, -1).
score_star(virgo, pisces, -1).
score_star(virgo, cancer, -1).
score_star(libra, sagittarius, -1).
score_star(scorpio, cancer, -1).
score_star(sagittarius, aries, -1).
score_star(taurus, capricorn, -1).
score_star(aquarius, sagittarius, -1).
score_star(pisces, taurus, -1).
score_star(_, _, 0).



%% The Database of Profiles of format: profile(ID, Name, Gender, Age, (MinAge, MaxAge), City, (Lat, Lon), Bio, StarSign)
profile(p1, "George", male, 25, (23,35), vancouver, (34,25), "I am George and I like tacos", leo).
profile(p2, "Barney", male, 60, (23,39), london, (2000,1342), "I am Barney and I like the beach", cancer).
profile(p3, "Chris", male, 26, (23,31), vancouver, (30,24), "I am Chris and I like tennis", virgo).
profile(p4, "Ashley", female, 24, (20,32), vancouver, (35,26), "I am Ashley and I like running", aquarius).
profile(p5, "Jessica", female, 30, (24,32), vancouver, (20,19), "I am Jessica and I like tacos", pisces).
