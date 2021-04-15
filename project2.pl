% Project 2
% Sam Fraser h0c2b
% Andy Worth k8m1b
% Prolog representation of a dating matching algorithm
% Builds a list of stable matches based on compatibility scores, 
% and provides the best match given a person X and a KB of profiles.

% Try:
%?- perfect_match(p1). % Perfect Match Example!
%?- perfect_match(p2). % No Match Example.
%?- perfect_match(p3). % Decent Match Example.

perfect_match(MaleID) :-
	callGS(Engaged),
	dict_pairs(Engaged, engaged, Matches),
	matches_contains_male(MaleID, Matches, FemaleID, Score) ->
		yes_match_handler(MaleID, FemaleID, Score);
		no_match_message(MaleID).

matches_contains_male(MaleID, [Key-(MaleID,[(FemaleID,Score)|RestPrefs])|Rest], FemaleID, Score).
matches_contains_male(MaleID, [_|Rest], FemaleID, Score) :-
	matches_contains_male(MaleID, Rest, FemaleID, Score).

yes_match_handler(MaleID, FemaleID, Score) :-
	menPrefsSorted(_,MaleID, Ranking),
	prefsPosition(FemaleID, Ranking, 0, Rank),
	match_message(MaleID, FemaleID, Score, Rank).

prefsPosition(FemaleID, [(FemaleID,Score)|Rest], Rank, Rank1) :- 
	Rank1 is Rank + 1.
prefsPosition(FemaleID, [_|Rest], Rank, Rank1) :-
	RankTemp is Rank + 1,
	prefsPosition(FemaleID, Rest, RankTemp, Rank1).

match_message(MaleID,FemaleID,Score,1) :-
	profile(MaleID, Name1 ,_,_,(_,_),_,(_,_),_,_),
	profile(FemaleID, Name2,_,_,(_,_),_,(_,_),_,_),
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

match_message(MaleID, FemaleID, Score, Rank) :-
	profile(MaleID, Name1 ,_,_,(_,_),_,(_,_),_,_),
	profile(FemaleID, Name2,_,_,(_,_),_,(_,_),_,_),
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

no_match_message(MaleID) :-
		profile(MaleID, Name,_,_,(_,_),_,(_,_),_,_),
		write("ʕ •ᴥ•ʔ < Hey there "),
		write(Name),
		write(", sorry to break it to you... \n"),
		write("✿      < We were unable to find your perfect_match...\n"),
		write("✿      < Don't be discouraged! You'll find someone one day.\n").

% Iteration Over List of Men & List of Women to Create Scores

% true if parameters for GS are obtained and GS algorithm is run
callGS(Engaged1) :-
	getMalesInfo(Males),
	dict_create(Engaged, engaged, []),
	galeShapley(Males, Engaged, Engaged1).

%galeShapley(_, [], []) :- print("No women -> No matches").
%galeShapley([], _, []) :- print("No men -> No matches").
%galeShapley(Males, Females, Engaged) :-
%	False.

galeShapley([],Engaged,Engaged).
galeShapley(Males, Engaged, Engaged1) :- 
	propose(Males, Engaged, Males1, EngagedTemp), 
	galeShapley(Males1, EngagedTemp, Engaged1).

%dictionaryTest() :-
%	dict_create(Res, engaged, []),
%	print(Res).


propose([(MaleID, [(FemaleID,Score)|Rest]) | RestMales], Engaged, Males1, Engaged1) :-
	dict_contains(FemaleID,Engaged,DoesContain),
	(DoesContain) ->  
		contestedProposal([(MaleID, [(FemaleID,Score)|Rest]) | RestMales], Engaged, Males1, Engaged1);
		Males1 = RestMales, 
		Engaged1 = Engaged.put(FemaleID, (MaleID, [(FemaleID,Score)|Rest])).

dict_contains(Key,Dict,DoesContain) :- % del_dict used as a contains function on a dict, because it does not exist!
	DoesContain = del_dict(Key,Dict,_,_).

contestedProposal([(MaleID, [(FemaleID,Score)|Rest]) | RestMales], Engaged, Males1, Engaged1) :-
	(OtherMaleID,[(FemaleID,OtherScore)|OtherRest]) = (Engaged.get(FemaleID)),
	(Score < OtherScore) ->  % We are minimizing score, therefore successful proposals come from lower scores
		successfulProposal([(MaleID, [(FemaleID,Score)|Rest]) | RestMales], Engaged, Males1, Engaged1);
		Engaged1 = Engaged, 
		failedMale([(MaleID, [(FemaleID,Score)|Rest]) | RestMales], Males1).
		


% Example Query
%?- successfulProposal([(p2, [(p3, 999999999),  (p4, 999999999)])],engaged{p3:(p1, [(p3, 7.47213595499958),  (p4, 999999999)])},Males1,Engaged1).

successfulProposal([(MaleID, [(FemaleID,Score)|Rest]) | RestMales], Engaged, Males1, Engaged1) :-
	OtherGuy = Engaged.get(FemaleID),
	del_dict(FemaleID,Engaged,Value,EngagedTemp),
	MalesTemp = [OtherGuy | RestMales],
	failedMale(MalesTemp, Males1),
	Engaged1 = EngagedTemp.put(FemaleID,(MaleID, [(FemaleID,Score)|Rest])).


% true if input males results in output males 
failedMale([(MaleID, [Pref|[]]) | RestMales], RestMales).
failedMale([(MaleID, [Pref|Rest]) | RestMales], [(MaleID, Rest) | RestMales]).

%% Preparing Males for GS Algorithm
getMalesInfo(Males) :-
	findall(ID, male(ID), MaleIDs), 
	attachInfoToID(menPrefsSorted, blank, MaleIDs, Males).

menPrefsSorted(ExtraInfo, MaleID, SortedPrefs) :-
	menPrefs(MaleID, Prefs),
	insertionSort(Prefs, SortedPrefs).

%%% Citation: https://stackoverflow.com/questions/12715293/prolog-insertion-sort
insert(X, [], [X]):- !.
insert((FemaleID, Score), [(FemaleID1, Score1)|L1], [(FemaleID, Score), (FemaleID1, Score1)|L1]):- Score=<Score1, !.
insert((FemaleID, Score), [(FemaleID1, Score1)|L1], [(FemaleID1, Score1)|L]):- insert((FemaleID, Score), L1, L).

insertionSort([], []):- !.
insertionSort([(FemaleID, Score)|L], S):- insertionSort(L, S1), insert((FemaleID, Score), S1, S).
%%%

menPrefs(MaleID, Prefs) :-
	findall(ID, female(ID), Females), 
	attachInfoToID(score, MaleID, Females, Prefs).

attachInfoToID(_, _, [], []).
attachInfoToID(P, M, [A|As], [B|Bs]) :-
	pairWithIDInfo(P, M, A, B),
	attachInfoToID(P, M, As, Bs).

pairWithIDInfo(P, ExtraInfo, ID, Pair) :-
	call(P, ExtraInfo, ID, Info),
	Pair = (ID, Info).

%pairWithIDInfo(score, MaleID, FemaleID, FemaleScore)
%addFScore(MaleID, FemaleID, FemaleScore) :-
	%score(MaleID, FemaleID, Score),
	%FemaleScore = (FemaleID, Score).

%pairWithIDInfo(menPrefsSorted, ExtraInfo, MaleID, Male)
%addMPrefs(blank, MaleID, Male) :-
	%menPrefsSorted(MaleID, SortedPrefs),
	%Male = (MaleID, SortedPrefs).

% true if ID belongs to a male profile
male(ID) :- profile(ID, _, male, _, (_,_), _, (_,_), _, _).

% true if ID belongs to a female profile
female(ID) :- profile(ID, _, female, _, (_,_), _, (_,_), _, _).



%% Compatibility Scoring Functions (Soft & Hard Constraints)

% true if Result is the softscore Result or 999999999 if the hard constraints failed (a large number to fit our minimizer goal).
score(MaleID, FemaleID, Result) :-
	hardscore(MaleID, FemaleID) ->
		softscore(MaleID, FemaleID, Result);
		Result is 999999999.

% true if the two profiles meet the hard constraints (same city, opposite gender, age within partner range)
hardscore(MaleID, FemaleID) :-
	profile(MaleID, _, male, Age1, (MinAge1,MaxAge1), City, (_,_), _, _),
	profile(FemaleID, _, female, Age2, (MinAge2,MaxAge2), City, (_,_), _, _),
	MinAge2=<Age1, Age1=<MaxAge2,
	MinAge1=<Age2, Age2=<MaxAge1.

% true if Result is the sum of the difference soft score aspect results
softscore(MaleID, FemaleID, Result) :-
	profile(MaleID, _, _, Age1, (_,_), _, (Lat1, Lon1), Bio1, Star_Sign1),
	profile(FemaleID, _, _, Age2, (_,_), _, (Lat2, Lon2), Bio2, Star_Sign2),
	scoreAge(Age1, Age2, ResultAge), 
	scoreDist((Lat1,Lon1), (Lat2,Lon2), ResultDist), 
	scoreBio(Bio1, Bio2, ResultBio), 
	scoreStar(Star_Sign1, Star_Sign2, ResultStar), 
	Result is ResultAge + ResultDist + ResultBio + ResultStar.

% true if ResultAge is the difference between Age1 and Age2
scoreAge(Age1, Age2, ResultAge) :-
	ResultAge is abs(Age1 - Age2).

% true if ResultDist is the euclidean distance between (Lat1,Lon1) and (Lat2,Lon2)
scoreDist((Lat1,Lon1), (Lat2,Lon2), ResultDist) :-
	ResultDist is sqrt((Lat1-Lat2)**2 + (Lon1-Lon2)**2).

% true if ResultBio is equal to the shared number of keywords between Bio1 and Bio2, multiplied by -1 (to fit our minimizer goal)
% scoreBio(Bio1, Bio2, ResultBio) : - TODO
scoreBio(_,_,0).

% true if ResultStar is -1 for compatible (to fit our minimizer goal), and 0 otherwise
scoreStar(aries,leo,-1).
scoreStar(aries,libra,-1).
scoreStar(taurus,scorpio,-1).
scoreStar(taurus,cancer,-1).
scoreStar(gemini,sagittarius,-1).
scoreStar(gemini,aquarius,-1).
scoreStar(cancer,capricorn,-1).
scoreStar(leo,aquarius,-1).
scoreStar(leo,gemini,-1).
scoreStar(virgo,pisces,-1).
scoreStar(virgo,cancer,-1).
scoreStar(libra,sagittarius,-1).
scoreStar(scorpio,cancer,-1).
scoreStar(sagittarius,aries,-1).
scoreStar(taurus,capricorn,-1).
scoreStar(aquarius,sagittarius,-1).
scoreStar(pisces,taurus,-1).
scoreStar(_,_,0).

% The Database of Profiles of format: profile(ID, Name, Gender, Age, (MinAge,MaxAge), City, (Lat, Lon), Bio, Star_Sign)
%profile(p1, george, male, 21, (18,25), vancouver, (34,25), "I am George and I like tacos", pisces).
%profile(p2, barney, male, 35, (29,39), new_york, (900,342), "I am Barney and I like the beach", cancer).
%profile(p3, ashley, female, 24, (20,27), vancouver, (38, 27), "I am Ashley and I like running", taurus).
%profile(p4, jessica, female, 40, (35,42), new_york, (900, 342), "I am Jessica and I like tacos", aquarius).
profile(p1, "George", male, 25, (23,35), vancouver, (34,25), "I am George and I like tacos", leo).
profile(p2, "Barney", male, 60, (23,39), london, (2000,1342), "I am Barney and I like the beach", cancer).
profile(p3, "Chris", male, 26, (23,31), vancouver, (30, 24), "I am Chris and I like tennis", virgo).
profile(p4, "Ashley", female, 24, (20,32), vancouver, (35, 26), "I am Ashley and I like running", aquarius).
profile(p5, "Jessica", female, 30, (24,32), vancouver, (20, 19), "I am Jessica and I like tacos", pisces).
