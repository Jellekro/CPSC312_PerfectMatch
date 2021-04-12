% Project 2
% Sam Fraser h0c2b
% Andy Worth k8m1b
% Prolog representation of a dating matching algorithm
% Builds a list of stable matches based on compatibility scores, 
% and provides the best match given a person X and a KB of profiles.

% Try:
%?- perfect_match().
%?- perfect_match().
%?- perfect_match().


% Iteration Over List of Men & List of Women to Create Scores

menPrefs(MaleID, Prefs) :-
	findall(ID, female(ID), Females), 
	scoreFemales(transform, MaleID, Females, Prefs).

scoreFemales(_, _, [], []).
scoreFemales(P, M, [A|As], [B|Bs]) :-
	call(P, M, A, B),
	scoreFemales(P, M, As, Bs).

transform(MaleID, FemaleID, FemaleScore) :-
	score(MaleID, FemaleID, Score),
	FemaleScore = (FemaleID, Score).

% true if ID belongs to a male profile
male(ID) :- profile(ID, _, male, _, (_,_), _, (_,_), _, _).

% true if ID belongs to a female profile
female(ID) :- profile(ID, _, female, _, (_,_), _, (_,_), _, _).



% Compatibility Scoring Functions (Soft & Hard Constraints)

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
%scoreStar(,,-1).
%scoreStar(,,-1).
%scoreStar(,,-1).
%scoreStar(,,-1).
%scoreStar(,,-1).
%scoreStar(,,-1).
%scoreStar(,,-1).
%scoreStar(,,-1).
%scoreStar(,,-1).
%scoreStar(,,-1).
%scoreStar(,,-1).
%scoreStar(,,-1).
%scoreStar(,,-1).
%scoreStar(,,-1).
%scoreStar(,,-1).
%scoreStar(,,-1).
%scoreStar(,,-1).
%scoreStar(,,-1).
%scoreStar(,,-1).
%scoreStar(,,-1).
%scoreStar(,,-1).
%scoreStar(,,-1).
scoreStar(_,_,0).

% The Database of Profiles of format: profile(ID, Name, Gender, Age, (MinAge,MaxAge), City, (Lat, Lon), Bio, Star_Sign)
profile(p1, george, male, 21, (18,25), vancouver, (34,25), "I am George and I like tacos", pisces).
profile(p2, barney, male, 35, (29,39), new_york, (900,342), "I am Barney and I like the beach", cancer). 
profile(p3, ashley, female, 24, (20,27), vancouver, (38, 27), "I am Ashley and I like running", taurus).
profile(p4, jessica, female, 40, (35,42), new_york, (900, 342), "I am Jessica and I like tacos", aquarius).
