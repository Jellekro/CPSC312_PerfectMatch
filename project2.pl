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

menPrefs(profile(_ ,male, Age, (_,_), _, (Lat, Lon), Bio, Star_Sign), Prefs) :-
findall(F, female(F), Females), 
ourmap(profile(_ ,male, Age, (_,_), _, (Lat, Lon), Bio, Star_Sign),transform, Females, Prefs).

ourmap(_, _,[],[]).
ourmap(M,P,[A|As],[B|Bs]) :- call(P,M,A,B),ourmap(M,P,As,Bs).

transform(profile(_ ,male, Age, (_,_), _, (Lat, Lon), Bio, Star_Sign), 
	profile(Name2 ,female, Age2, (_,_), _, (Lat2, Lon2), Bio2, Star_Sign2), Y) :-
Y is (Name2, score(profile(_ ,male, Age, (_,_), _, (Lat, Lon), Bio, Star_Sign), 
	profile(Name2 ,female, Age2, (_,_), _, (Lat2, Lon2), Bio2, Star_Sign2))).

% female(F) is true if person F is a female
female(profile(_, female, _, (_,_), _, (_,_), _, _)).

% male(M) is true if person M is a male
male(profile(_, male, _, (_,_), _, (_,_), _, _)).


% Compatibility Scoring Functions (Soft + Hard Constraints)

% score is true if Result is softscore or zero.
score(profile(_, Gender, Age, (MinAge,MaxAge), City, (Lat, Lon), Bio, Star_Sign), profile(_, Gender2, Age2, (MinAge2,MaxAge2), City2, (Lat2, Lon2), Bio2, Star_Sign2), Result) :-
(hardscore(profile(_, Gender, Age, (MinAge,MaxAge), City, (_,_), _, _), 
	profile(_, Gender2, Age2, (MinAge2,MaxAge2), City2, (_,_), _, _)) -> 
	softscore(profile(_, _, Age, (_,_), _, (Lat, Lon), Bio, Star_Sign), 
		profile(_, _, Age2, (_,_), _, (Lat2, Lon2), Bio2, Star_Sign2), Result) ; Result is 999999). 
% FIX MAX INT ABOVE TODO
% hardscore is true if the two profiles meet the hard constraints
hardscore(profile(_, male, Age, (MinAge,MaxAge), City, (_,_), _, _), 
	profile(_, female, Age2, (MinAge2,MaxAge2), City, (_,_), _, _)) :-
MinAge2=<Age, Age=<MaxAge2, MinAge=<Age2, Age2=<MaxAge.

% softscore is true if all scores are generated and added to result
softscore(profile(_, _, Age, (_,_), _, (Lat, Lon), Bio, Star_Sign), profile(_, _, Age2, (_,_), _, (Lat2, Lon2), Bio2, Star_Sign2), Result) :-
scoreAge(Age,Age2, Ares), 
scoreDist(Lat,Lon,Lat2,Lon2, Bres), 
scoreBio(Bio, Bio2, Cres), 
scoreStar(Star_Sign, Star_Sign2, Dres), 
Result is Ares + Bres + Cres + Dres.

% scoreAge is true if Ares is equal to the difference in Age and Age2.
scoreAge(Age,Age2,Ares) :-
	(Age > Age2 -> Ares is Age - Age2 ; Ares is Age2 - Age).

% scoreDist is true if Bres is equal to the difference in eucleandian distance between (Lat,Lon) and (Lat2,Lon2).
scoreDist(Lat,Lon,Lat2,Lon2, Bres) :-
	A is Lat - Lat2, B is A*A, C is Lon - Lon2, D is C*C, Bres is sqrt(B + D).

% scoreBio is true if Cres is equal to the shared number of keywords * -1.
% scoreBio(Bio, Bio2, Cres) : - TODO
scoreBio(_,_,0).

% scoreStar is true if Dres is -1 for compatible, 0 otherwise
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

% The Database of Facts to be queried

% profile(Name, Gender, Age, (MinAge,MaxAge), City, (Lat, Lon), Bio, Star_Sign)
profile(george, male, 21, (18,25), vancouver, (34,25), "I am George and I like tacos", pisces).
profile(barney, male, 35, (29,39), new_york, (900,342), "I am Barney and I like the beach", cancer). 
profile(ashley, female, 24, (20,27), vancouver, (38, 27), "I am Ashley and I like running", taurus).
profile(jessica, female, 40, (35,42), new_york, (900, 342), "I am Jessica and I like tacos", aquarius).

