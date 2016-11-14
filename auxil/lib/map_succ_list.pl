
%% map_succ_list( +Goal, ?InList, ?OutList, -Rejects ).
%% map_succ_list( +Goal, ?InList, ?OutList ).
% 
%  Applly Goal(In,Out) to InList, keeping in OutList all Out elements
% for calls that were successful. Also works for - InList, + OutList
%
% @version 0:0:3, 2013/03/13
% Nicos. 2000/04/07.
% Nicos. 2013/01/18, changed order of args to comply with library(apply).
% there was a recent email to swipl list, this should be argued for inclusion
% to library(apply). (Michael H)
%
map_succ_list( Goal, InList, OutList ) :-
	map_succ_list_1( InList, Goal, OutList, _Rej ).

map_succ_list( Goal, InList, OutList, Rej ) :-
	map_succ_list_1( InList, Goal, OutList, Rej ).

map_succ_list_1( [], _Goal, [], [] ) :- !.
map_succ_list_1( InList, Goal, OutList, Rej ) :-
	( var(OutList) ->
		InList = [H|T],
		( call(Goal,H,Ho) -> 
			OutList = [Ho|Tout],
			TRej = Rej
			;
			Tout = OutList,
			Rej = [H|TRej]
 		)
		;
		OutList = [Ho|Tout],
		( call(Goal,H,Ho) -> 
			InList = [H|T],
			TRej = Rej
			;
			T = InList,
			Rej = [Ho|TRej]
		)
	),
	map_succ_list_1( T, Goal, Tout, TRej ).
