%%  break_list_on( +List, +Element, ?LeftPartition, ?RightPartition ).
% Element does not appear in either the end of LeftPartition,
% or as first element of RightPartition.
% Only finds first partition so Element should be ground 
% ==
% ?- 
%     break_list_on( L, El, [a], [c,b,d,b,e] ).
% 
%  L = [a, El, c, b, d, b, e].
%
% ?-
%    break_list_on( [1,a,2,a,3], a, Left, Right ).
%
%    Left = [1],
%    Right = [2, a, 3].
%
% ==
break_list_on( [X|Xs], X, [], Xs ) :-
	!.
break_list_on( [X|Xs], Xa, [X|XLa], XRa ) :-
	break_list_on( Xs, Xa, XLa, XRa ).
