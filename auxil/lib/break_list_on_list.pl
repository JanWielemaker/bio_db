:- ensure_loaded( library(lists) ).		% append/3.

break_list_on_list( [X|Xs], [X|Ys], [], Rs ) :-
	append( Ys, Rs, Xs ),
	!.
break_list_on_list( [X|Xs], Ys, [X|Ls], Rs ) :-
	break_list_on_list( Xs, Ys, Ls, Rs ).
