%% repoint_link( Link, Target ).
%
% Repoint an existing Link to a new Target.
% Pred fails if Link exists but not a link.
% Pred is debug(repoint_link) aware.
%
%==
% ?- debug( repoint
% ?- shell( 'touch atzoumbalos' ).
% ?- shell( 'ln -s atzoumbalos shortos' ).
% ?- shell( 'touch atzoukos ).
% ?- repoint_link( shortos, atzoukos ).
% ?- repoint_link( shortolos, atzoukos ).
%==
% @author nicos angelopoulos
% @version  0.1 2014/7/23
% @tbd extend interface to control link_file/3 3rd argument?
%
repoint_link( Link, NewTarget ) :-
	exists_file( Link ),
	!,
	debug( repoint_link, 'Repointing existing link: ~p', Link ),
	repoint_link_exists_file( Link, NewTarget ).
repoint_link( Link, NewTarget ) :-
	debug( repoint_link, 'Warning, repointing link does not exist: ~p', Link ),
	link_file( NewTarget, Link, symbolic ),
	debug( repoint_link, 'Linked to: ~p', NewTarget ).

repoint_link_exists_file( Link, NewTarget ) :-
	read_link( Link, _, OldTarget ),
	!,
	debug( repoint_link, 'Old target was: ~p', OldTarget ),
	delete_file( Link ),
	link_file( NewTarget, Link, symbolic ),
	debug( repoint_link, 'Linked to: ~p', NewTarget ).
repoint_link_exists_file( Link, _NewTarget ) :-
	debug( repoint_link, 'Failure due to input being a file: ~p', Link ),
	fail.
