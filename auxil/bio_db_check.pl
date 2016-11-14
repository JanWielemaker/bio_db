
:- ensure_loaded( 'lib/prefix_atom' ).

:- debug( bio_db_check ).
:- lib( os_sub ).

/** bio_db_check.

Run some consistency checks on sources. First it check the local ../prolog/bio_db.pl file
and the compare the mod definition to the files in pack(bio_db_repo/data.

*/

bio_db_check :-
	expand_file_name( '$HOME/pl/packs/src/bio_db', [BioDir] ),
	debug( bio_db_check, 'bio_db home: ~p', BioDir ),
	Self = bio_db_check,
	working_directory( Old, BioDir ),
	open( 'prolog/bio_db.pl', read, In ),
	read( In, (:- module(bio_db,Defined)) ),
	partition( prefix_pid(edge), Defined, Graphs, _ ),
	partition( prefix_pid(map), Defined, Maps, _ ),
	debug( Self, 'Graphs:', true ),
	maplist( debug_sub, Graphs ),
	nl,
	debug( Self, 'Maps:', true ),
	maplist( debug_sub, Maps ),
	append( Graphs, Maps, DbPreds ),
	bio_db_check_clauses( DbPreds, In ),
	close( In ),
	working_directory( _, Old ),
	absolute_file_name( pack(bio_db_repo/data), RepoD, [file_type(directory),mode(exist)] ),
	debug( Self, 'Repo root installed at: ~p', RepoD ),
	bio_db_check_subs( RepoD, DbPreds ).

bio_db_check_subs( RepoD , DbPred ) :-
	os_sub_files( RepoD, Subs ),
	% debug( Self, 'Repo has sub files: ~p', [Subs] ),
	Subs = os( SubList ),
	bio_db_check_subs( SubList, DbPred, Undeclared, Undefined ),
	bio_db_check_pred_undef( Undefined ),
	bio_db_check_pred_undcl( Undeclared ).

bio_db_check_subs( [], Defs, [], Defs ).
bio_db_check_subs( [F|Fs], Defs, Undeclared, Undefined ) :-
	file_base_name( F, Base ),
	file_name_extension( Stem, _Ext, Base ),
	bio_db_check_subs_stem( Stem, Defs, Undeclared, TUndeclared, RemDefs ),
	bio_db_check_subs( Fs, RemDefs, TUndeclared, Undefined ).

bio_db_check_subs_stem( Stem, Defs, Undeclared, TUndeclared, RemDefs ) :-
	member( Pfx, [map_,edge_] ),
	atom_concat(Pfx,_,Stem),
	!,
	bio_db_check_subs_stem_select( Stem, Defs, Undeclared, TUndeclared, RemDefs ).
bio_db_check_subs_stem( Stem, Defs, Undeclared, TUndeclared, Defs ) :-
	debug( bio_db_check, 'Skipping non db stem: ~p', Stem ),
	!,
	TUndeclared = Undeclared.

bio_db_check_subs_stem_select( Stem, Defs, Undeclared, TUndeclared, RemDefs ) :-
	select( Stem/_, Defs, RemDefs ),
	% debug( bio_db_check, 'Matched, : ~w', [Stem] ),
	!,
	Undeclared = TUndeclared.
bio_db_check_subs_stem_select( Stem, Defs, Undeclared, TUndeclared, Defs ) :-
	Undeclared = [Stem|TUndeclared].

prefix_pid( Pfx, Pname/_Arity ) :-
	prefix_atom( Pfx, Pname ).

debug_sub( Sub ) :-
	debug( bio_db_check, ' + ~w', Sub ).

bio_db_check_clauses( DbPreds, In ) :-
	read( In, First ),
	bio_db_check_clauses( First, DbPreds, In, UndefPreds, UndclPreds ),
	bio_db_check_clauses_undef( UndefPreds ),
	bio_db_check_clauses_undcl( UndclPreds ).

bio_db_check_clauses( end_of_file, Undefined, _In, Undefined, [] ) :- !.
bio_db_check_clauses( Term, Defs, In, Undef, Undcl ) :-
	term_defines_pred( Term, Pid ),
	!,
	bio_db_check_pred( Pid, Defs, RemDefs, Undcl, TUndcl ),
	read( In, NxtTerm ),
	bio_db_check_clauses( NxtTerm, RemDefs, In, Undef, TUndcl ).

bio_db_check_clauses( _Term, Defs, In, Undef, Undcl ) :-
	read( In, NxtTerm ),
	bio_db_check_clauses( NxtTerm, Defs, In, Undef, Undcl ).

bio_db_check_pred( Pid, Defs, RemDefs, Undcl, TUndcl ) :-
	select( Pid, Defs, RemDefs ),
	!,
	Undcl = TUndcl.
bio_db_check_pred( Pid, Defs, RemDefs, Undcl, TUndcl ) :-
	( prefix_pid(map_,Pid);prefix_pid(edge_,Pid) ),
	!,
	Undcl = [Pid|TUndcl],
	RemDefs = Defs.
bio_db_check_pred( _Pid, Defs, Defs, Undcl, Undcl ).

bio_db_check_clauses_undef( [] ) :- !,
	debug( bio_db_check, '', true ),
	debug( bio_db_check, 'No undefined predicates', true ).
bio_db_check_clauses_undef( Undefined ) :-
	debug( bio_db_check, '', true ),
	debug( bio_db_check, 'Undefined: ~w', [Undefined] ).

bio_db_check_clauses_undcl( [] ) :- !,
	debug( bio_db_check, 'No undeclared predicates', true ),
	debug( bio_db_check, '', true ).
bio_db_check_clauses_undcl( Undeclared ) :-
	debug( bio_db_check, 'Undeclared: ~w', [Undeclared] ),
	debug( bio_db_check, '', true ).

term_defines_pred( ( Head :- _Body ), Functor/Arity ) :-   % we ignore facts on purpose
	functor( Head, Functor, Arity ).

bio_db_check_pred_undef( [] ) :- !,
	debug( bio_db_check, '', true ),
	debug( bio_db_check, 'No predicates where missing db files', true ).
bio_db_check_pred_undef( Undef ) :-
	debug( bio_db_check, '', true ),
	debug( bio_db_check, 'Following preds have no db file: ~w', [Undef] ).

bio_db_check_pred_undcl( [] ) :- !,
	debug( bio_db_check, 'No db files have missing declarations', true ),
	debug( bio_db_check, '', true ).
bio_db_check_pred_undcl( Undeclared ) :-
	debug( bio_db_check, 'These db files had no corresponding declarations: ~w', [Undeclared] ),
	debug( bio_db_check, '', true ).
