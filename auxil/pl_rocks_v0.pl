
:- use_module( library(requires) ).

:- lib(rocksdb).
:- lib(os).
:- lib(options).

:- requires( has_extension/3 ).

% :- debug( rock ).

/** pl_rocks( +PlFile ).

Convert a prolog bio_db file to rocks database file.
PlFile could be the full filename or the stem minus the extension 'pl'.

*/

pl_rocks_test :-
	cd( '/usr/local/users/nicos/work/bio_db/dnloads/hgnc/maps' ),
	% cd( '/usr/local/users/nicos/work/bio_db/data/maps/hgnc' ),
	pl_rocks( 'map_hgnc_hgnc_symb.pl' ).

pl_rocks( FoS ) :-
	en_list( FoS, Fos ),
	maplist( pl_rocks_file, Fos ).

pl_rocks_file( FoS ) :-
	has_extension( FoS, pl, PlF ),
	os_ext( pl, rocks, PlF, DbF ),
	/*
	( atom_concat(edge,_,PlF) -> 
		os_postfix(bk3,DbF,Bk3F),
		os_remove( Bk3F, [exists(false),debug(true)] ),
		pl_bdb_file_transcribe( PlF, true, Bk3F )
		;
		true
	),
	*/
	Mess = 'Prolog input: ~p, Rocks output: ~p',
	debug( rock, Mess, [PlF,DbF] ),
	os_remove( DbF, [exists(false),debug(true)] ),
	pl_rocks_file_transcribe( PlF, true, DbF ).

pl_rocks_file_transcribe( PlF, Bk3, DbF ) :-
	open( PlF, read, In ),
	read( In, Fst ),
	rocks_open( DbF, Rocks, [key(term),value(term)] ),
	pl_rocks_streams( Fst, In, Bk3, Rocks ),
	close( In ),
	rocks_close( Rocks ).

pl_rocks_streams( end_of_file, _In, _Bk3, _Out ) :- !.
pl_rocks_streams( InTerm, In, Bk3, Out ) :-
	% debug( pl_bdb, 'Doing term: ~w', InTerm ),
	pl_rocks_term( InTerm, Bk3, Out ),
	read( In, NxtTerm ),
	debug( rock, 'Read term: ~w', NxtTerm ),
	pl_rocks_streams( NxtTerm, In, Bk3, Out ).

pl_rocks_term( InTerm, Bk3, Out ) :-
	functor( InTerm, Pname, Arity ),
	pl_rocks_pname_term( Pname, Arity, InTerm, Bk3, Out ).

pl_rocks_pname_term( Pname, Arity, Term, _Bk3, Rock ) :-
	sub_atom( Pname, _, _, 0, info ), % 0 makes deterministic
	!,
	Arity =:= 2,  % fixme: give proper error if false,
	arg( 1, Term, InfoKey ),
	arg( 2, Term, InfoVal ),
	rocks_put_unique( info+InfoKey, InfoVal, Rock ).
pl_rocks_pname_term( _Pname, Arity, Term, _Bk3, Rock ) :-
	Arity =:= 2,
	!,
	arg( 1, Term, Key ),
	arg( 2, Term, Val ),
	rocks_put_unique( Key, Val, Rock ).
pl_rocks_pname_term( _Pname, Arity, Term, Bk3, Rock ) :-
	Arity =:= 3,
	!,
	pl_rocks_pname_ternary_term( Bk3, Term, Rock ).

pl_rocks_pname_ternary_term( false, Term, Rock ) :-
	throw( fixme(also_bdb) ),
	arg( 1, Term, Key ),
	arg( 2, Term, Val ),
	arg( 3, Term, Wei ),
	here( unimplemented ),
	rocks_put( Rock, Key, Val ),
	% bdb_put( Out, Key+Val, Wei ),
	rocks_put( Rock, Key, Val+Wei ),
	rocks_put( Rock, Val, Key+Wei ).

pl_rocks_pname_ternary_term( true, Term, Rock ) :-
	arg( 1, Term, Key ),
	arg( 2, Term, Val ),
	arg( 3, Term, Wei ),
	rocks_put_unique( Key, Val+Wei, Rock ).

rocks_put_unique( Key, Val, Rock ) :-
	rocks_get( Rock, Key, _ ),
	!,
	rocks_put_unique_iter( Key, Val, 1, Rock ).
rocks_put_unique( Key, Val, Rock ) :-
	debug( rock, 'Sending: ~w,', rocks_put( Rock, Key, Val ) ),
	rocks_put( Rock, Key, Val ).

rocks_put_unique_iter( Key, Val, I, Rock ) :-
	rocks_get( Rock, Key:I, _ ),
	!,
	J is I + 1,
	rocks_put_unique_iter( Key, Val, J, Rock ).
rocks_put_unique_iter( Key, Val, I, Rock ) :-
	rocks_put( Rock, Key:I, Val ).
