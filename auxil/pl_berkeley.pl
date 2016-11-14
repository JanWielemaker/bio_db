
:- use_module( library(requires) ).

:- lib(bdb).
:- lib(os_lib).
:- lib(options).

:- ensure_loaded( '../src/lib/has_extension' ).  % /3.
:- ensure_loaded( 'lib/bio_db_pl_info' ).        % /6.

/** pl_bdb( +PlFile ).

Convert a prolog bio_db file to bdb database file.
PlFile could be the full filename or the stem minus the extension 'pl'.

*/

pl_bdb_test :-
	cd( '/usr/local/users/nicos/work/bio_db/dnloads/hgnc/maps' ),
	% cd( '/usr/local/users/nicos/work/bio_db/data/maps/hgnc' ),
	pl_bdb( 'map_hgnc_hgnc_symb.pl' ).

pl_berkeley( FoS ) :-
	pl_bdb( FoS ).

pl_bdb( FoS ) :-
	en_list( FoS, Fos ),
	maplist( pl_bdb_file, Fos ).

pl_bdb_file( FoS ) :-
	has_extension( FoS, pl, PlF ),
	os_ext( pl, db, PlF, DbF ),
	/*
	( atom_concat(edge,_,PlF) -> 
		os_postfix(bk3,DbF,Bk3F),
		os_remove( Bk3F, [exists(false),debug(true)] ),
		pl_bdb_file_transcribe( PlF, true, Bk3F )
		;
		true
	),
	*/
	os_remove( DbF, [exists(false),debug(true)] ),
	Mess = 'Prolog input: ~p, Berkeley output: ~p',
	debug( bio_db, Mess, [PlF,DbF] ),
	pl_bdb_file_transcribe( PlF, false, DbF ),
	!.
pl_bdb_file( FoS ) :-
	throw( failed_to_convert_to_berkeley(FoS) ).

pl_bdb_file_transcribe( PlF, Bk3, DbF ) :-
	/*
	bio_db_pl_info( PlF, PnameX, ArityX, InfosX, NexTermX, PlStreamX ),
	findall( integer, between(1,Arity,_), DefTypes ),
	findall( atom, between(1,Arity,_), TrmTypes ),
	add_data_type_get_types( NexTerm, PnameX, ArityX, PlStreamX, DefTypes, TrmTypes, DataTypes ),
	*/

	bio_db_pl_info( PlF, Pname, Arity, Infos, NexTerm, PlStream ),
	os_postfix( info, DbF, InfoF, sep('_') ),

	bdb_open( InfoF, update, InfoHandle, [key(atom),value(term)] ),
	findall( _, ( member(InfoTerm,Infos),
	              arg(1,InfoTerm,InfoKey),
			    arg(2,InfoTerm,InfoVal),
			    bdb_put(InfoHandle,InfoKey,InfoVal)
	            ), _),
	bdb_close( InfoHandle ),
	Arity > 1,
	atom_concat( Pname, '_info', InfoName ),
	RelTypeG =.. [InfoName,relation_type,RelType],
	memberchk( RelTypeG, Infos ),
	DataTypeG =.. [InfoName,data_types,DataTypeT],
	memberchk( DataTypeG, Infos ),
	bio_db_info_db_types( berkeley, RelType, DataTypeT, Dup, _OpenTypes, KeyType, ValType ),

	% Open = bdb_open( DbF, update, Out, [duplicates(Dup)|OpenTypes] ),  % 0.5
	% make sure the line below mirrors the one in: bio_db_ensure_loaded_1(berkeley,_,_,_)
	Open = bdb_open( DbF, update, Out, [duplicates(Dup),key(KeyType),value(ValType)] ),  
	debug( bio_db, 'Opening bdb for updating: ~w', Open ),
	call( Open ),
	pl_bdb_streams( NexTerm, PlStream, Pname/Arity, KeyType, ValType, Bk3, Out ),
	close( PlStream ),
	bdb_close( Out ).

pl_bdb_streams( end_of_file, _In, _Pid, _Kt, _Vt, _Bk3, _Out ) :- !.
pl_bdb_streams( InTerm, In, Pid, Kt, Vt, Bk3, Out ) :-
	% debug( pl_bdb, 'Doing term: ~w', InTerm ),
	pl_bdb_term( InTerm, Pid, Kt, Vt, Bk3, Out ),
	read( In, NxtTerm ),
	pl_bdb_streams( NxtTerm, In, Pid, Kt, Vt, Bk3, Out ).

pl_bdb_term( InTerm, Pname/Arity, Kt, Vt, Bk3, Out ) :-
	functor( InTerm, InPname, InArity ),
	check_term( InPname, Pname, InArity, Arity ),
	pl_bdb_pname_term( Arity, Pname, InTerm, Kt, Vt, Bk3, Out ).

check_term( Pname, Pname, Arity, Arity ) :- !.
check_term( InPname, Pname, InArity, Arity ) :-
	throw( functor_mismatch(InPname/InArity,Pname/Arity) ).

/* infos are now hadled separately
pl_bdb_pname_term( Pname, Arity, Term, _Bk3, Out ) :-
	sub_atom( Pname, _, _, 0, info ), % 0 makes deterministic
	!,
	Arity =:= 2,  % fixme: give proper error if false,
	arg( 1, Term, InfoKey ),
	arg( 2, Term, InfoVal ),
	bdb_put( Out, info+InfoKey, InfoVal ).
	*/
pl_bdb_pname_term( 2, _Pname, Term, _Kt, _Vt, _Bk3, Out ) :-
	!,
	arg( 1, Term, Key ),    % do we need to cast to Kt here ? maybe pack(bdb) takes care of it
	arg( 2, Term, Val ),    % do we need to cast to Vt here ? maybe pack(bdb) takes care of it
	bdb_put( Out, Key, Val ).
/* fixme delete bk3 ? or document what it does and use it
pl_bdb_pname_term( _Pname, Arity, Term, Bk3, Out ) :-
	Arity =:= 3,
	!,
	pl_bdb_pname_ternary_term( Bk3, Term, Out ).*/
pl_bdb_pname_term( _Arity, _Pname, Term, _Kt, _Vt, _Bk3, Out ) :-
	% then Arity > 2,
	Term =.. [_PnameP,Key|Vals],
	pl_bdb_vals_atom( Vals, Val ),
	bdb_put( Out, Key, Val ).

pl_bdb_vals_atom( [H], H ) :- !.
pl_bdb_vals_atom( [H|T], H+R ) :-
	pl_bdb_vals_atom( T, R ).

pl_bdb_pname_ternary_term( false, Term, Out ) :-
	arg( 1, Term, Key ),
	arg( 2, Term, Val ),
	arg( 3, Term, Wei ),
	bdb_put( Out, Key, Val ),
	% bdb_put( Out, Key+Val, Wei ),
	bdb_put( Out, Key, Val+Wei ),
	bdb_put( Out, Val, Key+Wei ).

pl_bdb_pname_ternary_term( true, Term, Out ) :-
	arg( 1, Term, Key ),
	arg( 2, Term, Val ),
	arg( 3, Term, Wei ),
	bdb_put( Out, Key, Val+Wei ).
