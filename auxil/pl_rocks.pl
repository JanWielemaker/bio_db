
% :- ensure_loaded( 'lib/requires_minimal' ).
:- use_module( library(requires) ).

:- lib(rocksdb).
:- lib(os).
:- lib(options).

:- ensure_loaded( 'lib/bio_db_pl_info' ).

% :- debug( rocks ).

/** pl_rocks( +PlFile ).

Convert a prolog bio_db file to rocks database file.
PlFile could be the full filename or the stem minus the extension 'pl'.

*/

pl_rocks( FoS ) :-
	en_list( FoS, Fos ),
	maplist( pl_rocks_file, Fos ).

pl_rocks_file( FoS ) :-
	( file_name_extension(_,pl,FoS) ->
		PlF = FoS
		;
		file_name_extension( FoS, pl, PlF )
	),
	os_ext( pl, rocks, PlF, DbF ),
	os_remove( DbF, [exists(false),debug(true)] ),
	Mess = 'Prolog input: ~p, rocksdb output: ~p',
	debug( rocks, Mess, [PlF,DbF] ),
	pl_rocks_file_transcribe( PlF, DbF ),
	!.
pl_rocks_file( FoS ) :-
	throw( failed_to_convert_to_rocks(FoS) ).

pl_rocks_file_transcribe( PlF, DbF ) :-
	bio_db_pl_info( PlF, Pname, Arity, Infos, NexTerm, PlStream ),
	os_postfix( info, DbF, InfoF, sep('_') ),
	rocks_open( InfoF, InfoHandle, [key(atom),value(term)] ),
	findall( _, ( member(InfoTerm,Infos),
	              arg(1,InfoTerm,InfoKey),
			    arg(2,InfoTerm,InfoVal),
			    rocks_put(InfoHandle,InfoKey,InfoVal)
	            ), _),
	rocks_close( InfoHandle ),
	Arity > 1,
	atom_concat( Pname, '_info', InfoName ),
	RelTypeG =.. [InfoName,relation_type,RelType],
	memberchk( RelTypeG, Infos ),
	DataTypeG =.. [InfoName,data_types,DataTypeT],
	memberchk( DataTypeG, Infos ),
	bio_db_info_db_types( rocks, RelType, DataTypeT, Dup, _OpenTypes, KeyType, ValType ),

	% currently rocks doesn't allow duplicates
	( Dup == false -> KeyType = NoDupKeyType; NoDupKeyType = term ),
	NoDupOpenTypes = [key(NoDupKeyType),value(ValType)],

	Open = rocks_open( DbF, Out, NoDupOpenTypes ),
	debug( rocks, 'Opening rocksdb for updating: ~w', Open ),
	% rocks_open( DbF, Out, NoDupOpenTypes ),
	call( Open ),
	pl_rocks_streams( NexTerm, PlStream, Pname/Arity, Dup, KeyType, ValType, Out ),
	close( PlStream ),
	rocks_close( Out ).

pl_rocks_streams( end_of_file, _In, _Pid, _Dup, _Kt, _Vt, _Out ) :- !.
pl_rocks_streams( InTerm, In, Pid, Dup, Kt, Vt, Out ) :-
	% debug( pl_rocks, 'Doing term: ~w', InTerm ),
	pl_rocks_term( InTerm, Pid, Dup, Kt, Vt, Out ),
	read( In, NxtTerm ),
	% debug( rocks, 'In term: ~w', NxtTerm ),
	pl_rocks_streams( NxtTerm, In, Pid, Dup, Kt, Vt, Out ).

pl_rocks_term( InTerm, Pname/Arity, Dup, Kt, Vt, Out ) :-
	functor( InTerm, InPname, InArity ),
	check_term( InPname, Pname, InArity, Arity ),
	pl_rocks_pname_term( Arity, Pname, InTerm, Dup, Kt, Vt, Out ).

check_term( Pname, Pname, Arity, Arity ) :- !.
check_term( InPname, Pname, InArity, Arity ) :-
	throw( functor_mismatch(InPname/InArity,Pname/Arity) ).

pl_rocks_pname_term( 2, _Pname, Term, Dup, _Kt, _Vt, Handle ) :-
	!,
	arg( 1, Term, Key ),    
	arg( 2, Term, Val ),
	rocks_dup_put( Dup, Handle, Key, Val ).

pl_rocks_pname_term( _Arity, _Pname, Term, Dup, _Kt, _Vt, Out ) :-
	% then Arity > 2,
	Term =.. [_PnameP,Key|Vals],
	pl_rocks_vals_term( Vals, Val ),
	rocks_dup_put( Dup, Out, Key, Val ).

rocks_dup_put( false, Handle, Key, Val ) :-
	rocks_put( Handle, Key, Val ).

rocks_dup_put( true, Handle, Key, Val ) :-
	( rocks_get(Handle,Key,_) ->
		rocks_dup_put_iter( Key, 1, Handle, Val )
		;
	     % debug( rocks, 'Puts in Rock : ~w - ~w', [Key,Val] ),
		rocks_put( Handle, Key, Val )
	).

rocks_dup_put_iter( Key, I, Handle, Val ) :-
	rocks_get( Handle, Key:I, _ ),
	!,
	J is I + 1,
	rocks_dup_put_iter( Key, J, Handle, Val ).
rocks_dup_put_iter( Key, I, Handle, Val ) :-
	% debug( rocks, 'Puts in Rock : ~w - ~w', [Key,Val] ),
	rocks_put( Handle, Key:I, Val ).

pl_rocks_vals_term( [H], H ) :- !.
pl_rocks_vals_term( [H|T], H+R ) :-
	pl_rocks_vals_term( T, R ).
