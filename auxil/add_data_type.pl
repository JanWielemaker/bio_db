
:- ensure_loaded( 'lib/holds' ).    % /2.

:- use_module( library(os) ).
:- use_module( library(options) ).

:- debug( add_data_type ).

/** add_data_type( FileSorDirS ).

Adds data_type, info entry to a single or multiple .pl files and directories
*/
add_data_type( OseS ) :-
	en_list( OseS, Oses ),
	maplist( add_data_type_to, Oses ).

add_data_type_to( Os ) :-
	os_file( Os ),
	os_ext( pl, Os ),
	!,
	add_data_type_to_file( Os ).
add_data_type_to( Os ) :-
	os_dir( Os ),
	debug( add_data_type, 'Descending to dir: ~p', Os ),
	working_directory( Old, Os ),
	directory_contents( '.', Oses ),
	maplist( add_data_type_to, Oses ),
	working_directory( _, Old ).

add_data_type_to_file( Os ) :-
	bio_db_pl_info( Os, Pname, Arity, Infos, NexTerm, DbStream ),
	maplist( writeln, Infos ),
	add_data_type_to_file_infos( Infos, Os, Pname, Arity, NexTerm, DbStream ).

add_data_type_to_file_infos( Infos, Os, Pname, _Arity, _NexTerm, DbStream ) :-
	atom_concat( Pname, '_info', Iname ),
	ITerm =.. [Iname,data_types,_], 
	memberchk( ITerm, Infos ),
	close( DbStream ),
	!,
	debug( add_data_type, 'Data types info term already present in file: ~p', Os ).
add_data_type_to_file_infos( _Infos, Os, Pname, Arity, NexTerm, DbS ) :-
	findall( integer, between(1,Arity,_), DefTypes ),
	findall( atom, between(1,Arity,_), TrmTypes ),
	add_data_type_get_types( NexTerm, Pname, Arity, DbS, DefTypes, TrmTypes, DataTypes ),
	close( DbS ),
	os_ext( tmp, Os, TmpOs ),
	open( TmpOs, write, OutSt ),
	DataTypeVal =.. [data_types|DataTypes],
	atom_concat( Pname, '_info', Iname ),
	DataTypeInfo =.. [Iname,data_types,DataTypeVal],
	portray_clause( OutSt, DataTypeInfo ),
	open( Os, read, InSt ),
	copy_stream_data( InSt, OutSt ),
	close( OutSt ),
	close( InSt ),
	debug( add_data_type, 'Tmp file: ~p', TmpOs ),
	delete_file( Os ),
	rename_file( TmpOs, Os ).

add_data_type_get_types( end_of_file, _Pname, _Arity, _DBs, Types, _TrmTypes, Types ) :- !.
add_data_type_get_types( Term, Pname, Arity, DbS, CurrTypes, TrmTypes, DataTypes ) :-
	functor( Term, Pname, Arity ),
	add_data_type_update_types( true, 1, Arity, Term, CurrTypes, NextTypes ),
	holds( NextTypes == TrmTypes, Holds ),
	add_data_type_get_types_cont( Holds, Pname, Arity, DbS, NextTypes, TrmTypes, DataTypes ).

add_data_type_get_types_cont( true, _Pname, _Arity, _DbS, Types, _TrmTypes, Types ).
add_data_type_get_types_cont( false, Pname, Arity, DbS, CurrTypes, TrmTypes, DataTypes ) :-
	read( DbS, NexTerm ),
	add_data_type_get_types( NexTerm, Pname, Arity, DbS, CurrTypes, TrmTypes, DataTypes ).

add_data_type_update_types( false, _I, _Arity, _Term, [], [] ).
add_data_type_update_types( true, I, Arity, Term, [Y1|T1], [Y2|T2] ) :-
	arg( I, Term, Ith ),
	add_data_type_of( Ith, NewType ),
	once( add_data_type_cohese( NewType, Y1, Y2 ) ),
	J is I + 1,
	holds( J =< Arity, Cont ),
	add_data_type_update_types( Cont, J, Arity, Term, T1, T2 ).

add_data_type_of( Data, Type ) :-
	integer(Data),
	!,
	Type = integer.
add_data_type_of( Data, Type ) :-
	number(Data),
	!,
	Type = number.
add_data_type_of( _Data, atom ).

add_data_type_cohese( integer, Type2, Type2 ).
add_data_type_cohese( number, integer, number ).
add_data_type_cohese( number, Type2, Type2 ).
add_data_type_cohese( atom, _, atom ).


