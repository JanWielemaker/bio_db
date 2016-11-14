
:- lib( os ).
:- lib( options ).
:- lib( debug_call ).
:- lib( mtx ).

:- requires( read_terms/2 ).
:- set_prolog_stack(global, limit(2*10**11)).

:- debug( sql3 ).

pl_sqlite3( FileS ) :-
	en_list( FileS, Files ),
	maplist( pl_sqlite3_file, Files ).

pl_sqlite3_file( File ) :-
	os_ext( pl, Stem, File ),
	debug( sql3, 'Stem: ~p', Stem ),
	read_terms( File, Terms ),
	debug_call( sql3, length, read/Terms ),
	os_ext( csv, Stem, CsvF ),
	atomic_list_concat( [Stem,info], '_', Sifno ),
	pl_sqlite3_split_terms( Terms, Stem, Sifno, Data, Info ),
	debug_call( sql3, length, data/Data ),

	debug( sql3, 'Csv: ~p', CsvF ),
	% mtx( CsvF, Data ),
	csv_dump( CsvF, Data ),
	debug( sql3, 'done Csv: ~p', CsvF ),
	Terms = [Ferm|_],
	pl_sqlite3_name_types( Stem, Ferm, Nypes, Cnm1 ),
	atomic_list_concat( Nypes, ', ', Aypes ),
	sqlite3_quote( Stem, StemQ ),
	atomic_list_concat( ['create table ',StemQ,'(',Aypes,');'], Create ),
	os_ext( com, Stem, ComF ),
	open( ComF, write, ComO ),
	write( ComO, Create ), nl( ComO ),
	write( ComO, '.separator ,' ), nl( ComO ),
	atomic_list_concat( ['.import ',CsvF,' ',StemQ], Import ),
	write( ComO, Import ), nl( ComO ),

	atomic_list_concat( ['Create index idx on ',StemQ,'(',Cnm1,');'], Index ),
	write( ComO, Index ), nl( ComO ),

	pl_sqlite3_file_info( Info, Sifno, ComO ),

	close( ComO ),
	os_ext( sqlite, Stem, SqlF ),
	os_remove( SqlF, [exists(false),debug(true)] ),
	atomic_list_concat( [sqlite3,SqlF,'<',ComF], ' ', Sqlite3 ),
	debug( sql3, 'shell: ~w', Sqlite3 ),
	shell( Sqlite3 ).

pl_sqlite3_file_info( [], _Sifno, _ComO ) :- !.
pl_sqlite3_file_info( Infos, Sifno, ComO ) :-
	sqlite3_quote( Sifno, SifnoQ ),
	atomic_list_concat( ['create table ',SifnoQ,'(key text, val text);'], '', Crinfo ),
	maplist( atomed_args, Infos, Anfos ),
	os_ext( csv, Sifno, SifnoF ),
	mtx( SifnoF, Anfos ),
	atomic_list_concat( ['.import ',SifnoF,' ',SifnoQ], Import ),
	write( ComO, Crinfo ), nl( ComO ),
	write( ComO, Import ), nl( ComO ).

atomed_args( Tin, Tout ) :-
	Tin =.. [Tname|Targs],
	maplist( term_to_atom, Targs, Aargs ),
	Tout =.. [Tname|Aargs].

pl_sqlite3_split_terms( [], _Stem, _Sifno, [], [] ).
pl_sqlite3_split_terms( [T|Ts], Stem, Sifno, Data, Info ) :-
	functor( T, Tname, _ ),
	Tname == Sifno,
	!,
	Data = Tata,
	Info = [T|Tinfo],
	pl_sqlite3_split_terms( Ts, Stem, Sifno, Tata, Tinfo ).
pl_sqlite3_split_terms( [T|Ts], Stem, Sifno, Data, Info ) :-
	functor( T, Tname, Tarity ),
	Tname == Stem,
	!,
	pl_sqlite3_data_thread( Tarity, T, Data, Tdata ),
	% Data = [T|Ts],
	pl_sqlite3_split_terms( Ts, Stem, Sifno, Tdata, Info ).
pl_sqlite3_split_terms( [T|_Ts], _Stem, _Sifno, _Data, _Info ) :-
	throw( gazoomped(T) ).

% fixme ensure that we are talking about edge.../3 terms.
pl_sqlite3_data_thread( 2, T, [T|Tdata], Tdata ).
pl_sqlite3_data_thread( 3, T, [T,R|Tdata], Tdata ) :-
	T =.. [Tnm,X,Y,W],
	R =.. [Tnm,Y,X,W].

	% Data = [T|Ts],

pl_sqlite3_name_types( Stem, Ferm, Nypes, Cnm1 ) :-
	atomic_list_concat( [DbType, DbSrc|Rest], '_', Stem ),
	pl_sqlite3_db_type_name_types( DbType, DbSrc, Ferm, Rest, Nypes, Cnm1 ).

pl_sqlite3_db_type_name_types( map, _DbSrc, Ferm, [A,B], [Aype,Bype], Aq ) :-
	pl_sqlite_map_name_types( Ferm, At, Bt ),
	!,
	maplist( sqlite3_quote, [A,B], [Aq,Bq] ),
	atomic_list_concat( [Aq,At], ' ', Aype ),
	atomic_list_concat( [Bq,Bt], ' ', Bype ).
pl_sqlite3_db_type_name_types( map, _DbSrc, _Ferm, [A,B], [Aype,Bype], Aq ) :-
	maplist( pl_sqlite3_map_name_type, [A,B], [At,Bt] ),
	maplist( sqlite3_quote, [A,B], [Aq,Bq] ),
	atomic_list_concat( [Aq,At], ' ', Aype ),
	atomic_list_concat( [Bq,Bt], ' ', Bype ).
% fixme: below should be obsolete ? we renaming OO inter-term relations to maps
pl_sqlite3_db_type_name_types( edge, gont, _Ferm, _, [Aype,Bype], arg1 ) :- !,
	A = arg1, B = arg2,
	!,
	At = text, Bt = text,
	atomic_list_concat( [A,At], ' ', Aype ),
	atomic_list_concat( [B,Bt], ' ', Bype ).
pl_sqlite3_db_type_name_types( edge, string, _Ferm, _, [Aype,Bype,Cype], node1 ) :- !,
	A = node1, B = node2, C = weight,
	At = text, Bt = text, Ct = integer,
	atomic_list_concat( [A,At], ' ', Aype ),
	atomic_list_concat( [B,Bt], ' ', Bype ),
	atomic_list_concat( [C,Ct], ' ', Cype ).

pl_sqlite3_map_name_type( Name, Type ) :-
	pl_sqlite3_map_name_type_known( Name, Type ),
	!.
pl_sqlite3_map_name_type( Name, _Type ) :-
	throw( unknwown_map_name(Name) ).

csv_dump( CsvF, Data ) :-
	open( CsvF, write, Out ),
	csv_dump_stream( Data, Out ),
	close( Out ).

csv_dump_stream( [], _Out ).
csv_dump_stream( [H|T], Out ) :-
	H =.. [_|Args],
	csv_dump_args( Args, Out ),
	csv_dump_stream( T, Out ).

csv_dump_args( [A], Out ) :- !,
	write( Out, '"' ), 
	write( Out, A ),
	write( Out, '"' ), 
	nl( Out ).
csv_dump_args( [A|As], Out ) :-
	write( Out, '"' ), 
	write( Out, A ), 
	write( Out, '"' ), 
	write( Out, ',' ),
	csv_dump_args( As, Out ).

	

pl_sqlite3_map_name_type_known( gont, text ).
pl_sqlite3_map_name_type_known( gonm, text ).
pl_sqlite3_map_name_type_known( symb, text ).
pl_sqlite3_map_name_type_known( hgnc, integer ).
pl_sqlite3_map_name_type_known( name, text ).
pl_sqlite3_map_name_type_known( prev, text ).
pl_sqlite3_map_name_type_known( syno, text ).
pl_sqlite3_map_name_type_known( entz, integer ).
pl_sqlite3_map_name_type_known( ensg, text ).
pl_sqlite3_map_name_type_known( ensp, text ).
pl_sqlite3_map_name_type_known( rnuc, text ).
pl_sqlite3_map_name_type_known( unig, text ).
pl_sqlite3_map_name_type_known( unip, text ).
pl_sqlite3_map_name_type_known( trem, text ).
pl_sqlite3_map_name_type_known( nucs, text ).
pl_sqlite3_map_name_type_known( sprt, text ).
pl_sqlite3_map_name_type_known( seqn, text ).
pl_sqlite3_map_name_type_known( 'entz-appv', integer ).
pl_sqlite3_map_name_type_known( 'entz-ncbi', integer ).

pl_sqlite_map_name_types( map_gont_consists_of, text, text ).

% sqlite3_quote( Stem, StemQ ),
sqlite3_quote( Stem, StemQ ) :-
	atom_codes( Stem, Codes ),
	replace_hyphen( Codes, Hodes ),
	atom_codes( StemQ, Hodes ).

replace_hyphen( [], [] ).
replace_hyphen( [H|T], [N|R] ) :-
	( H =:= 0'- ->
		N = 0'_
		;
		N = H
	),
	replace_hyphen( T, R ).
	
