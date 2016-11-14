
:- lib( prosqlite ).
:- lib( db_facts ).
:- lib( options ).  % en_list/2.

% :- debug( sqlite ).

/*
bio_db_pl_sqlite :-
	Pname = map_hgnc_hgnc_symb,
	bio_db_pl_sqlite( Pname ).
	*/

/** pl_prosqlite( FileS ). 

Convert a number of files, or a single one, from a Prolog fact base to an SWLite database file.

@version 0.2  2016/9/5  it nwo uses blaocked db_asserts (size 500) and it is much faster.

*/

pl_prosqlite( FileS ) :-
	en_list( FileS, Files ),
	maplist( bio_db_pl_sqlite, Files ).

bio_db_pl_sqlite( PnameF ) :-
	( file_name_extension(Stem,pl,PnameF) ->
		file_base_name( Stem, Pname ),
		PlF = PnameF
		;
		PnameF = Pname,
		file_name_extension( Pname, pl, PlF )
	),
	ConOpts = exists(false),
	sqlite_connect( Stem , Conn, ConOpts ),

	open( PlF, read, Input ),
	read_term( Input, Read, [] ),

	atomic_list_concat( [Pname,info], '_', Pinfo ),
	InfoCreate =.. [Pinfo,key+text,val-text],
	db_create( Conn, InfoCreate ),
	bio_db_info_top( Read, Pname, First, Input, Conn ),

	bio_db_pl_term_sqlite_create( First, Conn ),
	bio_db_pl_sqlite_stream( First, Pname, 1, [], Input, Conn ),
	close( Input ),
	db_disconnect( Conn ).

% fixme create info table
bio_db_info_top( Term, Pname, First, _Input, _Conn ) :-
	functor( Term, Pname, _ ),
	!,
	First = Term.
bio_db_info_top( Term, Pname, First, Input, Conn ) :-
	functor( Term, Tname, 2 ),
	atomic_list_concat( [Pname,info], '_', Tname ),
	!,
	arg( 1, Term, Fst ),
	arg( 2, Term, Sec ),
	term_to_atom( Fst, FstA ),
	term_to_atom( Sec, SecA ),
	Fact =.. [Tname,FstA,SecA],
	db_assert( Conn, Fact, _Aff ),
	read_term( Input, Read, [] ),
	bio_db_info_top( Read, Pname, First, Input, Conn ).
bio_db_info_top( Term, _Pname, _First, _Input, _Conn ) :-
	throw( bogus_read_term_in_pl_sqlite_call(Term) ).

bio_db_pl_sqlite_stream( end_of_file, _Pname, _N, Acc, _Input, _Conn ) :- !,
	( Acc == [] -> true; db_assert( Acc ) ).
bio_db_pl_sqlite_stream( Term, Pname, M, Acc, Input, Conn ) :-
	functor( Term, Pname, _ ),
	% Term =.. [Pname|Args],
	!,
	% db_assert_dube( M, Term, Acc, N, Next ),
	db_acc_assert( M, Term, Acc, N, Next ),
	% db_assert( Term ),
	/*
	maplist( quote_sql_atom, Args, Qrgs ),
	atomic_list_concat( Qrgs, ', ', ArgsAtom ),
	atomic_list_concat( ['INSERT INTO ',Pname,' VALUES ( ',ArgsAtom,' );'], '', Query ),
	write( query(Query) ), nl,
	sqlite_query( Conn, Query ),
	*/
	read_term( Input, Read, [] ),
	bio_db_pl_sqlite_stream( Read, Pname, N, Next, Input, Conn ).
bio_db_pl_sqlite_stream( _Term, Pname, M, Acc, Input, Conn ) :-
	read_term( Input, Read, [] ),
	bio_db_pl_sqlite_stream( Read, Pname, M, Acc, Input, Conn ).

db_assert_dube( _M, Term, _, 1, [] ) :-
	db_assert( Term ).

% tested with M > 500, but it seems to flatten out after 500
% 
db_acc_assert( 500, Term, Acc, N, Next ) :- !,
	db_assert( [Term|Acc] ),
	Next = [],
	N is 1.
db_acc_assert( M, Term, Acc, N, Next ) :-
	M < 500,
	Next = [Term|Acc],
	N is M + 1.

bio_db_pl_term_sqlite_create( First, Conn ) :-
	bio_db_pl_sqlite_create_term( First, Create ),
	db_create( Conn, Create ).

bio_db_pl_sqlite_create_term( First, Create ) :-
	First =.. [Pname|PlArgs],
	maplist( bio_pl_term_sqlite_type, PlArgs, Types ),
	atomic_list_concat( [DbType,_Db|FieldsPrv], '_', Pname ),
	bio_db_pl_sqlite_db_type_fields( DbType, FieldsPrv, Fields ),
	findall( Field+Type, (  nth1(N,Fields,Field),
	                   nth1(N,Types,Type)
				 ), CTerms ),
	% Create =.. [Pname,hgnc+int,symb-text],
	Create =.. [Pname|CTerms].

bio_db_pl_sqlite_db_type_fields( map, Fields, Fields ).
bio_db_pl_sqlite_db_type_fields( edge, _Garbage, Fields ) :-
	Fields = [prod1,prod2,weight].

bio_pl_term_sqlite_type( Pl, Type ) :-
	number( Pl ),
	!,
	Type = int.
bio_pl_term_sqlite_type( _Pl, text ).

quote_sql_atom( Atom, Quoted ) :-
	atomic_list_concat( ['\'',Atom,'\''], '', Quoted ).
