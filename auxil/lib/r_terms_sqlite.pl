
:- use_module( library(requires) ).
:- use_module( library(real) ).
:- use_module( library(mtx) ).
:- use_module( library(debug_call) ).

:- r_library( "RSQLite" ).

:- requires( mtx_df/2 ).
:- requires( read_terms/2 ).

:- set_prolog_stack(global, limit(2*10**9)). % for edge_string_hs.pl

:- debug( r_sql ).

/** r_terms_sqlite( +File ).

Load a csv or prolog facts data from a file to the corresponding .sqlite file.

?- cd( '/usr/local/users/nicos/work/bio_db/dnloads/go/maps' ).
?- cd( '/tmp/map' ).

?- r_terms_sqlite( 'map_gont_symb_gont.pl', Load ).

*/
testo :- % THIS IS THE BEST WAY
	cd( '/tmp/map/' ),
	% PlF = 'map_gont_gont_gonm.pl',
	PlF = 'map_gont_gont_symb.pl',
	r_terms_sqlite( PLF ).

r_terms_sqlite( PLF ) :-
	read_terms( PlF, Facts ),
	file_name_extension( Stem, _Ext, PlF ),
	file_name_extension( Stem, csv, CsvF ),
	mtx( CsvF, [row(go_term,go_name)|Facts] ),
	file_name_extension( Stem, sqlite, SqliteF ),
	r_sql <- read.csv( +CsvF ),
	rs_con <- dbConnect('SQLite()', dbname = +SqliteF ),
	Facts = [Fact|_],
	functor( Fact, Pname, _Arity ),
	r_sql <- as.data.frame(r_sql),
	<- dbWriteTable(rs_con, +Pname, r_sql),
	<- dbDisconnect(rs_con),
	delete_file( CsvF ).
	
/*
	
testo1 :-
	% cd( '/usr/local/users/nicos/work/bio_db/dnloads/go/maps' ),
	cd( '/tmp/map/' ),
	% r_terms_sqlite( 'map_gont_symb_gont.pl' ).
	% r_terms_sqlite( 'edge_string_hs_symb.pl' ).
	% r_terms_sqlite( 'edge_string_hs.pl' ).
	r_terms_sqlite( 'map_gont_gont_gonm.pl' ).
	% r_sqlite_load( 'map_gont_gont_symb.pl' ).
	
r_terms_sqlite( File ) :-
	file_name_extension( Stem, Ext, File ),
	file_name_extension( Stem, sqlite, SqliteF ),
	r_ext_sqlite_load( Ext, File, SqliteF ).

r_ext_sqlite_load( csv, CsvF, SqliteF ) :-
	fixme( CsvF, SqliteF ).
r_ext_sqlite_load( pl, PlF, SqliteF ) :-
	% mtx( MtxF, Rows ),
	read_terms( PlF, Facts ),
	% mtx_df( Facts, r_sql ),
	% length( Facts, Len ),
	mtx_df_vectors( Facts, r_sql ),

	% <- r_sql,

	debug_call( r_sql, length, rows/Facts ),
	debug_call( r_sql, wrote, SqliteF ),

	Facts = [First|_], 
	functor( First, Pname, _ ),
	rs_con <- dbConnect('SQLite()', dbname=+SqliteF),
	<- dbWriteTable(rs_con, +Pname, r_sql),
	<- dbDisconnect(rs_con),
	debug( r_sql, 'done', false ),
	true.

mtx_df_vectors( Facts, Df ) :-
	Facts=[First|_],
	functor( First, _, Arity ),
	mtx_df_args( Arity, Facts, Vects ),
	DfCall =.. ['data.frame'|Vects],
	Df <- DfCall.

mtx_df_args( 0, _Facts, Pairs ) :-
	!,
	Pairs = [].
mtx_df_args( I, Facts, [Arg|Tairs] ) :-
	atom_concat( arg, I, Arg ),
	maplist( arg(I), Facts, Iths ),
	Arg <- Iths,
	H is I - 1,
	mtx_df_args( H, Facts, Tairs ).

*/
