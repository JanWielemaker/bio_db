

:- ensure_loaded( 'lib/requires_minimal' ).

:- lib(os).
:- lib(bio_db).
:- lib(mtx).
:- lib(by_unix).
:- ensure_loaded( bio_db_build_aliases ).    % sets up user's build directory path
:- ensure_loaded( 'lib/bio_db_dnt_times' ).  % /3.
:- ensure_loaded( bio_db_add_infos ).        % bio_db_add_infos_to/2

:- requires( map_succ_list/3 ).
:- requires( link_to_bio_sub/3 ). % link_to_map_sub/2.
:- requires( url_file_local_date_mirror/3 ).
% :- requires( url_file/2 ).

:- debug( std_maps_go ).
:- set_prolog_stack(global, limit(8*10**11)).

gont_goa_hs_url( Url ) :-
	Url = 'http://geneontology.org/gene-associations/gene_association.goa_human.gz'.

gont_term_db_url( Url ) :-
	Url = 'http://archive.geneontology.org/latest-termdb/go_daily-termdb-tables.tar.gz'.

% std_maps_go.
% 
% Set up some standard maps for gene ontology (GO, gont) data.
%
% http://geneontology.org/gene-associations/gene_association.goa_human.gz
% Currrently this sets ups 
%  * map_gont_gont_symb( GOTERM, Symb ).
%  * map_gont_symb_gont( Symb, GOTERM ).
%
%	TermGz = 'go_daily-termdb-tables.tar.gz',
%  * map_gont_gont_gonm( GOTERM, GONM ).
%
% @author  nicos angelopoulos
% @version 0.1 2015/3/26
% @tbd     is this the best dataset for these maps ?
% @tbd     add dates to the file
%
std_maps_gont :-
	% DnDir = '/usr/local/users/nicos/work/db/data/go',
	absolute_file_name( bio_db_build_downloads(gont), DnDir ),
	os_make_path( DnDir, debug(true) ),
	working_directory( Here, DnDir ),
	gont_goa_hs_url( Url ),
	% fixme: used dated mirror:
	% url_file( Url, GoaHsGz ),
	url_file_local_date_mirror( Url, DnDir, true ),

	GoaHsGz = 'gene_association.goa_human.gz',
	% @ gunzip( -k, GoaHsGz ),
	@ gunzip( --force, -k, GoaHsGz ),
	file_name_extension( GoaHs, gz, GoaHsGz ),
	% mtx( 'gene_association.goa_human.tsv', Mtx ),
	% mtx( GoaHs, Mtx, csv_read(sep=0'\t) ),
	csv_read_file( GoaHs, MtxPrv, [separator(0'\t),match_arity(false)] ),
	clense_goa_hs( MtxPrv, Mtx ),
	debug( std_maps_go, 'loaded data...', [] ),
	make_directory_path( maps ),
	findall( row(GoTerm,Symb), ( member(Row,Mtx), 
	                             arg(5,Row,GoTerm),
				              arg(11,Row,Bared),
						    go_bared_symbol(Bared,Symb)
	                ),
				 NewRows ),
	% fixme: parse Go through grammar
	sort( NewRows, OrdRows ),
	mtx( 'maps/map_gont_gont_symb.csv', OrdRows ),
	GSopts = [predicate_name(map_gont_gont_symb)],
	mtx_prolog( OrdRows, 'maps/map_gont_gont_symb.pl', GSopts ),
	working_directory( AtHere, AtHere ),
	debug( std_maps_go, 'Currently at: ~p', AtHere ),   % add it to debug_call/3.
	bio_db_dnt_times( 'gene_association.goa_human.gz', UrlDntSt, _DntEnd ),
	AddOpts = [source(Url),datetime(UrlDntSt)],
	bio_db_add_infos_to( [header(row('GO Term','HGNC Symbol'))|AddOpts], 'maps/map_gont_gont_symb.pl' ),
	
	findall( row(Symb,Gont), member(row(Gont,Symb),OrdRows), SGRows ),
	sort( SGRows, OrdSGRows ),
	SGopts = [predicate_name(map_gont_symb_gont)],
	mtx_prolog( OrdSGRows, 'maps/map_gont_symb_gont.pl', SGopts ),
	bio_db_add_infos_to( [header(row('HGNC Symbol','GO Term'))|AddOpts], 'maps/map_gont_symb_gont.pl' ),
	
	debug( std_maps_go, 'Building term to name map', true ),
	gont_term_db_url( TermUrl ),
	% url_file( TermUrl, TermGz ),
	url_file_local_date_mirror( TermUrl, DnDir, true ),

	TermGz = 'go_daily-termdb-tables.tar.gz',
	@ gunzip( --force, -k, TermGz ),
	@ rm( -rf, 'go_daily-termdb-tables' ),
	file_name_extension( TermTar, gz, TermGz ),
	@ tar( xf, TermTar ),
	csv_read_file( 'go_daily-termdb-tables/term.txt', TermRows, [separator(0'\t)] ),
	% consult( go_assoc_db_term:'go_assoc_db_term' ), 
	% findall( row(GoT,GoN), go_assoc_db_term:term(_,GoN,_,GoT,_,_,_), GTNRows ),
	findall( row(GoT,GoN), member(row(_,GoN,_,GoT,_,_,_),TermRows), GTNRows ),
	GTopts = predicate_name(map_gont_gont_gonm),
	sort( GTNRows, OrdGTNRows ),
	mtx_prolog( OrdGTNRows, 'maps/map_gont_gont_gonm.pl', GTopts ),
	debug( std_maps_go, 'Fixme: add GOterm -> go Section', true ),
	delete_file( 'maps/map_gont_gont_symb.csv' ),
	% here
	bio_db_dnt_times( 'go_daily-termdb-tables.tar.gz', TermUrlDntSt, _TermDntEnd ),
	TermOpts = [header('GO Term','GO Name'),source(TermUrl),datetime(TermUrlDntSt)],
	bio_db_add_infos_to( TermOpts, 'maps/map_gont_gont_gonm.pl' ),

	working_directory( First, maps ),
	OutFs = ['map_gont_gont_symb.pl','map_gont_symb_gont.pl','map_gont_gont_gonm.pl'],
	maplist( link_to_map_sub(gont), OutFs ),

	working_directory( _, First ),
	delete_file( GoaHs ),
	delete_file( TermTar ),
	working_directory( _, Here ).

go_bared_symbol( Bared, Symb ) :-
	atomic_list_concat( Parts, '|', Bared ),
	map_succ_list( hgnc_symb, Parts, NestSymbs ),
	flatten( NestSymbs, Symbs ),
	go_bared_symbol_single( Symbs, Bared, Symb ).

hgnc_symb( Symb, Symb ) :-
	map_hgnc_symb_hgnc( Symb, _ ),
	!.
hgnc_symb( Part, Symbs ) :-
	atomic_list_concat( Subs, ':', Part ),
	Subs \= [Part],
	map_succ_list( hgnc_symb, Subs, Symbs ).

% This is a generic pattern...
go_bared_symbol_single( [], Bared, _Symb ) :-
	debug( _, 'GO bared:~w did not lead to a symbol...', Bared ),
	fail.
go_bared_symbol_single( [Symb], _Bared, Symb ).
go_bared_symbol_single( [S1,S2|Sail], Bared, Symb ) :-
	debug( _, 'GO bared:~w did led to multiple symbols...,~w', [Bared,[S1,S2|Sail]] ),
	member( Symb, [S1,S2|Sail] ).

clense_goa_hs( [H|T], Mtx ) :-
	functor( H, _, 1 ),
	!,
	clense_goa_hs( T, Mtx ).
clense_goa_hs( Mtx, Mtx ).
