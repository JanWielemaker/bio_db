
% :- use_module( library(requires) ).
:- ensure_loaded( 'lib/requires_minimal' ).

:- use_module( library(mtx) ).
:- use_module( library(os) ).
:- use_module( library(by_unix) ).

:- ensure_loaded( bio_db_add_infos ).
:- ensure_loaded( lib/bio_db_dnt_times ).

/*
loc_lib( Loc/_ ) :-
	ensure_loaded( lib/Loc ).
	*/

:- multifile( user:library_directory/1 ).
:- dynamic( user:library_directory/1 ).

:- prolog_load_context( directory, Dir ),
   directory_file_path( Dir, lib, Lib ),
   assert( library_directory(Lib) ).

:- requires( message_report/3 ).
:- requires( url_file/2 ).
:- requires( link_to_bio_sub/3 ).
:- requires( portray_clauses/2 ).

:- set_prolog_stack( global, limit(2*10**9) ).
:- debug( std_graphs_string ).
:- debug( by_unix ).

:- use_module( library(bio_db) ).

/** std_graphs_string.

String does not provide an easy way to get the current version. 
You need to insert the version manually by visiting

  * http://string-db.org',

and then calling something similar to the following depending
on the latest version

==
  ?- std_graphs_string( '9.1' ).
==

@tbd automate the extraction of current version from string-db.org
@tbd add information file or info terms/comments in edges file

*/
std_graphs_string :-
	Mess1 = 'Please check number of latest version at http://string-db.org',
	Mess2 = '... and run, for example, std_graphs_string(\'9.1\').',
	message_report( Mess1, [], information ),
	message_report( Mess2, [], information ).

% last good one: std_graphs_string( '10' ).  2016/09/08
std_graphs_string( Version ) :-
	ensure_loaded( bio_db_build_aliases ),
	debug( std_graphs_string, 'Version: ~w', Version ),
	std_graphs_string_version_base_name( Version, Bname, From ),
	Self = std_graphs_string,
	debug( Self, 'Base name: ~w', Bname ),
	absolute_file_name( bio_db_build_downloads(string), Parent ),
	% absolute_file_name( baio_db_downloads(string/Bname), LocalFile ),
	% directory_file_path( Parent, _BnameAgain, LocalFile ),
	directory_file_path( Parent, Bname, LocalFile ),
	os_make_path( Parent, debug(true) ),
	std_graph_string_download_string( LocalFile, From, Self ),
	working_directory( Here, Parent ),
	@ gunzip( -k, Bname ),  % keeps .gz file
	% @ gunzip( '9606.protein.links.v10.txt.gz' ),
	Edge = edge_string_hs,
	Opt = [ csv_read(separator(0' )),predicate_name(Edge),
	        rows_transform(maplist(user:de_hs)),header_remove(true)
		 ],
	file_name_extension( TxtF, gz, Bname ),
	Mess1 = 'Converting string file: ~p, to Prolog',
	% file_name_extension( Stem, txt, TxtF ),
	% file_name_extension( Stem, pl, PlF ),
	debug( Self, 'Directory: ~p', [Parent] ),
	debug( Self, Mess1, [TxtF] ),
	mtx_prolog( TxtF, File, Opt ),
	debug( _, 'Edges output: ~w', File ),
	delete_file( TxtF ),
	@ rm( -rf, graphs ),
	os_make_path( graphs, debug(true) ),
	Trg = 'graphs/edge_string_hs.pl',
	@ rm( -f, Trg ),
	@ mv( File, Trg ),

	consult( edge_string_hs:Trg ),
	debug( _, 'consulted hs: ~w', [edge_string_hs:Trg] ),

	findall( edge_string_hs_symb(SymbA,SymbB,W),
	                     ( edge_string_hs:edge_string_hs(EnsP1,EnsP2,W),
					   ensp_symb(EnsP1,Symb1),
					   ensp_symb(EnsP2,Symb2),
					   sort(Symb1,Symb2,SymbA,SymbB)
					 ),
		  	UnoSymbEdges
		  ),
	sort( UnoSymbEdges, SymbEdges ),
	EdgeSymbsF = 'graphs/edge_string_hs_symb.pl',
	portray_clauses( SymbEdges, file(EdgeSymbsF) ),

	bio_db_dnt_times( Bname, DnDt, _EndDt ),
	HsOpts = [source(From),datetime(DnDt),header(row('Ensembl Protein','Ensembl Protein',weight))],
	bio_db_add_infos_to( HsOpts, Trg ),

	SymbOpts = [source(From),datetime(DnDt),header(row('HGNC Symbol','HGNC Symbol',weight))],
	bio_db_add_infos_to( SymbOpts, EdgeSymbsF ),

	link_to_bio_sub( string, graphs, Trg ),
	link_to_bio_sub( string, graphs, EdgeSymbsF ),
	working_directory( _, Here ).

ensp_symb( EnsP, Symb ) :-
	map_ncbi_ensp_entz( EnsP, Entz ),
	map_hgnc_entz_symb( Entz, Symb ),
	!.

sort( X, Y, A, B ) :-
	Y @< X,
	!,
	A = Y, B = X.
sort( A, B, A, B ).

std_graph_string_download_string( LocalFile, _From, Self ) :-
	exists_file( LocalFile ),
	debug( Self, 'Using existing local string file: ~p', LocalFile ),
	!.
std_graph_string_download_string( Local, Remote, Self ) :-
	debug( Self, 'Downloading from: ~p', Remote ),
	url_file( Remote, Local ),
	debug( Self, '... to local file: ~p', Local ).

std_graphs_string_version_base_name( VersionPrv, Bname, Remote ) :-
	( atom_concat(v,Version,VersionPrv)->true;Version=VersionPrv ),
	atom_concat( v, Version, Vied ),
	Pfx = 'http://string-db.org/newstring_download/protein.links.v',
	atom_concat( Pfx, Version, RemoteDir ),
	atomic_list_concat( [9606,protein,links,Vied,txt,gz], '.', Bname ),
	directory_file_path( RemoteDir, Bname, Remote ).
	% 10/9606.protein.links.v10.txt.gz

bio_db_std_string :-
	Opt = [ csv_read(separator(0' )), predicate_name(hs_string_edge),
	        rows_transform(maplist(user:de_hs))
		 ],
	mtx_prolog( bio_dn(string/'protein.links.hs.txt'), File, Opt ),
	debug( _, 'Edges output: ~w', File ),
	bio_db_std_string_link( File ).

de_hs( row(HsEnsP1,HsEnsP2,W), row(EnsP1,EnsP2,W) ) :-
	atom_concat( '9606.', EnsP1, HsEnsP1 ),
	atom_concat( '9606.', EnsP2, HsEnsP2 ),
	!.
de_hs( Row, _ ) :-
	debug( _, 'Failed to translate row: ~w', Row ),
	abort.

bio_db_std_string_link( File ) :-
	HsBio = bio_db_build_data( graphs/string/'hs_string_edge.pl' ),
	absolute_file_name( HsBio, HsTarget ),
	bio_db_std_string_link_target( HsTarget, File ).

bio_db_std_string_link_target( HsTarget, File ) :-
	read_link( HsTarget, _OldFile, _What ),
	delete_file( HsTarget ),
	!,
	atomic_list_concat( ['ln -s',File,HsTarget], ' ', Shell ),
	shell( Shell ).
bio_db_std_string_link_target( HsTarget, File ) :-
	atomic_list_concat( ['ln -s',File,HsTarget], ' ', Shell ),
	shell( Shell ).
