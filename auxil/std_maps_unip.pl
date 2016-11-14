
:- ensure_loaded( 'lib/requires_minimal' ).

% :- use_module( library(requires) ).
% :- lib(bio_db).
:- lib(os). 

:- ensure_loaded( bio_db_add_infos ).
:- ensure_loaded( lib/bio_db_dnt_times ).

:- requires( csv_ids_map/6 ).

:- multifile( user:library_directory/1 ).
:- dynamic( user:library_directory/1 ).

:- prolog_load_context( directory, Dir ),
   directory_file_path( Dir, lib, Lib ),
   assert( library_directory(Lib) ).

:- set_prolog_stack(global, limit(8*10**11)).

:- ensure_loaded( map_uniprot                  ).  % /4.
:- requires(      link_to_bio_sub/3            ). % link_to_map_sub/2 
:- requires(      url_file_local_date_mirror/3 ).

:- ensure_loaded( bio_db_build_aliases         ).
   

:- debug( uniprot ).
:- debug( link_to_map_sub ).

unip_hs( 'ftp://ftp.ebi.ac.uk/pub/databases/uniprot/current_release/knowledgebase/idmapping/by_organism/HUMAN_9606_idmapping.dat.gz' ).
trem_hs( 'ftp://ftp.ebi.ac.uk/pub/databases/uniprot/current_release/knowledgebase/idmapping/by_organism/HUMAN_9606_idmapping_selected.tab.gz' ).
% use this if from outside europe:
% unip_hs( 'ftp://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/idmapping/by_organism/HUMAN_9606_idmapping.dat.gz' ).
%trem_hs( 'ftp://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/idmapping/by_organism/HUMAN_9606_idmapping_selected.tab.gz' ).

unip_dnload( Loc ) :-
	absolute_file_name( bio_db_build_downloads(unip), Loc ),
	write( loc(Loc) ), nl,
	os_make_path( Loc, debug(true) ).

%% maps_std_uniprot.
%
% Create some uniprot maps.
%
%==
% ?- maps_std_uniprot.
% ?- shell( 'wc -l uniprot_*' ).
%==
%
% @author nicos angelopoulos
% @version  0.2 2015/4/27
% @tbd use hgnc as template to download from 
% @tbd ftp://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/idmapping/by_organism/
%
std_maps_unip :-
	bio_db_build_aliases( true ),

	unip_dnload( DnDir ),  %
	/* double check unip part works with the nucl part // 15.05.15 */
	working_directory( Old, DnDir ),
	unip_hs( Url ),
	% url_file( Url, 
	UrlOpts = [debug(url_local),interface(wget),file(File)],
	url_file_local_date_mirror( Url, DnDir, UrlOpts ),
	% cd( bio_dn_root(uniprot) ),
	os_rm_rf( maps ),
	os_make_path( maps, debug(true) ),

	debug( uniprot, 'Dir location: ~p', DnDir ),
	Rev = [interface(prolog),reverse(true)],
	map_uniprot( 'Ensembl_PRO', Csv, [EnspF], Rev ),

	Fgnc = [interface(prolog),f_call(de_semi('HGNC'))],
	map_uniprot( 'HGNC', Csv, [FromHgncF], Fgnc ),

	Sem = [interface(prolog),f_call(de_semi('HGNC')),reverse(true)],
	map_uniprot( 'HGNC', Csv, [HgncF], Sem ),

	Ifc = interface(prolog),
	map_uniprot( 'GeneID', Csv, [EtzF], Ifc ),
	map_uniprot( 'UniGene', Csv, [UniGF], Ifc ),
	Files = [HgncF,FromHgncF,EtzF,UniGF,EnspF],
	% working_directory( _, maps ),
 	maplist( link_to_map_sub(unip), Files ),

	bio_db_dnt_times( File, SwDnDt, _SwDnEn ),
	SwOpts = [source(Url),datetime(SwDnDt)],
	bio_db_add_infos_to( [header(row('Ensembl Protein','Uni Protein'))|SwOpts], 'maps/map_unip_ensp_unip.pl' ),
	bio_db_add_infos_to( [header(row('Uni Protein','Entrez ID'))|SwOpts], 'maps/map_unip_unip_entz.pl' ),
	bio_db_add_infos_to( [header(row('Uni Protein','HGNC ID'))|SwOpts], 'maps/map_unip_unip_hgnc.pl' ),
	bio_db_add_infos_to( [header(row('Uni Protein','Uni Gene'))|SwOpts], 'maps/map_unip_unip_unig.pl' ),

	working_directory( _, DnDir ),


	trem_hs( TremUrl ),
	% 15.05.14 adding support for treMBL, at least that 's what i think the selected file is all about
	TrUrlOpts = [debug(url_local),interface(wget),file(TrFile)],
	url_file_local_date_mirror( TremUrl, DnDir, TrUrlOpts ),
	bio_db_dnt_times( TrFile, TrDnDt, _TrDnEn ),

	os_make_path( maps, afresh(false) ),
	os_make_path( trembl, afresh(true) ),
	directory_file_path( _, TremFile, TremUrl ),
	pwd,
	directory_file_path( trembl, TremFile, TremTrg ),
	copy_file( TremFile, TremTrg ),
	working_directory( _, trembl ), 
	file_name_extension( TremDatF, gz, TremFile ),
	atom_concat( 'gunzip ', TremFile, Gunzip ),
	debug( _, 'Gunzipping: ~p', TremFile ),
	shell( Gunzip ),
	csv_read_file( TremDatF, TremRows, [separator(0'\t)] ),
	length( TremRows, TremLen ), 
	write( trem_length(TremLen) ), nl,
	% 17/22
	findall( map_unip_trem_nucs(TremId,Nucs), (
	                  member(TremRow,TremRows), arg(1,TremRow,TremId), \+ empty(TremId), 
	                  arg(17,TremRow,NucsConcat), \+ empty(NucsConcat), 
				   atomic_list_concat(NucsList,'; ',NucsConcat),
				   member(Nucs,NucsList)
				            ), 
						        TNRows ),
	length(TNRows, TNLen), 
	write( tn_len(TNLen) ), nl,
	sort( TNRows, TNOrdRows ),
	open( '../maps/map_unip_trem_nucs.pl', write, TNOut ),
	maplist( portray_clause(TNOut), TNOrdRows ),
	close( TNOut ),
	working_directory( _, '../maps' ),
 	link_to_map_sub(unip, 'map_unip_trem_nucs.pl' ),

	TrOpts = [source(TremUrl),datetime(TrDnDt),header(row('treMBLE Protein','Nucleotide Sequence'))],
	bio_db_add_infos_to( TrOpts, map_unip_trem_nucs.pl ),

	% run this manually, it is a biggie: 
	% uniprot_sprot.dat is 2.9 G
	% std_map_usyn_unip,
	%

	working_directory( _, Old ).

empty( '' ).
	
std_map_usyn_unip :-
	open( '/media/nicos/lmtk3/downloads/uniprot_sprot.dat', read, In ),
	read_line_to_codes( In, Line ),
	sprot_synonym_rows( Line, In, SynRs ),
	debug_call( uniprot, length, syn_rows/SynRs ),
	close( In ),
	sort( SynRs, OrdRs ),
	unip_dnload( DnDir ),
	os_path( DnDir, maps, MapsD ),
	Opts = [prefix(unip),dir(MapsD)],
	csv_ids_map( _, usyn, unip, [row(usyn,unip)|OrdRs], MapF, Opts ),
	os_path( MapsD, MapF, AbsMapF ),
 	link_to_map_sub( unip, AbsMapF ).


sprot_synonym_rows( end_of_file, _In, [] ) :- !.
sprot_synonym_rows( Line, In, SynRs ) :-
	atom_codes( Atom, Line ),
	( atom_concat('AC   ',SynsPsf,Atom) ->
		atom_concat(SynsAtom,';',SynsPsf),
		atomic_list_concat(Syns,'; ',SynsAtom),
		Syns = [Cannon|Ryns],
		sport_prot_synonym_rows( Ryns, Cannon, SynRs, TSynRs )
		;
		TSynRs = SynRs
	),
	read_line_to_codes( In, Codes ),
	sprot_synonym_rows( Codes, In, TSynRs ).
	
sport_prot_synonym_rows( [], _Cannon, SynRs, SynRs ).
sport_prot_synonym_rows( [H|T], Cannon, [row(H,Cannon)|TSynRs], SynRs ) :-
	sport_prot_synonym_rows( T, Cannon, TSynRs, SynRs ).

os_rm_rf( Dir ) :-
	exists_directory(Dir) ->
	delete_directory_and_contents(Dir),
	!.
os_rm_rf( _Dir ).

de_semi( Pfx, AccPrv, Acc ) :-
	atomic_list_concat( [Pfx,AccAtm], ':', AccPrv ), 
	atom_number( AccAtm, Acc ),
	!.
de_semi( Pfx, AccPrv, _Acc ) :-
	write( de_semi_disaster(Pfx,AccPrv) ), nl,
	abort.
