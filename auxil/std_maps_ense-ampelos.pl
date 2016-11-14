
:- lib( os ).
:- lib( mtx ).
:- lib( bio_db).
:- lib( by_unix ).
:- lib( debug_call ).
:- ensure_loaded( bio_db_build_aliases ).

:- ensure_loaded( 'lib/url_file_local_date_mirror' ). % /3.
:- ensure_loaded( 'lib/kv_decompose' ). % /3.

:- set_prolog_stack(global, limit(2*10**9)).

:- debug( ense ).

/** std_maps_ense.

Maps based on ensembl .gtf file. Including transcripts to genes, genes
to hgncs and locations of transcipt and genes to chromosomome locations.

Nonmeclature:
    * ense: the database abbv.
    * enst: ensembl transcript
    * ensg: ensembl gene
    * chrl: chromosomal location

Produces:
	map_ense_enst_ensg
	map_ense_ensg_hgnc
	map_ense_ensg_symb
	map_ense_ensg_chrl
	map_ense_enst_chrl

@author  nicos angelopoulos
@version 0.1, 2016/6/17
@see ftp://ftp.ensembl.org/pub/release-84/gtf/homo_sapiens/Homo_sapiens.GRCh38.84.gtf.gz
@see http://ftp.ensembl.org/pub/current_gtf/homo_sapiens/
@tbd automate selection of latest version
*/
std_maps_ense :-
	debug( ense, 'Starting...', true ),
	absolute_file_name( bio_db_build_downloads(ense), DnDir ),
	os_make_path( DnDir ),
	debug( ense, 'Downloads dir for ense: ~p', DnDir ),
	Url = 'ftp://ftp.ensembl.org/pub/release-84/gtf/homo_sapiens/Homo_sapiens.GRCh38.84.gtf.gz',
	url_file_local_date_mirror( Url, DnDir, file(File) ),
	debug( ense, 'Dnload done, file is: ~p', File ),
	working_directory( Old, DnDir ),
	debug( by_unix ),
	@ gunzip( -f, File ),
	os_ext( gz, Stem, File ),
	os_ext( tab, Stem, TabF ),
	Stem = 'Homo_sapiens.GRCh38.84.gtf-16.06.17', TabF = 'Homo_sapiens.GRCh38.84.gtf-16.06.17.tab', 
	atomic_list_concat( [grep,'-v','"^#"',Stem,'>',TabF], ' ', Shell ),
	shell( Shell ),
	% @ grep( -v, '"^#"', Stem, '>', TabF ),
	@ ls(),
	mtx( TabF, Rows, csv_read(separator(0'\t)) ),
	debug_call( ense, length, rows/Rows ),
	ense_transcripts( Rows, EnsTGRows, EnsTLRows ),
	mtx( 'map_ense_enst_ensg.csv', EnsTGRows ),
	mtx( 'map_ense_enst_chrl.csv', EnsTLRows ),

	ense_genes( Rows, EnsGHRows, EnsGSRows, EnsGCRows ),
	mtx( 'map_ense_ensg_hgnc.csv', EnsGHRows ),
	mtx( 'map_ense_ensg_symb.csv', EnsGSRows ),
	mtx( 'map_ense_ensg_chrl.csv', EnsGCRows ),

	/*

	RowG = row(ChrG,_Db,gene,SrtG,EndG,_,DirG,_,
	findall( row(EnsG,ChrG,SrtG,EndG,DirG),  (
										member(RowG,Rows),


	*/
	working_directory( _, Old ),
	debug( ense, '...Done', true ).
	
ense_genes( [], [], [], [] ).
ense_genes( [RowG|Rows], GHRows, GSRows, [EnsGC|GCRows] ) :-
	RowG = row(ChrG,_Db,gene,SrtG,EndG,_,DirG,_,InfoG),
	EnsGC= row(EnsG,ChrG,SrtG,EndG,DirG),
	ense_info( gene_id, InfoG, EnsG ),
	ense_info( gene_name, InfoG, EnsN ),
	ense_gene_hgnc( EnsG, EnsN, GHRows, GSRows, TGHRows, TGSRows ),
	!,
	ense_genes( Rows, TGHRows, TGSRows, GCRows ).
ense_genes( [RowG|Rows], _, _, _ ) :-
	RowG = row(_ChrG,_Db,gene,_SrtG,_EndG,_,_DirG,_,_InfoG),
	!,
	length( Rows, Len ),
	throw( tripped_on_gene_row(RowG,Len) ).
ense_genes( [_RowG|Rows], GHRows, GSRows, GCRows ) :-
	ense_genes( Rows, GHRows, GSRows, GCRows ).

ense_gene_hgnc( EnsG, EnsN, GHRows, GSRows, TGHRows, TGSRows ) :-
	map_hgnc_ensg_hgnc( EnsG, Hgnc ),
	!,
	map_hgnc_hgnc_symb( Hgnc, Symb ),
	ense_gene_hgnc_symbols( EnsN, Symb ),
	GHRows = [row(EnsG,Hgnc)|TGHRows],
	GSRows = [row(EnsG,Symb)|TGSRows].
ense_gene_hgnc( EnsG, EnsN, GHRows, GSRows, TGHRows, TGSRows ) :-
	\+ map_hgnc_ensg_hgnc( EnsG, _ ),
	map_hgnc_symb_hgnc( EnsN, Hgnc ),
	!,  								% fixme: count how many of those we have ...
	GHRows = [row(EnsG,Hgnc)|TGHRows],
	GSRows = [row(EnsG,EnsN)|TGSRows].
ense_gene_hgnc( _EnsG, _EnsN, GHRows, GSRows, GHRows, GSRows ).

ense_gene_hgnc_symbols( Symb, Symb ) :- !.
ense_gene_hgnc_symbols( EnsN, Symb ) :-
	write( ense_gene_symbol_disagreement(EnsN,Symb) ), nl.

ense_transcripts( [], [], [] ).
ense_transcripts( [RowT|Rows], [EnsTG|TGRows], [EnsTL|TLRows] ) :-
	RowT = row(ChrT,_Db,transcript,SrtT,EndT,_,DirT,_,InfoT),
	% findall( row(EnsT,EnsG)-row(EnsT,ChrT,SrtT,EndT,DirT), ( )).
	ense_info( transcript_id, InfoT, EnsT ),
	ense_info( gene_id, InfoT, EnsG ),
	ense_chromosome( ChrT ),
	!,
	EnsTG = row(EnsT,EnsG),
	EnsTL = row(EnsT,ChrT,SrtT,EndT,DirT),
	ense_transcripts( Rows, TGRows, TLRows ).
ense_transcripts( [RowT|Rows], _, _ ) :-
	RowT = row(ChrT,_Db,transcript,_SrtT,_EndT,_,_DirT,_,_InfoT),
	ense_chromosome( ChrT ),
	!,
	length( Rows, Len ),
	throw( tripped_on_transcript_row(RowT,Len) ).
ense_transcripts( [_RowT|Rows], TGRows, TLRows ) :-
	ense_transcripts( Rows, TGRows, TLRows ).

ense_info( Key, Lookup, Value ) :-
	ense_info( Key, Lookup, true, Value ).
	
ense_info( Key, Lookup, _Strict, Value ) :-
	atomic_list_concat( Parts, '"; ', Lookup ),
	atom_concat( Key, ' "', Left ),
	member( Part, Parts ),
	atom_concat( Left, Value, Part ),
	!.
ense_info( Key, Lookup, Strict, false ) :-
	ense_info_failure( Strict, Key, Lookup ).

ense_info_failure( true, Key, Lookup ) :-
	throw( lookup_failure(Key,Lookup) ).
ense_info_failure( false, _Key, _Lookup ).

ense_chromosome( 'X' ) :- !.
ense_chromosome( 'Y' ) :- !.
ense_chromosome( N ) :- integer( N ), !.
