
:- set_prolog_stack(global, limit(2*10**11)).
% :- set_prolog_stack(local, limit(2684354560)).

:- ensure_loaded( 'lib/requires_minimal' ).

% :- lib( bio_db ).
:- lib( os ).
:- lib( mtx ).
:- lib( options ).
:- lib( by_unix ).

:- ensure_loaded( bio_db_add_infos ).
:- ensure_loaded( lib/bio_db_dnt_times ).
:- ensure_loaded( bio_db_build_aliases ).  % /1.

:- requires( io_sections/3 ).
:- requires( io_prefixed_lines/3 ).
:- requires( break_list_on_list/4 ).
:- requires( break_list_on/4 ).
:- requires( kv_decompose_vs/2 ).

:- requires( url_file_local_date_mirror/3 ).
:- requires( csv_ids_map/6 ).
:- requires( link_to_bio_sub/2 ).

:- lib( bio_db ).

:- debug( maps_unip_seqs ).

unip_sprot_hs_seqs_url( 'ftp://ftp.ebi.ac.uk/pub/databases/uniprot/current_release/knowledgebase/taxonomic_divisions/uniprot_sprot_human.dat.gz' ).
unip_trembl_hs_seqs_url( 'ftp://ftp.ebi.ac.uk/pub/databases/uniprot/current_release/knowledgebase/taxonomic_divisions/uniprot_trembl_human.dat.gz' ).

unip_dnload_dir( Old, Loc, Opts ) :-
	bio_db_build_aliases( Opts ),
	absolute_file_name( bio_db_build_downloads(unip), Loc ),
	os_make_path( Loc, Opts ),
	debug( maps_unip_seqs, 'Uniprot build directory: ~p', Loc ),
	working_directory( Old, Loc ).

maps_unip_seqs_defaults( debug(false) ).

/** maps_unip_seqs.

	Starting support for uniprot homo sapiens sequences.

	Step 1, do the currated Swissprot first: sprot

	Derived name map_unip_sprot_seqn

Options
  * debug(Dbg=false)
    produce debugging messages if Dbg==true

@author nicos angelopoulos
@version  0.1 2015/10/05

*/
maps_unip_seqs :-
	maps_unip_seqs( [debug(true),date_stem('16.09.08')] ).
	
maps_unip_seqs( Args ) :-
	Self = maps_unip_seqs,
	% cd( '/usr/local/users/nicos/work/2015/15.10.05-lmtk3_substrates/' ),
	options_append( Self, Args, Opts ),
	unip_dnload_dir( Old, DnDir, Opts ),
	unip_sprot_hs_seqs_url( SprotUrl ),
	unip_hs_seqs_file( SprotUrl, DnDir, row('Swiss Prot ID','Sequence') ),
	unip_trembl_hs_seqs_url( TremblUrl ),
	unip_hs_seqs_file( TremblUrl, DnDir, row('TrEMBL Prot ID','Sequence') ),
	working_directory( _, Old ).

unip_hs_seqs_file( Url, DnDir, Hdr ) :-
	/* website has the human staff already don't download the generic: 
	% UnipF = 'uniprot_sprot.dat',
	% map_unip_human( UnipF ),
	os_postfix( human, UnipF, HumF ),
	debug( maps_unip_seqs, 'Human file: ~w', HumF ),
	*/

	UrlOpts = [debug(url_local),interface(wget),file(SprotF)],
	url_file_local_date_mirror( Url, DnDir, UrlOpts ),
	debug( maps_unip_seqs, 'Uniprot local file: ~p', SprotF ),
	file_base_name( Url, HumGzF ),
	% HumGzF = 'uniprot_sprot_human.dat.gz' ,
	os_ext( gz, HumF, HumGzF ),
	@ gunzip( -k, -f, HumGzF ),
	debug( Self, 'Gunzipped: ~p', HumGzF ),

	IoOpts = [   process(io_prefixed_lines),
			   process_options([])
	],
	io_sections( HumF, Parsed, IoOpts ),
	debug( maps_unip_seqs, 'Human file parsed: ~w', HumF ),
	% Parsed = [Hp|_],
	% debug( Self, 'First parsed: ~w', Hp ),
	maplist( sequence_row, Parsed, KRows ),
	keysort( KRows, SRows ),
	kv_decompose_vs( SRows, Rows ),
	os_postfix( seqs, HumF, SeqFdat ),
	MtxHdr = row('Symbol','Protein','Length','Sequence'),
	os_ext( dat, SeqStem, SeqFdat ),
	os_ext( csv, SeqStem, SeqF ),
	debug( Self, 'Sequences file: ~p', SeqF ),

	mtx( SeqF, [MtxHdr|Rows] ),
	/*
	findall( row(Prot,Seq), member(row(_,Prot,_,Seqn),Rows), PSRowsDiso ),
	sort( PSRowsDiso, PSRows ),
	MapHdr = row('Protein'
	mtx( MapF, [MapHdr|PSRows] ),
	*/
	atomic_list_concat( [_,SrcStem,_,_], '_', SeqStem ),
	% unip_seqs_stem_token( SrcStem, SrcTkn ),
	% atomic_list_concat( [map,unip,SrcTkn], '_', MapStem ),
	% os_ext( csv, MapStem, MapF ),

	bio_db_dnt_times( SprotF, DnSt, _DnEn ),
	IdsOpts = [ delim(','), cnm_transform(unip_seqs_stem_token(SrcStem)), prefix(unip),
	            source(Url), header(Hdr), datetime(DnSt)
	          ],
	csv_ids_map( SeqF, 'Protein', 'Sequence', _, MapF, IdsOpts ), 
	debug( Self, 'Map file: ~p', RelMapF ),
	pwd,
	directory_file_path( maps, MapF, RelMapF ),
	rename_file( MapF, RelMapF ),
	working_directory( Old, maps ),
	link_to_map_sub(unip, MapF ),
	working_directory( _, Old ),

	@ rm( -f, HumF ).

unip_seqs_stem_token( sprot, 'Protein', sprt ) :- !.
unip_seqs_stem_token( sprot, 'Sequence', seqn ) :- !.
unip_seqs_stem_token( trembl, 'Protein', trem ) :- !.
unip_seqs_stem_token( trembl, 'Sequence', seqn ) :- !.

map_unip_human( UnipF ) :-
	os_postfix( human, UnipF, HumF ),
	open( HumF, write, Out ),
	io_sections( UnipF, _, process(human_prot_write(Out)) ),
	close( Out ).

human_prot_write( Out, Lines, Lines ) :-
	Lines = [Fst|_],
	break_list_on_list( Fst, `HUMAN`, _, _ ),
	!,
	maplist( write_codes(Out), Lines ),
	put_code( Out, 0'/ ), put_code( Out, 0'/ ), nl( Out ).
human_prot_write( _Out, _Lines, [] ).

write_codes( Out, Codes ) :-
	maplist( put_code(Out), Codes ), nl( Out ).

sequence_row( Parsed, Symb-row(Symb,AC,Length,Seq) ) :-
	protein_primary_accession( Parsed, AC, AcSymbs ),
	findall( PaSymb, protein_dat_symbol(Parsed,PaSymb), PaSymbs ),
	append( PaSymbs, AcSymbs, AllSymbs ),
	sort( AllSymbs, Symbs ),
	protein_report_mulitple_symbols( Symbs, AC, PaSymbs, AcSymbs, Symb ), 
	memberchk( 'SQ'-[InfoCs|SQCls], Parsed ),
	append( `SEQUENCE   `, LeftCs, InfoCs ),
	once( break_list_on(LeftCs,0' ,LenCs,_) ),
	number_codes( Length, LenCs ),
	flatten( SQCls, SpcSQCs ),
	partition( =(0' ), SpcSQCs, _, SQCs ),
	atom_codes( Seq, SQCs ).

protein_report_mulitple_symbols( [Symb], _AC, _PaSymbs, _AcSymbs, Symb ) :- !.
protein_report_mulitple_symbols( PrvSymbs, _AC, _PaSymbs, _AcSymbs, Symb ) :-
	% include( hgnc_symb, Symbs, Hymbs ),
	% throw( protein_non_singleton_symbols_list(AC,prot_dat(PaSymbs),map_ac(AcSymbs),Symbs) ).
	( PrvSymbs == [] -> Symbs = [z_unk]; Symbs = PrvSymbs ), 
	atomic_list_concat( Symbs, ';', Symb ).

hgnc_symb( Symb ) :-
	map_hgnc_symb_hgnc( Symb, _ ).

protein_dat_symbol( Parsed, Symb ) :-
	member( 'DR'-[DRCs], Parsed ),
	HGNC = `HGNC; `,
	append( HGNC, MidCs, DRCs ),
	break_list_on( MidCs, 0' , _, SymbCsDot ),
	append( SymbCs, [0'.], SymbCsDot ),
	atom_codes( Symb, SymbCs ),
	!. % there are some with duplicates; Q9BTE6

protein_primary_accession( [_,'AC'-[AllACCs]|_], AC, Symbs ) :-
	!,
	break_list_on( AllACCs, 0';, ACCs, _Right ),
	atom_codes( AC, ACCs ),
	findall( Symb, (map_unip_unip_hgnc(AC,Hgnc),map_hgnc_hgnc_symb(Hgnc,Symb)), Symbs ).
