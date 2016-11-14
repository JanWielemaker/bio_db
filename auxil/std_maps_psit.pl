
:- ensure_loaded( 'lib/requires_minimal' ).

:- lib( os      ).
:- lib( by_unix ).
:- lib( options ).
:- lib( debug_call ).

:- ensure_loaded( bio_db_build_aliases ).  % /1.

:- ensure_loaded( bio_db_add_infos ).
:- ensure_loaded( lib/bio_db_dnt_times ).

:- requires( url_file_local_date_mirror/3 ).
:- requires( file_to_list_of_lines/2 ).
:- requires( at_con/3 ).
:- requires( map_succ_list/3 ).
:- requires( portray_clauses/3 ).
:- lib( bio_db  ).  % ! fixme

pros_alignments_url( 'ftp://ftp.expasy.org/databases/prosite/prosite_alignments.tar.gz' ).

pros_dnload_dir( Old, Loc, Opts ) :-
	bio_db_build_aliases( Opts ),
	absolute_file_name( bio_db_build_downloads(pros), Loc ),
	os_make_path( Loc, Opts ),
	debug( maps_unip_seqs, 'Prosite build directory: ~p', Loc ),
	working_directory( Old, Loc ).

std_maps_pros_defaults( debug(true) ).

/** std_maps_psite.

Support for prosite annotations of uniprot proteins.

@author nicos angelopoulos
@version  0.1 2016/2/4

*/
std_maps_psit :-
	std_maps_pros( [] ).
	
std_maps_pros( Args ) :-
	Self = std_maps_pros,
	options_append( Self, Args, Opts ),
	debug( std_maps_pros, 'Starting Prosite maps', true ),
	pros_dnload_dir( Old, DnDir, Opts ),
	pros_alignments_url( Url ),
	UrlOpts = [debug(url_local),interface(wget),file(ProsF),ext('tar.gz')],
	url_file_local_date_mirror( Url, DnDir, UrlOpts ),
	bio_db_dnt_times( ProsF, DnSt, _DnEn ),
	debug( Self, 'Prosite local file: ~p', ProsF ),
	( os_dir(prosite_alignments) ->
		@mv( prosite_alignments, /tmp ),
		debug( Self, 'Moved existing dir prosite alignments to /tmp/', true )
		;
		true
	),
	@ gunzip( -f, -k, 'prosite_alignments.tar.gz' ),
	@ tar( xvf, 'prosite_alignments.tar' ),
	@ rm( -f, 'prosite_alignments.tar' ),
	directory_files( prosite_alignments, PalignFs ),
	include( os_ext(msa), PalignFs, MsaFs ),
	map_succ_list( msa_file_pros_prsn, MsaFs, ProsPrsnClauses ),
	PrsnF = 'map_pros_pros_prsn.pl',
	portray_clauses( ProsPrsnClauses, file(PrsnF) ),
	debug_call( std_map_pros, wrote, PrsnF ),
	PrsnOpts = [header(),datime(DnSt),source(Url)],
	bio_db_add_infos_to( PrsnOpts, PrsnF ),
	os_make_path( maps ),
	os_path( maps, PrsnF, RelPrsnF ),
	rename_file( PrsnF, RelPrsnF ),

	maplist( msa_file_pros_sprt, MsaFs, ProsSprtClausesNest ),
	flatten( ProsSprtClausesNest, ProsSprtClauses ),
	SprtF = 'map_pros_pros_sprt.pl',
	portray_clauses( ProsSprtClauses, file(SprtF) ),
	debug_call( std_map_pros, wrote, SprtF ),
	SprtOpts = [header(),datime(DnSt),source(Url)],
	bio_db_add_infos_to( SprtOpts, SprtF ),
	os_path( maps, SprtF, RelSprtF ),
	rename_file( SprtF, RelSprtF ),

	working_directory( _, Old ),
	debug( std_maps_pros, 'Done Prosite maps', true ).
	
msa_file_pros_prsn( File, map_pros_pros_prsn(Base,Prsn) ) :-
	debug( std_maps_pros, 'prositing prosite names: ~p', File ),
	os_ext( msa, Base, File ),
	os_path( prosite_alignments, File, Path ),
	file_to_list_of_lines( Path, Lines ),
	findall( PrsnA, (member(Line,Lines),Line = [0'>|Rine],
	                 atom_codes(Ratm,Rine), 
				  at_con([A,B,_C],'|',Ratm),
				  at_con([_,'HUMAN'],'_',A),
				  at_con([Prot,RightB],'/',B),
				  map_unip_sprt_seqn( Prot, _ ),
				  at_con([_,PrsnA],': ',RightB)
				  % we can also match Pros to Left C (sep /)
				  )
				  , PrsnAs ),
	sort( PrsnAs, PrsnOrd ),
	% ( File == 'PS51172.msa' -> trace; true ),
	report_non_singleton_prsns( PrsnOrd, Prsn ).

report_non_singleton_prsns( [], _Prsn ) :- !, fail.
report_non_singleton_prsns( [Prsn], Prsn ) :- !.
report_non_singleton_prsns( Other, _Prsn ) :-
	throw( non_unique_prsn(Other) ).

msa_file_pros_sprt( File, Maps ) :-
	debug( std_maps_pros, 'prositing prosite swiprots: ~p', File ),
	os_ext( msa, Base, File ),
	os_path( prosite_alignments, File, Path ),
	file_to_list_of_lines( Path, Lines ),
	findall( Map, (  nth1(N,Lines,Line1),
	                 Line1 = [0'>|Rine],
	                 atom_codes(Ratm,Rine), 
				  at_con([A,B,_C],'|',Ratm),
				  at_con([_,'HUMAN'],'_',A),
				  at_con([Prot,RightB],'/',B),
				  map_unip_sprt_seqn( Prot, _ ),
				  ( (map_unip_unip_hgnc(Prot,Hgnc),
				     map_hgnc_hgnc_symb(Hgnc,Symb))
					-> 
					true
					;
					Symb = unk
				  ),
				  at_con([FromTo,PrsnA],': ',RightB),
				  at_con([From,To],'-',FromTo),
				  M is N + 1,
				  nth1(M,Lines,Line2),
				  atom_codes(AtmL2,Line2),
			       Map = map_pros_pros_sprt(Base,PrsnA,Prot,Symb,From,To,AtmL2)
			    ),
			    		Maps ).

