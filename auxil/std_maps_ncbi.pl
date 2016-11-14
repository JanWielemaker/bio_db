
:- ensure_loaded( 'lib/requires_minimal' ).

% :- use_module( library(requires) ).
% :- lib( bio_db ).
:- lib( by_unix ).
:- lib( os ).
% :- requires( (@)/2 ).

:- ensure_loaded( bio_db_build_aliases ), bio_db_build_aliases( [] ).
:- ensure_loaded( ens_fa_peptide_gene_rows ).

:- requires( url_file_local_date_mirror/2 ).
:- requires( csv_ids_map/6 ).
:- requires( link_to_bio_sub/3 ). % link_to_map_sub/2.
% :- requires( link_to_map_sub/2 ).
% :- requires( csv_filter_by_column/4 ). % mtx_column_include_rows...
:- requires( prefix_atom/2 ).
% :- requires( maps_ncbi_ensp_ensg/0 ).
:- ensure_loaded( lib/bio_db_dnt_times ).


% :- debug( url_local ). % use option, debug(url_local)
% :- debug( _ ).

:- set_prolog_stack(global, limit(2*10**9)).

ncbi_repo( 'ftp://ftp.ncbi.nih.gov/gene/DATA/' ).
ncbi_dnload( Loc ) :-
	absolute_file_name( bio_db_build_downloads(ncbi), Loc ),
	os_make_path( Loc, debug(true) ).

% maps_ncbi_ensp_ensg.
%
% This is a later addition.
% See ens_fa_peptide_gene_rows..pl
%
maps_ncbi_ensp_ensg :-
	% Dir = '/usr/local/users/nicos/work/db/data/ncbi',
	fixme,
	ncbi_dnload( Dir ),
	EnsF= 'Homo_sapiens.GRCh38.pep.all.fa',
	working_directory( Old, Dir ),
	ens_fa_peptide_gene_rows( EnsF, EnsRows ),
	csv_ids_map( _CsvF, ensp, ensg, EnsRows, OutF, [prefix(ncbi),header(row('Ensembl Protein','Ensembl Gene'))] ),
	link_to_map_sub(ncbi, OutF ),
	working_directory( _, Old ).

maps_ncbi_entz_gont :-
	% Dir = '/usr/local/users/nicos/work/db/data/ncbi',
	ncbi_dnload( Dir ),
	ncbi_repo( Repo ),
	os_path( Repo, 'gene2go.gz', Url ),
	url_file_local_date_mirror( Url, Dir, debug(url_local) ),
	working_directory( Old, Dir ),
	@ rm( -f, gene2go_hs ),
	@ rm( -f, gene2go ),
	@ gunzip( -f, -k, 'gene2go.gz' ),
	debug( by_unix ),
	grep(gene2go, '^9606', gene2go_hs),
	% system( 'grep "^9606" gene2go | cat gene2go_hs' ),
	working_directory( _, Old ).

maps_ncbi_rnuc_symb :-
	debug( by_unix ),
	ncbi_dnload( Dir ),
	ncbi_repo( Repo ),
	ncbi_humanise_data( gene2accession, Dir, Repo, Old, HsStem, HsUrl, HsDnDt ),

	file_name_extension( HsStem, tmp, TmpF ),
	@ mv( -f, HsStem, TmpF ),
	open( HsStem, write, HsOut ),
	write( HsOut, 'tax_id	GeneID	status	RNA_nucleotide_accession.version	RNA_nucleotide_gi	protein_accession.version	protein_gi	genomic_nucleotide_accession.version	genomic_nucleotide_gi	start_position_on_the_genomic_accession	end_position_on_the_genomic_accession	orientation	assembly	mature_peptide_accession.version	mature_peptide_gi	Symbol' ),
	nl( HsOut ),
	close( HsOut ),
	atomic_list_concat( [cat,TmpF,'>>',HsStem], ' ', Cat ),
	shell( Cat ),
	% @ mv( -f, HsStem, HsStem ),

	CIMOpts = [ cnm_transform(ncbi_gene2asseccion_cnms), prefix(ncbi),
	            to_value_1(de_versionise),
			  to_valuse_2(is_a_symbol),
			  datetime(HsDnDt), source(HsUrl), header(row('RNA Nucleotide','HGNC Symbol'))
	],
	csv_ids_map( HsStem, 'RNA_nucleotide_accession.version', 'Symbol', _Csv1, OutF, CIMOpts ),


	DNAOpts = [ cnm_transform(ncbi_gene2asseccion_cnms), prefix(ncbi),
	            to_value_1(de_versionise),
			  to_valuse_2(is_a_symbol),
			  datetime(HsDnDt), source(HsUrl), header(row('DNA Nucleotide','HGNC Symbol'))
	],
	csv_ids_map( HsStem, 'genomic_nucleotide_accession.version', 'Symbol', _Csv2, DNAF, DNAOpts ),

	delete_file( TmpF ),
	os_make_path( maps ),
	@ mv( -f, OutF, maps ),
	@ mv( -f, DNAF, maps ),
	working_directory( _, maps ),
	link_to_map_sub(ncbi, OutF ),
	link_to_map_sub(ncbi, DNAF ),
	working_directory( _, Old ).

maps_ncbi_unig_entz :-
	ncbi_dnload( Dir ),
	ncbi_repo( Repo ),
	os_path( Repo, 'gene2unigene', Url ),
	url_file_local_date_mirror( Url, Dir, debug(url_local) ),
	working_directory( Old, Dir ),
	bio_db_dnt_times( 'gene2unigene', UgDnDt, _DnEn ),

	csv_read_file( gene2unigene, [_|Csv], [separator(0'\t),match_arity(false)] ),
	Hdr = row(entz,unig),
	MOpts = [prefix(ncbi),to_value_1(hs_unig),datetime(UgDnDt),source(Url),header(row('Uni Gene','Entrez ID'))], 
	csv_ids_map( _, 'unig', 'entz', [Hdr|Csv], OutF, MOpts ),
	os_make_path( maps ),
	@ mv( -f, OutF, maps ),
	working_directory( _, maps ),
	link_to_map_sub( ncbi, OutF ), 
	working_directory( _, Old ).

ncbi_humanise_data( Stem, Dir, Repo, Old, HsStem, Url, DnDt ) :-
	file_name_extension( Stem, gz, GzF ),
	os_path( Repo, GzF, Url ),
	url_file_local_date_mirror( Url, Dir, debug(url_local) ),
	os_path( Dir, GzF, DnlF ),
	bio_db_dnt_times( DnlF, DnDt, _DnEn ),

	working_directory( Old, Dir ),
	atomic_list_concat( [Stem,hs], '_', HsStem ),
	@ rm( -f, HsStem ),
	@ rm( -f, Stem ),
	@ gunzip( -f, -k, GzF ),
	grep( Stem, '^9606', HsStem ).

hs_unig( In, In ) :-
	atom_concat( 'Hs.', _, In ).

de_versionise( ProductVersion, Product ) :-
	atomic_list_concat( [Product,_Version], '.', ProductVersion ),
	!.

is_a_symbol( Symb, Symb ) :-
	map_hgnc_symb_hgnc( Symb, _ ),
	!.

ncbi_gene2asseccion_cnms( 'RNA_nucleotide_accession.version', rnuc ).
ncbi_gene2asseccion_cnms( 'genomic_nucleotide_accession.version', dnuc ).
ncbi_gene2asseccion_cnms( 'Symbol', symb ).

%% std_maps_ncbi.
%
% Download latest NCBI gene to ensembl map file and convert it to 
% a few standard maps.
%==
% std_maps_ncbi.
%==
% @author nicos angelopoulos
% @version  0.1 2014/7/23
%
std_maps_ncbi :-
	% expand_file_name( '$local/../work/db/data/ncbi', [NcbiD] ),
	ncbi_dnload( NcbiD ),
	Url = 'ftp://ftp.ncbi.nih.gov/gene/DATA/gene2ensembl.gz',
	url_file_local_date_mirror( Url, NcbiD ),
	file_base_name( Url, RemB ),
	working_directory( Old, NcbiD ),
	MapsD = maps,
	make_directory_path( MapsD ),
	directory_file_path( MapsD, RemB, ToP ),
	copy_file( RemB, ToP ),
	bio_db_dnt_times( RemB, DnDt, _DnEn ),
	working_directory( _ParentD, MapsD ),
	@ gunzip( RemB ),
	file_name_extension( RemS, gz, RemB ),
	std_maps_ncbi( RemS, Url, DnDt ),
	delete_file( RemS ),
	maps_ncbi_rnuc_symb,
	maps_ncbi_unig_entz,
	working_directory( _, Old ).

std_maps_ncbi( File, Url, DnDt ) :-
	TsvOpts = [match_arity(false),separator(0'\t)],
	csv_read_file( File, Csv, TsvOpts ),
	Csv = [_Comment|Rows],
	New = [row(tax_id,entz,ensg,nucl_acc,ensr,prot_acc,ensp)|Rows],
	% GEnsGF = entrez_gene_id_ensg.pl,
	% csv_filter_by_column( New, tax_id, =(9606), HS ),
	mtx_column_values_select( New, tax_id, 9606, HS, _, true ),
	length( HS, HsLen ),
	write( hs_len(HsLen) ), nl,
	Lens = [prefix(ncbi),to_value_1(pos_integer),to_value_2(pfx_by('ENS')),datetime(DnDt),source(Url)],
	Rens = [prefix(ncbi),to_value_2(pos_integer),to_value_1(pfx_by('ENS')),datetime(DnDt),source(Url)],
	csv_ids_map( File, entz, ensg, HS, GEnsGF, [header(row('Entrez ID','Ensembl Gene'))|Lens] ),
	csv_ids_map( File, ensg, entz, HS, EnsGGF, [header(row('Ensembl Gene','Entrez ID'))|Rens] ),
	% need to ensure prots are of ENSP  there are - in some entries
	csv_ids_map( File, entz, ensp, HS, GEnsPF, [header(row('Entrez ID','Ensembl Protein'))|Lens] ),
	csv_ids_map( File, ensp, entz, HS, EnsPGF, [header(row('Ensembl Protein','Entrez ID'))|Rens] ),
	maplist( link_to_map_sub(ncbi), [GEnsGF,EnsGGF,GEnsPF,EnsPGF] ).

pos_integer( Numb, Numb ) :-
     integer( Numb ),
     Numb > 0.

pfx_by( Pfx, Full, Full ) :-
	prefix_atom( Pfx, Full ).

grep(File, Pattern, OutF) :-
        process_create(path(grep), [ Pattern, file(File) ],
                       [ stdout(pipe(Out))
                       ]),
        % read_lines(Out, Lines).
	   open( OutF, write, Write ),
	   write_lines_out(Out, Write),
	   close( Write ).

write_lines_out(Out, Write) :-
	   read_line_to_codes( Out, Line1 ),
        write_lines(Line1, Out, Write ).

write_lines(end_of_file, _, _) :- !.
write_lines(Codes, Out, Write) :-
        atom_codes(Line, Codes),
	   write( Write, Line ), nl( Write ),
        read_line_to_codes(Out, Line2),
        write_lines(Line2, Out, Write).

ncbi_cname_known( 'HGNC Symbol', symb ).
ncbi_cname_known( 'Ensembl Gene', ensg ).
ncbi_cname_known( 'Ensembl Protein', ensp ).
ncbi_cname_known( 'Entrez ID', entz ).
ncbi_cname_known( 'Uni Gene', unig ).
ncbi_cname_known( 'RNA Nucleotide', rnuc ).
ncbi_cname_known( 'DNA Nucleotide', dnuc ).
