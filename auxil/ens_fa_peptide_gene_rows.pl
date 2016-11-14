%% ens_fa_peptide_gene_rows( +EnsFaF, -Pairs ).
%
% Parses all EnsP-EnsG peptide/protein-gene pairs in a ensemb fasta file.
%
% Example input file:
%
% ftp://ftp.ensembl.org/pub/release-78/fasta/homo_sapiens/pep/Homo_sapiens.GRCh38.pep.all.fa.gz
%
% As downloaded on 2015/03/06
% 
% Segments look like:
% >ENSP00000439323 pep:known chromosome:GRCh38:14:21846537:21847221:1 gene:ENSG00000211786 transcript:ENST00000390434 gene_biotype:TR_V_gene transcript_biotype:TR_V_gene
% MLLLLVPVLEVIFTLGGTRAQSVTQLDSHVSVSEGTPVLLRCNYSSSYSPSLFWYVQHPN
% KGLQLLLKYTSAATLVKGINGFEAEFKKSETSFHLTKPSAHMSDAAEYFCVVS
%
ens_fa_peptide_gene_rows( File, Pairs ) :-
	open( File, read, In ),
	read_line_to_codes( In, Cs ),
	ens_fa_peptide_gene_rows_segment( Cs, In, Pairs ),
	close( In ).

ens_fa_peptide_gene_rows_segment( end_of_file, _In, [] ) :- !.
ens_fa_peptide_gene_rows_segment( First, In, [Pair|TPairs] ) :-
	ens_fa_line_pair( First, Pair ),
	!,
	ens_fa_skip_fasta_headers( In, Next ),
	ens_fa_peptide_gene_rows_segment( Next, In, TPairs ).

ens_fa_line_pair( Cs, row(EnsP,EnsG) ) :-
	[0'>,0'E,0'N,0'S,0'P|Tail] = Cs,
	append( EnsPID, [0' |RemEnsP], Tail ),
	atom_codes( EnsP, [0'E,0'N,0'S,0'P|EnsPID] ),
	append( _Left, [0'g,0'e,0'n,0'e,0':,0'E,0'N,0'S,0'G|GeneTail], RemEnsP ),
	append( EnsGID, [0' |_], GeneTail ),
	atom_codes( EnsG, [0'E,0'N,0'S,0'G|EnsGID] ).

ens_fa_skip_fasta_headers( In, Next ) :-
	read_line_to_codes( In, Cs ),
	ens_fa_skip_fasta_headers_cont( Cs, In, Next ).

ens_fa_skip_fasta_headers_cont( end_of_file, _, end_of_file ) :- !.
ens_fa_skip_fasta_headers_cont( [0'>,0'E,0'N,0'S,0'P|Tail], _, Cs ) :- !,
	[0'>,0'E,0'N,0'S,0'P|Tail] = Cs.
ens_fa_skip_fasta_headers_cont( _, In, Cont ) :- !,
	read_line_to_codes( In, Next ),
	ens_fa_skip_fasta_headers_cont( Next, In, Cont ).
	
