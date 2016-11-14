
:- use_module( library(csv) ).
:- use_module( library(debug_call) ).
% :- requires( debug_call/4 ).
:- use_module( library(os) ).
:- use_module( library(options) ). % en_list/2.
% :- requires( os_dir_stem_ext/4 ).

map_uniprot_defaults( [   uniprot('HUMAN_9606_idmapping.dat'),
                          interface(sqlite),
					 f_call(=),
					 ext([]),
					 destination(maps),
					 reverse(false)
					    ] ).

% :- requires( options_append/3 ).
% :- requires( gunzip/2 ).
% :- requires( true/2 ).
:- requires( map_predicate_name/4 ).
% see above,... :- requires( map_predicate_name_stem/3 ).
:- requires( csv_ids_map/6 ). % :- requires( csv_ids_interface_map/9 ).

:- debug( uniprot ).

%% map_uniprot( +Foreign ).
%% map_uniprot( +Foreign, +Opts ).
%% map_uniprot( +Foreign, ?UniprotCsv, -Written, +Opts ).
%
%  Create a map file from Uniprot identifiers to a Foreign table. UniprotCsv allows multiple
%  calls to map_uniprot/3 to utilise the read Csv.
%  Written is the list of all files written on.
%
% Options
%    * uniprot('HUMAN_9606_idmapping.dat.gz')   file
%    * f_call(=)          call to make on input Foreign accession to transorm to storable accessions
%    * interface(sqlite)  which interfaces to save to give a list if more than one required (sqilte,prolog)
%    * predicate(Predicate)    predicate name, default is uniprot_<low_case(Foreign)>
%    * stem(Stem)         stem of the output file(s) (defaults to Predicate.
%    * destination('.')   destination directory
%    * ext([])            default depends on Interface (sqlite-> sqlite, prolog -> pl) (can be a list if Interface is one)
%    * reverse(Rev=false) reverse the order of 'Protein' and 'Foreign'
% 
%==
%  uniprot_sqlite_map( 'GeneID' ).
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/7/2
%
map_uniprot( Foreign ) :- 
	map_uniprot( Foreign, _, _, [] ).
map_uniprot( Foreign, Opts ) :- 
	map_uniprot( Foreign, _, _, Opts ).
map_uniprot( Foreign, Uniprot, Fouts, Args ) :-
	options_append( map_uniprot, Args, Opts ),
	memberchk( uniprot(UniprotFile), Opts ),
	memberchk( f_call(Fcall), Opts ),
	uniprot_load( UniprotFile, Uniprot ),
	findall( row(Prot,FAcc),  (
							member(row(Prot,Foreign,FAccPrv),Uniprot),
							call( Fcall, FAccPrv, FAcc )
						 ),
									PFRows ),
	debug_call( uniprot, length, '', pf_rows/PFRows ),
	memberchk( interface(FcePrv), Opts ),
	en_list( FcePrv, Fces ),
	memberchk( destination(Dst), Opts ),
	memberchk( reverse(Rev), Opts ),
	reverse_column_names( Rev, uniprot, Foreign, Cnm1, Cnm2 ),

	% map_predicate_name( uniprot, Foreign, Pname, Opts ),
	maplist( uniprot_cname, [Cnm1,Cnm2], [Tnm1,Tnm2] ),
	map_predicate_name( Tnm1, Tnm2, Pname, [map_prefix(true),prefix(unip)|Opts] ),
	map_predicate_name_stem( Pname, Stem, Opts ),
	output_extensions( Opts, Fces, FExtsPrv ),
	sort( FExtsPrv, FExts ),
	maplist( map_uniprot_interface(Foreign,Pname,Stem,Dst,PFRows,Rev), FExts, Fouts ).

map_uniprot_interface( Foreign, Pname, Stem, Dst, PFRows, Rev, Fce-Ext, Fout ) :-
	map_uniprot_save( Fce, Ext, Foreign, Pname, Stem, Dst, PFRows, Rev, Fout ).

uniprot_load( _Partial, Rows ) :-
	ground( Rows ),
	!.
uniprot_load( Partial, Rows ) :-
	member( Ext, ['',dat,'dat.gz','gz'] ),
	file_name_extension( Partial, Ext, File ),
	exists_file( File ),
	!,
	uniprot_load_file( File, Rows ).
uniprot_load( Partial, Rows ) :-
	% expand_file_name( '$local/../work/db/data/uniprot', [UnipD] ),
	absolute_file_name( bio_db_downloads(unip), UnipD ),
	working_directory( Old, UnipD ),
	Old \== UnipD,
	!,
	uniprot_load( Partial, Rows ),
	working_directory( _, Old ).
uniprot_load( _Partial, _Rows ) :-
	throw( could_not_locate_uniprot_id_mappings_file ). % fixme

uniprot_load_file( File, Rows ) :-
	file_name_extension( Stem, gz, File ),
	!,
	% gunzip( File, Stem ),
	@ gunzip( --keep, --force, File ),
	\+ file_name_extension( _, gz, Stem ), % just in case
	uniprot_load_file( Stem, Rows ),
	delete_file( Stem ).
uniprot_load_file( File, Rows ) :-
	debug( uniprot, 'Reading UniProt source at:~p', File ),
	csv_read_file( File, Rows, [separator(0'\t)] ).

map_uniprot_save( sqlite, Ext, Foreign, Pname, Stem, Dst, PFRows, Rev, File ) :-
	% fixme: Rev not implemented here
	( Rev == false -> true; throw( unimplemented_rev_flag_in_sqlite_interface(Rev) ) ),
	use_module( library(prosqlite) ),
	use_module( library(db_facts) ),
	os_dir_stem_ext( Dst, Stem, Ext, File ),
	sqlite_connect( File, uniprot, exists(false) ),
	debug( uniprot, 'Opened sqlite connection to: ~p', File ),
	Create =.. [Pname,uniprot+text,Foreign+text],
	db_create( uniprot, Create ),
	debug_call( uniprot, time(start), 'write on file' ),
	findall( _, (member(row(UniP,Fid),PFRows),Assert=..[Pname,UniP,Fid],db_assert(Assert)), _ ),
	debug_call( uniprot, time(finish), 'write on file' ),
	sqlite_disconnect( uniprot ).
map_uniprot_save( prolog, Ext, Foreign, Pname, Stem, Dst, PFRows, Rev, File ) :-
	os_dir_stem_ext( Dst, Stem, Ext, File ),
	os_dir_stem_ext( Dst, Stem, '', DstStem ),
	debug( uniprot, 'Opened prolog output to: ~p', File ),
	% ComClause =.. [Pname,uniprot_accession,Foreign],
	% write( Out, '% ' ), portray_clause( Out, ComClause ),
	findall( U, member(row(U,_),PFRows), Us ),
	findall( N, member(row(_,N),PFRows), Ns ),
	IfcOpts = [to_value_1(=),to_value_2(=)],
	reverse_columns( Rev, 'Uniprot', Foreign, Us, Ns, Cnm1, Cnm2, Clm1, Clm2 ),
	csv_ids_interface_map( prolog, Pname, DstStem, Cnm1, Cnm2, Clm1, Clm2, _PlFile, IfcOpts ),
	% csv_ids_interface_map( prolog, Pname, DstStem, 'Uniprot', Foreign, Us, Ns, _PlFile, IfcOpts ),
	/* why is this :  here? csv_ids_interface_map already writes the damn file???
	length( PFRows, PFLen ),
	Itv is floor( PFLen // 10 ),
	debug_call( uniprot, time(start), 'write on file' ),
	open( File, write, Out ),
	findall( _, (   nth1(N,PFRows,row(UniP,Fid)),
				 uniprot_report_progress(N,Itv),
	                Clause=..[Pname,UniP,Fid],portray_clause(Out,Clause)), _ ),
	debug_call( uniprot, time(finish), 'write on file' ),
	close( Out ).
	*/
	true.

reverse_column_names( false, Cnm1, Cnm2, Cnm1, Cnm2 ).
reverse_column_names( true, Cnm1, Cnm2, Cnm2, Cnm1 ).

reverse_columns( false, Cnm1, Cnm2, Clm1, Clm2, Cnm1, Cnm2, Clm1, Clm2 ).
reverse_columns(  true, Cnm2, Cnm1, Clm2, Clm1, Cnm1, Cnm2, Clm1, Clm2 ).

map_uniprot_save( pl, Ext, Foreign, Pname, Stem, Dst, PFRows, File ) :-
	map_uniprot_save( prolog, Ext, Foreign, Pname, Stem, Dst, PFRows, File ).

uniprot_report_progress( N, Itv ) :-
	N mod Itv =:= 0,
	!,
	write( '.' ), flush_output.
uniprot_report_progress( _N, _Itv ).

output_extensions( Opts, Fces, Fexts ) :-
	memberchk( ext(ExtPrv), Opts ),
	en_list( ExtPrv, Exts ),
	findall( Fce-Ext, (nth1(N,Fces,Fce),nth_interface_ext(N,Exts,Fce,Ext)), Fexts ).

nth_interface_ext( N, Exts, _Fce, Ext ) :-
	nth1( N, Exts, Ext ),
	!.
nth_interface_ext( _N, _Exts, Fce, Ext ) :-
	interface_extension( Fce, Ext ).

interface_extension( sqlite, sqlite ).
interface_extension( prolog, pl ).
interface_extension( pl, pl ).

uniprot_cname( A, B ) :-
	debug( uniprot, 'trying ~w', uniprot_cname_known(A, B) ),
	uniprot_cname_known( A, B ),
	!.
uniprot_cname( A, A ).

uniprot_cname_known(  'Ensembl_PRO', ensp ).
uniprot_cname_known(  'uniprot', unip ).
uniprot_cname_known(  'HGNC', hgnc ).
uniprot_cname_known(  'GeneID', entz ).
uniprot_cname_known(  'UniGene', unig ).
