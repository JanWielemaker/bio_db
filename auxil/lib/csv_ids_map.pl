
:- use_module( library(real) ).
:- use_module( library(mtx) ).
:- use_module( library(options) ).
% :- requires( mtx_column/5 ).

:- ensure_loaded( bio_add_info_kvs_lengths_rel ).
:- ensure_loaded( portray_clauses ).              % portray_clauses_onto/2.
:- ensure_loaded( 'lib/map_predicate_name' ).     % /4.

% :- requires( which/3 ).
% :- requires( map_predicate_name_stem/3 ).
% :- requires( true/2 ).
% :- requires( get_date/1 ).
% :- requires( kv_decompose_vs/2 ).

csv_ids_map_defaults( [ cnm_transform(=),dir('.'),
                        delim('\t'),
                        interface(prolog),map_prefix(true),
				    prefix(''), sort_by(1), 
                        to_value_1(=),to_value_2(=),
				    source('not_known')
				    ] ).

%% csv_ids_map( +CsvF, +Cnm1, +Cnm2, -Csv, -OutF, +Opts ).
%
% Create an ids map (sqlite,prolog,odbc) from CsvF the rows are in Csv.
% If Csv is ground then CsvF is not read. Csv allows for chain calls on same CsvF.
% OutF is the filename used for the output.
%
% Options
%   * cnm_transform(Ctr=true)  
%     call which transforms column names
%   * datetime(DnDt)
%     download datime
%   * delim(Dlm='\t'
%     separator for read.delim()
%   * dir(Dir='.')
%     directory for the result
%   * filter(Cnm,Fgoal)
%     fixme:
%   * header(Hdr)
%     a header term, something line row(CnmDesc1,CnmDesc2)
%   * interface(Ifc=prolog)
%     also known is sqlite
%   * map_prefix(MapPfx=true)
%     shall map prefix be included 
%   * prefix(Pfx='')
%     include this prefix after map_ component if given (usual db name)
%   * source(Src='not_known'
%     source info tag
%   * sort_by(Sby=1)
%     sort clauses by 1st (or 2nd) argument
%   * to_value_1(Tv1=true)
%     transforms left data, or removes row if it fails
%   * to_value_2(Tv2=true)
%     transforms right data, or removes row if it fails
%
% @author nicos angelopoulos
% @version  0.1 2014/7/2
% @version  0.2 2015/3/19    added map_prefix(MapPfx), 
% @tbd odbc interface, sqlite + mysql,  and debug
%
csv_ids_map( CsvF, Cid1, Cid2, Tbl, File, Args ) :-
	options_append( csv_ids_map, Args, Opts ),
	options( delim(Dlm), Opts ),
	csv_ids_rows( CsvF, Dlm, Tbl ),
	csv_or_frame_column( Tbl, Cid1, Clm1, Cnm1 ),
	csv_or_frame_column( Tbl, Cid2, Clm2, Cnm2 ),

	% memberchk( Cid2=Clm2, Tbl ), Cnm2 = Cid2, % fixme
	% csv_column( Csv, Cid1, Clm1, Cnm1, _Nhdr1 ),
	% csv_column( Csv, Cid2, Clm2, Cnm2, _Nhdr2 ),
	filter_columns( Opts, Tbl, Clm1, Clm2, Filt1, Filt2 ), % fixme: this is wasteful
	memberchk( interface(Fce), Opts ),
	memberchk( cnm_transform(CnmT), Opts ),
	maplist( CnmT, [Cnm1,Cnm2], [Tnm1,Tnm2] ),
	map_predicate_name( Tnm1, Tnm2, Pname, Opts ),
	map_predicate_name_stem( Pname, Stem, Opts ),
	memberchk( dir(Dir), Opts ),
	directory_file_path( Dir, Stem, Dtem ),
	csv_ids_interface_map( Fce, Pname, Dtem, Cnm1, Cnm2, Filt1, Filt2, File, Opts ).

csv_ids_interface_map( prolog, Pname, Stem, Tnm1, Tnm2, Filt1, Filt2, File, Opts ) :-
	file_name_extension( Stem, pl, File ),
	open( File, write, Out ),
	% ComClause =.. [Pname,Tnm1,Tnm2],
	% write( Out, '% ' ), portray_clause( Out, ComClause ),
	% nl( Out ),
	memberchk( to_value_1(Tv1), Opts ),
	memberchk( to_value_2(Tv2), Opts ),
	% collect_map( Filt1, Filt2, [], Tv1, Tv2, Pname, MapUno ),
	collect_map( Filt1, Filt2, Tv1, Tv2, Pname, MapUno ),
	atomic_list_concat( [Pname,info], '_', Pinfo ),
	csv_ids_map_date( Datetime, Opts ),
	( memberchk(source(SrcUrl),Opts) -> true; SrcUrl = not_known ),

	UrlInfo =.. [Pinfo,source_url,SrcUrl],
	DtInfo =.. [Pinfo,datetime,Datetime],
	% length( MapUno, MapLen ),
	sort( MapUno, MapByFirst ), 
	( memberchk(sort_by(Sby),Opts) -> true; Sby = 1 ),
	sort_map_by( Sby, MapByFirst, Map ),

	maplist( arg(1), Map, RealKs ),
	maplist( arg(2), Map, RealVs ),
	cohesed_data_type( RealKs, integer, atom, Ktype ),
	cohesed_data_type( RealVs, integer, atom, Vtype ),
	DataTypeInfo =.. [Pinfo,data_types,data_types(Ktype,Vtype)],

	% length( Map, MapLen ),
	% map_type_term( Map, Pinfo, UnqInfo, RelInfo, DataTypeInfo ),
	map_type_term( Map, Pinfo, UnqInfo, RelInfo ),
	% Cclause =.. [Pinfo,arg_names,arg_names(Tnm1,Tnm2)],

	% Lclause =.. [Pinfo,unique_lengths,unique_lengths(MapLen,FLen,SLen)],
	% Lclause =.. [Pinfo,unique_lengths,UnqInfo],
	% Mclause =.. [Pinfo,rel_type,RelInfo],
	( memberchk(header(HdrRow),Opts) ->
		true
		;
		HdrRow = row(Tnm1,Tnm2)
	),
	HdrInfo =.. [Pinfo,header,HdrRow],
	portray_clauses_onto( [UrlInfo,DtInfo,DataTypeInfo,UnqInfo,RelInfo,HdrInfo], Out ),
	nl( Out ),
	portray_clauses_onto( Map, Out ),
	close( Out ).

cohesed_data_type( [], Type, _Bottom, Type ).
cohesed_data_type( [H|T], Current, Bottom, Type ) :-
	add_data_type_of( H, HType ),
	add_data_type_cohese( Current, HType, NextType ),
	holds( NextType \= Bottom, Cont ),
	cohesed_data_type_cont( Cont, T, NextType, Bottom, Type ).

cohesed_data_type_cont( true, T, Next, Bottom, Type ) :-
	cohesed_data_type( T, Next, Bottom, Type ).
cohesed_data_type_cont( false, _T, Bottom, Bottom, Bottom ).

csv_ids_map_date( Datetime, Opts ) :-
	memberchk( datetime(Datetime), Opts ),
	!.
csv_ids_map_date( Datetime, _Opts ) :-
	get_time( Stamp ),
     stamp_date_time( Stamp, date(Y,M,D,Hr,Mn,Sc,_,_,_), local ),
	Datetime = date(Y,M,D,Hr,Mn,Sc).

sort_map_by( 1, Map, Map ).
sort_map_by( 2, Map, ByMap ) :-
	findall( Scn-Elem, (member(Elem,Map),arg(2,Elem,Scn)), Pairs ),
	keysort( Pairs, Sortairs ),
	findall( V, member(_-V,Sortairs), ByMap ).
	% kv_decompose_vs( Sortairs, ByMap ).

map_type_term( Map, Pinfo, UnqLenInfo, RelTypeInfo ) :-
	% see packs(bio_db/auxil/bio_add_infos)
	map_to_kvs( Map, KVs, Ks, Vs ),
	/*
	maplist( arg(1), Map, Fsts ),
	maplist( arg(2), Map, Secs ),
	length( Map, MLen ),
	sort( Fsts, FOrd ),
	sort( Secs, SOrd ),
	length( FOrd, FLen ),
	length( SOrd, SLen ),
	maplist( map_type_length_atom(MLen), [FLen,SLen], [Ftype,Stype] ).
	*/
	bio_add_info_kvs_lengths_rel( KVs, Ks, Vs, Pinfo, UnqLenInfo, RelTypeInfo ).
	

map_to_kvs( [], [], [], [] ).
map_to_kvs( [H|T], [K-V|KVs], [K|Ks], [V|Vs] ) :-
	H =.. [_,K|V],
	map_to_kvs( T, KVs, Ks, Vs ).

map_type_length_atom( Records, Order, Type ) :-
	compare( Op, Records, Order ),
	map_type_length_op_atom( Op, Type ).

map_type_length_op_atom( =, 1 ).
map_type_length_op_atom( >, m ).
map_type_length_op_atom( <, _ ) :- throw( impossible_map_type_op_length_op ).

% collect_map( [], [], _Seen, _Tv1, _Tv2, _Pname, [] ). % old version
collect_map( [], [], _Tv1, _Tv2, _Pname, [] ).
collect_map( [F|Fs], [T|Ts], Tv1, Tv2, Pname, Clauses ) :-
	% collect_map_to_values( F, T, Seen, Tv1, Tv2, Pname, Clauses, Next, TClauses ),
	( (call(Tv1,F,V1),call(Tv2,T,V2)) ->
		Clause =.. [Pname,V1,V2],
		Clauses = [Clause|TClauses]
		;
		TClauses = Clauses
	),
	collect_map( Fs, Ts, Tv1, Tv2, Pname, TClauses ).

/*
collect_map_to_values( F, T, Seen, Tv1, Tv2, Pname, Clauses, Next, TClauses ) :-
	call( Tv1, F, V1 ),
	call( Tv2, T, V2 ),
	!,
	Clause =.. [Pname,V1,V2].
	% old version:
	% collect_map_un_seen( V1, V2, Pname, Seen, Next, Clauses, TClauses ).
collect_map_to_values( _F, _T, Seen, _Tv1, _Tv2, _Pname, Clauses, Seen, Clauses ).

collect_map_un_seen( V1, V2, _Pname, Seen, Next, Clauses, TClauses ) :-
	ord_memberchk( V1-V2, Seen ),
	!,
	Next = Seen, Clauses = TClauses.
collect_map_un_seen( V1, V2, Pname, Seen, Next, Clauses, TClauses ) :-
	Clause =.. [Pname,V1,V2],
	% portray_clause( Out, Clause ),
	ord_add_element( Seen, V1-V2, Next ),
	Clauses = [Clause|TClauses].
*/

csv_or_frame_column( Tbl, Cid1, Clm1, Cnm1 ) :-
	memberchk( Cid1=Clm1, Tbl ), 
	!,
	Cnm1 = Cid1. % fixme
csv_or_frame_column( Tbl, Cid1, Clm1, Cnm1 ) :-
	% mtx_column( Tbl, Cid1, Clm1, _, Cnm1 ).
	mtx_column( Tbl, Cid1, Clm1, Cnm1, _ ).

csv_ids_rows( _CsvF, _Dlm, Csv ) :-
	ground( Csv ),
	!.
csv_ids_rows( CsvF, Dlm, Csv ) :-
	tbl <- 'read.delim'( +CsvF, 'check.names'='FALSE', sep=+Dlm, 'as.is'='TRUE' ),
	Csv <- tbl.

% filter_columns( Opts, Csv, Clm1, Clm2, Filt1, Filt2 ) :-
filter_columns( Opts, Tbl, Clm1, Clm2, Filt1, Filt2 ) :-
	memberchk( filter(FiltCnm,FiltCall), Opts ),
	!,
	% csv_column( Csv, FiltCnm, FiltClm ),
	memberchk( FiltCnm=FiltClm, Tbl ),
	% which( FiltCall, FiltClm, FiltIdc ),
	findall( I, (nth1(I,FiltClm,Ith),once(call(FiltCall,Ith))), FiltIdc ),
	sieve_indices( FiltIdc, Clm1, _, Filt1 ),
	sieve_indices( FiltIdc, Clm2, _, Filt2 ).
filter_columns( _Opts, _Csv, Clm1, Clm2, Clm1, Clm2 ).
