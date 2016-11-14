
:- lib(os).
:- lib(options).

:- ensure_loaded( add_data_type ).  % add_data_type_get_types/7.
:- ensure_loaded( lib/bio_db_pl_info ).
:- ensure_loaded( lib/bio_add_info_kvs_lengths_rel ).
:- ensure_loaded( lib/get_date_time ).  % /1.

/** bio_db_add_infos( OsS ).

Add infos terms to prolog bio_db file. Can include options.

To be done: 

Opts 
 * use_existing(Uex=true)
   use existing _info term values where these exist (for date and header, only) 
   and only if non explicit values have been ginen

*/
bio_db_add_infos( OsS ) :-
	en_list( OsS, All ),
	partition( atomic, All, Oss, Opts ),
	maplist( bio_db_add_infos_to(Opts), Oss ).


bio_db_add_infos_to( Opts, Os ) :-
	os_file( Os ),
	os_ext( pl, Os ),
	!,
	bio_db_add_infos_file( Os, Opts ).
bio_db_add_infos_to( Opts, Os ) :-
	os_dir( Os ),
	!,
	debug( bio_db_add_infos, 'Descending to: ~p', Os ),
	directory_files( Os, Entries ),
	working_directory( Old, Os ),
	maplist( bio_db_add_infos_to(Opts), Entries ),
	working_directory( _, Old ).
bio_db_add_infos_to( Etc ) :-
	debug( bio_db_add_infos, 'Skipping non-prolog, non-dir os entry: ~p', Etc ).

bio_db_add_infos_file( Os, InOpts ) :-
	% bio_db_pl_info( Os, Pname, Arity, Infos, NexTerm, DbStream1 ),
	bio_db_pl_info( Os, Pname, Arity, Infos, NexTerm, DbStream1 ),
	findall( Info, (member(Info,Infos),debug(bio_db_add_info,'Info term will be removed: ~w', [Info])), _ ),
	findall( IOpt, (member(Info,Infos),arg(1,Info,IKey),arg(2,Info,IVal),IOpt=..[IKey,IVal]), InfoOpts ),
	append( InOpts, InfoOpts, Opts ),

	findall( integer, between(1,Arity,_), DefTypes ),
	findall( atom, between(1,Arity,_), TrmTypes ),
	add_data_type_get_types( NexTerm, Pname, Arity, DbStream1, DefTypes, TrmTypes, DataTypes ),
	close( DbStream1 ),
	DataTypeVal =.. [data_types|DataTypes],
	atom_concat( Pname, '_info', Iname ),
	DataTypeInfo =.. [Iname,data_types,DataTypeVal],   % data_types  DONE

	( memberchk(source(Source),Opts) ->   
		true
		;
		Source = 'not_known'
	),
	SrcInfo =.. [Iname,source,Source],				%  source DONE

	( (memberchk(datetime(Date),Opts);(memberchk(datetime(Y,M,D,Hr,Mn,Sc),Opts),Date=datetime(Y,M,D,Hr,Mn,Sc))) ->
		true
		;
		get_date_time( Date )
	),
	DateInfo =.. [Iname,datetime,Date],				%  datetime DONE
	( memberchk(header(Hdr),Opts) ->
		true
		;
		atomic_list_concat( [PType,_,KeyTkn,ValTkn], '_', Pname ),
		bio_add_infos_header_val_tokens( PType, KeyTkn, ValTkn, Arity, AllTkns ),
		Hdr =.. [row|AllTkns]
	),
	HeaderInfo =.. [Iname,header,Hdr],			%  header DONE


	bio_db_pl_info( Os, _Pname2, _Arity2, _Infos2, NexTerm2, DbStream2 ),
	bio_db_relation_stream_pairs( NexTerm2, Pname, Arity, DbStream2, KVs, Ks, Vs ),
	close( DbStream2 ),
	bio_add_info_kvs_lengths_rel( KVs, Ks, Vs, Iname, UnqLensInfo, RelTypeInfo ),

	debug( add_infos, '...done', [] ),

	NewInfos = [SrcInfo,DateInfo,HeaderInfo,DataTypeInfo,UnqLensInfo,RelTypeInfo],
	bio_db_file_add_infos( Os, NewInfos ).

bio_db_file_add_infos( Os, NewInfos ) :-
	os_ext( tmp, Os, TmpOs ),
	bio_db_pl_info( Os, _Pname, _Arity, _Infos, NexTerm, DbS ),
	open( TmpOs, write, Onto ),
	maplist( portray_clause(Onto), NewInfos ),
	nl( Onto ),
	copy_term_data( NexTerm, DbS, Onto ),
	close( DbS ),
	close( Onto ),
	delete_file( Os ),
	debug( add_infos, 'Replacing: ~p, with: ~p', [Os,TmpOs] ),
	rename_file( TmpOs, Os ).

copy_term_data( end_of_file, _DbS, _Onto ) :- !.
copy_term_data( InTerm, DbS, Onto ) :-
	portray_clause( Onto, InTerm ),
	read( DbS, NexTerm ),
	copy_term_data( NexTerm, DbS, Onto ).

bio_db_relation_stream_pairs( end_of_file, _Pname, _Arity, _DbS, [], [], [] ) :- !.
bio_db_relation_stream_pairs( Term, Pname, Arity, DbS, [K-V|KVs], [K|Ks], [V|Vs] ) :-
	functor( Term, Pname, Arity ),
	Term =.. [_,K|V],
	read( DbS, Next ),
	bio_db_relation_stream_pairs( Next, Pname, Arity, DbS, KVs, Ks, Vs ).

bio_add_infos_header_val_tokens( edge, KeyTkn, ValTkn, Arity, Tkns ) :-
	bio_add_infos_header_val_tokens_edge( Arity, KeyTkn, ValTkn, Tkns ).
bio_add_infos_header_val_tokens( map, KeyTkn, ValTkn, Arity, [KeyTkn|Tkns] ) :-
	bio_add_infos_header_val_tokens_map( Arity, ValTkn, Tkns ).

bio_add_infos_header_val_tokens_map( 2, ValTkn, [ValTkn] ) :- !.
bio_add_infos_header_val_tokens_map( N, ValTkn, Tkns ) :- 
	N > 2,
	M is N - 1,
	findall( Tkn, (between(1,M,I),atomic_list_concat([ValTkn,I],'_',Tkn)), Tkns ).

bio_add_infos_header_val_tokens_edge( 2, Key, Val, [Tkn1,Tkn2] ) :-
	atomic_list_concat( [Key,Val,1], '_', Tkn1 ),
	atomic_list_concat( [Key,Val,2], '_', Tkn2 ).
bio_add_infos_header_val_tokens_edge( 3, Key, Val, [Tkn1,Tkn2,weight] ) :-
	atomic_list_concat( [Key,Val,1], '_', Tkn1 ),
	atomic_list_concat( [Key,Val,2], '_', Tkn2 ).
