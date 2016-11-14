
:- ensure_loaded( add_data_type ).

bio_add_info_kvs_lengths_rel( KVs, Ks, Vs, Iname, UnqLensInfo, RelTypeInfo ) :-
	sort( KVs, OrdKVs ), length( OrdKVs, UnqKVs ),
	sort( Ks,  OrdKs  ), length( OrdKs,  UnqKs  ),
	sort( Vs,  OrdVs  ), length( OrdVs,  UnqVs  ),
	UnqLensInfo =.. [Iname,unique_lengths,unique_lengths(UnqKs,UnqVs,UnqKVs)],    % unique_lengths  DONE
	length( KVs, LenKVs ),
	bio_db_lengths_to_rel_type( UnqKs, UnqVs, UnqKVs, LenKVs, RelType ),
	% bio_db_lengths_to_rel_type( UnqKs, UnqVs, UnqKVs, RelType ),
	RelTypeInfo =.. [Iname,relation_type,RelType].
	% bio_db_data_type( OrdKs, integer, atom, Ktype ),
	% bio_db_data_type( OrdVs, integer, atom, Vtype ),
	% DtTypeInfo =.. [Iname,data_types,data_types(Ktype,Vtype)].


bio_db_lengths_to_rel_type( UnqKs, _UnqVs, UnqKVs, _LenKVs, RelType ) :-
	UnqKs =:= UnqKVs,
	!,
	RelType =  relation_type(1,1).
bio_db_lengths_to_rel_type( UnqKs, _UnqVs, _UnqKVs, LenKVs, RelType ) :-
	UnqKs =:= LenKVs,  % hmmmm which one ?, this one guarantees determinism
	% UnqKs =:= UnqKVs,
	!,
	RelType = relation_type(1,m).
bio_db_lengths_to_rel_type( _UnqKs, UnqVs, UnqKVs, _LenKVs, RelType ) :-
	UnqVs =:= UnqKVs,
	!,
	RelType = relation_type(m,1).
% we only really intrested on the K -> V  (and not V -> K) so we stop looking here
bio_db_lengths_to_rel_type( _UnqKs, _UnqVs, _UnqKVs, _LenKVs, RelType ) :-
	RelType = relation_type(m,m).


/*

*/
