/** bio_db_map( +Ifce, +Map ).

Map predicate Map/2 to a db of type Ifce.

Map should be _ separated and first being map and second part is the database.

*/
bio_db_map( load, Map ) :-
	MCall =.. [Map,_,_],
	once( MCall ),
	!.

bio_db_map( prolog, Map ) :-
	% just check that it exists, the real loading is done 
	% in the module file
	MCall =.. [Map,_,_],
	bio_db_load_prolog_map( MCall, check ).
