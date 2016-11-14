/** bio_db_pl_info( +DbF, -Pname, -Arity, -Infos, -NexTerm, -Stream ).

	Read leading info terms of a prolog db at DbF, returning the infor terms in Infos,
	the first non info term in NexTerm and the open stream at Stream.

==
?- lib( bio_db ).
?- bio_db_pl_info( bio_db_data(maps/hgnc/map_hgnc_hgnc_symb), Infos ), maplist( writeln, Infos ), fail.
map_hgnc_hgnc_symb_info(date,date(2016,9,6))
map_hgnc_hgnc_symb_info(map_type,map_type(1,1))
map_hgnc_hgnc_symb_info(unique_lengths,c(44266,44266,44266))
map_hgnc_hgnc_symb_info(header,row(HGNC ID,Approved Symbol))
false.
==

@author nicos angelopoulos
@version  0.1 2016/9/7

*/
bio_db_pl_info( DbFin, Infos ) :-
	bio_db_pl_info( DbFin, _Pname, _Arity, Infos ).

bio_db_pl_info( DbFin, Pname, Arity, Infos ) :-
	bio_db_pl_info( DbFin, Pname, Arity, Infos, _NexTerm, In ),
	close( In ).

bio_db_pl_info( DbFin, Pname, Arity, Infos, NexTerm, In ) :-
	AbsOpts = [solutions(first),access(exist),file_type(prolog)],
	absolute_file_name( DbFin, DbF, AbsOpts ),
	open( DbF, read, In ),
	read( In, Term ),
	functor( Term, PnameTerm, _PArity ),
	( atom_concat(Pname,'_info',PnameTerm) ->
		true
		;
		Pname = PnameTerm,
		atom_concat( Pname, '_info', Iname )
	),
	bio_db_pl_info( Term, Iname, In, Infos, NexTerm ),
	functor( NexTerm, Pname, Arity ).

bio_db_pl_info( Term, Iname, In, Infos, NexTerm ) :-
	functor( Term, Iname, 2 ),
	!,
	Infos = [Term|Tnfos],
	read( In, Next ),
	bio_db_pl_info( Next, Iname, In, Tnfos, NexTerm ).
bio_db_pl_info( Term, _Iname, _In, [], Term ).
