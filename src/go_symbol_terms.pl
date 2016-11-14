:- lib( bio_db ). % for now

/** go_symbol_terms( +Symb, -Terms, +Opts ).

Return and possibly report all terms in which Symb is implicated. 
Optionally add the population size for each term

@author  nicos angelopoulos
@version 0.1 2016/4/12

*/

go_symbol_terms( Symb, GOnts, _Opts ) :-
	findall( GO-GOnm:Pop, ( map_gont_gont_symb(GO,Symb),
	                    map_gont_gont_gonm(GO,GOnm),
					findall(Osymb,map_gont_gont_symb(GO,Osymb),Osymbs),
					length(Osymbs,Pop)
			        ),
				         Gos ),
	sort( Gos, GOnts ), 
	length( GOnts, Length ),
	writeln( number_of_ters:Length ),
	maplist( writeln, GOnts ).

