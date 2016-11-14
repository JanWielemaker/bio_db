%
:- lib(ordsets).

symbols_string_graph_defaults( Defs ) :-
	Defs = [
				minw(0),
				sort_pairs(true),
				include_orphans(true),
				sort_graph(true)
	].

/** symbols_string_graph( +Symbols, -Graph, +Opts ).

Create the string database Graph between Symbols.

Opts 
  * minw(0)
    minimum weight (0 =< x =< 999) - not checked

  * sort_pairs(Spairs=true)
    set to false to leave order of edges dependant on order of Symbols

  * include_orphans(Orph=true)
    set false to exclude orphans from Graph

  * sort_graph(Sort=true)
    set to false for not sorting the results

==
?- Gont = 'GO:0043552', findall( Symb, map_gont_gont_symb(Gont,Symb), Symbs ),
   symbols_string_graph( Symbs, Graph, [] ),
   length( Graph, Len ).
   
==
@author nicos angelopoulos
@version  0.1 2016/01/18

Update comments also in bio_db.pl

**/
symbols_string_graph( Symbols, Graph, Args ) :-
	options_append( symbols_string_graph, Args, Opts ),
	options( minw(MinW), Opts ),
	options( sort_pairs(Sprs), Opts ),
	findall( SymbA-SymbB:W, ( member(Symb1,Symbols),
						 member(Symb2,Symbols),
						 Symb1 \== Symb2,
						 symbols_string_graph_pair(Sprs,Symb1,Symb2,SymbA,SymbB),
						 sort( [Symb1,Symb2], [SymbA,SymbB] ),
	                          edge_string_hs_symb(SymbA,SymbB,W),
						 MinW =< W
					    ),
					    		Wgraph ),
	options( include_orphans(IncO), Opts ),
	symbols_string_graph_orphans( IncO, Wgraph, Symbols, Ograph ),
	options( sort_graph(Sgra), Opts ),
	symbols_string_graph_sort( Sgra, Ograph, Graph ).

symbols_string_graph_pair( true, Symb1, Symb2, SymbA, SymbB ) :-
	sort( [Symb1,Symb2], [SymbA,SymbB] ).
symbols_string_graph_pair( false, Symb1, Symb2, Symb1, Symb2 ).

symbols_string_graph_sort( true, Ograph, Graph ) :-
	sort( Ograph, Graph ).
symbols_string_graph_sort( false, Graph, Graph ).

symbols_string_graph_orphans( true, Wgraph, Symbols, Ograph ) :-
	string_add_vertices_1( Symbols, Wgraph, Ograph ).
symbols_string_graph_orphans( false, Wgraph, _Symbols, Wgraph ).

string_add_vertices_1( [], New, New ).
string_add_vertices_1( [V|Vs], G, New ) :-
	atomic( V ),
	string_has_vertex( G, V ),
	!,
	string_add_vertices_1( Vs, G, New ).
string_add_vertices_1( [V|Vs], G, New ) :-
	atomic( V ),
	ord_add_element( G, V, G1 ),
	string_add_vertices_1( Vs, G1, New ).

string_has_vertex( [E|_Es], V ) :-
	string_edge_has_vertex( E, V ),
	!.
string_has_vertex( [_E|Es], V ) :-
	string_has_vertex( Es, V ).

string_edge_has_vertex( V, V ).
string_edge_has_vertex( V-_:_, V ).
string_edge_has_vertex( _-V:_, V ).

