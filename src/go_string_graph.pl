
:- suggests( wgraph ).
:- lib( bio_db ).   % for now
:- lib( options ).

go_string_graph_defaults( Defs ) :-
	Defs = [ save(false),   % see wgraph_plot/2.
		    stem_type(go_name),  % use null for none
		    plot(false)   % 
            ].

/** go_string_graph( +GoTerm, ?Graph, Opts ).


Not yet on the pack interface...

The Graph can be saved via options to wgraph_plot/2.

Opts are passed to symbols_string_graph/3, go_term_symbols/3 and wgraph_plot/2.

Opts 
  * save(Save=false)
    whether to save the graph (passed to wgraph_plot/2
    
  * plot(Plot=false)

==
?- go_string_graph( 'GO:0016601', G, true ).
?- go_string_graph( 'GO:0016601', G, plot(true) ).

==

@author nicos angelopoulos
@version  0.1 2016/4/12
@tbd make sure underlying options are compatible.

*/

go_string_graph( GoTerm, Graph, Args ) :-
	options_append( go_string_graph, Args, Opts ),
	go_term_symbols( GoTerm, Symbs, Opts ),
	symbols_string_graph( Symbs, Graph, Opts ),
	options( plot(Plot), Opts ),
	go_string_graph_plot( Plot, Graph, GoTerm, Opts ).

go_string_graph_plot( true, Graph, GoTerm, Opts ) :-
	options( stem_type(Stype), Opts ),
	go_string_graph_stem( Stype, GoTerm, Opts, StemOpts ),
	wgraph_plot( Graph, StemOpts ).
go_string_graph_plot( false, _Graph, _GoTerm, _Opts ).

go_string_graph_stem( null, _GoTerm, Opts, Opts ).
go_string_graph_stem( go_name, GoTerm, Opts, [stem(Stem)|Opts] ) :-
	map_gont_gont_gonm( GoTerm, GoName ),
	atom_codes( GoName, GoNameCs ),
	codes_replace( GoNameCs, 0' , 0'_, GoUnderCs ),
	atom_codes( GoUnder, GoUnderCs ),
	atomic_list_concat( [go,GoUnder], '_', Stem ).
	
codes_replace( [], _C, _W, [] ).
codes_replace( [C|T], C, W, [W|R] ) :-
	!,
	codes_replace( T, C, W, R ).
codes_replace( [H|T], C, W, [H|R] ) :-
	codes_replace( T, C, W, R ).
