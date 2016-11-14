
:- lib( bio_db ).
:- debug( bio_db ).

berkeley :-
	bio_db_interface( berkeley ),
	map_hgnc_hgnc_symb( 1, Symb ),
	debug( bio_db, 'found: ~w', Symb ),
	edge_string_hs_symb( 'EPHB4', Rel, W ),
	debug( bio_db, 'edge found for: ~w, ~w', [Rel,W] ),
	fail.
