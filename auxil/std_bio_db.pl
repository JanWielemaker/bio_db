:- lib( by_unix ).
:- debug( by_unix ).

% not tested in its entirety, 15.09.15
%
std_bio_db :-
	std( Std ),
	atomic_list_concat( [std,Std], Script ),
	@ pupsh( Script ).

std( maps_hgnc ).
std( maps_ncbi ).
std( maps_unip ).
std( maps_gont ).
std( graphs_string ).
std( graphs_gont ).
std( maps_ense ).
