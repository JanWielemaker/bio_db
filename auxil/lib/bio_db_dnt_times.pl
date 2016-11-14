
:- lib(os).

/** bio_db_dnt_times( +File, -Start, -End ).

	Get the datetime terms from File or File.dnt .

*/
bio_db_dnt_times( File, Start, End ) :-
	os_ext( dnt, File, DntF ),
	open( DntF, read, In ),
	read( In, Start ),
	read( In, End ),
	close( In ).
