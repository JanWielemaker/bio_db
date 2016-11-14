kv_decompose_vs( [], [] ).
kv_decompose_vs( [_K-V|T], [V|Tv] ) :-
	kv_decompose_vs( T, Tv ).
