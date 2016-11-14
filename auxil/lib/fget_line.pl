

/** fget_line( Stream, Line ).
	
	Get Line as a codes-list from Stream.
     last change: 2007/01/31

     Returns eof at end of file. 


	@tbd change eof to std end of file atom

*/
fget_line( Stream, Cs ) :-
	get_code( Stream, C ),
     fget_line( C, Stream, Cs ).

fget_line( -1, _Stream, Cs ) :- !, Cs = eof.
fget_line( 0'\n, _Stream, Cs ) :- !, Cs = [].
fget_line( C, Stream, [C|Cs] ) :-
	get_code( Stream, NxC ),
	fget_line( NxC, Stream, Cs ).
