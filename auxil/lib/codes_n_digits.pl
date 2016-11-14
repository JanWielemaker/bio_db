
:- requires( break_nth/4 ).

/** codes_n_digits( -InCodes, +N, Codes ).

	Codes is of length N and contains either the last N digits of InCodes
	or all codes of Numb left-padded by 0s to make its codes representation
	up to N (see n_digits_integer_codes/3).

==
?- codes_n_digits( '2', 3, Codes ), atom_codes( Atom, Codes ).
Codes = [48, 48, 50],
Atom = '002'.
==

@ author nicos angelopoulos
@ version  0.1 2014/03/17  (curved out of n_digits_integer_codes/3.)

*/

codes_n_digits( ICodes, N, Codes ) :-
	length( ICodes, ILen ),
	Pad is max(N-ILen,0),
	findall( 0'0, between(1,Pad,_), PadL ),
	Del is max(ILen-N,0),
	break_nth( Del, ICodes, _DCodes, KCodes ),
	append( PadL, KCodes, Codes ).
