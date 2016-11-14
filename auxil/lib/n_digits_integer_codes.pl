:- requires( codes_n_digits/3 ).

/** n_digits_integer_codes( +N, +Numb, -Codes ).

	Codes is of length N and contains either the last N digits of Numb or
	all digits of Numb left-padded by 0s to make its codes representation up to N.

==
?- n_digits_integer_codes( 2, 120, Codes ), atom_codes( Atom, Codes ).
Codes = [50, 48],
Atom = '20'.

?- n_digits_integer_codes( 2, 2, Codes ), atom_codes( Atom, Codes ).
Codes = [48, 50],
Atom = '02'.

==
*/

n_digits_integer_codes( N, Int, Codes ) :-
	integer( Int ),
	number_codes( Int, ICodes ),
	codes_n_digits( ICodes, N, Codes ).
