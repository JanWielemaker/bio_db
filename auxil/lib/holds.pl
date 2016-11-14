/** holds( +Goal, -Holds ).
    
    Goal is called deterministacally with =|Holds = true|= iff Goal
    succeeds. Else, =|Holds = false|=.

    Note that if Holds is instantiated, Goal will still be called.

==
?- holds( X=3, Holds ).
X = 3,
Holds = true.

?- holds( 4=3, Holds ).
Holds = false.

?- holds( member(X,[a,b]), Holds ).
X = a,
Holds = true.


?- holds( member(X,[a,b]), non_true ).
false.

?- holds( (write(x),nl), non_true ).
x
false.
==

@author nicos angelopoulos
@version  0.1 2015/12/9

*/
holds( Goal, Holds ) :-
	call( Goal ),
	!,
	Holds = true.
holds( _Goal, false ).
