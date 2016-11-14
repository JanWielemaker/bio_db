% true( A ).
%
%  Always true, deterministically.
%==
% ?- true( x ).
% true.
%==
true(_).

% true( A, B ).
%
%  Always true, deterministically.
%==
% ?- true( x, y ).
% true.
%==
true(_A,_B). % careful, was true(A,A).
