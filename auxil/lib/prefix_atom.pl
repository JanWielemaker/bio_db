%% prefix_atom( ?Pfx, ?Atom ).
%
% Version suitable for apply calls, such as in include/3.
%
% ==
% ?-  directory_files( '.', All ), 
%     exclude( prefix_atom('.'), All, Adots ).
%     All = ['.claws_cache', '.', '.mh_sequences', '541', .., '.claws_mark'],
%     Adots = ['541'].
%    
% ==
%@author Nicos Angelopoulos
%@version 0.1 2012/05/05.
%
prefix_atom( Pfx, Atom ) :-
     atom_concat( Pfx, _, Atom ).
	
%% prefix_atom( ?Pfx, ?Atom, -Postfix  ).
%
% Pfx is a prefix of Atom with Postfix being the remainder of Atom.
% This is a resuffle of atom_concat/3 arguments, with this version being suitable for apply calls, such as in map_succ_list/3.
%
% ==
% ?-  directory_files( '.', All ), 
%     map_succ_list( prefix_atom('.'), All, DotPsfxs ).
%    
% ==
%@author Nicos Angelopoulos
%@version 0.1 2013/04/17.
%
prefix_atom( Pfx, Atom, Psf ) :-
     atom_concat( Pfx, Psf, Atom ).
