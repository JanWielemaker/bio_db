%% de_semi( +Pfx, +AccIn, -Acc ).
% 
% Remove Pfx: from AccIn to get Acc. 
% This is useful for string biodatabase prefixes from ID fields,
% such HGNC:1234
%
% Note that 1234 will be a number as per atomic_list_concat/3 split up.
% This might be a non stable feature.
%
%==
% de_semi( 'HGNC', 'HGNC:1234', Acc ).
% Acc = 1234.
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/7/2
%
de_semi( Pfx, AccPrv, Acc ) :-
	atomic_list_concat( [Pfx,AccAtm], ':', AccPrv ), 
	atom_number( AccAtm, Acc ),
	!.
de_semi( Pfx, AccPrv, _Acc ) :-
	throw( de_semi_disaster(Pfx,AccPrv) ).
	% abort.
