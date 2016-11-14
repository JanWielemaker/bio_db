%% get_date_time( -CurrDatime ).
%
% Just a wrapper to SWI's =|get_time(Stamp), stamp_date_time(Dtime).|=
% CurrDatime should be a date_time/1 term.
% SWI specific. Check YAP.
%
% @author nicos angelopoulos
% @version  0.1 2014/03/31 (some time well before)
%
get_date_time( Datime ) :-
     get_time( Stamp ),
     stamp_date_time( Stamp, Datime, local ).
