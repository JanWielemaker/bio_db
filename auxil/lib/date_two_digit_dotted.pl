
:- requires( get_date_time/1 ).
:- requires( n_digits_integer_codes/3 ).


date_two_digit_dotted( Dotted ) :-
	get_date_time( Curr ),
	date_two_digit_dotted( Curr, Dotted ).

%% date_two_digit_dotted( -Dotted ).
%% date_two_digit_dotted( +Date, -Dotted ).
%	Generate a YY.MM.DD atom from date/n term structures. Implied Date is the current date.
%    Current version assumes 1st, 2nd and 3rd terms of Date are Year, Month and date.
%    So it works with both date/1 and date_time/1.
%
%==
%?- get_date_time( Curr ), date_two_digit_dotted( Curr, Dotted ).
%Curr = date(2013, 5, 22, 17, 21, 12.714296102523804, -7200, 'CEST', true),
%Dotted = '13.05.22'.
%
%?- date_two_digit_dotted( Dotted ).
%Dotted = '13.11.12'.
%==
%
%
% @author nicos angelopoulos
% @version  0.2 2014/3/31  % original date_two_digit_dotted should have  benn date_time_...
%
date_two_digit_dotted( Datime, Dotted ) :-
	% date_time_value( year, Datime, Year ),
	% date_time_value( month, Datime, Month ),
	% date_time_value( day, Datime, Day ),
	arg( 1, Datime, Year ),
	arg( 2, Datime, Month ),
	arg( 3, Datime, Day ),
	maplist( n_digits_integer_codes(2), [Year,Month,Day], YMD ),
	maplist( atom_codes, YMDatoms, YMD ), 
	atomic_list_concat( YMDatoms, '.', Dotted ).
