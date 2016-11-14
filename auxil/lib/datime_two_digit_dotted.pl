:- requires( get_date_time/1 ).
:- requires( n_digits_integer_codes/3 ).


datime_two_digit_dotted( Dotted ) :-
	datime_two_digit_dotted( false, Dotted ).
	
datime_two_digit_dotted( IncSec, Dotted ) :-
	get_date_time( Curr ),
	datime_two_digit_dotted( Curr, IncSec, Dotted ).

datime_two_digit_dotted( Datime, IncSec, Dotted ) :-
	date_time_value( year, Datime, Year ),
	date_time_value( month, Datime, Month ),
	date_time_value( day, Datime, Day ),
	date_time_value( hour, Datime, Hour ),
	date_time_value( minute, Datime, Minute ),
	( IncSec == true ->
		date_time_value( second, Datime, Second ),
		Components = [Year,Month,Day,Hour,Minute,Second]
		;
		Components = [Year,Month,Day,Hour,Minute]
	),
	maplist( n_digits_integer_codes(2), Components, YMD ),
	maplist( atom_codes, YMDatoms, YMD ), 
	atomic_list_concat( YMDatoms, '.', Dotted ).
