
:- requires( get_date_time/1 ).  

%% csv_to_pl( Stem ).
% 
%  Convert Stem.csv or Stem (with .csv extension) to a terms file RealStem.pl.
%
% @author nicos angelopoulos
% @version  0.2 2016/20
%
csv_to_pl( InStem ) :-
     file_name_extension( InStem, csv, CsvF ),
     file_name_extension( Stem, csv, CsvF ),
     file_name_extension( Stem, pl, PlF ),
	write( output_on_file(PlF) ), nl,
	% 16.06.20: changed default functor from row() to basename of stem
	file_base_name( Stem, Base ),
     csv_to_pl( CsvF, PlF, Base, [functor(Base)] ).

csv_to_pl( CsvF, PlF, _Base, Opts ) :-
     csv_read_file( CsvF, Csv, Opts ),
     open( PlF, write, Out ),
	write( Out, '% ' ), 
	get_date_time( Date ),
	Date = date(Y,M,D,Hr,Mn,_Sc,_Off,Tzone,_DST),
	write( Out, Y/M/D ), write( Out, ' @ ' ), write( Out, Hr:Mn ), 
	write( Out, ' (' ), write( Out, Tzone ), write( Out, ')' ), nl( Out ),
	% atomic_list_concat( [Base,date], '_', Bate ),
	% Bterm =.. [Bate,Date],
     % maplist( portray_clause(Out), [Bterm|Csv] ),
     maplist( portray_clause(Out), Csv ),
     close( Out ).
