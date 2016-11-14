%% portray_clauses( +List, +Opts ).
%
% Opts : 
%  * mode/1 (append,or *write*), 
%  * stream/1
%  * file/1 overwrites stream/1.
%

:- ensure_loaded( library(by_unix) ).
:- ensure_loaded( library(options) ).  % en_list/2.

% :- requires( memberchk/2 ).
% :- requires( en_list/2 ).

portray_clauses( List, Opt ) :-
     en_list( Opt, Opts ),
     ( memberchk(file(FilePrv),Opts) ->
          ( memberchk(mode(Mode),Opts) -> true; Mode = write ),
		by_unix_term_to_serial( FilePrv, File ),
          open( File, Mode, Out )
          ;
          ( memberchk(stream(Out),Opts) -> true; Out= user_output )
     ),
     portray_clauses_onto( List, Out ),
     ( var(File) -> true; close( Out ) ).

portray_clauses_onto( [], _ ).
portray_clauses_onto( [H|T], Out ) :-
	portray_clause( Out, H ),
	portray_clauses_onto( T, Out ).
