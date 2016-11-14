%% message_report( Format, Args, Kind ).
%
%  An Swi shortcut for printing messages.
%  The predicate first phrases onto a list the Format message
%  filled by Args, as it would do for debug( _, Format, Args ), 
%  then prints these lines as of Kind (error,warning,debug(_)).
%
%==
% ?- Mess = 'Destination:~w already pointed to:~w, repointing to:~w',
% |    F1 = 'file1', F2 = file2, F3 = file3,
% |    message_report( Mess, [F1,F2,F3], warning ).
%
% Warning: Destination:file1 already pointed to:file2, repointing to:file3
% 
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/02/28
%
message_report( Format, Args, Kind ) :-
	phrase('$messages':translate_message(debug(Format,Args)), Lines),
	print_message_lines(current_output, kind(Kind), Lines).
