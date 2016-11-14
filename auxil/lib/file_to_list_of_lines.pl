% Rewrote on 2004/02/10

:- requires( fget_line/2 ).
% :- requires( at_end_of_stream/1 ).

/** file_to_list_of_lines( +File, -Lines ).

	Read a file to a list of character code Lines.

*/

%%%  file+stream to_list_of_lines
file_to_list_of_lines( File, Lines ) :-
     open( File, read, Stream ),
     ( at_end_of_stream( Stream ) ->
          Lines = []
          ;
          fget_line( Stream, Line ),
          stream_to_list_of_lines( Line, Lines, Stream )
     ),
     close( Stream ).

stream_to_list_of_lines( Line, Ls, Stream ) :-
     at_end_of_stream( Stream ),
     Ls = [Line],
     !.

stream_to_list_of_lines( L, [L|Ls], Stream ) :-
     fget_line( Stream, NxL ),
     stream_to_list_of_lines( NxL, Ls, Stream ).
