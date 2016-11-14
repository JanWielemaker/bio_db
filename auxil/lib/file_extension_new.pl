/** file_extension_new( +Fname, ?Old, +New, -ResFname ).

	Change the extension of a filename. Old can be generated 
	as well as tested for. (Fails if Old does not match Fname's extension.

*/
file_extension_new( Fname, Old, New, ResF ) :-
	file_name_extension( Stem, Old, Fname ),
	file_name_extension( Stem, New, ResF ).
