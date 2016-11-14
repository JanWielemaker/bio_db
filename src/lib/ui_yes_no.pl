/* ui_yes_no( +Mess, +Args, +Def, -Reply ).

Mess and Args are message and args that can used as 2,3 arguments
of debug/3, Def is y or n and Reply is true or false.

@author nicos angelopoulos
@version  0.2 2016/9/6, added auto flag
@version  0.1 2015/4/28
@see pack(bio_db)
@tbd don't use internals, use print_message instead
*/

ui_yes_no( MessIn, Args, Def, Reply ) :-
	ui_yes_no_parenthesis( Def, YesNo ),
	atomic_list_concat( [MessIn,YesNo,'? '], ' ', Mess ),
	phrase('$messages':translate_message(debug(Mess,Args)), Lines),
	print_message_lines(current_output, kind(informational), Lines),
	ui_yes_no_wait( Def, Reply ).

ui_yes_no_wait( Def, Reply ) :-
	current_prolog_flag( bio_db_ok, true ),
	!,
	atom_codes( Def, [Code|_] ),
	ui_yes_no_reply( Def, Code, Reply ).
ui_yes_no_wait( Def, Reply ) :-
	get_single_char( Char ),
	ui_yes_no_reply( Def, Char, Reply ).
	
ui_yes_no_reply( y, Char, Reply ) :-
	ui_yes_no_reply_no( Char ),
	!,
	Reply = false.
ui_yes_no_reply( y, _Char, true ) :- !.
ui_yes_no_reply( n, Char, Reply ) :-
	ui_yes_no_reply_yes( Char ),
	!,
	Reply = true.
ui_yes_no_reply( n, _Char, false ).

ui_yes_no_reply_yes( 0'y ).
ui_yes_no_reply_yes( 0'Y ).

ui_yes_no_reply_no( 0'n ).
ui_yes_no_reply_no( 0'N ).

ui_yes_no_parenthesis( y, '(Y/n)' ).
ui_yes_no_parenthesis( n, '(y/N)' ).
