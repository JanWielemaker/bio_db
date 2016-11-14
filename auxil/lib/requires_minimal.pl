% minimalist's version of requires/1.

requires( Name/Arity ) :-
     % This seems awkaward, but Yap doesnt show
     % built-ins in current_predicate/2.
	functor( Head, Name, Arity ),
	( predicate_property(Head,_) ->
		true
		;
		ensure_loaded( library(Name) )
			
	).

lib( LibIn ) :-
	( LibIn == os -> Lib = os_lib; Lib = LibIn ),
	( catch(use_module(library(Lib)),_,fail)  ->
		true
		;
		write( 'Library ' ), write( pack(Lib) ), write( ' is not installed in your system'), nl,
		write( 'Install with ' ), write( pack_install(Lib) ), nl
	).
