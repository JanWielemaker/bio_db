io_prefixed_lines_defaults( Defs ) :-
	Defs = [
			starts_key(upper_case_n_codes_and_spaces_m(2,3)),
			starts_continuation(spaces_n(5))
			].

/** io_prefixed_lines( +Lines, -ParsedKVs, +Opts ).

Parse prefixed lines into KV pairs. 

Lines are nested code lists, that look like 

AA   Seom text
     more text for AA
BB   my own text

Which should be parsed to
	['AA'-[`Seom text`,`more text for AA`],'BB'-[`my own text`]]

Opts 
 * starts_key(StK=upper_case_n_codes_and_spaces_m(2,3))
   predicate for identifying keys
 * starts_continuation(StC=spaces_n(5)
   predicate for identifying continuation of a value line

@author nicos angelopoulos
@version  0.1 2015/10/06
@see maps_unip_seqs/0.

*/
io_prefixed_lines( [], [], _Args ) :- !.
io_prefixed_lines( Lines, KVs, Args ) :-
	Self = io_prefixed_lines,
	options_append( Self, Args, Opts ),
	options( [starts_key(KeyG),starts_continuation(CntG)], Opts ),
	Lines = [HLine|_], 
	atom_codes( Hatom, HLine ),
	debug( io_prefixed_lines, 'doing section starting with: ~w', [Hatom] ),
	io_prefixed_lines_goals( Lines, KeyG, CntG, '', [], KVs ).

io_prefixed_lines_goals( [], _KeyG, _CntG, Key, Acc, KVs ) :-
	( Acc == [] -> KVs = []; reverse( Acc, Val ), KVs = [Key-Val] ),
	!.
io_prefixed_lines_goals( [H|T], KeyG, CntG, Key, Acc, KVs ) :-
	io_prefixed_line( H, KeyG, CntG, Key, Acc, NxtKey, Cont, KVs, TKVs ),
	io_prefixed_lines_goals( T, KeyG, CntG, NxtKey, Cont, TKVs ).

io_prefixed_line( Line, KeyG, _CntG, Key, Acc, NxtKey, Cnt, KVs, TKVs ) :-
	call( KeyG, Line, NxtKey, Val ),
	!,
	( (Key=='',Acc==[]) -> 
		KVs = TKVs
		;
		reverse( Acc, Rev ),
		KVs = [Key-Rev|TKVs]
	),
	Cnt = [Val].
io_prefixed_line( Line, _KeyG, CntG, Key, Acc, NxtKey, Cnt, KVs, TKVs ) :-
	call( CntG, Line, Val ),
	!,
	Cnt = [Val|Acc],
	NxtKey = Key, 
	KVs = TKVs.
io_prefixed_line( Line, _KeyG, _CntG, _Key, _Acc, _NxtKey, _Cont, _KVs, _TKvs ) :-
	atom_codes( Atom, Line ),
	throw( cannot_parse(Atom) ).

upper_case_n_codes_and_spaces_m( N, M, Line, Key, ValCs ) :-
	length( KeyCs, N ),
	append( KeyCs, Rem1Cs, Line ),
	maplist( is_upper_case_code, KeyCs ),
	findall( Spc, (between(1,M,_),Spc=0' ), Spcs ),
	append( Spcs, ValCs, Rem1Cs ),
	atom_codes( Key, KeyCs ),
	!.
	
spaces_n( N, Line, ValCs ) :-
	findall( Spc, (between(1,N,_),Spc=0' ), Spcs ),
	append( Spcs, ValCs, Line ),
	!.

is_upper_case_code( Code ) :-
	0'A =< Code, Code =< 0'Z.
