% :- requires( replace_non_alphanums/3 ).
:- requires( at_con/3 ).

%% map_predicate_name( +Cnm1, +Cnm2, -Pname, +Opts ).
%
% Construct a predicate name, first by looking into Opts for predicate/1
% or by downcasing both, and concatenating Cnm2 to Cnm1.
%
%==
% ?- map_predicate_name( prosqlite, 'GeneID', Pname, [] ).
% Pname = prosqlite_geneid.
% 
% ?- map_predicate_name( prosqlite, 'GeneID', Pname, [predicate(what)] ).
% Pname = what.
% 
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/7/2
%
map_predicate_name( _Cnm1, _Cnm2, Pname, Opts ) :-
	memberchk( predicate(Pname), Opts ),
	!.
map_predicate_name( Cnm1, Cnm2, Pname, Opts ) :-
	map_predicate_atom_component( Cnm1, Comp1 ),
	map_predicate_atom_component( Cnm2, Comp2 ),
	% atomic_list_concat( [Comp1,Comp2], '_', Pname ).
	% use at_con/3 as it ignores '' prefixes
	options( prefix(Prefix), Opts ),
	map_predicate_map_prefix( MapPfx, Opts ),
	at_con( [MapPfx,Prefix,Comp1,Comp2], '_', Pname ).

map_predicate_map_prefix( map, Opts ) :-
	options( map_prefix(true), Opts ),
	!.
map_predicate_map_prefix( '', _Opts ).

map_predicate_atom_component( Atom, Comp ) :-
	downcase_atom( Atom, Comp ).
	% replace_non_alphanums( Down, 0'_, CompCs ),
	% atom_codes( Comp, CompCs ).

%% map_predicate_name_stem( Pname, Stem, Opts ).
%
% Stem is either the argument to stem/1 appearing in Opts or Pname.
% 
%==
% ?- map_predicate_name( pname, Stem, [abc(d),stem(file_stem)] ).
% Stem = file_stem.
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/7/2
%
map_predicate_name_stem( _Pname, Stem, Opts ) :-
	memberchk( stem(Stem), Opts ),
	!.
map_predicate_name_stem( Pname, Pname, _Opts ).
