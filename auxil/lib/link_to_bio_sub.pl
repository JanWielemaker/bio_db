
% :- requires( kinase_search_paths/0 ). % file_search_path( maps ).
% fimxe: following is here becuase this file is included in lib(bio_db).
:- use_module( library(os) ).
:- requires( message_report/3 ).

%% link_to_bio_sub( +Sub, +File ).
%
% Create a symbolic link from File to bio_Type(Sub(File)) if one does not exist.
% Deletes with a warning an existing link at that location if one exists pointing
% to a different location and an error if a regular file exists.
%
% Listens to debug( link_to_map_sub ).
%
%==
% ?- expand_file_name( '$local/../work/2014/hgnc', [Local] ), cd( Local ).
% ?- link_to_map_sub( hgnc, hgnc_approved_symbol ).
%==
% @author nicos angelopoulos
% @version  0.1 2014/7/2
%
link_to_map_sub( Sub, File ) :-
	link_to_bio_sub( Sub, maps, File ).

link_to_bio_sub( Sub, Type, File ) :-
	% exists_file( File ),
	absolute_file_name( File, AbsFile, [access(exist)] ),
	% file_search_path( maps, Pfx ),
	% directory_file_path( Pfx, Sub, Path ),
	% atomic_list_concat( [bio,Type], '_', Alias ),
	absolute_file_name( bio_db_build_data(Type), Dir ),

	directory_file_path( Dir, Sub, ToDir ),
	% ToDirTerm =.. [Alias,Sub],
	% absolute_file_name( ToDirTerm, ToDir ),
	file_base_name( AbsFile, Base ),
	os_make_path( ToDir, debug(true) ),
	directory_file_path( ToDir, Base, Dest ),
	link_to_map_sub_read( Dest, AbsFile ).

link_to_map_sub_read( Dest, AbsFile ) :-
	read_link( Dest, _Link, Target ),
	!,
	link_to_map_sub_target( Target, AbsFile, Dest ).
link_to_map_sub_read( Dest, AbsFile ) :-
	exists_file( Dest ),
	!,
	Mess = 'Destination:~w already exists & is a file. Refusing to connect to:~w', 
	message_report( Mess, [Dest,AbsFile], error ),
	fail.
link_to_map_sub_read( Dest, AbsFile ) :-
	debug( link_to_map_sub, 'Symbolic linking, ~p, to ~p', [AbsFile,Dest] ),
	link_file( AbsFile, Dest, symbolic ).

link_to_map_sub_target( Target, Target, _ ) :- !.
link_to_map_sub_target( Target, AbsF, Dest ) :- !,
	Mess = 'Destination:~w was pointing to:~w, repointing to:~w', 
	message_report( Mess, [Dest,Target,AbsF], warning ),
	delete_file( Dest ),
	link_file( AbsF, Dest, symbolic ).
