
:- lib( os ).
:- lib( options ).
:- requires( date_two_digit_dotted/1 ).
:- debug( bio_db_build_aliases ).

bio_db_build_aliases_defaults( date_stem(Dotted) ) :-
	date_two_digit_dotted( Dotted ).
	
:- getenv( 'USER', Uname ),
   atomic_list_concat( ['/usr/local/users/',Uname,'/work/bio_db'], BioRoot ),
   assert( build_aliases_root_stem(BioRoot) ).

%
% keeps the abolute locations for the bio_db build location to a single place.
% to deploy, simply change absolute location(s) of these aliases 
% and the scripts should work fine.
%
bio_db_build_aliases( Args ) :-
	Self = bio_db_build_aliases,
	options_append( Self, Args, Opts ),
	% Dotted = '15.09.14',
	build_aliases_root_stem( Stem ),
	options( date_stem(Dotted), Opts ),
	atomic_list_concat( [Stem,Dotted], '-', Dir ),
	debug( bio_db_build_aliases, 'Building at: ~p', Dir ),
	os_make_path( Dir ),
	( file_search_path(bio_db_build,_) ->
		% write warning
		true
		;
		assert( file_search_path(bio_db_build,Dir) ),
		directory_file_path( Dir, dnloads, Dnloads ),
		directory_file_path( Dir, data, Data ),
		assert( file_search_path(bio_db_build_downloads,Dnloads) ),
		os_make_path( Dnloads ),
		assert( file_search_path(bio_db_build_data,Data) ),
		os_make_path( Data )
	).

% include this to loader... most std_... files expect the initialisation here
% :- initialization( bio_db_build_aliases([]) ).
