
:- lib(os).

:- ensure_loaded( bio_db_build_aliases ).  % sets up user's build directory path

:- debug( bio_db_install ).

/** bio_db_install( Db ).

	Install a database's (Db) prolog interface files from (today's) workspace to the swi.lib.

@version 0.1 2016/9/8
*/
bio_db_install( Db ) :-
	absolute_file_name( pack(bio_db_repo), BdRepo ),
	bio_db_install( Db, BdRepo ).

bio_db_install( Db, Repo ) :-
	AbsOpts = [mode(exist),file_type(directory),solutions(first)],
	debug( bio_db_install, 'Abs: ~w', absolute_file_name( bio_db_build_downloads(Db), DbDnDir, AbsOpts ) ),
	absolute_file_name( bio_db_build_downloads(Db), DbDnDir, AbsOpts ),
	debug( bio_db_install, 'DB download dir: ~p', DbDnDir ),

	% absolute_file_name( pack(bio_db_repo), BdRepo ),
	os_path( Repo, data, Dinst ),
	maplist( bio_install_type(DbDnDir,Dinst,Db), [maps,graphs] ).

bio_install_type( DbDnDir, Dinst, Db, Type ) :-
	os_path( DbDnDir, Type, DbTypeDir ),
	exists_directory( DbTypeDir ),
	!,
	os_path( Dinst, Type, DinsType ),
	os_path( DinsType, Db, DinsTypeDb ),
	debug( bio_db_install, 'Installing type: ~w, for DB: ~w, at: ~w', [Type,Db,DinsTypeDb] ),
	( exists_directory(DinsTypeDb) -> 
		debug( bio_db_install, 'Obliterating directory: ~p', DinsTypeDb ),
		delete_directory_and_contents( DinsTypeDb )
		;
		true
	),
	copy_directory( DbTypeDir, DinsTypeDb ).
bio_install_type( _DbDnDir, _Dinst, Db, Type ) :-
	debug( bio_db_install, 'Database: ~w, didnot produce bases of type: ~w', [Db,Type] ).
