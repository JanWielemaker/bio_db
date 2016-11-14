
:- lib( os ).
:- lib( by_unix ).

:- ensure_loaded( bio_db_install ).

:- debug( new_data ).
:- debug( bio_db_install ).

bio_db_site( hgnc ).
bio_db_site( gont ).
bio_db_site( ncbi ).
bio_db_site( ense ).
bio_db_site( unip ).
bio_db_site( string ).

/** bio_db_new_data.

Create a complete new data directory for bio_db_repo.

*/

:- bio_db_build_aliases( date_stem('16.09.10') ).

os_pack_path( Pack, Path ) :-
	current_prolog_flag( executable, Exec ),
	os_path( Arch, _, Exec ),
	os_path( Bin,  _, Arch ),
	os_path( Base,  _, Bin ),
	os_path( Base, pack, PackD ),
	os_path( PackD, Pack, Path ).

bio_db_pack :-
	expand_file_name( '$HOME/pl/packs/src/bio_db_repo/data', [PackData] ),
	delete_directory_and_contents( PackData ),
	debug( new_data, 'Pack dir at: ~w', PackData ),
	bio_db_copy_data( PackData ).

bio_db_copy_data( Repo ) :-
	os_path( DataD, data, Repo ),
	bio_db_new_data_tmp_repo( Repo ),
	@ mkdir( Repo ),
	os_path( Repo, maps, Maps ),
	@ mkdir( Maps ),
	os_path( Repo, graphs, Graphs ),
	@ mkdir( Graphs ),
	bio_db_sites( DataD ).

bio_db_new_data :-
	% bio_db_build_aliases( date_stem('16.09.10') ),
	% bio_db_build_aliases,
	% os_pack_path( bio_db_repo, Repo ),
	expand_file_name( '$HOME/web/sware/packs/bio_db_repo/data', [Repo] ),
	bio_db_copy_data( Repo ),
	@ pupsh( zip, 'remove=true', Repo ),
	@ mkvis( Repo ),
	true.

bio_db_sites( Repo ) :-
	bio_db_site( Site ),
	debug( new_data, 'Doing site: ~w', Site ),
	bio_db_install( Site, Repo ),
	debug( new_data, 'Succeeded on site: ~w', Site ),
	fail.
bio_db_sites( _ ).

bio_db_new_data_tmp_repo( Repo ) :-
	exists_directory( Repo ),
	!,
	@ mv( Repo, '/tmp/' ),
	@ ls( '/tmp' ),
	debug( new_data, 'Moved repo to /tmp', [] ).
bio_db_new_data_tmp_repo( _Repo ) :-
	debug( new_data, 'Repo does not exist', [] ).
