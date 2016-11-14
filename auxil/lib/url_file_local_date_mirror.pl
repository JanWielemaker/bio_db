
:- use_module( library(os) ).
:- use_module( library(debug_call) ).
:- use_module( library(by_unix) ).
% :- use_module( library(options) ).
% :- requires( debug_chain/2 ).
:- lib( options ).

:- requires( url_file/2 ).
% :- requires( repoint_link/2 ).
:- requires( date_two_digit_dotted/1 ).
:- requires( datime_two_digit_dotted/1 ).
:- requires( get_datetime/1 ).

url_file_local_date_mirror_defaults( [ date(postfix),file(_), link_stem(true),
                                       replace_todays(false), interface(prolog),
							    stamp(date),ext(_),record_time(true)
                                     ] ).

%% url_file_local_date_mirror( +Url, +LocalD ).
%% url_file_local_date_mirror( +Url, +LocalD, +Opts ).
% 
% Keep a local copy of Url to directory LocalD as dated file LocF. 
% The local version is placed in Stem-Date.Ext with a symbolic link (redirected)
% from Stem.Ext to the downloaded version. The idea is that Stem.Ext always points to the latest version.
%
%  Predicate listens to debug( url_local ).
% 
%  Opts, a term or list of the following
%  * date(postfix)    or prefix if date stamp is prefered before the stem (then LocF = Date-Basename)
%
%  * ext(Ext)
%    insist Ext is the extension of the file. that allows you to strip tar.gz as an extension, but make sure
%    your extension is the correct one, else the whole thing will fail
%
%  * file(LocF)    if free variable, returns the local file name (basename only). if ground it is taken as the local output file.
%  * interface(Iface=prolog)  
%    alternatively you can use wget (works for anonymous ftps
%  * link_stem(true)  or false if LocF is to be linked to Basename.
%  * record_time(true)
%    creates   <Stem>.dnt  with datime(Y,M,D,H,M,S) term
%  * replace_todays(false)  or false if you want to overide today's earlier download
%  * stamp(Stamp=date)
%    or datetime if you want to include time (up to and including minutes)
%
%==
% ?- debug( url_local ).
% ?- assert( ncbi('$local/../work/db/ncbi') ).
% ?- ncbi(Ncbi), url_file_local_date_mirror( 'ftp://ftp.ncbi.nih.gov/gene/DATA/gene2ensembl.gz', Ncbi ).
% % Using local directory: '/usr/local/users/nicos/local/../work/2014/ncbi'
% % Downloading URL: 'ftp://ftp.ncbi.nih.gov/gene/DATA/gene2ensembl.gz', onto file: '/usr/local/users/nicos/local/../work/2014/ncbi/gene2ensembl-14.07.23.gz'
% % Downloaded url: 'ftp://ftp.ncbi.nih.gov/gene/DATA/gene2ensembl.gz', to local: '/usr/local/users/nicos/local/../work/2014/ncbi/gene2ensembl-14.07.23.gz'
% % Warning, repointing link does not exist: '/usr/local/users/nicos/local/../work/2014/ncbi/gene2ensembl.gz'
% % Linked to: '/usr/local/users/nicos/local/../work/2014/ncbi/gene2ensembl-14.07.23.gz'
% ?- ncbi(Ncbi), ls( Ncbi ).
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/7/23
% @tbd  option: delete_older: false,true,affirm
% @tbd  option: report if the same as last
% @tbd  option: update only_if... 
% @tbd  rel(Rel) option to get the relative location of the URL
%
url_file_local_date_mirror( Url, LocalD ) :-
	url_file_local_date_mirror( Url, LocalD, [] ).

url_file_local_date_mirror( Url, LocalD, Args ) :-
	options:options_append( url_file_local_date_mirror, Args, Opts ),
	Self = url_local,
	memberchk( file(LocB), Opts ),
	file_base_name( Url, RemB ),
	options( ext(Ext), Opts ),
	url_file_local_date_mirror_local_file_name( LocB, Opts, RemB, Self, Ext ),
	expand_file_name( LocalD, [LocD|_] ),
	debug( url_local, 'Using local directory: ~p', LocD ), % pacify debug/3
	directory_file_path( LocD, LocB, LocP ),
	os_ext( dnt, LocP, LocDt ),
	options( replace_todays(Repl), Opts ),
	options( interface(Iface), Opts ),
	url_file_replace( Repl, Url, LocD, LocP, LocDt, Iface, RemB, Self ).


url_file_replace( false, Url, _LocD, LocP, _LocDt, _Iface, _RemB, _Self ) :-
	exists_file( LocP ),
	!,
	% fixme: non debug
	debug( _, 'File with today\'s date exists: ~p, so skipping download of:~p.', [LocP,Url] ).
url_file_replace( _, Url, LocD, LocP, LocDt, Iface, RemB, Self ) :-
	debug_chain( Self, url_file, UfPrior ),
  
	get_datetime( BefStamp ),
  	url_interface_file( Iface, Url, LocP ),
	get_datetime( AftStamp ),
	debug_set( UfPrior, url_file ),
	debug( Self, 'Downloaded url: ~p, to local: ~p', [Url,LocP] ),
	open( LocDt, write, DtOut ),
	portray_clause( DtOut, BefStamp ),
	portray_clause( DtOut, AftStamp ),
	close( DtOut ),
	directory_file_path( LocD, RemB, LnkP ),
	debug_chain( Self, os_repoint, RlPrior ),
	os_ext( dnt, LnkP, LnkPDnt ),
	os_repoint( LnkPDnt, LocDt ),
	os_repoint( LnkP, LocP ),
	% repoint_link( LnkP, LocP ),
	debug_set( RlPrior, os_repoint ).

url_interface_file( prolog, Url, LocP ) :-
	url_file( Url, LocP ).
url_interface_file( wget, Url, LocP ) :-
	@ wget( Url, '-O', LocP ).

url_file_local_date_mirror_local_file_name( LocB, Opts, RemB, Self, Ext ) :-
	var( LocB ),
	!, 
	debug( Self, 'Creating dated local basename.', [] ),
	url_file_date_stamp( Date, Opts ),
	memberchk( date(DatePos), Opts ),
	url_file_local_date_mirror_local_file_name_date( DatePos, RemB, Date, Ext, LocB ).
url_file_local_date_mirror_local_file_name( LocB, _Opts, _RemB, Self, _Ext ) :-
	atom( LocB ),
	debug( Self, 'Using given local basename.', LocB ).

url_file_local_date_mirror_local_file_name_date( prefix, RemB, Date, _Ext, LocB ) :-
	atomic_list_concat( [Date,RemB], '-', LocB ).
url_file_local_date_mirror_local_file_name_date( postfix, RemB, Date, Ext, LocB ) :-
	% atomic_concat( '-', Date, DDate ),
	os_postfix( Date, RemB, LocB, [separator('-'),ext(Ext)] ).

url_file_date_stamp( Date, Opts ) :-
	options( stamp(Stamp), Opts ),
	url_file_stamp_date( Stamp, Date ).

url_file_stamp_date( date, Date ) :-
	date_two_digit_dotted( Date ).
url_file_stamp_date( datetime, Date ) :-
	datime_two_digit_dotted( Date ).
