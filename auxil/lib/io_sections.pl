
:- lib( options ).
:- requires( true/2 ).

io_sections_defaults( [separator(`//`),process(=),process_options(false),
                       terminating_separator(true),
				   separator_id(false)] ).

/** io_sections( +File, -Sections, +Opts ).

	Read a file to a list of Sections. 
	In vanilla operation, each section is a list of the codes read-in. 
	Each section is delimited by a marker line. 

Opts 
  * process(Pgoal= (=))
    Goal to process the Sections before storing.

  * process_opts(Popts=false)
    else pass Sopts to processor Pgoal (as last arg)

  * separator_call(SepCall)
    if given it is used to separate sections

  * separator_id(Sid=false)
    if true SepCall is called with an extra argument which is 
    used to create SectionId-Section pairlists of sections 

  * separator(Sep=`\\`)
    section separating line, used if SepCall is not present 
    (back compatibility, this is now define as sep_call(==(Line))
    
  * terminating_separator(Tmn=true)
    section separating line

==
 ?- cd( '/usr/local/users/nicos/work/2015/15.10.05-lmtk3_substrates' ).
 ?- io_sections( 'uniprot_sprot.dat', Sects, process(length) ).

==
@author nicos angelopoulos
@version  0.1 2015/10/05
@version  0.2 2016/02/04
@tbd generalise delimiting to a call

*/
io_sections( File, Sections, Args ) :-
	options_append( io_sections, Args, Opts ),
	( memberchk(separator_call(Call),Opts) ->
		true
		;
		options( separator(Sep), Opts ),
		Call = ==(Sep)
	),
	options( separator_id(Sid), Opts ),
	options( [separator(Sep),process(Proc)], Opts ),
	options( [process_options(Popts)], Opts ),
	open( File, read, Stream ),
	read_line_to_codes( Stream, Line ),
	options( terminating_separator(Tmn), Opts ),
	io_sections_stream( Line, Stream, Call, Popts, Proc, Tmn, Sid, null, [], Sections ),
	close( Stream ).

io_sections_stream( end_of_file, _Stream, _Call, Popts, Proc, Tmn, Sid, Id, Acc, Sections ) :-
	!,
	io_sections_end_acc( Tmn, Sid, Id, Popts, Proc, Acc, Sections ).
io_sections_stream( Line, Stream, Call, Popts, Proc, Tmn, Sid, Id, Acc, Sections ) :-
	% Line == Sep,
	io_section_sep_call( Sid, Call, Line, NxtId ),
	!,
	io_accumulator_section( Acc, Sid, Id, Popts, Proc, Sections, Tections ),
	read_line_to_codes( Stream, New ),
	io_sections_stream( New, Stream, Call, Popts, Proc, Tmn, Sid, NxtId, [], Tections ).
io_sections_stream( Line, Stream, Call, Popts, Proc, Tmn, Sid, Id, Acc, Sections ) :-
	read_line_to_codes( Stream, New ),
	io_sections_stream( New, Stream, Call, Popts, Proc, Tmn, Sid, Id, [Line|Acc], Sections ).

io_section_sep_call( true, Call, Line, Id ) :-
	call( Call, Line, Id ).
io_section_sep_call( false, Call, Line, _Id ) :-
	call( Call, Line ).

% the first clause is to catch empty ones, particularly the first one when we have Sid=true
% maybe a lookahead would be better
io_accumulator_section( [], _Sid, _Id, _Popts, _Proc, Sections, Sections ) :- !.
io_accumulator_section( Acc, Sid, Id, Popts, Proc, Sections, Tections ) :-
	reverse( Acc, Provisional ),
	optioned_call( Popts, Proc, Provisional, Section ),
	io_section_id( Sid, Id, Section, Iection ),
	Sections = [Iection|Tections].

io_section_id( true, Id, Section, Id-Section ).
io_section_id( false, _Id, Section, Section ).

optioned_call( false, Proc, Provisional, Section ) :-
	!,
	call( Proc, Provisional, Section ).
optioned_call( Popts, Proc, Provisional, Section ) :-
	call( Proc, Provisional, Section, Popts ).

io_sections_end_acc( [] ) :- !.
io_sections_end_acc( Acc ) :- 
	% fixme: use pack(pack_errors)
	throw( non_empty_end_accumulator(Acc) ).

io_sections_end_acc( true, _Sid, _Id, _Popts, _Proc, Acc, [] ) :-
	io_sections_end_acc( Acc ).
io_sections_end_acc( false, Sid, Id, Popts, Proc, Acc, Sections ) :-
	io_accumulator_section( Acc, Sid, Id, Popts, Proc, Sections, [] ).
