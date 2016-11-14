
go_term_symbols_defaults( Defs ) :-
	Defs = [ descent(true), 
	         as_child_includes(true),
		    as_child_consists_of(true),
		    as_child_regulates(false),
		    as_child_positively_regulates(false),
		    as_child_negatively_regulates(false)
		  ].

go_term_symbols( GO, Symbs, Args ) :-
	options_append( go_term_symbols, Args, Opts ),
	options( descent(Desc), Opts ),
	go_symbs_descent_term( Desc, GoT, Child, Term, Opts ),
	go_symbs( [GO], GoT, Child, Term, [], [], Symbs ).
              
/** go_term_symbols( +GoT, -Symbols, -Opts ).

Gets the symbols belonging to a GO term. Descents to GO child relations, 
which by default are includes (reverse of is_a) and consists_of (reverse of part_of)
to pick up Symbols recursively.

==
Opts 
  * descent(Desc=true)
    whether to collect symbols from descendant GO terms
  * as_child_includes(Inc=true)
    collect from edge_gont_include/2
  * as_child_consists_of(Cns=true)
    collect from edge_gont_consists_of/2
  * as_child_regulates(Reg=false)
    collect from edge_gont_regulates/2
  * as_child_negatively_regulates(Reg=false)
    collect from edge_gont_negatively_regulates/2
  * as_child_positively_regulates(Reg=false)
    collect from edge_gont_positively_regulates/2
  * debug(Dbg=false)
    see options_append/3

Listens to debug(go_term_symbols).

go_term_symbols( 'GO:0000375', Symbs ).

Symbs = [ALYREF,AQR,ARC,BCAS2,BUD13,BUD31,C7orf55-LUC7L2,CACTIN,CCAR1,CD2BP2,CDC40,CDC5L,CDK13,CELF1,CELF2,CELF3,CELF4,CLNS1A,CLP1,CPSF1,CPSF2,CPSF3,CPSF7,CRNKL1,CSTF1,CSTF2,CSTF3,CTNNBL1,CWC15,CWC22,CWC27,DBR1,DCPS,DDX1,DDX17,DDX20,DDX23,DDX39A,DDX39B,DDX41,DDX46,DDX5,DGCR14,DHX15,DHX16,DHX32,DHX35,DHX38,DHX8,DHX9,DNAJC8,DQX1,EFTUD2,EIF4A3,FRG1,FUS,GCFC2,GEMIN2,GEMIN4,GEMIN5,GEMIN6,GEMIN7,GEMIN8,GPATCH1,GTF2F1,GTF2F2,HNRNPA0,HNRNPA1,HNRNPA2B1,HNRNPA3,HNRNPC,HNRNPD,HNRNPF,HNRNPH1,HNRNPH2,HNRNPH3,HNRNPK,HNRNPL,HNRNPM,HNRNPR,HNRNPU,HNRNPUL1,HSPA8,ISY1,KHSRP,LSM1,LSM2,LSM3,LSM6,LSM7,LSM8,LUC7L,LUC7L2,LUC7L3,MAGOH,MBNL1,METTL14,METTL3,MPHOSPH10,NCBP1,NCBP2,NCBP2L,NHP2L1,NOL3,NOVA1,NUDT21,PABPC1,PABPN1,PAPOLA,PAPOLB,PCBP1,PCBP2,PCF11,PHAX,PHF5A,PLRG1,PNN,POLR2A,POLR2B,POLR2C,POLR2D,POLR2E,POLR2F,POLR2G,POLR2H,POLR2I,POLR2J,POLR2K,POLR2L,PPIE,PPIH,PPIL1,PPIL3,PPWD1,PQBP1,PRMT5,PRMT7,PRPF19,PRPF3,PRPF31,PRPF4,PRPF4B,PRPF6,PRPF8,PSIP1,PTBP1,PTBP2,RALY,RBM17,RBM22,RBM5,RBM8A,RBMX,RBMXP1,RNPS1,RSRC1,SAP130,SART1,SART3,SCAF11,SETX,SF1,SF3A1,SF3A2,SF3A3,SF3B1,SF3B2,SF3B3,SF3B4,SF3B5,SF3B6,SFPQ,SFSWAP,SKIV2L2,SLU7,SMC1A,SMN1,SMN2,SMNDC1,SNRNP200,SNRNP40,SNRNP70,SNRPA,SNRPA1,SNRPB,SNRPB2,SNRPC,SNRPD1,SNRPD2,SNRPD3,SNRPE,SNRPF,SNRPG,SNRPGP15,SNUPN,SNW1,SRPK2,SRRM1,SRRM2,SRSF1,SRSF10,SRSF11,SRSF12,SRSF2,SRSF3,SRSF4,SRSF5,SRSF6,SRSF7,SRSF9,STRAP,SUGP1,SYF2,SYNCRIP,TDRD12,TFIP11,TGS1,TRA2A,TRA2B,TXNL4A,TXNL4B,U2AF1,U2AF2,UBL5,UPF3B,USP39,USP4,USP49,WBP4,WDR77,WDR83,XAB2,YBX1,YTHDC1,ZCCHC8,ZRSR2]
==

@author nicos angelopoulos
@version  0.1 2015/7/26

also change doc in bio_db.pl
*/
go_symbs( [], _GoT, _Ch, _Term, _, Symbs, Symbs ).
go_symbs( [GO|GOs], GoT, ChGo, FTerm, GoSeen, Seen, Symbs ) :-
	ord_add_element( GoSeen, GO, NxGoSeen ),
	findall( GoSymb, map_gont_gont_symb(GO,GoSymb), GoSymbs ),
	sort( GoSymbs, OSymbs ),
	debug( go_term_symbols, '~w, Symbols: ~w', [GO,OSymbs] ),
	ord_union( OSymbs, Seen, NxSeen ),
	findall( ChGo, (GoT=GO,call(FTerm)), ChGos ),
	sort( ChGos, ChGosOrd ),
	debug( go_term_symbols, '~w, Children: -~w', [GO,ChGosOrd] ),
	ord_subtract( ChGosOrd, GoSeen, ChGosAdd ),
	ord_union( GOs, ChGosAdd, NxGos ),
	go_symbs( NxGos, GoT, ChGo, FTerm, NxGoSeen, NxSeen, Symbs ).

go_symbs_descent_term( false, _, false, false, _ ).
go_symbs_descent_term( true, GoT, Child, Term, Opts ) :-
	Rships = [ includes, consists_of, regulates,
	           positively_regulates, negatively_regulates
			 ],
	findall( Pname, (
				   member(Rship,Rships),
				   atom_concat( as_child_, Rship, Oname ),
				   Opt =.. [Oname,true],
				   options(Opt,Opts),
				   atom_concat(edge_gont_,Rship,Pname)
	                ), 
				      Pnames ),
	go_symbs_descent_disjunction( Pnames, GoT, Child, Term ).

go_symbs_descent_disjunction( [], _, false, false ).
go_symbs_descent_disjunction( [H|T], Term, Child, Goal ):-
	G =.. [H,Term,Child],
	go_symbs_descent_disjunction_1( T, Term, Child, G, Goal ).


go_symbs_descent_disjunction_1( [], _Term, _Child, Goal,  Goal ).
go_symbs_descent_disjunction_1( [H|T], Term, Child, Left, Goal ) :-
	G =.. [H,Term,Child],
	go_symbs_descent_disjunction_1( T, Term, Child, (Left;G), Goal ).
