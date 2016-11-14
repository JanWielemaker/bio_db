:- requires( break_list_on/4 ).

%% has_extension( +File ).
%
%  True if File has a non empty extension.
%  As this version is based on Swi/Yap's file_name_extension/3, 
%  it cannot distinguish between files that have no extension (ie not '.' in 
%  their filename and files that end in dots.

has_extension( File ) :-
	file_name_extension( _Stem, Ext, File ),
	Ext \== ''.

%% has_extension( +File, ?Ext ).
%
%  True if Ext is File's extension. (No dot included.).
%
has_extension( File, Ext ) :-
     file_name_extension( _, Ext, File ).

%% has_extension( +File, ?Ext, -FileWith ).
%
%  True if Ext is File's extension o  FileWith is File with extension Ext. (No dot included.).
%
has_extension( File, Ext, With ) :-
     file_name_extension( _, Ext, File ),
	!,
	With = File.
has_extension( File, Ext, With ) :-
     file_name_extension( File, Ext, With ).
