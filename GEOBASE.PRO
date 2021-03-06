/*****************************************************************************

		Copyright (c) 1984 - 2000 Prolog Development Center A/S

 Project:  GEOBASE
 FileName: GEOBASE.PRO
 Purpose: Natural language interface to U.S. geography
 Written by: Eugene Akimov
 Comments:
******************************************************************************/

include "geobase.inc"
include "geobase.con"
include "hlptopic.con"

include "geobase.prg"

CONSTANTS
  idc_update_1  = 100
  idc_update_2  = 101
  idc_update_3  = 102
  idc_update_4  = 103
  idc_update_5  = 104
  idc_update_6  = 105
  idc_update_7  = 106
  idc_update_8  = 107
  idc_update_9  = 108
  idc_update_10 = 109
  idc_update_11 = 110
  idc_update_12 = 111
  
/*--------------------------- Set Flag for Control --------------------------*/
DOMAINS
  Flag_List=WSFLAGS*
  
PREDICATES
  control_State(WINDOW,ILIST,Flag_List)

CLAUSES
  control_State(_Win,[],[]):-!.
  control_State(_Win,[H|T],[H1|T1]):-
	CtrlWin=win_GetCtlHandle(_Win,H),
	win_SetState(CtrlWin,H1),
	control_State(_Win,T,T1).
/*---------- Adds a new item (or several items) to  list control ------------*/
DOMAINS
  L=SLIST*
PREDICATES
  add_List_to_ListBox(WINDOW,ILIST,L)

CLAUSES
  add_List_to_ListBox(_Win,[],[]):-!.
  add_List_to_ListBox(_Win,[H|T],[H1|T1]):-
  	CtrlWin=win_GetCtlHandle(_Win,H),
  	lbox_Add(CtrlWin,-1,H1),
  	add_List_to_ListBox(_Win,T,T1).
  	
/*------------------------------- Load Database -----------------------------*/
DATABASE - db
  not_DB
  open(string)
  
PREDICATES
  load_DB

CLAUSES
  load_DB:-schema(_,_,_),!.  /* Database already loaded */
  load_DB:-
        existfile("geobase.lan"),existfile("geobase.dba"),!,
	consult("geobase.lan",lang),
	consult("geobase.dba",data),
	assert(open("geobase.lan")),
	!.	
  load_DB:-
	Title="Warning",
	dlg_Note(Title,"Geobase.dba  not  in  default  directory..." ),
	assert(not_DB).

/*----------------------- Output Solution --------------------------*/
PREDICATES
  ch_s(WINDOW,SLIST,string,string)

CLAUSES
  ch_s(_Win,_,"0",_):-
	LBoxWin=win_GetCtlHandle(_Win,idc_lbox_answer),
	lbox_Clear(LBoxWin),
	SBoxWin=win_GetCtlHandle(_Win,idc_edit),
	win_SetText(SBoxWin,""),
	UBoxWin=win_GetCtlHandle(_Win,idc_edit1),
	win_SetText(UBoxWin,""),
	Title="Attention",
	dlg_Note(Title,"  N O      S O L U T I O N S  !  "),
	!.
  ch_s(_Win,Answer,Solutions,Units):-
	LBoxWin=win_GetCtlHandle(_Win,idc_lbox_answer),
	lbox_Clear(LBoxWin),	
	lbox_add(LBoxWin,-1,Answer),
	SBoxWin=win_GetCtlHandle(_Win,idc_edit1),
	win_SetText(SBoxWin,Solutions),
	UBoxWin=win_GetCtlHandle(_Win,idc_edit),
	win_SetText(UBoxWin,Units),
	!.

/* -------------------------- Convert String to List ------------------------*/
DATABASE - listedit
	association(string)
	expression(string)
	all_sen(slist)
	all_strings(slist)

PREDICATES
  list_Query
  list_Query1
  list_Language(string)
  check_expression(WINDOW,string)

CLAUSES
  check_expression(_Win,STR):-expression(STR),!.
  check_expression(_Win,STR):-
	asserta(expression(STR)),
	list_Query,all_sen(X),!,
	lbox_Clear(_Win),
	lbox_add(_Win,-1,X),
	win_SetText(_Win,STR),
	retractall(all_sen(_)),
	asserta(all_sen([])).
	
  list_Query:-
	expression(Y),
	all_sen(A),
	Z=[Y],
	append(A,Z,X),
	retractall(all_sen(_)),
	asserta(all_sen(X)),
	fail.
  list_Query.

  list_Query1:-
	association(Y),
	all_sen(A),
	Z=[Y],
	append(A,Z,X),
	retractall(all_sen(_)),
	asserta(all_sen(X)),
	fail.
  list_Query1.

  list_Language(String):-
	all_strings(A),
	Z=[String],
	append(A,Z,X),
	retractall(all_strings(_)),
	assert(all_strings(X)),
	fail.
  list_Language(_).

/* ----------------------------- Update Language ----------------------------*/
DATABASE - list
  out_String(string)

PREDICATES
  list_to_string(SLIST,string)
  update_synonyms
  update_alternatives
  
CLAUSES
  list_to_String([],S):-retractall(out_String(_)),assert(out_String(S)),!.
  list_to_String([H|T],S):-
	format(Final,"%  %",S,H),
	list_to_String(T,Final).
	
  update_synonyms:-
	synonym(E,S),
	format(String,"% - %",E,S),
	list_Language(String),
	fail.
  update_synonyms.

  update_alternatives:-
	assoc(String,List),
	list_to_String(List,""),
	out_String(A),
	format(OutStr,"%  %",String,A),
	list_Language(OutStr),
	fail.
  update_alternatives.
  
/*----------------------------- Add Query to List ----------------------------*/
PREDICATES
  add_QueryList_to_ListBox

CLAUSES
  add_QueryList_to_ListBox:-
  	expression(String),
  	list_Language(String),
  	fail.
  add_QueryList_to_ListBox.
  
/*------------------ Create and Destroy Dyn. Controls -----------------------*/
DATABASE - lang_db
  synonyms_flag
  alternatives_flag
  ignore_flag

PREDICATES
  destroy_controls(WINDOW,ILIST)
  create_controls(WINDOW,string,string,string,string)
  create_controls1(WINDOW,string,string,string)
  update_controls(WINDOW)
  update_controls1(WINDOW)
  set_String(WINDOW)

CLAUSES
  destroy_controls(_Win,[]):-!.
  destroy_controls(_Win,[H|T]):-
  	CtrlWin=win_GetCtlHandle(_Win,H),
  	win_Destroy(CtrlWin),
  	destroy_controls(_Win,T).
  	
  create_controls(_Win,Title,LBoxTitle,LBoxButtonTitle,EditTitle):-
	ControlList=[
	ctl(wdef(wc_LBox,rct(8,91,122,147),"",u_DlgBase),idc_update_2,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_Sort,wsf_NoIntegralHeight]),
	ctl(wdef(wc_Edit,rct(136,120,249,132),"",u_DlgBase),idc_update_3,[wsf_Group,wsf_AutoHScroll,wsf_AlignLeft]),
	ctl(wdef(wc_LBoxEdit,rct(136,91,249,131),"",u_DlgBase),idc_update_4,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_Sort]),
	ctl(wdef(wc_PushButton,rct(221,135,249,147),"A&dd",u_DlgBase),idc_update_5,[wsf_Group,wsf_TabStop]),
	ctl(wdef(wc_PushButton,rct(45,153,85,165),"D&elete",u_DlgBase),idc_update_6,[wsf_Group,wsf_TabStop,wsf_Disabled]),
	ctl(wdef(wc_PushButton,rct(136,153,184,165),"Ca&ncel",u_DlgBase),idc_update_7,[wsf_Group,wsf_TabStop]),
	ctl(wdef(wc_Text,rct(8,80,122,90),LBoxTitle,u_DlgBase),idc_update_8,[wsf_AlignLeft]),
	ctl(wdef(wc_Text,rct(136,80,236,90),LBoxButtonTitle,u_DlgBase),idc_update_9,[wsf_AlignLeft]),
	ctl(wdef(wc_Text,rct(136,108,249,118),EditTitle,u_DlgBase),idc_update_10,[wsf_AlignLeft]),
	ctl(wdef(wc_GroupBox,rct(2,70,256,170),Title,u_DlgBase),idc_update_1,[wsf_NoClipSiblings])
	],
	win_CreateDynControl(ControlList,_Win).
		
  create_controls1(_Win,Title,LBoxTitle,EditTitle):-
	ControlList=[
	ctl(wdef(wc_LBox,rct(8,91,122,147),"",u_DlgBase),idc_update_2,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_Sort,wsf_NoIntegralHeight]),
	ctl(wdef(wc_Edit,rct(136,120,249,132),"",u_DlgBase),idc_update_3,[wsf_Group,wsf_AutoHScroll,wsf_AlignLeft]),
	ctl(wdef(wc_PushButton,rct(221,135,249,147),"A&dd",u_DlgBase),idc_update_5,[wsf_Group,wsf_TabStop]),
	ctl(wdef(wc_PushButton,rct(45,153,85,165),"D&elete",u_DlgBase),idc_update_6,[wsf_Group,wsf_TabStop,wsf_Disabled]),
	ctl(wdef(wc_PushButton,rct(136,153,184,165),"Ca&ncel",u_DlgBase),idc_update_7,[wsf_Group,wsf_TabStop]),
	ctl(wdef(wc_Text,rct(8,80,122,90),LBoxTitle,u_DlgBase),idc_update_8,[wsf_AlignLeft]),
	ctl(wdef(wc_Text,rct(136,108,249,118),EditTitle,u_DlgBase),idc_update_10,[wsf_AlignLeft]),
	ctl(wdef(wc_GroupBox,rct(2,70,256,170),Title,u_DlgBase),idc_update_1,[wsf_NoClipSiblings])
	],
	win_CreateDynControl(ControlList,_Win).
		
  update_controls(_Win):-ignore_flag,!,
	Constants_List=[idc_update_1,idc_update_2,idc_update_3,idc_update_5,
			idc_update_6,idc_update_7,idc_update_8,idc_update_10],
	destroy_controls(_Win,Constants_List),
	update_controls1(_Win).
  update_controls(_Win):-
	Constants_List=[idc_update_1,idc_update_2,idc_update_3,idc_update_5,idc_update_4,
			idc_update_6,idc_update_7,idc_update_8,idc_update_9,idc_update_10],
	destroy_controls(_Win,Constants_List),
	update_controls1(_Win).

  update_controls1(_Win):-
	ControlList=[
		ctl(wdef(wc_PushButton,rct(142,80,190,92),"&Synonyms",u_DlgBase),idc_synonyms,[wsf_Group,wsf_TabStop]),
		ctl(wdef(wc_PushButton,rct(198,80,248,92),"A&lternatives",u_DlgBase),idc_alternatives,[wsf_Group,wsf_TabStop]),
		ctl(wdef(wc_PushButton,rct(142,95,222,107),"Words to be &Ignored",u_DlgBase),idc_words_to_be_ignored,[wsf_Group,wsf_TabStop]),
		ctl(wdef(wc_CheckBox,rct(142,112,205,122),"&Update language",u_DlgBase),idc_update_language,[wsf_Group,wsf_TabStop,wsf_Auto]),
		ctl(wdef(wc_PushButton,rct(210,150,250,162),"&Ask",u_DlgBase),idc_ask,[wsf_Group,wsf_TabStop,wsf_Default]),
		ctl(wdef(wc_LBoxEdit,rct(7,150,207,198),"",u_DlgBase),idc_query_text,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_Sort]),
		ctl(wdef(wc_PushButton,rct(135,128,205,140),"&Open LAN database",u_DlgBase),idc_open,[wsf_Group,wsf_TabStop,wsf_Auto]),
		ctl(wdef(wc_LBox,rct(8,91,58,131),"",u_DlgBase),idc_lbox_states,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_Sort,wsf_NoIntegralHeight]),
		ctl(wdef(wc_LBox,rct(72,91,122,131),"",u_DlgBase),idc_lbox_cities,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_Sort,wsf_NoIntegralHeight]),
		ctl(wdef(wc_Text,rct(8,80,56,90),"States",u_DlgBase),idct_states,[wsf_AlignLeft]),
		ctl(wdef(wc_Text,rct(73,80,121,90),"Cities",u_DlgBase),idct_cities,[wsf_AlignLeft]),
		ctl(wdef(wc_GroupBox,rct(2,140,256,167),"Query",u_DlgBase),idc_query,[]),
		ctl(wdef(wc_GroupBox,rct(135,70,255,126),"Language",u_DlgBase),idc_language,[]),
		ctl(wdef(wc_GroupBox,rct(2,70,129,138),"States and Cities",u_DlgBase),idc_groupbox,[])
	],
	win_CreateDynControl(ControlList,_Win),
	Window=win_GetCtlHandle(_Win,idc_update_language),
	win_Check(Window,checkbox_on),
	fillbox("states",List_States),  
	fillbox("cities",List_Cities),
	Constants_List1=[idc_lbox_states,idc_lbox_cities],
	List=[List_States,List_Cities],
	add_List_to_ListBox(_Win,Constants_List1,List),
 	EdWin  = win_GetCtlHandle(_Win,idc_query_text),
	lbox_Clear(EdWin),
	add_QueryList_to_ListBox,
	all_strings(X),!,
	lbox_Add(EdWin,-1,X),
	set_String(EdWin).
	
  set_String(_Win):-
	expression(Query),!,
	win_SetText(_Win,Query).
  set_String(_Win):-
	Query="what is the biggest city in california",
	win_SetText(_Win,Query).

DATABASE - str
  token(string)
  rest_str(string)
  final_Str(string)
  
PREDICATES
  del(SLIST)
  out
  out1
  recurs(SLIST,string)
  constr_string(string)

CLAUSES
  del(List):-
	recurs(List,""),
	out.
  
  out:-
	token(Word),
	retractall(final_Str(_)),
	assert(final_Str(Word)),
	retract(token(Word)),!,
	out1.

  out1:-
	token(Word),
	constr_string(Word),
	fail.
  out1.
  
  constr_string(Word):-
	final_Str(X),!,
	concat(X," ",OutStr),
	concat(OutStr,Word,Final),
	retractall(final_Str(_)),
	assert(final_Str(Final)).
	
  recurs([List|_],"-"):-retract(token("-")),!,assert(rest_str(List)).
  recurs([H|T],_):-
	assert(token(H)),
	recurs(T,H).

DATABASE - landb
  landb

PREDICATES
  update_LAN_database(WINDOW)
  check_landb(WINDOW)

CLAUSES
  update_LAN_database(_Win):-landb,!.
  update_LAN_database(_Win):-synonyms_flag,!,
	CtrlWin=win_GetCtlHandle(_Win,idc_update_7),
	GrbWin=win_GetCtlHandle(_Win,idc_update_1),
	win_Destroy(GrbWin),
ifdef os_os2
	win_Move(CtrlWin,rct(435,303,498,327)),
elsedef
	win_Move(CtrlWin,rct(435,306,498,330)),
enddef	
	ControlList=[
	ctl(wdef(wc_PushButton,rct(136,153,170,165),"&Save",u_DlgBase),idc_update_11,[wsf_Group,wsf_TabStop]),
	ctl(wdef(wc_PushButton,rct(176,153,212,165),"Sa&ve as..",u_DlgBase),idc_update_12,[wsf_Group,wsf_TabStop]),
	ctl(wdef(wc_GroupBox,rct(2,70,256,170),"Update synonyms",u_DlgBase),idc_update_1,[wsf_NoClipSiblings])
	],
	win_CreateDynControl(ControlList,_Win),
	assert(landb).
  update_LAN_database(_Win):-alternatives_flag,!,
	CtrlWin=win_GetCtlHandle(_Win,idc_update_7),
	GrbWin=win_GetCtlHandle(_Win,idc_update_1),
	win_Destroy(GrbWin),
ifdef os_os2
	win_Move(CtrlWin,rct(435,303,498,327)),
elsedef
	win_Move(CtrlWin,rct(435,306,498,330)),
enddef
	ControlList=[
	ctl(wdef(wc_PushButton,rct(136,153,170,165),"&Save",u_DlgBase),idc_update_11,[wsf_Group,wsf_TabStop]),
	ctl(wdef(wc_PushButton,rct(176,153,212,165),"Sa&ve as..",u_DlgBase),idc_update_12,[wsf_Group,wsf_TabStop]),
	ctl(wdef(wc_GroupBox,rct(2,70,256,170),"Update alternatives",u_DlgBase),idc_update_1,[wsf_NoClipSiblings])
	],
	win_CreateDynControl(ControlList,_Win),
	assert(landb).
  update_LAN_database(_Win):-
	CtrlWin=win_GetCtlHandle(_Win,idc_update_7),
	GrbWin=win_GetCtlHandle(_Win,idc_update_1),
	win_Destroy(GrbWin),
ifdef os_os2
	win_Move(CtrlWin,rct(435,303,498,327)),
elsedef
	win_Move(CtrlWin,rct(435,306,498,330)),
enddef
	ControlList=[
	ctl(wdef(wc_PushButton,rct(136,153,170,165),"&Save",u_DlgBase),idc_update_11,[wsf_Group,wsf_TabStop]),
	ctl(wdef(wc_PushButton,rct(176,153,212,165),"Sa&ve as..",u_DlgBase),idc_update_12,[wsf_Group,wsf_TabStop]),
	ctl(wdef(wc_GroupBox,rct(2,70,256,170),"Update words to ignore",u_DlgBase),idc_update_1,[wsf_NoClipSiblings])
	],
	win_CreateDynControl(ControlList,_Win),
	assert(landb).
	
  check_landb(_Win):-landb,!,
	Constants_List=[idc_update_11,idc_update_12],
	destroy_controls(_Win,Constants_List),
	retractall(_,landb).
  check_landb(_Win).
	
/*----------------------- Add and Delete in Database -------------------------*/
PREDICATES
  add_to_database(WINDOW)
  delete_from_database(WINDOW)

CLAUSES
  add_to_database(_Win):-synonyms_flag,
  	CtrlWin1=win_GetCtlHandle(_Win,idc_update_4),
	CtrlWin2=win_GetCtlHandle(_Win,idc_update_3),
	String1=win_GetText(CtrlWin1),
	String2=win_GetText(CtrlWin2),
	String2<>"",!,
	upper_lower(String2,String3),
	assert(synonym(String3,String1)),
	update_LAN_database(_Win),
	update_synonyms,
	all_strings(X),!,
	CtrlWin=win_GetCtlHandle(_Win,idc_update_2),
	lbox_Clear(CtrlWin),
	Constants_List1=[idc_update_2],
	List=[X],
	add_List_to_ListBox(_Win,Constants_List1,List).
	
  add_to_database(_Win):-alternatives_flag,
  	CtrlWin1=win_GetCtlHandle(_Win,idc_update_4),
	CtrlWin2=win_GetCtlHandle(_Win,idc_update_3),
	String1=win_GetText(CtrlWin1),
	String2=win_GetText(CtrlWin2),
	String2<>"",!,
	upper_lower(String2,String3),
	scan(String3,List_Items),
	assert(assoc(String1,List_Items)),
	update_LAN_database(_Win),
	update_alternatives,
	all_strings(X),!,
	CtrlWin=win_GetCtlHandle(_Win,idc_update_2),
	lbox_Clear(CtrlWin),
	Constants_List=[idc_update_2],
	List=[X],
	add_List_to_ListBox(_Win,Constants_List,List).
  add_to_database(_Win):-ignore_flag,
	CtrlWin1=win_GetCtlHandle(_Win,idc_update_3),
	CtrlWin2=win_GetCtlHandle(_Win,idc_update_2),
	String=win_GetText(CtrlWin1),
	String<>"",!,
	upper_lower(String,String1),
	assert(ignore(String1)),
	update_LAN_database(_Win),
	findall(Var,ignore(Var),L),
	lbox_Clear(CtrlWin2),
	lbox_Add(CtrlWin2,-1,L).
  add_to_database(_Win):-
	Text="String is empty",
	dlg_Note("",Text).

  delete_from_database(_Win):-synonyms_flag,
	CtrlWin=win_GetCtlHandle(_Win,idc_update_2),
        Index=lbox_GetSelIndex(CtrlWin),
        Item=lbox_GetItem(CtrlWin,Index),
	scan(Item,Item_List),
	del(Item_List),
	final_str(T),
	rest_str(R),
	retract(synonym(T,R)),
	update_LAN_database(_Win),
	update_synonyms,
	all_strings(X),!,
	CtrlWin1=win_GetCtlHandle(_Win,idc_update_2),
	lbox_Clear(CtrlWin1),
	Constants_List1=[idc_update_2],
	List=[X],
	add_List_to_ListBox(_Win,Constants_List1,List).
  delete_from_database(_Win):-alternatives_flag,
	CtrlWin=win_GetCtlHandle(_Win,idc_update_2),
        Index=lbox_GetSelIndex(CtrlWin),
        Item=lbox_GetItem(CtrlWin,Index),
	scan(Item,[H|T]),
	retract(assoc(H,T)),
	update_LAN_database(_Win),
	update_alternatives,
	all_strings(X),!,
	CtrlWin1=win_GetCtlHandle(_Win,idc_update_2),
	lbox_Clear(CtrlWin1),
	Constants_List1=[idc_update_2],
	List=[X],
	add_List_to_ListBox(_Win,Constants_List1,List).
  delete_from_database(_Win):-ignore_flag,
	CtrlWin=win_GetCtlHandle(_Win,idc_update_2),
        Index=lbox_GetSelIndex(CtrlWin),
        Item=lbox_GetItem(CtrlWin,Index),
        retract(ignore(Item)),!,
        update_LAN_database(_Win),
	findall(Var,ignore(Var),L),
	lbox_Clear(CtrlWin),
	lbox_Add(CtrlWin,-1,L).

PREDICATES
  check_assoc(string)
  string_to_ListButton

CLAUSES
  string_to_ListButton:-
	assoc(S,_),
	check_assoc(S),
	fail.
  string_to_ListButton.
  
  check_assoc(Str):-association(Str),!.
  check_assoc(Str):-
	asserta(association(STR)),
	retractall(all_sen(_)),
	asserta(all_sen([])),
	list_Query1.
	
%BEGIN_WIN Task Window
/***************************************************************************
	Event handling for Task Window
***************************************************************************/

PREDICATES

  task_win_eh : EHANDLER

CONSTANTS

%BEGIN Task Window, CreateParms, 17:12:08-25.11.1999, Code automatically updated!
  task_win_Flags = [wsf_SizeBorder,wsf_TitleBar,wsf_Close,wsf_Maximize,wsf_Minimize,wsf_ClipSiblings,wsf_Invisible]
  task_win_Menu  = res_menu(idr_task_menu)
  task_win_Title = "Geobase"
  task_win_Help  = idh_contents
%END Task Window, CreateParms

CLAUSES

%BEGIN Task Window, e_Create
  task_win_eh(_Win,e_Create(_),0):-!,
%BEGIN Task Window, InitControls, 17:12:08-25.11.1999, Code automatically updated!
%END Task Window, InitControls
%BEGIN Task Window, ToolbarCreate, 17:12:08-25.11.1999, Code automatically updated!
%END Task Window, ToolbarCreate
IFDEF use_message
	msg_Create(100),
ENDDEF
	load_DB,
	asserta(all_sen([])),
	asserta(all_strings([])),
	dlg_geobase_Create(_Win),
	!.
%END Task Window, e_Create

%MARK Task Window, new events

%BEGIN Task Window, id_help_contents
  task_win_eh(_Win,e_Menu(id_help_contents,_ShiftCtlAlt),0):-!,
ifdef platform_16bit
  	vpi_ShowHelp("geobase.hlp"),
elsedef
  	vpi_ShowHelp("geobas32.hlp"),
enddef
	!.
%END Task Window, id_help_contents

%BEGIN Task Window, id_help_about
  task_win_eh(Win,e_Menu(id_help_about,_ShiftCtlAlt),0):-!,
	dlg_about_dialog_Create(Win),
	!.
%END Task Window, id_help_about

%BEGIN Task Window, id_file_exit
  task_win_eh(Win,e_Menu(id_file_exit,_ShiftCtlAlt),0):-!,
  	win_Destroy(Win),
	!.
%END Task Window, id_file_exit

%BEGIN Task Window, e_Size
  task_win_eh(_Win,e_Size(_Width,_Height),0):-!,
IFDEF use_tbar
	toolbar_Resize(_Win),
ENDDEF
IFDEF use_message
	msg_Resize(_Win),
ENDDEF
	!.
%END Task Window, e_Size

%END_WIN Task Window

/***************************************************************************
	Invoking on-line Help
***************************************************************************/

  project_ShowHelpContext(HelpTopic):-
ifdef platform_16bit
  	vpi_ShowHelpContext("geo_w16.hlp",HelpTopic),!.
elsedef
  	vpi_ShowHelpContext("geobase.hlp",HelpTopic),!.
enddef	

/***************************************************************************
	Main Goal
***************************************************************************/

GOAL

IFDEF use_mdi
  vpi_SetAttrVal(attr_win_mdi,b_true),
ENDDEF
IFDEF ws_win
  IFDEF use_3dctrl
    vpi_SetAttrVal(attr_win_3dcontrols,b_true),
  ENDDEF
ENDDEF  
  vpi_Init(task_win_Flags,task_win_eh,task_win_Menu,"geobase",task_win_Title).

%BEGIN_DLG About dialog
/**************************************************************************
	Creation and event handling for dialog: About dialog
**************************************************************************/

CONSTANTS

%BEGIN About dialog, CreateParms, 17:53:26-18.4.2000, Code automatically updated!
  dlg_about_dialog_ResID = idd_dlg_about
  dlg_about_dialog_DlgType = wd_Modal
  dlg_about_dialog_Help = idh_contents
%END About dialog, CreateParms

PREDICATES

  dlg_about_dialog_eh : EHANDLER

CLAUSES

  dlg_about_dialog_Create(Parent):-
	win_CreateResDialog(Parent,dlg_about_dialog_DlgType,dlg_about_dialog_ResID,dlg_about_dialog_eh,0),!.

%BEGIN About dialog, idc_ok _CtlInfo
  dlg_about_dialog_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-!,
	win_Destroy(_Win),
	!.
%END About dialog, idc_ok _CtlInfo
%MARK About dialog, new events

  dlg_about_dialog_eh(_,_,_):-!,fail.

%END_DLG About dialog

%BEGIN_DLG Geobase
/**************************************************************************
	Creation and event handling for dialog: Geobase
**************************************************************************/

CONSTANTS

%BEGIN Geobase, CreateParms, 23:13:50-16.5.2020, Code automatically updated!
  dlg_geobase_DlgType = wd_Modeless
  dlg_geobase_Title = "myGeobase"
  dlg_geobase_RCT = rct(10,25,267,254)
  dlg_geobase_Flags = [wsf_TitleBar]
  dlg_geobase_Help = idh_contents
%END Geobase, CreateParms

PREDICATES

  dlg_geobase_eh : EHANDLER

CLAUSES

  dlg_geobase_Create(Parent):-
	win_CreateDynDialog(Parent,
		[
%BEGIN Geobase, WinDefList, 23:13:50-16.5.2020, Code automatically updated!
		 dlg(wdef(dlg_geobase_DlgType,dlg_geobase_RCT,dlg_geobase_Title,u_DlgBase),dlg_geobase_Flags),
		 ctl(wdef(wc_LBox,rct(8,21,58,61),"",u_DlgBase),idc_lbox_nouns,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_Sort,wsf_NoIntegralHeight]),
		 ctl(wdef(wc_LBox,rct(72,21,122,61),"",u_DlgBase),idc_lbox_verbs,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_Sort,wsf_NoIntegralHeight]),
		 ctl(wdef(wc_LBox,rct(136,21,186,61),"",u_DlgBase),idc_lbox_pronouns,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_Sort,wsf_NoIntegralHeight]),
		 ctl(wdef(wc_LBox,rct(199,21,249,61),"",u_DlgBase),idc_lbox_adjectives,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_Sort,wsf_NoIntegralHeight]),
		 ctl(wdef(wc_LBox,rct(8,91,58,131),"",u_DlgBase),idc_lbox_states,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_Sort,wsf_NoIntegralHeight]),
		 ctl(wdef(wc_LBox,rct(72,91,122,131),"",u_DlgBase),idc_lbox_cities,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_Sort,wsf_NoIntegralHeight]),
		 ctl(wdef(wc_LBoxEdit,rct(7,150,207,198),"",u_DlgBase),idc_query_text,[wsf_Group,wsf_TabStop,wsf_VScroll]),
		 ctl(wdef(wc_PushButton,rct(210,150,250,162),"&Ask",u_DlgBase),idc_ask,[wsf_Group,wsf_TabStop,wsf_Default]),
		 ctl(wdef(wc_PushButton,rct(142,80,190,92),"&Synonyms",u_DlgBase),idc_synonyms,[wsf_Group,wsf_TabStop,wsf_Disabled]),
		 ctl(wdef(wc_PushButton,rct(198,80,248,92),"A&lternatives",u_DlgBase),idc_alternatives,[wsf_Group,wsf_TabStop,wsf_Disabled]),
		 ctl(wdef(wc_PushButton,rct(142,95,222,107),"Words to be &Ignored",u_DlgBase),idc_words_to_be_ignored,[wsf_Group,wsf_TabStop,wsf_Disabled]),
		 ctl(wdef(wc_CheckBox,rct(142,112,205,122),"&Update language",u_DlgBase),idc_update_language,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_LBox,rct(8,180,83,220),"",u_DlgBase),idc_lbox_answer,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_Sort,wsf_NoIntegralHeight]),
		 ctl(wdef(wc_PushButton,rct(196,190,244,202),"&Help",u_DlgBase),idc_help,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(196,174,244,186),"A&bout..",u_DlgBase),idc_about,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(196,213,244,225),"&Close",u_DlgBase),idc_ok,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(135,128,205,140),"&Open LAN database",u_DlgBase),idc_open,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_Text,rct(8,10,56,20),"Nouns",u_DlgBase),idct_nouns,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(72,10,120,20),"Verbs",u_DlgBase),idct_verbs,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(136,10,184,20),"Pronouns",u_DlgBase),idct_pronouns,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(199,10,247,20),"Adjectives",u_DlgBase),idct_adjectives,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(8,80,56,90),"States",u_DlgBase),idct_states,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(73,80,121,90),"Cities",u_DlgBase),idct_cities,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(90,191,153,201),"Member of solution",u_DlgBase),idct_member_of_solution,[wsf_AlignLeft]),
		 ctl(wdef(wc_Edit,rct(156,190,179,202),"",u_DlgBase),idc_edit1,[wsf_AlignLeft,wsf_Group,wsf_AutoHScroll,wsf_ReadOnly]),
		 ctl(wdef(wc_Text,rct(90,208,110,218),"Units",u_DlgBase),idct_units,[wsf_AlignLeft]),
		 ctl(wdef(wc_Edit,rct(114,207,179,219),"",u_DlgBase),idc_edit,[wsf_AlignLeft,wsf_Group,wsf_AutoHScroll,wsf_ReadOnly]),
		 ctl(wdef(wc_GroupBox,rct(2,0,256,68),"Grammar",u_DlgBase),idc_grammar,[]),
		 ctl(wdef(wc_GroupBox,rct(2,70,129,138),"States and Cities",u_DlgBase),idc_groupbox,[]),
		 ctl(wdef(wc_GroupBox,rct(2,140,256,167),"Query",u_DlgBase),idc_query,[]),
		 ctl(wdef(wc_GroupBox,rct(135,70,256,126),"Language",u_DlgBase),idc_language,[]),
		 ctl(wdef(wc_GroupBox,rct(3,170,183,225),"Answer",u_DlgBase),idc_answer,[])
%END Geobase, WinDefList
		],dlg_geobase_eh,0),!.

%BEGIN Geobase, idc_ok _CtlInfo
  dlg_geobase_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-!,
	TaskWin=vpi_GetTaskWin(),
	win_Destroy(_Win),
	win_Destroy(TaskWin),
	!.
%END Geobase, idc_ok _CtlInfo
%MARK Geobase, new events

%BEGIN Geobase, idc_open _CtlInfo
  dlg_geobase_eh(_Win,e_Control(idc_open,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	FileName = dlg_GetFileName("*.lan", ["*.lan","*.lan"],"Open LAN database",[dlgfn_MultiSel],"", _OutListFiles ),
	FileName<>"",!,
	filenamepath(FileName,_,Title),
	retractall(_,lang),
	consult(Title,lang),
	retract(open(_)),
	assert(open(Title)),
	!.
%END Geobase, idc_open _CtlInfo

%BEGIN Geobase, idc_lbox_answer activated
  dlg_geobase_eh(_Win,e_Control(idc_lbox_answer,_CtrlType,LbWinAnswer,activated),0):-!,
        Index=lbox_GetSelIndex(LbWinAnswer),
        Item=lbox_GetItem(LbWinAnswer,Index),
        EdWin=win_GetCtlHandle(_Win,idc_query_text),
        OldTxt=win_GetText(EdWin),
        concat(OldTxt," ",OldTxt1),
        concat(OldTxt1,Item,TmpTxt),
        concat(TmpTxt," ",FinalTxt),
        win_SetText(EdWin,FinalTxt),
	!.
%END Geobase, idc_lbox_answer activated

%BEGIN Geobase, idc_help _CtlInfo
  dlg_geobase_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext(dlg_geobase_Help),
	!.
%END Geobase, idc_help _CtlInfo

%BEGIN Geobase, idc_words_to_be_ignored _CtlInfo
  dlg_geobase_eh(_Win,e_Control(idc_words_to_be_ignored,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	Constants_List=[idc_language,idc_synonyms,idc_alternatives,idc_words_to_be_ignored,
			idc_update_language,idc_open,idc_query_text,idc_ask,idc_query,
			idc_groupbox,idc_lbox_states,idc_lbox_cities,idct_states,idct_cities],
	destroy_controls(_Win,Constants_List),
	Title="Update words to ignore",
	LBoxTitle="Ignore",
	EditTitle="New Words to be ignored",
	create_controls1(_Win,Title,LBoxTitle,EditTitle),
	CtrlWin=win_GetCtlHandle(_Win,idc_update_2),
	findall(Var,ignore(Var),L),
	lbox_Add(CtrlWin,-1,L),
	assert(ignore_flag),
	Constants_List2=[idc_lbox_nouns,idc_lbox_verbs,idc_lbox_pronouns,idc_lbox_adjectives,idc_lbox_answer],
	Flags_List=[[wsf_Disabled],[wsf_Disabled],[wsf_Disabled],[wsf_Disabled],[wsf_Disabled]],
	control_State(_Win,Constants_List2,Flags_List),
	!.
%END Geobase, idc_words_to_be_ignored _CtlInfo

%BEGIN Geobase, idc_alternatives _CtlInfo
  dlg_geobase_eh(_Win,e_Control(idc_alternatives,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	Constants_List=[idc_language,idc_synonyms,idc_alternatives,idc_words_to_be_ignored,
			idc_update_language,idc_open,idc_query_text,idc_ask,idc_query,
			idc_groupbox,idc_lbox_states,idc_lbox_cities,idct_states,idct_cities],
	destroy_controls(_Win,Constants_List),
	Title="Update alternatives",
	LBoxTitle="Associations",
	LBoxButtonTitle="Association",
	EditTitle="New Alternatives for associations",
	create_controls(_Win,Title,LBoxTitle,LBoxButtonTitle,EditTitle),
	retractall(all_strings(_)),
	assert(all_strings([])),
	update_alternatives,
	string_to_ListButton,
	all_sen(L),
	all_strings(X),
	Constants_List1=[idc_update_2,idc_update_4],
	List=[X,L],
	add_List_to_ListBox(_Win,Constants_List1,List),
	assert(alternatives_flag),
	Constants_List2=[idc_lbox_nouns,idc_lbox_verbs,idc_lbox_pronouns,idc_lbox_adjectives,idc_lbox_answer],
	Flags_List=[[wsf_Disabled],[wsf_Disabled],[wsf_Disabled],[wsf_Disabled],[wsf_Disabled]],
	control_State(_Win,Constants_List2,Flags_List),
	L=[H|_],
	CtrlWin=win_GetCtlHandle(_Win,idc_update_4),
	win_SetText(CtrlWin,H),
	!.
%END Geobase, idc_alternatives _CtlInfo

%BEGIN Geobase, idc_synonyms _CtlInfo
  dlg_geobase_eh(_Win,e_Control(idc_synonyms,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	Constants_List=[idc_language,idc_synonyms,idc_alternatives,idc_words_to_be_ignored,
			idc_update_language,idc_open,idc_query_text,idc_ask,idc_query,
			idc_groupbox,idc_lbox_states,idc_lbox_cities,idct_states,idct_cities],
	destroy_controls(_Win,Constants_List),
	Title="Update synonyms",
	LBoxTitle="Synonym - Entity",
	LBoxButtonTitle="Entity",
	EditTitle="New Synonyms for entities",
	create_controls(_Win,Title,LBoxTitle,LBoxButtonTitle,EditTitle),
	retractall(all_strings(_)),
	assert(all_strings([])),
	update_synonyms,
	all_strings(X),
	findall(Var,entity(Var),L),
	Constants_List1=[idc_update_2,idc_update_4],
	List=[X,L],
	add_List_to_ListBox(_Win,Constants_List1,List),
	assert(synonyms_flag),
	Constants_List2=[idc_lbox_nouns,idc_lbox_verbs,idc_lbox_pronouns,idc_lbox_adjectives,idc_lbox_answer],
	Flags_List=[[wsf_Disabled],[wsf_Disabled],[wsf_Disabled],[wsf_Disabled],[wsf_Disabled]],
	control_State(_Win,Constants_List2,Flags_List),
	L=[H|_],
	CtrlWin=win_GetCtlHandle(_Win,idc_update_4),
	win_SetText(CtrlWin,H),
	!.	
%END Geobase, idc_synonyms _CtlInfo

%BEGIN Geobase, idc_about _CtlInfo
  dlg_geobase_eh(_Win,e_Control(idc_about,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	dlg_about_dialog_Create(_Win),
	!.
%END Geobase, idc_about _CtlInfo

%BEGIN Geobase, e_Create
  dlg_geobase_eh(_Win,e_Create(_CreationData),0):-not_DB,!,
	Constants_List=[idc_ask,idc_update_language,idc_open],
	Flags_List=[[wsf_Disabled],[wsf_Disabled],[wsf_Disabled]],
	control_State(_Win,Constants_List,Flags_List),
	!.
  dlg_geobase_eh(_Win,e_Create(_CreationData),0):-!,
	List_Nouns=["abbreviation","area","border","capital","city","cities","continent","citizen",
		    "frontier","height","inhabitant","lake","lakes","length","limit","mountain",
		    "name","names","point","population","river","road","roads","state","states"],
	List_Verbs=["are","as","can","do","does","give","has","have","how","is","in","live","lives",
		    "living","many","number","of","please","tell","that","the","there","to","under",
		    "what","which","with"],
	List_Pronouns=["I","me","you","we","us"],
	List_Adjectives=["above","big","bigger","biggest","great","greater","greatest","high",
			 "higher","highest","in","large","largest","least","long","longer","longest",
			 "lowest","maximum","more","of","over","short","shortest","small","smaller",
			 "smallest","with"],
	fillbox("states",List_States),  
	fillbox("cities",List_Cities),
	Constants_List=[idc_lbox_nouns,idc_lbox_verbs,idc_lbox_pronouns,idc_lbox_adjectives,
			idc_lbox_states,idc_lbox_cities],
	List=[List_Nouns,List_Verbs,List_Pronouns,List_Adjectives,List_States,List_Cities],
	add_List_to_ListBox(_Win,Constants_List,List),
	CtrlWin=win_GetCtlHandle(_Win,idc_query_text),
	Text="what is the biggest city in california",
	win_SetText(CtrlWin,Text),
	!.
%END Geobase, e_Create

%BEGIN Geobase, idc_lbox_cities activated
  dlg_geobase_eh(_Win,e_Control(idc_lbox_cities,_CtrlType,ListBoxWin,activated),0):-!,
	Index  = lbox_GetSelIndex(ListBoxWin),
	Item   = lbox_GetItem(ListBoxWin,Index),
 	EdWin  = win_GetCtlHandle(_Win,idc_query_text),
 	OldTxt = win_GetText(EdWin),
	concat(OldTxt,Item,TmpTxt),
 	concat(TmpTxt," ",FinalTxt),
 	win_SetText(EdWin,FinalTxt),
	!.
%END Geobase, idc_lbox_cities activated

%BEGIN Geobase, idc_lbox_states activated
  dlg_geobase_eh(_Win,e_Control(idc_lbox_states,_CtrlType,ListBoxWin,activated),0):-!,
	Index  = lbox_GetSelIndex(ListBoxWin),
	Item   = lbox_GetItem(ListBoxWin,Index),
 	EdWin  = win_GetCtlHandle(_Win,idc_query_text),
 	OldTxt = win_GetText(EdWin),
	concat(OldTxt,Item,TmpTxt),
 	concat(TmpTxt," ",FinalTxt),
 	win_SetText(EdWin,FinalTxt),
	!.
%END Geobase, idc_lbox_states activated

%BEGIN Geobase, idc_lbox_adjectives activated
  dlg_geobase_eh(_Win,e_Control(idc_lbox_adjectives,_CtrlType,ListBoxWin,activated),0):-!,
	Index  = lbox_GetSelIndex(ListBoxWin),
	Item   = lbox_GetItem(ListBoxWin,Index),
 	EdWin  = win_GetCtlHandle(_Win,idc_query_text),
 	OldTxt = win_GetText(EdWin),
	concat(OldTxt,Item,TmpTxt),
 	concat(TmpTxt," ",FinalTxt),
 	win_SetText(EdWin,FinalTxt),
	!.
%END Geobase, idc_lbox_adjectives activated

%BEGIN Geobase, idc_lbox_pronouns activated
  dlg_geobase_eh(_Win,e_Control(idc_lbox_pronouns,_CtrlType,ListboxWin,activated),0):-!,
	Index  = lbox_GetSelIndex(ListBoxWin),
	Item   = lbox_GetItem(ListBoxWin,Index),
 	EdWin  = win_GetCtlHandle(_Win,idc_query_text),
 	OldTxt = win_GetText(EdWin),
	concat(OldTxt,Item,TmpTxt),
 	concat(TmpTxt," ",FinalTxt),
 	win_SetText(EdWin,FinalTxt),
	!.
%END Geobase, idc_lbox_pronouns activated

%BEGIN Geobase, idc_lbox_verbs activated
  dlg_geobase_eh(_Win,e_Control(idc_lbox_verbs,_CtrlType,ListBoxWin,activated),0):-!,
	Index  = lbox_GetSelIndex(ListBoxWin),
	Item   = lbox_GetItem(ListBoxWin,Index),
 	EdWin  = win_GetCtlHandle(_Win,idc_query_text),
 	OldTxt = win_GetText(EdWin),
	concat(OldTxt,Item,TmpTxt),
 	concat(TmpTxt," ",FinalTxt),
 	win_SetText(EdWin,FinalTxt),
	!.
%END Geobase, idc_lbox_verbs activated

%BEGIN Geobase, idc_lbox_nouns activated
  dlg_geobase_eh(_Win,e_Control(idc_lbox_nouns,_CtrlType,ListBoxWin,activated),0):-!,
	Index  = lbox_GetSelIndex(ListBoxWin),
	Item   = lbox_GetItem(ListBoxWin,Index),
 	EdWin  = win_GetCtlHandle(_Win,idc_query_text),
 	OldTxt = win_GetText(EdWin),
	concat(OldTxt,Item,TmpTxt),
 	concat(TmpTxt," ",FinalTxt),
 	win_SetText(EdWin,FinalTxt),
	!.
%END Geobase, idc_lbox_nouns activated

%BEGIN Geobase, idc_update_language _CtlInfo
  dlg_geobase_eh(_Win,e_Control(idc_update_language,_CtrlType,CheckBoxWin,_CtlInfo),0):-
	State=win_IsChecked(CheckBoxWin),
	State=checkbox_on,!,
	Constants_List=[idc_synonyms,idc_alternatives,idc_words_to_be_ignored],
	Flags_List=[[wsf_Enabled],[wsf_Enabled],[wsf_Enabled]],
	control_State(_Win,Constants_List,Flags_List),
	!.
  dlg_geobase_eh(_Win,e_Control(idc_update_language,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	Constants_List=[idc_synonyms,idc_alternatives,idc_words_to_be_ignored],
	Flags_List=[[wsf_Disabled],[wsf_Disabled],[wsf_Disabled]],
	control_State(_Win,Constants_List,Flags_List),
	!.
%END Geobase, idc_update_language _CtlInfo

%BEGIN Geobase, idc_ask _CtlInfo
  dlg_geobase_eh(_Win,e_Control(idc_ask,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
 	EdWin  = win_GetCtlHandle(_Win,idc_query_text),
 	QuetionTxt = win_GetText(EdWin),      		
	question(QuetionTxt,Answer,Solutions,Units),
	ch_s(_Win,Answer,Solutions,Units),
	check_expression(EdWin,QuetionTxt),
	!.
%END Geobase, idc_ask _CtlInfo
% Cancel
  dlg_geobase_eh(_Win,e_Control(idc_update_7,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	retractall(all_strings(_)),
	assert(all_strings([])),
	update_controls(_Win),
	retractall(_,lang_db),
	Constants_List=[idc_lbox_nouns,idc_lbox_verbs,idc_lbox_pronouns,idc_lbox_adjectives,idc_lbox_answer],
	Flags_List=[[wsf_Enabled],[wsf_Enabled],[wsf_Enabled],[wsf_Enabled],[wsf_Enabled]],
	control_State(_Win,Constants_List,Flags_List),
	check_landb(_Win),
	!.
% ListBox	
  dlg_geobase_eh(_Win,e_Control(idc_update_2,_CtrlType,_CtrlWin,selchanged),0):-!,
	CtrlWin=win_GetCtlHandle(_Win,idc_update_6),
	win_SetState(CtrlWin,[wsf_Enabled]),
	!.
% Add	
  dlg_geobase_eh(_Win,e_Control(idc_update_5,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	retractall(all_strings(_)),
	assert(all_strings([])),
	add_to_database(_Win),
	!.
% Delete
  dlg_geobase_eh(_Win,e_Control(idc_update_6,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	retractall(all_strings(_)),
	assert(all_strings([])),
	retractall(_,str),
	assert(final_Str("")),
	delete_from_database(_Win),
	!.
% Save	
  dlg_geobase_eh(_Win,e_Control(idc_update_11,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	open(FileName),
	save(FileName,lang),
	!.
	
% Save	as
  dlg_geobase_eh(_Win,e_Control(idc_update_12,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	FileName = dlg_GetFileName("*.lan", ["*.lan","*.lan"],"Save LAN database",[dlgfn_Save],"", _OutListFiles ),
	FileName<>"",!,
	filenamepath(FileName,_,Title),
	save(Title,lang),
	!.

  dlg_geobase_eh(_,_,_):-!,fail.

%END_DLG Geobase




















