/*****************************************************************************

		Copyright (c) 1984 - 2000 Prolog Development Center A/S

 Project:  GEOBASE
 FileName: GEOBASE.INC
 Purpose:  Project's global declarations
 Written by: De Tre
 Modifyed by: Eugene Akimov
 Comments:

*****************************************************************************/

CONSTANTS
%BEGIN_CONST Project configuration constants
  use_3dctrl = 1
  use_dlgpack = 1
  use_message = 1
%END_CONST

include "vpi.con"
include "vpi.dom"
include "vpi.pre"
IFDEF use_tabdlg
  IFNDEF use_dlgpack
    CONSTANTS
       use_dlgpack =1
  ENDDEF
ENDDEF
IFDEF use_dlgpack
  include "dialog\\dialog.dom"
  include "dialog\\dialog.pre"
ENDDEF
IFDEF use_message
  IFNDEF use_editor
    CONSTANTS
       use_editor =1
  ENDDEF
ENDDEF
IFDEF use_editor
  include "edit.dom"
  include "edit.pre"
ENDDEF
IFDEF use_message
  include "messages\\messages.pre"
ENDDEF
IFDEF use_tbar
  include "toolbar\\toolbar.dom"
  include "toolbar\\toolbar.pre"
ENDDEF
IFDEF use_tree
  include "tree\\vpitree.dom"
  include "tree\\vpitree.pre"
ENDDEF
IFDEF use_tabdlg
  include "tabdlg\\tabdlg.dom"
  include "tabdlg\\tabdlg.pre"
ENDDEF
IFDEF use_dlgdir
  IFNDEF use_ownerdraw
    CONSTANTS
       use_ownerdraw =1
  ENDDEF
  include "dlgdir\\dlgdir.pre"
ENDDEF
IFDEF use_ownerdraw
  include "owndraw\\owndraw.dom"
  include "owndraw\\owndraw.pre"
ENDDEF
IFDEF use_grid
  include "grid\\grid.dom"
  include "grid\\grid.pre"
ENDDEF
IFDEF use_date
  include "date\\date.dom"
  CONSTANTS
    language_english = 1
%    language_danish  = 1
%    language_russian = 1
  include "date\\date.pre"
ENDDEF
IFDEF use_treebrowser
GLOBAL DOMAINS
  BROWSELIST_USERITEMKEY = INTEGER % User specified

  include "treebrws\\treebrws.dom"
  include "treebrws\\treebrws.pre"
ENDDEF
IFDEF use_listproperty
  include "property\\property.dom"
  include "property\\property.pre"
ENDDEF
IFDEF use_palette
  include "palette\\palette.dom"
  include "palette\\palette.pre"
ENDDEF
IFDEF use_progress
  include "progress\\progress.pre"
ENDDEF
IFDEF use_doc
  include "docstr.dom"
  include "html.pre"
  include "ipf.pre"
  include "rtf.pre"
  include "errhndl.pre"
ENDDEF
IFDEF use_sql
  include "sqlbind.dom"
  include "sqlbind.pre"
ENDDEF
IFDEF use_odbc
  include "odbc.dom"
  include "odbc.pre"
ENDDEF
IFDEF use_socket
  IFNDEF use_runtime
    CONSTANTS
      use_runtime = 1
  ENDDEF
ENDDEF
IFDEF use_runtime
  include "pdcrunt.dom"
  include "pdcrunt.pre"
ENDDEF
IFDEF use_socket
  include "include\\pdcsock.dom"
  include "include\\pdcsock.con"
  include "include\\pdcsock.pre"
ENDDEF

/*****************************************************************************
			User's Declarations.
*****************************************************************************/
GLOBAL DOMAINS
  DB_SELECTOR = browselist_db;	% For treebrowser tool
		geobase
  FILE = fileselector1; fileselector2 % To be edited

include "geobase.dom"
include "geobase.pre"
