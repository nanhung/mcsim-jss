/* mac.c

   Copyright (C) 1989, 1990, 1991, 1992 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

   -- Revisions -----
     Logfile:  SCCS/s.mac.c
    Revision:  1.8
        Date:  14 Nov 1997
     Modtime:  06:39:01
      Author:  @a
   -- SCCS  ---------

   FB 21/9/97: this file contains code that is accessed and used only if
   the symbol _MACOSLEVEL2_ is defined. Level 2 for Macintosh gives a 
   windows interface and some conformity to the MacOS interface guidelines.
   The behavior of this file maybe compiler-dependent (I use Symantec's 
   Think C).

   Level 1 Macintosh implementation is obtained by defining the symbol 
   _MACOSLEVEL1_ and gives a standard console which should be provided by 
   the compiler.
*/

#ifdef _MACOSLEVEL2_

#include <PPC Macheaders>

#include <stdlib.h>
#include <string.h>
#include <console.h>

#include "mac.h"
#include "lex.h"
#include "lexerr.h"
#include "sim.h"
#include "simi.h"
#include "siminit.h"

#define ErrorAlert 128

MenuHandle  appleMenu, fileMenu, editMenu;

enum  {
  appleID = 128,
  fileID,
  editID
};

enum  {
  runItem = 1,
  stopItem,
  quitItem = 4
};

static Point SFGwhere = { 90, 82 };
static Point SFPwhere = { 106, 104 };
static SFReply reply;

PANALYSIS panal;

/* -----------------------------------------------------------------------------
 *  AdjustMenus()
 *
 *  Enable or disable the items in the Edit menu if a DA window
 *  comes up or goes away. Our application doesn't do anything with
 *  the Edit menu.
 *
 */
void AdjustMenus(void)
{
  register WindowPeek wp = (WindowPeek) FrontWindow();
  short kind = wp ? wp->windowKind : 0;
  Boolean DA = kind < 0;
  
  Enable(editMenu, 1, DA);
  Enable(editMenu, 3, DA);
  Enable(editMenu, 4, DA);
  Enable(editMenu, 5, DA);
  Enable(editMenu, 6, DA);
  
}


/* -----------------------------------------------------------------------------
 *  Enable()
 *
 */
static void Enable(MenuHandle menu, short item, short ok)
{
  if (ok)
    EnableItem(menu, item);
  else
    DisableItem(menu, item);
}


/* -----------------------------------------------------------------------------
   FileError
 */
void FileError(Str255 s, Str255 f)

{
	ParamText(s, f,"\p !", "\p");
	Alert(ErrorAlert, 0L);
}


/* -----------------------------------------------------------------------------
 * HandleEvent()
 *
 *    The main event dispatcher. This routine should be called
 *    repeatedly (it  handles only one event).
 *
 */
void HandleEvent (void)

{
  register int ok;
  static EventRecord theEvent;

  /* disabled
  HiliteMenu(0);
  SystemTask ();    Handle desk accessories
  */
  
  ok = GetNextEvent (everyEvent, &theEvent);
  if (ok)
    switch (theEvent.what) {

    case mouseDown:
      HandleMouseDown(&theEvent);
      break;
      
    case keyDown: 
    case autoKey:
      if ((theEvent.modifiers & cmdKey) != 0) {
        AdjustMenus();
        HandleMenu(MenuKey((char) (theEvent.message & charCodeMask)));
      }
      break;
      
 /* case updateEvt: 
      BeginUpdate(theWindow);
      DrawBullseye(((WindowPeek) theWindow)->hilited);
      EndUpdate(theWindow); 
      break;
        
    case activateEvt: 
      InvalRect(&theWindow->portRect);
      break; */

  } /* switch */

} /* end HandleEvent */


/* -----------------------------------------------------------------------------
 * HandleMenu(mSelect)
 *
 *  Handle the menu selection. mSelect is what MenuSelect() and
 *  MenuKey() return: the high word is the menu ID, the low word
 *  is the menu item
 *
 */
void HandleMenu (long mSelect)

{
  int        menuID = HiWord(mSelect);
  int        menuItem = LoWord(mSelect);
  short      refNum;
  Str255     name;
  GrafPtr    savePort;
  WindowPeek frontWindow;
  
  switch (menuID) {
    {
    case  appleID:
      if (menuItem == 1) ShowSplash();
      else {
        GetPort(&savePort);
        GetItem(appleMenu, menuItem, name);
        OpenDeskAcc(name);
        SetPort(savePort);
      }
      break;
  
    case  fileID: 
    switch (menuItem)
      {
      case runItem:
        Enable(fileMenu, runItem,  0);
        Enable(fileMenu, stopItem, 1);
        Enable(fileMenu, quitItem, 1);
        Main_for_mac();
        Enable(fileMenu, stopItem, 0);
        break;
                  
      case stopItem:           
      case quitItem:
        FreeLevels(panal);
        ExitToShell();
        break;
      }
    break;
          
    case  editID:
    if (!SystemEdit(menuItem-1))
      SysBeep(5);
    break;
    
    }
  }
} /* end HandleMenu */


/* -----------------------------------------------------------------------------
 * HandleMouseDown (theEvent)
 *
 *  Take care of mouseDown events.
 *
 */
void HandleMouseDown (EventRecord *theEvent)

{
  WindowPtr theWindow;
  int windowCode = FindWindow (theEvent->where, &theWindow);
  
  switch (windowCode) {
    case inSysWindow: 
      SystemClick (theEvent, theWindow);
      break;
      
    case inMenuBar:
      AdjustMenus();
      HandleMenu(MenuSelect(theEvent->where));
      break;
      
    case inDrag: /*
      if (theWindow == theWindow)
        DragWindow(theWindow, theEvent->where, &dragRect);
        break; */
        
    case inContent: /*
      if (theWindow == theWindow)
        {
        if (theWindow != FrontWindow())
          SelectWindow(theWindow);
        else
          InvalRect(&theWindow->portRect);
        } */
      break;
      
    case inGoAway: /*
      if (theWindow == theWindow && TrackGoAway(theWindow, theEvent->where))
        HideWindow(theWindow); */
      break;

  } /* switch */

} /* end HandleMouseDown */


/* -----------------------------------------------------------------------------
 */
void InitMacintosh ()
{
  InitGraf (&qd.thePort);
  InitFonts();
  FlushEvents(everyEvent, 0);
  InitWindows();
  InitMenus();
  TEInit();
  InitDialogs(0L);
  InitCursor ();

  SetUpMenus();
  ShowSplash();
  
} /* end InitMacintosh */


/* -----------------------------------------------------------------------------
 * Main_for_mac
 */
void Main_for_mac ()
{
  PSTR      szFileIn, szFileOut;
  INPUTBUF  ibIn;
  Str255    filename1, filename2;
  int       vRef;

  panal = (PANALYSIS) malloc (sizeof(ANALYSIS));

  console_options.nrows = 10;
  console_options.ncols = 60;
  console_options.pause_atexit = 0;
  console_options.title = "\pMCsim";
  console_options.procID = 16;
  
  
  if (OpenSimFile(filename1, &vRef)) {
    OpenOutFile(filename2, &vRef);
  }
  else {
    FileError("\pError opening ", filename1);
    return;
  }

  /* cleanup[ the stupid strings */
  filename1[(size_t) filename1[0] + 1] = 0;
  memmove(filename1, &filename1[1], (size_t) filename1[0] + 1);

  filename2[(size_t) filename2[0] + 1] = 0;
  memmove(filename2, &filename2[1], (size_t) filename2[0] + 1);
  
  if (!panal)
    ReportError (NULL, RE_OUTOFMEM | RE_FATAL,
                 "ANALYSIS specification too large", NULL);

  InitAnalysis (panal);

  /* Define the output file as the global experiment default  */
  panal->expGlobal.os.szOutfilename = (PSTR) filename2;
  panal->expGlobal.os.bCommandLineSpec = TRUE;

  if (!InitBuffer (&ibIn, (PSTR) filename1))
    ReportError (&ibIn, RE_INIT | RE_FATAL, "ReadInput", NULL);

  ibIn.pInfo = (PVOID) panal; /* Attach analysis specification to input */

  if (ReadAnalysis (&ibIn)) {
    PrepAnalysis (panal);
    DoAnalysis (panal);
  }
  FreeLevels(panal);

} /* Main_for_mac */


/* -----------------------------------------------------------------------------
 * OpenOutFile
 */
int OpenOutFile (Str255 fn, int *vRef)
{
  fn[0] = 0;
  SFPutFile(SFPwhere, "\pSave output as", fn, 0L, &reply);
  if (!reply.good)
    return (0);
  else {
    pStrCopy(reply.fName, fn);
    *vRef = reply.vRefNum;
    return(1);
  }
} /* OpenOutFile */


/* -----------------------------------------------------------------------------
 * OpenSimFile
 */
int OpenSimFile (Str255 fn, int *vRef)

{
  SFTypeList	myTypes;
	
  myTypes[0]='TEXT';

  SFGetFile(SFGwhere, "\p", 0L, 1, myTypes, 0L, &reply);

  if (!reply.good)
    return (0);
  else {
    pStrCopy(reply.fName, fn);
    *vRef = reply.vRefNum;
    return(1);
  }
} /* OpenSimFile */


/* -----------------------------------------------------------------------------
   pStrCopy
   copies a pascal string from p1 to p2
 */
void pStrCopy (StringPtr p1, StringPtr p2)
{
	register int len;
	
	len = *p2++ = *p1++;
	while (--len>=0) *p2++=*p1++;
}


/* -----------------------------------------------------------------------------
 * SetUpMenus()
 *
 *  Set up the menus. Normally, we’d use a resource file, but
 *  for this example we’ll supply “hardwired” strings.
 *
 */
void SetUpMenus(void)

{
  InsertMenu(appleMenu = GetMenu(128),  0);
  InsertMenu(fileMenu  = NewMenu(fileID,  "\pFile"),  0);
  InsertMenu(editMenu  = NewMenu(editID,  "\pEdit"),  0);

  DrawMenuBar();

  AddResMenu(appleMenu, 'DRVR');

  AppendMenu(fileMenu,  "\pRun Input File/R;Stop/.;(-;Quit/Q");
  AppendMenu(editMenu,  "\pUndo/Z;(-;Cut/X;Copy/C;Paste/V;Clear");

  Enable(fileMenu, runItem,  1);
  Enable(fileMenu, stopItem, 0);
    
} /* end SetUpMenus */


/* ----------------------------------------------------------------------------
 * ShowSplash
 */
void ShowSplash ()
{
  EventRecord theEvent;
  DialogPtr   dlgInfo = 0;
  GrafPtr     savePort;
	
  GetPort(&savePort);
  dlgInfo = GetNewDialog(600, 0L, (WindowPtr)-1L);
  ShowHide(dlgInfo, true);
  DrawDialog(dlgInfo);

  while (!GetNextEvent(keyDownMask + mDownMask, &theEvent)) 
    SystemTask();

  DisposDialog(dlgInfo);

  SetPort(savePort);

} /* ShowSplash */

#endif /* _MACOSLEVEL2_ */

/* End */
