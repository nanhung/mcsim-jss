/* mac.h

   written by Frederic Bois
   4 September 1995

   Copyright (c) 1995.  Don Maszle, Frederic Bois.  All rights reserved.

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

   contact
     Frédéric Bois / Don Maszle
     BEHS, School of Public Health
     University of California at Berkeley
     Berkeley, CA 94720

     fbois@diana.lbl.gov

   -- Revisions -----
     Logfile:  SCCS/s.mac.h
    Revision:  1.7
        Date:  14 Nov 1997
     Modtime:  06:39:14
      Author:  @a
   -- SCCS  ---------

*/


/* ----------------------------------------------------------------------------
   Prototypes */

/* Public */


/* Private */

#ifdef _MACOSLEVEL2_

void AdjustMenus(void);
void FileError(Str255 s, Str255 f);
void HandleEvent(void);
void HandleMenu(long mSelect);
void HandleMouseDown(EventRecord  *theEvent);
void InitMacintosh( void );
void Main_for_mac(void);
int  OpenOutFile(Str255 fn, int *vRef);
int  OpenSimFile(Str255 fn, int *vRef);
void pStrCopy(StringPtr a, StringPtr b);
void SetUpMenus(void);
void ShowSplash (void);

static void Enable (MenuHandle menu, short item, short ok);

#endif

/* End */

