/* list.h

   written by D.R.Maszle
   28 October 1991

   Copyright (c) 1993.  Don Maszle, Frederic Bois.  All rights reserved.

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
     FrŽdŽric Bois / Don Maszle
     BEHS, School of Public Health
     University of California at Berkeley
     Berkeley, CA 94720

     fbois@diana.lbl.gov

   -- Revisions -----
     Logfile:  SCCS/s.list.h
    Revision:  1.9
        Date:  14 Nov 1997
     Modtime:  06:39:13
      Author:  @a
   -- SCCS  ---------

*/

#ifndef _LIST_H_

/* -----------------------------------------------------------------------------
   Inclusions  */

#include "hungtype.h"


/* -----------------------------------------------------------------------------
   Typedefs  */

typedef struct tagLISTELEM {   /* List element record */
  PVOID pData;                 /* User data, to be recast */
  struct tagLISTELEM *pleNext; /* Next in the list */
} LISTELEM, *PLISTELEM; /* tagLISTELEM */


typedef struct tagLIST { /* List header record */
  PLISTELEM pleHead;     /* First elem in list */
  PLISTELEM pleTail;     /* Last elem in list */
  int    iSize;          /* Number of elems in list */
} LIST, *PLIST; /* tagLIST */

/* Callback function for ForAllList, ForAllList3 */
typedef int (*PFI_FORLISTCALLBACK) (PVOID pData, PVOID pUserInfo);
typedef void (*PFI_FORLISTCALLBACK3) (PVOID pData, PVOID pUserInfo1,
                                     PVOID pUserInfo2, PVOID pUserInfo3);

/* Callback for FreeList() */
typedef void (*PFV_FREELISTCALLBACK) (PVOID pData);

/* -----------------------------------------------------------------------------
   Macros  */

#define ListLength(plist) ((plist) ? (plist)->iSize : 0)


/* -----------------------------------------------------------------------------
   Prototypes  */


int  ForAllList (PLIST plist, PFI_FORLISTCALLBACK pfiForAllData, PVOID pInfo);
void ForAllList3 (PLIST plist, PFI_FORLISTCALLBACK3 pfiCallback,
                  PVOID pUserInfo1, PVOID pUserInfo2, PVOID pUserInfo3);

void FreeList(PLIST *pplist, PFV_FREELISTCALLBACK pfvFreeData, BOOL bAndData);

PLIST InitList (void);

void QueueListItem (PLIST plist, PVOID pData);

#define  _LIST_H_
#endif

/* end */

