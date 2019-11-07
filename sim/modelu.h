/* modelu.h

   written by Don Maszle
   7 October 1991

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
     Logfile:  SCCS/s.modelu.h
    Revision:  1.10
        Date:  14 Nov 1997
     Modtime:  06:39:17
      Author:  @a
   -- SCCS  ---------

   Model utilities include file.

   Utility prototypes and structures used by the generated model
   file.  The model typedefs need to be defined here so that
   'modelu.c' can use them.

*/


#ifndef _MODELU_H_

/* -----------------------------------------------------------------------------
   Inclusions  */

#include "modiface.h"

/* -----------------------------------------------------------------------------
   Constants  */

#define ID_NULL     0x0000
#define ID_STATE    0x1000
#define ID_INPUT    0x2000
#define ID_OUTPUT   0x3000
#define ID_PARM     0x4000  /* Global parameter */

#define HV_TYPE_MASK   0xF000 /* Handle to variable is a WORD */
#define HV_INDEX_MASK  0x0FFF /* == 0xtiii, type and index */


/* -----------------------------------------------------------------------------
   Typedefs  */

/* Global Variable Map */

typedef struct tagVM {
    PSTR szName;     /* Name of the variable */
    PVOID pVar;      /* Ptr to C variable */
    HVAR hvar;       /* Handle to variable: ID_TYPE | index */
} VMMAPSTRCT, *PVMMAPSTRCT; /* Variable Map element */


/* -----------------------------------------------------------------------------
   Macros  */

#define TYPE(pvm)    ((pvm) ? (pvm)->hvar & HV_TYPE_MASK : ID_NULL)
#define INDEX(pvm)   ((pvm) ? (pvm)->hvar & HV_INDEX_MASK: ID_NULL)

#define HTYPE(hvar)  ((hvar) & HV_TYPE_MASK)
#define HINDEX(hvar) ((int) ((hvar) & HV_INDEX_MASK))


/* -----------------------------------------------------------------------------
   Prototypes  */

void FixupDependentInputs(void);

void GetStartPeriods (PDOUBLE pdTime);
void GetStateHandles (HVAR *phvar);
PVMMAPSTRCT  GetVarPtr (PVMMAPSTRCT pvm, PSTR szName);
int  GetVarType (HVAR hvar);

void PostUpdateSpikes (PDOUBLE pdTime);

void UpdateDefaultInput (PIFN pifn, PDOUBLE pdTnext, PDOUBLE pdTime);
void UpdateNDoses (PIFN pifn, PDOUBLE pdTnext, PDOUBLE pdTime);
BOOL UpdateSpikes (PIFN pifn, PDOUBLE pdTnext, PDOUBLE pdTime);

#define _MODELU_H_
#endif

/* End */

