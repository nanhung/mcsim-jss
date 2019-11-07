/* lexfn.h

   written by Don Maszle
   15 October 1991

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
     Frédéric Bois / Don Maszle
     BEHS, School of Public Health
     University of California at Berkeley
     Berkeley, CA 94720

     fbois@diana.lbl.gov

   -- Revisions -----
     Logfile:  SCCS/s.lexfn.h
    Revision:  1.11
        Date:  14 Nov 1997
     Modtime:  06:39:13
      Author:  @a
   -- SCCS  ---------

   Header file for input definitions.
*/

#ifndef _LEXFN_H_

/* -----------------------------------------------------------------------------
   Inclusions  */

#include "lex.h"

/* -----------------------------------------------------------------------------
   Constants  */

#define N_TAU_EXPOSE 40 /* Number of Tau's to expose exp() inputs */

/* Input Function constants */

#define IFN_NULL     0
#define IFN_CONSTANT 1
#define IFN_PERDOSE  2
#define IFN_PERRATE  3
#define IFN_PEREXP   4
#define IFN_NDOSES   5
#define IFN_SPIKES   6

/* -----------------------------------------------------------------------------
   Typedefs  */

/* IFN:  Predefined input functions

   The parameters of interest vary depending on the type of the function.
   I know, this should really be a union, but they are kind of messy and
   this needs to be redone more intelligently anyway.

   At any point in time, dVal is the current value (this is calculated
   for a given point in time by CalcInputs().  For pulsed inputs, bOn
   flags whether or not the pulse is active.  Dependencies of IFN parms on
   on model parameters are respected via the handle fields

   Here is what the parms mean:

     IFN_CONSTANT  dMag is the constant

   -- Periodic functions:  Period = dTper, magnitude = dMag, start time = dT0

     IFN_PERDOSE  Periodic dose lasting dTexp
     IFN_PEREXP   Periodic exponential with decay constant dDecay

   -- Multiple pulse functions:

     IFN_NDOSES   nDoses of rgMags[] starting at rgT0s[]
     IFN_SPIKES   nDoses spikes of rgMags[] at time rgT0s[]
*/

typedef struct tagIFN {
  /* Bookkeeping */

  int iType;               /* One of the IFN_ types */
  BOOL bOn;                /* TRUE if exposure is On */
  double dTStartPeriod;    /* Start of current period */
  double dVal;             /* Current value: CalcInputs updates */

  /* Periodic functions */

  double dMag;             /* Magnitude of input */
  double dTper;            /* Duration of Period */
  double dT0;              /* Starting time of exposure */
  double dTexp;            /* Exposure duration */

  /* For exponential inputs, the exponential decay rate
     Exposure lasts for N_TAU_EXPOSE Tau periods. (tau=1/Decay)
     After this, input is considered to be neglible. */

  double dDecay;

  /* Dependencies for the periodic parms */

  HANDLE hMag;             /* Handle to magnitude */
  HANDLE hTper;            /* Handle to period */
  HANDLE hT0;              /* Handle to starting time */
  HANDLE hTexp;            /* Handle to exposure time */
  HANDLE hDecay;           /* Handle to exponential decay rate */

  /* Multiple dose inputs */

  int nDoses;              /* Number of doses of Spikes */
  int iDoseCur;            /* Current Dose */

  /* For value input */
  PDOUBLE rgT0s;           /* Array of start times */
  PDOUBLE rgMags;          /* Array of magnitudes */

  /* For variable input */
  HANDLE *rghT0s;          /* Handles to start times */
  HANDLE *rghMags;         /* Handles to magnitudes */

} IFN, *PIFN; /* struct tagIFN */


/* -----------------------------------------------------------------------------
   Prototypes */

BOOL DefDepParm (PSTR szLex, PDOUBLE pdValue, HANDLE *phvar);

int  GetFnType (PSTR szName);
BOOL GetInputArgs (PINPUTBUF pibIn, PIFN pifn);
BOOL GetInputFn (PINPUTBUF pibIn, PSTR sz, PIFN pifn);
BOOL GetNDoses (PINPUTBUF pibIn, PSTR szLex, PIFN pifn);
BOOL GetNNumbers (PINPUTBUF pibIn, PSTR szLex, int nNumbers, PDOUBLE rgd);
BOOL GetSpikes (PINPUTBUF pibIn, PSTR szLex, PIFN pifn);

void InitIFN (PIFN pifn);

#define _LEXFN_H_
#endif

/* End */

