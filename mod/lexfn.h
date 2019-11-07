/* lexfn.h

   written by Don Maszle
   15 October 1991
   
   Copyright (c) 1993.  Don Maszle, Frederic Bois.  All rights reserved.

   -- Revisions -----
     Logfile:  %F%
    Revision:  %I%
        Date:  %G%
     Modtime:  %U%
      Author:  @a
   -- SCCS  ---------

   Header file for input definitions.
*/

#ifndef _LEXFN_INCLUDED

/* ----- Inclusions  */

#include "lex.h"

/* ----- Constants  */

#define N_TAU_EXPOSE	40	/* Number of Tau's to expose exp() inputs */

/* Input Function constants */

#define IFN_NULL	0
#define IFN_CONSTANT	1
#define IFN_PERDOSE	2
#define IFN_PERRATE	3
#define IFN_PEREXP	4
#define IFN_NDOSES	5

/* ----- Enumerations  */

/* ----- Typedefs  */

/* The following structure is used for predefined period input
   functions.
*/

typedef struct tagIFN {		/* Input Function struct */
  int iType;	/* IFN_ */

  BOOL bOn; 	/* Flag to indicate exposure is On */

  double dMag;	/* Magnitude of input */
  double dTper;	/* Duration of Period */
  double dT0;	/* Starting time of exposure */
  double dTexp;	/* Exposure time, input is 0.0 after T0 + Texp */

  double dDecay;/* For exponential inputs, the exponential decay rate
		   Exposure lasts for N_TAU_EXPOSE Tau periods. (tau=1/Decay)
		   After this, input is considered to be neglible. */

  double dVal;	/* Current value as calculated by CalcInputs */

  double dTStartPeriod;	/* Start of current period - for NextTransitionTime */

  HANDLE hMag;	  /* Handle to magnitude */
  HANDLE hTper;	  /* Handle to period */
  HANDLE hT0;	  /* Handle to starting time */
  HANDLE hTexp;	  /* Handle to exposure time */
  HANDLE hDecay;  /* Handle to exponential decay rate */

  int nDoses;	/* Number of dose/transition pairs for IFN_NDOSES */
  int iDoseCur;	/* Current Dose */
  PDOUBLE rgT0s;
  PDOUBLE rgTexps;
  PDOUBLE rgMags;
  
} IFN, *PIFN;	/* struct tagIFN */



/* ----- Macros  */

/* ----- Globals/Externals  */

/* ----- Prototypes  */

int GetFnType (PSTR szName);
BOOL GetInputFn (PINPUTBUF pibIn, PSTR sz, PIFN pifn);


#define _LEXFN_INCLUDED
#endif
