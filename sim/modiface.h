/* modiface.h

   written by Don Maszle
   8 October 1991

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
     Logfile:  SCCS/s.modiface.h
    Revision:  1.10
        Date:  14 Nov 1997
     Modtime:  06:39:18
      Author:  @a
   -- SCCS  ---------

*/

#ifndef _MODIFACE_INCLUDED

/* ----------------------------------------------------------------------------
   Inclusions  */

#include "hungtype.h"
#include "lexfn.h"


/* ----------------------------------------------------------------------------
   Typedefs  */

typedef HANDLE HVAR;    /* Handle to a model variable  */
typedef HVAR *PHVAR;    /* and a pointer to the handle */


/* ----------------------------------------------------------------------------
   Globals/Externals  */

extern char szModelDescFilename[];
extern char szModelSourceFilename[];
extern char szModelGenAndVersion[];


/* ----------------------------------------------------------------------------
   Prototypes  */

/* Model Initialization and calculation */

/* void InitExpt (void); */
void InitModel (void);    /* Initialize model to nominal values */
void ScaleModel (void);   /* Scale the model as defined */

void CalcDeriv (double rgModelVars[], double rgDerivs[], PDOUBLE pdTime);
void CalcJacob (double rgModelVars[], double *rgdSave[], PDOUBLE pdTime);
void CalcOutputs (double rgModelVars[], double rgDerivs[], PDOUBLE pdTime);

void CalcInputs (PDOUBLE pdTime);


/* For exchanging info with the model */

void DumpSymbolTable (char *szFilename); /* For diagnostics */

PDOUBLE GetModelVector (void);    /* Vector of model vars */
int     GetNModelVars (void);     /* Number of model vars */
int     GetNStates (void);        /* First n Model vars are states */

/* All interactions with individual variables and parameters
   of the model are managed through HANDLEs.  The variable name
   is submitted and a handle to the variable, HVAR, is returned.
   This handle can then be used to get the current value of the
   variable, or to change the value.

   Note that inputs are defined by IFN function records and thus
   have a separate assignment routine.  For inputs, GetVarValue()
   returns the current value of the input, as defined in the last
   calculation, and SetInput() takes a pointer to a defining IFN
   record.  The record is copied into the model, and is assumed to
   be valid--no verification of parameters is performed.
*/

HVAR   GetVarHandle (PSTR szName);/* Returns a handle to szName */

double GetVarValue (HVAR hVar);   /* Returns current value of hVar */
char   *GetVarName (HVAR hVar);   /* Returns the variable name of hVar */

BOOL IsInput (HVAR hVar);       /* Returns TRUE if hVar is an input */
BOOL IsState (HVAR hVar);       /* Returns TRUE if hVar is a state */
BOOL IsOutput (HVAR hVar);      /* Returns TRUE if hVar is an output */
BOOL IsModelVar (HVAR hVar);    /* Returns TRUE if hVar is a state or output */
BOOL IsParm (HVAR hVar);        /* Returns TRUE if hVar is a parm */

/* Finds time of next input transition */
void UpdateInputs (PDOUBLE pdTime, PDOUBLE pdNextTransTime);

BOOL SetInput (HVAR hVar,            /* Returns TRUE if assignment succeeds */
               PIFN pInputFnRecord); /* Function description for hvar */

BOOL SetVar (HVAR hVar,     /* Returns  TRUE if assignment succeeds */
             double dVal);  /* Value to be assigned */

int ModelIndex (HVAR hvar); /* Returns the index of a model variable */

#define _MODIFACE_INCLUDED
#endif

/* End */


