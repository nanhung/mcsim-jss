/* modelu.c

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
     Frédéric Bois / Don Maszle
     BEHS, School of Public Health
     University of California at Berkeley
     Berkeley, CA 94720

     fbois@diana.lbl.gov

   -- Revisions -----
     Logfile:  SCCS/s.modelu.c
    Revision:  1.25
        Date:  14 Nov 1997
     Modtime:  06:39:04
      Author:  @a
   -- SCCS  ---------

   Model utilities.

   This file contains a number of utility routines used by the model
   file written by the code generator.  These routines must know of
   the structure of the model file, as they manipulate the model
   variables.  Since they do not vary with the model, however, it
   makes sense to have them in their own file, instead of being
   written each time.
*/

#include <float.h> /* Floating point limits */
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define _MODEL_DEF_FILE

#include "lexerr.h"
#include "modelu.h"
#include "strutil.h"
#include "hungtype.h"


/* -----------------------------------------------------------------------------
   Model externs

   The following will be defined inside the file created by the
   code generator.  These names must match with those that will be
   written.  These should NOT be in the include file created by the
   generator as they are private variable to the model section of the
   code.
*/
extern int vnStates;
extern int vnOutputs;
extern int vnModelVars;
extern int vnInputs;
extern int vnParms;

extern double     vrgModelVars[]; /* States and Outputs */
extern IFN        vrgInputs[];    /* Input Function records */
extern VMMAPSTRCT vrgvmGlo[];     /* Global variable map */

/* WARNING! ===== GLOBAL VARIABLE ! */

/* This is used by UpdateInputs() which needs to know if the model
   has been re-initialized in order to fixup dependent inputs, and
   to set the start period heuristic. */

BOOL vbModelReinitd = FALSE;    /* Model has been initialized */


/* ----------------------------------------------------------------------------
   FixupDependentInputs

   Fixes input definitions dependent on model variables.

   This routine resolves input parameter that are dependent on model
   parameters.  If a parameter has a non-zero handle, the nominal
   value of the variable associated with that handle is used for the
   nominal value of the input parameter.  As an example, an
   exponential input might have a decay rate dependent on a model
   variable called 'Decay', which changes with each experiment.  The
   handle to 'Decay' would be the hDecay parameter of that input
   function.

   This routine also adjust variant exposure times, e.g. for
   PerExp's, the exposure time is set to N_TAU_EXPOSE Tau's.  At this
   point, the input has decayed to a neglible quantity, and we don't
   have to waste time calling the exp() function.

   After N_TAU_EXPOSE == 10 Tau's, the input has decayed to 0.0045 %
   of the magnitude.  For some reason this causes a numerical error.

   After N_TAU_EXPOSE == 40 Tau's, the input has decayed to 4.2e-18
   of the magnitude.
*/

void FixupDependentInputs()
{
  int i, j;

  for (i = 0; i < vnInputs; i++) {

    /* Resolve dependencies */

    if (vrgInputs[i].hMag)
      vrgInputs[i].dMag = GetVarValue (vrgInputs[i].hMag);
    if (vrgInputs[i].hTper)
      vrgInputs[i].dTper = GetVarValue (vrgInputs[i].hTper);
    if (vrgInputs[i].hT0)
      vrgInputs[i].dT0 = GetVarValue (vrgInputs[i].hT0);
    if (vrgInputs[i].hTexp)
      vrgInputs[i].dTexp = GetVarValue (vrgInputs[i].hTexp);
    if (vrgInputs[i].hDecay)
      vrgInputs[i].dDecay = GetVarValue (vrgInputs[i].hDecay);

    /* NDoses or Spikes dependencies */
    if ((vrgInputs[i].iType == IFN_NDOSES) || /* FB 12/10/96 */
        (vrgInputs[i].iType == IFN_SPIKES))   /* FB 11/06/97 */
      for (j = 0; j < vrgInputs[i].nDoses; j++) {
        if (vrgInputs[i].rghMags[j])
          vrgInputs[i].rgMags[j] = GetVarValue (vrgInputs[i].rghMags[j]);
        if (vrgInputs[i].rghT0s[j])
          vrgInputs[i].rgT0s[j] = GetVarValue (vrgInputs[i].rghT0s[j]);

        /* check that the times are in order */
        if (j > 0) {
          if (vrgInputs[i].rgT0s[j] <= vrgInputs[i].rgT0s[j-1]) {
            printf ("\nError: unordered pair of times (%g, %g) in %s "
                    "statement - Exiting\n", 
                    vrgInputs[i].rgT0s[j-1], vrgInputs[i].rgT0s[j],
                    (vrgInputs[i].iType == IFN_NDOSES ? "NDoses" : "Spikes"));
            exit (0);
          }
        }
      }

    /* Fix exponential exposure times for efficiency */
    if (vrgInputs[i].iType == IFN_PEREXP) {
      vrgInputs[i].dTexp = N_TAU_EXPOSE / vrgInputs[i].dDecay;
      if (vrgInputs[i].dTper < vrgInputs[i].dTexp)
        vrgInputs[i].dTexp = vrgInputs[i].dTper;
    } /* if */

    /* FB 9/9/97: Assure that input does not exceed the period */
    if (vrgInputs[i].dTexp != 0.0
        && vrgInputs[i].dT0 + vrgInputs[i].dTexp >= vrgInputs[i].dTper)
      vrgInputs[i].dTexp = vrgInputs[i].dTper - vrgInputs[i].dT0 /* - 1e-6 */;

    /* For doses and spikes allow scaling in Scale{} by use of dMag */
    if (vrgInputs[i].iType == IFN_NDOSES || vrgInputs[i].iType == IFN_SPIKES)
      vrgInputs[i].dMag = 1.0; /* Initially no scaling */

    /* If the input is ill-defined, turn it off */
    if (vrgInputs[i].iType != IFN_NDOSES && vrgInputs[i].iType != IFN_CONSTANT
        && vrgInputs[i].iType != IFN_SPIKES) {
      if (vrgInputs[i].dTexp == 0.0
          || vrgInputs[i].dT0 > vrgInputs[i].dTper
          || vrgInputs[i].dT0 < 0.0
          || vrgInputs[i].dTper < 0.0 )
        vrgInputs[i].dMag = 0.0;
    } /* if */

  } /* for */

} /* FixupDependentInputs */


/* ----------------------------------------------------------------------------
   GetStartPeriods

   Calculates the start of the current period for each input.  This
   is 0.0 if *pdTime is 0.0, otherwise we have
   to figure out where we are.  Note that this is only called
   once at the begining of a simulation and for most simulations, the
   start time will be 0.
   FB - 22 Oct 94 replaced a modf(x,dTmp) call by x - (long) (x)
*/

void GetStartPeriods (PDOUBLE pdTime)
{
  int i;
  double dTmp;

  if (*pdTime == 0.0)
    for (i = 0; i < vnInputs; i++) {
      vrgInputs[i].dTStartPeriod = 0.0;
      if (vrgInputs[i].iType == IFN_NDOSES ||
          vrgInputs[i].iType == IFN_SPIKES)
        vrgInputs[i].iDoseCur = 0;
    } /* for */

  else
    for (i = 0; i < vnInputs; i++) {
      if (vrgInputs[i].iType == IFN_NDOSES || 
          vrgInputs[i].iType == IFN_SPIKES) {
        for (vrgInputs[i].iDoseCur = 0;
             vrgInputs[i].iDoseCur < vrgInputs[i].nDoses;
             vrgInputs[i].iDoseCur++)
        if (*pdTime < vrgInputs[i].rgT0s[vrgInputs[i].iDoseCur])
          break;
        vrgInputs[i].iDoseCur--; /* The one that fails is too far */

        /* this is for possible negative times... Frederic 20/3/96 */
        if (vrgInputs[i].iDoseCur < 0) vrgInputs[i].iDoseCur = 0;
      } /* if */

      else {
        if (vrgInputs[i].dTper == 0.0) /* Not periodic - only one pulse */
          vrgInputs[i].dTStartPeriod = 0.0;
        else {
          dTmp = (*pdTime / vrgInputs[i].dTper);
          vrgInputs[i].dTStartPeriod = vrgInputs[i].dTper *
                                       (dTmp - (long) dTmp);
        } /* else */
      } /* else */
    } /* for */

} /* GetStartPeriods */


/* -----------------------------------------------------------------------------
   UpdateNDoses

   Update an NDoses type input.
*/

void UpdateNDoses (PIFN pifn, PDOUBLE pdTnext, PDOUBLE pdTime)
{
  int j;

  j = pifn->iDoseCur; /* Current pulse */

  if (j < pifn->nDoses) { /* More pulses left */
    *pdTnext = pifn->rgT0s[j];
    pifn->bOn = (*pdTime >= *pdTnext);

    if (pifn->bOn) {
      /* Look at end of pulse */
      *pdTnext = pifn->rgT0s[j+1];
      pifn->bOn = (*pdTime < *pdTnext);

      if (!pifn->bOn)
        if (++pifn->iDoseCur < pifn->nDoses) {
          *pdTnext = pifn->rgT0s[pifn->iDoseCur + 1]; /* Use next time */
          pifn->bOn = TRUE;
        }

    } /* if pulse is On */
  } /* if more pulses */
  else
    /* No pulses left, make dT unusable */
    *pdTnext = DBL_MAX;

  /* If not exposure, clear input */
  if (!pifn->bOn)
    pifn->dVal = 0.0;

} /* UpdateNDoses */


/* -----------------------------------------------------------------------------
   UpdateSpikes

   Update a Spikes type input.
*/
BOOL UpdateSpikes (PIFN pifn, PDOUBLE pdTnext, PDOUBLE pdTime)
{
  register double *rgT0s = pifn->rgT0s; /* The schedule times */
  register int j = pifn->iDoseCur; /* Index to current spike */

  *pdTnext = DBL_MAX; /* If no spikes, make dTnext infinite */
  pifn->bOn = FALSE;  /* FB 9/9/97 */
  if (j < pifn->nDoses) {
    if (*pdTime < rgT0s[j])
      *pdTnext = rgT0s[j]; /* Time of upcoming spike */

    else if (*pdTime == rgT0s[j]) {
      /* At a spike, return the time of the following spike */
      pifn->bOn = TRUE;
      if (j + 1 < pifn->nDoses)
        *pdTnext = rgT0s[j + 1];
    } /* else if */

    else /* Oops */
      printf ("\nUpdateSpikes: Discontinuity was passed over\n");
  } /* if */

  return (pifn->bOn);

} /* UpdateSpikes */


/* -----------------------------------------------------------------------------
   UpdateDefaultInput

   Update default input types.  PerRate, PerDose, PerExp.
*/
void UpdateDefaultInput (PIFN pifn, PDOUBLE pdTnext, PDOUBLE pdTime)
{
  *pdTnext = pifn->dTStartPeriod + pifn->dT0;
  pifn->bOn = (*pdTime >= *pdTnext); /* Time is after pulse goes on */

  if (pifn->bOn) {
    *pdTnext += pifn->dTexp; /* Check second transition of pulse */
    pifn->bOn = (*pdTime < *pdTnext); /* Time is before pulse goes off */

    if (!pifn->bOn) {
      if (pifn->dTper != 0.0) /* Periodic */
    *pdTnext = (pifn->dTStartPeriod += pifn->dTper); /* Next period */

      else
    *pdTnext = pifn->dTStartPeriod = DBL_MAX - pifn->dTper;
    } /* if */
  } /* if */

  if (!pifn->bOn) /* If not exposure, clear input */
    pifn->dVal = 0.0;
  else
    pifn->dVal = pifn->dMag; /* Set to exposure */

} /* UpdateDefaultInput */


/* -----------------------------------------------------------------------------
   UpdateInputs

   The controlling routine to integrator must know when inputs change
   states abruptly to correct for integration across transition
   boundaries.

   Given the current time it returns (in *pdNextTransTime) the time of the 
   next closest input transition (which is a possible discontinuity).

   It also updates information about whether inputs are on
   or off, increments the start of periods for periodic inputs.

   It helps optimize the calculation of inputs which is done inside 
   the model. It does not calculate the inputs.  CalcInputs() does that.  

   UpdateInputs() assumes that time proceeds positively.
*/
void UpdateInputs (PDOUBLE pdTime, PDOUBLE pdNextTransTime)
{
  double dT; /* New time to try */
  int i;

  if (vbModelReinitd) {         /* If model has been re-initialized */
    FixupDependentInputs ();    /* Fixup input fields dependent on vars */
    ScaleModel();               /* ...then scale the model parms */
    GetStartPeriods (pdTime);   /* Initialize the current period */
  }

  dT = *pdNextTransTime = DBL_MAX;
  for (i = 0; i < vnInputs; i++) {
    switch (vrgInputs[i].iType) {

      case IFN_CONSTANT:
        break;

      case IFN_NDOSES:
        UpdateNDoses (&vrgInputs[i], &dT, pdTime);
        break;

      case IFN_SPIKES:
        /* FB 9/9/97: make it advance only if this input spike is on.
           At the start it is off. The on/off switch is only used here,
           CalcInputs does not use it but checks that the current time is 
           equal to the spike time */
        if (vrgInputs[i].bOn)
          vrgInputs[i].iDoseCur++;

        UpdateSpikes (&vrgInputs[i], &dT, pdTime);

        break;

      default:
      case IFN_PERDOSE:
      case IFN_PERRATE:
      case IFN_PEREXP:
        if (vrgInputs[i].dMag != 0.0)
          UpdateDefaultInput (&vrgInputs[i], &dT, pdTime);
        break;

    } /* switch */

    if (dT < *pdNextTransTime)
      /* Return minimum time */
      *pdNextTransTime = dT;

  } /* for */

  if (vbModelReinitd) { /* If model has just been initialized */
    vbModelReinitd = FALSE; /* don't do it again */
  }    

} /* UpdateInputs */


/* -----------------------------------------------------------------------------
   GetVarPtr

   Returns a PVMMAPSTRCT to the structure of the variable
   specified by szName, or NULL if it does not exist.
*/

PVMMAPSTRCT GetVarPtr (PVMMAPSTRCT pvm, PSTR szName)
{
  while (*pvm->szName && MyStrcmp (szName, pvm->szName))
    pvm++;

  return (*pvm->szName ? pvm : NULL);

} /* GetVarPtr */


/* -----------------------------------------------------------------------------
   GetVarHandle

   Returns a handle to the variable szName given.  If the string
   does not reference a declared variable, returns 0.
*/

HVAR GetVarHandle (PSTR szName)
{
  PVMMAPSTRCT pvm = GetVarPtr (vrgvmGlo, szName);

  return (pvm ? pvm->hvar : ID_NULL);
} /* GetVarHandle */


/* -----------------------------------------------------------------------------
   GetVarType

   Returns the ID_ type of the HVAR given.  If hvar is a valid
   handle, returns the type of the variable, else returns 0.
*/

int GetVarType (HVAR hvar)
{
  BOOL bOK = FALSE;

  switch (HTYPE(hvar)) {
    case ID_INPUT:
      bOK = HINDEX(hvar) < vnInputs;
      break;

    case ID_STATE:
      bOK = HINDEX(hvar) < vnStates;
      break;

    case ID_OUTPUT:
      bOK = HINDEX(hvar) >= vnStates
        && HINDEX(hvar) < vnModelVars;
      break;

    case ID_PARM:
    {
      int nSOI = vnStates + vnOutputs + vnInputs;

      bOK = (HINDEX(hvar) >= nSOI) && (HINDEX(hvar) < nSOI + vnParms);
      break;
    } /* ID_PARM */

    default:
      break;
  } /* switch */

  return (bOK ? HTYPE(hvar) : ID_NULL);

} /* GetVarType */


/* -----------------------------------------------------------------------------
   GetVarName

   returns the name of HVAR given, or "InvalidVariable?" if hvar is
   not a valid handle.

   This is embarrassingly inefficient because of the screwy manner
   in which I setup the symbol table.
*/

char *GetVarName (HVAR hvar)
{
static char szInvalid[] = "InvalidVariable?";
  PVMMAPSTRCT pvm = vrgvmGlo;

  while (*pvm->szName && hvar != pvm->hvar)
    pvm++;

  return (*pvm->szName ? pvm->szName : szInvalid);
} /* GetVarType */


/* -----------------------------------------------------------------------------
   GetVarValue

   Returns the value of the HVAR given.  If hvar is not a valid
   handle, returns 0.0.
*/

double GetVarValue (HVAR hvar)
{
  double dReturn = 0.0;

  switch (GetVarType(hvar)) {
    case ID_INPUT:
      dReturn = vrgInputs[HINDEX(hvar)].dVal;
      break;

    case ID_OUTPUT:
    case ID_STATE:
      dReturn = vrgModelVars[HINDEX(hvar)];
      break;

    case ID_PARM:
    {
      PVMMAPSTRCT pvm;

      pvm = &vrgvmGlo[HINDEX(hvar)];
      dReturn = *(PDOUBLE) pvm->pVar;
      break;
    } /* ID_PARM: */

    default:
      break;
  } /* switch */

  return (dReturn);

} /* GetVarValue */


/* -----------------------------------------------------------------------------
   IsInput

   Returns TRUE if hvar is an input
*/

BOOL IsInput (HVAR hvar)
{
  return (GetVarType(hvar) == ID_INPUT);
} /* IsInput */

/* -----------------------------------------------------------------------------
   IsState

   Returns TRUE if hvar is a state
*/

BOOL IsState (HVAR hvar)
{
  return (GetVarType(hvar) == ID_STATE);
} /* IsState */


/* -----------------------------------------------------------------------------
   IsOutput

   Returns TRUE if hvar is a output
*/

BOOL IsOutput (HVAR hvar)
{
  return (GetVarType(hvar) == ID_OUTPUT);
} /* IsOutput */


/* -----------------------------------------------------------------------------
   IsModelVar

   Returns TRUE if hvar is a state or output
*/

BOOL IsModelVar (HVAR hvar)
{
  int iType = GetVarType(hvar);
  return (iType == ID_STATE || iType == ID_OUTPUT);
} /* IsState */


/* -----------------------------------------------------------------------------
   IsParm

   Returns TRUE if hvar is a parameter
*/

BOOL IsParm (HVAR hvar)
{
  return (GetVarType(hvar) == ID_PARM);
} /* IsParm */


/* -----------------------------------------------------------------------------
   ModelIndex

   Returns the index of the model variable given or 0 if hvar is not
   a model varible.  Use this with caution.
*/

int ModelIndex (HVAR hvar)
{
  return (IsModelVar(hvar) ? HINDEX(hvar) : 0);
} /* ModelIndex */


/* -----------------------------------------------------------------------------
   SetVar

   Sets the variable referenced by handle to the value dVal. Returns
   TRUE on success, FALSE if szName is not declared, or is input.
*/

BOOL SetVar (HVAR hvar, double dVal)
{
  BOOL bReturn = TRUE;

  switch (GetVarType (hvar)) {
    default:
    case ID_INPUT:
      bReturn = FALSE;
      break;

    case ID_OUTPUT:
    case ID_STATE:
      vrgModelVars[HINDEX(hvar)] = dVal;
      break;

    case ID_PARM:
    {
      PVMMAPSTRCT pvm = &vrgvmGlo[HINDEX(hvar)];
      *(PDOUBLE) pvm->pVar = dVal;
      break;
    } /* ID_PARM: */

  } /* switch */

  return (bReturn);
} /* SetVar */

/* -----------------------------------------------------------------------------
   SetInput

   Sets the input referenced by hvar to the input function record
   given.  Returns TRUE on success, FALSE if hvar does not reference
   an input.
*/

BOOL SetInput (HVAR hvar, PIFN pifn)
{
  if (!pifn || !IsInput(hvar))
    return (FALSE);

  memcpy (&vrgInputs[HINDEX(hvar)], pifn, sizeof(IFN));
  return (TRUE);
} /* SetVar */


/* -----------------------------------------------------------------------------
   GetModelVector

   Returns a pointer to the model vector.
*/

PDOUBLE GetModelVector (void)
{
  return ((PDOUBLE) vrgModelVars);
} /* GetModelVector */

/* -----------------------------------------------------------------------------
   GetNModelVars

   Returns the number of model variables,
   i.e. the size of the model vector.
*/

int GetNModelVars (void)
{
  return (vnModelVars);
} /* GetNModelVars */

/* -----------------------------------------------------------------------------
   GetNStates

   Returns the number of states, i.e. the first number of vars in
   the model vector that are states
*/

int GetNStates (void)
{
  return (vnStates);
} /* GetNStates */


/* -----------------------------------------------------------------------------
   CalcInputs

   Calculates the inputs which change with time at time dTime.  This
   routine assumes that the bExpoOn flag and dTStartPeriod time are
   correctly set by the input scheduler UpdateInputs().

   The scheduler will have set the flag during exposure periods and
   also will have set dVal current value to 0.0 for inputs that are
   off, and to dMag for constant exposure inputs.

   This routine will only calculate On inputs whose magnitude
   change with time, for example exponentials and pulses.
*/

void CalcInputs (PDOUBLE pdTime)
{
  int i;

  for (i = 0; i < vnInputs; i++) {

    /* Constants do not change, exposure must be on, or it's a spike 
       (always on) */
    if ((vrgInputs[i].iType != IFN_CONSTANT) && 
       (vrgInputs[i].bOn || (vrgInputs[i].iType != IFN_SPIKES))) {  

      switch (vrgInputs[i].iType) {
        case IFN_CONSTANT:
          break; /* dVal does not change */

        case IFN_NDOSES:
          if (vrgInputs[i].iDoseCur < vrgInputs[i].nDoses)
            vrgInputs[i].dVal = vrgInputs[i].rgMags[vrgInputs[i].iDoseCur] *
                                vrgInputs[i].dMag; /* Scaled */
          break;

        case IFN_PERDOSE:
        case IFN_PERRATE: /* Does not change step-wise */
          break;

        case IFN_PEREXP: /* Calc current exp() */
          vrgInputs[i].dVal = vrgInputs[i].dMag * 
                              exp ((vrgInputs[i].dTStartPeriod - *pdTime) * 
                                   vrgInputs[i].dDecay);
          break;

        case IFN_SPIKES:
          if ((*pdTime == vrgInputs[i].rgT0s[vrgInputs[i].iDoseCur]) &&
              (vrgInputs[i].iDoseCur < vrgInputs[i].nDoses))
          vrgInputs[i].dVal = vrgInputs[i].rgMags[vrgInputs[i].iDoseCur] *
                              vrgInputs[i].dMag; /* Scaled */

          else
            vrgInputs[i].dVal = 0;

          break;

      } /* switch */
    } /* if */
  } /* for all inputs */

} /* CalcInputs */


/* -----------------------------------------------------------------------------
   DumpSymbolTable

   prints the entire table and values.
*/

void DumpSymbolTable (char *szFilename)
{
  static char szStderr[] = "<stdout>";
  FILE *pfile;
  PVMMAPSTRCT pvm = &vrgvmGlo[0];

  if (szFilename)
    pfile = fopen(szFilename, "a");
  else {
    pfile = stdout;
    szFilename = szStderr;
  } /* else */

  if (!pfile) {
    printf ("Cannot dump symbol table to %s\n", szFilename);
    return;
  } /* if */

  fprintf (pfile, "\nSymbol Table:\n");
  if (!pvm) {
    fprintf (pfile, "<null>\n");
    return;
  } /* if */

  while (*pvm->szName) {
    fprintf (pfile, "%s \t= ", pvm->szName);
    if (IsInput(pvm->hvar)) {
      PIFN pifn = (PIFN) pvm->pVar;

      fprintf (pfile, "Mag=%g [Val=%g]\n", pifn->dMag, pifn->dVal);
    } /* if */
    else
      fprintf (pfile, "%g\n", *(double *) pvm->pVar);
    pvm++;
  } /* while */

  if (szFilename != szStderr) fclose (pfile);

} /* DumpSymbolTable */


/* ----------------------------------------------------------------------------
   GetStateHandles

   Sets up an array of state variable handles
*/

void GetStateHandles (HVAR *phvar)
{
  int i = 0;

  VMMAPSTRCT *pvm = vrgvmGlo;

  while (pvm->szName) {
    if (IsState(pvm->hvar))
      phvar[i++] = pvm->hvar;
    ++pvm;
  };

} /* GetStateHandles */

