/* lexfn.c

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
     Logfile:  SCCS/s.lexfn.c
    Revision:  1.17
        Date:  14 Nov 1997
     Modtime:  06:38:57
      Author:  @a
   -- SCCS  ---------

   Handles lexical parsing for functions.

   This file contains the GetInputFn() routine and the definition of
   input functions which are shared between the model code generator
   facility and the simulation input routines.

   This is in a separate file because it is not strictly part of the
   lexical parser, yet is a shared feature between the two programs,
   and thus should be encapsulated in one file.  Actually, the entire
   lexical [sic: it's actually a mixture of lexical and grammatical]
   parsing should be redone and written cohesively with YACC.

   The code is slightly different for the two programs, though, with
   the differences #ifdef'd using the symbol MODGEN to delimit the
   model generator specific code.  This symbol needs to be defined in
   the makefile.
*/

#include <ctype.h>
#include <float.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lex.h"
#include "lexerr.h"
#include "lexfn.h"
#include "matutil.h"
#include "modelu.h"
#include "strutil.h"

#ifdef MODGEN
#include "mod.h"
#else
#include "modiface.h"
#endif

/* -----------------------------------------------------------------------------
   Macros */

#define IsIdentifier(sz)  ((sz) ? isalpha(*(sz)) || *(sz) == '_' : FALSE)

/* If this is compiled for the model generator, the symbol table
   doesn't exist yet.  We defined it here.  GetParmHandle is
   defined to calculate the parm handle *knowing how the symbol
   table is going to be written*.  Be careful if this is monkeyed with. */

#ifdef MODGEN
HANDLE CalculateVarHandle (PVMMAPSTRCT pvm, PSTR sz);
static PVMMAPSTRCT vpvmGlo; /* Global symbol table */

#define GetParmHandle(sz)  (CalculateVarHandle(vpvmGlo, (sz)))

/* Otherwise (compiled for the simulation), the generated model file
   contains the symbol table definition and modelu.c contains a
   utility for retrieving the handle. */

#else
/* Call model utility routine */
#define GetParmHandle(sz)  (GetVarHandle((sz)))

#endif /* MODGEN */

/* Keyword Map Structure */

typedef struct tagINPUTFUNCTIONMAP {
  PSTR szName;
  int  iIFNType;  /* Input function type */
} IFM, *PIFM; /* Input function map */


IFM vrgifmMap[] = {    /* Input function map */

  {"PerDose",   IFN_PERDOSE},    /* Predef'd Input functions */
  {"PerRate",   IFN_PERRATE},
  {"PerExp",    IFN_PEREXP},
  {"NDoses",    IFN_NDOSES},
  {"Spikes",    IFN_SPIKES},

  {"", IFN_NULL} /* End flag */

};  /* vrgifnMap[] = */

/*  variables from lex.c, ZGY  */
extern PSTR vrgszLexTypes[];
extern VMMAPSTRCT vrgvmGlo[];

/* -----------------------------------------------------------------------------
   GetFnType

   Returns the code of the szKeyword given.  If the string is not
   a valid keyword or abbreviation, returns 0.
*/
int GetFnType (PSTR szName)
{
  PIFM pifm = &vrgifmMap[0];

  while (*pifm->szName && MyStrcmp (szName, pifm->szName))
    pifm++;

  return (pifm->iIFNType);  /* Return Keyword Code or 0 */

} /* GetFnType */


/* -----------------------------------------------------------------------------
   InitIFN

   Initialize an input function.
*/
void InitIFN (PIFN pifn)
{
  pifn->dTStartPeriod = 0.0;
  pifn->bOn = FALSE;

  pifn->dMag   = 0.0;  /* Initialize parameters */
  pifn->dTper  = 0.0;
  pifn->dT0    = 0.0;
  pifn->dTexp  = 0.0;
  pifn->dDecay = 0.0;
  pifn->dVal   = 0.0;
  pifn->nDoses = 0;

  pifn->hMag   = 0;    /* Initialize handles to parameters */
  pifn->hTper  = 0;
  pifn->hT0    = 0;
  pifn->hTexp  = 0;
  pifn->hDecay = 0;

  pifn->nDoses   = 0;  /* Initialize  nDoses param */
  pifn->iDoseCur = 0;

  /* The arrays and handles of nDoses will be initialized in DefDepParm */

} /* InitIFN */


/* -----------------------------------------------------------------------------
   DefDepParm

   Defines a parameter or a handle to a model parameter on which the
   parameter is dependent.  Returns TRUE on success.

   NOTE:  The call to GetParmHandle() is actually a macro that is
      dependent on whether or not the symbol MODGEN is defined,
      that is, on whether or we are compiling the model generator
      or the simulation.

      If it is the simulation, GetParmHandle() should inquire
      into the symbol table for a handle to the dependent variable.
      If it is the model generator, the symbol table has not yet
      been created and so we actually have to calculate what the
      handle will be.
*/

BOOL DefDepParm (PSTR szLex, PDOUBLE pdValue, HANDLE *phvar)
{
  BOOL bReturn = TRUE;

  if (IsIdentifier(szLex)) { /* Define handle to model parameter */

    if (!(*phvar = (HANDLE) GetParmHandle(szLex))) {
      bReturn = FALSE;
      ReportError (NULL, RE_UNDEFINED, szLex, NULL);
    }
  }
  else {
    *pdValue = atof(szLex); /* Define actual parm from number */
    *phvar = 0;
  }

  return (bReturn);

} /* DefDepParm */


/* -----------------------------------------------------------------------------
   GetInputsArgs

   Reads the arguments to the input function pifn.

   * If the argument is a number, the parameter is defined.

   * If the argument is an identifier which is a valid model
     parameter, the input function parameter is defined as
     being dependent on this model parameter.  The input parameter
     will be defined in the first UpdateInputs() call after model
     initialization.

*/
BOOL GetInputArgs (PINPUTBUF pibIn, PIFN pifn)
{
  PSTRLEX rgszLex[4];
  int rgiTypes[4], i;
  BOOL bReturn = FALSE;

  for (i = 0; i < 4; i++)
    rgiTypes[i] = LX_INTEGER | LX_FLOAT | LX_IDENTIFIER;

  if (GetFuncArgs (pibIn, 4, rgiTypes, rgszLex[0])) {

    /* Try to get each parm to show all errors */

    bReturn = TRUE;
    bReturn &= DefDepParm (rgszLex[0], &pifn->dMag, &pifn->hMag);
    bReturn &= DefDepParm (rgszLex[1], &pifn->dTper, &pifn->hTper);
    bReturn &= DefDepParm (rgszLex[2], &pifn->dT0, &pifn->hT0);

    if (pifn->iType == IFN_PEREXP)
      bReturn &= DefDepParm (rgszLex[3], &pifn->dDecay, &pifn->hDecay);
    else
      bReturn &= DefDepParm (rgszLex[3], &pifn->dTexp, &pifn->hTexp);

    if (!bReturn)
      ReportError (pibIn, RE_EXPECTED, "input-spec", NULL);
  } /* if */

  return (bReturn);

} /* GetInputArgs */


/* -----------------------------------------------------------------------------
   GetNNumbers

   Tries to read nNumbers from pibIn.  Returns TRUE on error.
*/

BOOL GetNNumbers (PINPUTBUF pibIn, PSTR szLex, int nNumbers, PDOUBLE rgd)
{
  BOOL bErr = FALSE;
  int i;

  for (i = 0; i < nNumbers  && !bErr; i++) {
    if (i)
      GetOptPunct (pibIn, szLex, ',');
    if (!(bErr = ENextLex (pibIn, szLex, LX_NUMBER)))
      rgd[i] = atof(szLex);
  } /* for */

  return bErr;

} /* GetNNumbers */


/* -----------------------------------------------------------------------------
   GetNDoses

   Reads the arguments for the NDoses() input type.
   Return TRUE if the structure is defined, FALSE on error.
   The command syntax includes the number of doses then
   the list of the dose magnitudes and then the list of
   starting times. The final time is set to DBL_MAX (i.e.
   the last dose is forever.
*/

BOOL GetNDoses (PINPUTBUF pibIn, PSTR szLex, PIFN pifn)
{
  PSTRLEX *rgszLex;
  int *rgiTypes, iType;
  int i, iDoseArg;
  BOOL bOK = TRUE;
  BOOL bErr = FALSE; /* Return value flags error condition */

  if ((bErr = EGetPunct (pibIn, szLex, CH_LPAREN)))
    goto Exit_GetNDoses;

  if ((bErr = ENextLex (pibIn, szLex, LX_INTEGER)))
    goto Exit_GetNDoses;

  pifn->nDoses = atoi(szLex);

  if ((bErr = (pifn->nDoses <= 0))) {
    ReportError (pibIn, RE_LEXEXPECTED | RE_FATAL, "positive-integer", szLex);
    goto Exit_GetNDoses;
  }

  /* iDoseArg is the number of arguments (doses and starting times)
     in NDoses after the first integer, number of doses */
  iDoseArg = 2 * pifn->nDoses;

  if ( !(rgiTypes = InitiVector (iDoseArg)))
    ReportError (pibIn, RE_OUTOFMEM | RE_FATAL, "GetNDoses", NULL);

  if ( !(rgszLex = (PSTRLEX *) malloc (iDoseArg * sizeof(PSTRLEX))))
    ReportError (pibIn, RE_OUTOFMEM | RE_FATAL, "GetNDoses", NULL);

  if ( !(pifn->rgT0s = InitdVector (pifn->nDoses + 1)) ||
       !(pifn->rgMags = InitdVector (pifn->nDoses)))
    ReportError (pibIn, RE_OUTOFMEM | RE_FATAL, "GetNDoses", NULL);

  if (!(pifn->rghT0s =
       (HANDLE *) malloc ((pifn->nDoses+1) * sizeof(unsigned int))) || 
      !(pifn->rghMags =
       (HANDLE *) malloc (pifn->nDoses * sizeof(unsigned int))))
    ReportError (pibIn, RE_OUTOFMEM | RE_FATAL, "GetNDoses", NULL);

  /* Try to get doses list: n Mag's, n T0's */

  for (i = 0; i < iDoseArg && bOK; i++) {

    rgiTypes[i] = LX_INTEGER | LX_FLOAT | LX_IDENTIFIER;

    if (!(bOK = GetOptPunct (pibIn, rgszLex[i], ','))) {
      *(rgszLex[i] + 1)  = ',';
      ReportError(pibIn, RE_EXPECTED | RE_FATAL, rgszLex[i], NULL);
      break; /* Error: Stop getting args */
    }

    NextLex (pibIn, rgszLex[i], &iType);
    if (!(bOK &= (iType & rgiTypes[i]) > 0))
      ReportError(pibIn, RE_LEXEXPECTED | RE_FATAL, vrgszLexTypes[rgiTypes[i]],
                  rgszLex[i]);
  } /* for */

  if ((bErr = EGetPunct (pibIn, szLex, CH_RPAREN)))
    goto Exit_GetNDoses;

  /* Try to get each parm to show all errors */

  bOK = TRUE;

  /* magnitudes */
  for (i = 0; i < pifn->nDoses; i++)
    bOK &= DefDepParm (rgszLex[i], pifn->rgMags + i, pifn->rghMags + i);

  /* starting times */
  for (i = 0; i < pifn->nDoses; i++)
    bOK &= DefDepParm (rgszLex[i+pifn->nDoses], pifn->rgT0s+i, pifn->rghT0s+i);

  /* final time */
  i = pifn->nDoses;
  pifn->rgT0s[i] = DBL_MAX;
  pifn->rghT0s[i] = 0;

  if (!bOK) ReportError (pibIn, RE_EXPECTED | RE_FATAL, "input-spec", NULL);

Exit_GetNDoses:

  if (bErr)
    printf ("Syntax: NDoses (nInputs, <n Magnitudes>, <n T0's>)\n\n");

  return (!bErr);

} /* GetNDoses */


/* -----------------------------------------------------------------------------
   GetSpikes

   Reads the arguments for the Spikes() input type.  Return TRUE if
   the structure is defined, FALSE on error.
*/

BOOL GetSpikes (PINPUTBUF pibIn, PSTR szLex, PIFN pifn)
{
  PSTRLEX *rgszLex;
  int *rgiTypes, iType;
  int i, iDoseArg;
  BOOL bOK = TRUE;
  BOOL bErr = FALSE;    /* Return value flags error condition */

  if ((bErr = EGetPunct (pibIn, szLex, CH_LPAREN)))
    goto Exit_GetSpikes;

  /* Get positive integer number of spikes */

  if ((bErr = ENextLex (pibIn, szLex, LX_INTEGER)))
    goto Exit_GetSpikes;

  pifn->nDoses = atoi(szLex);

  if ((bErr = (pifn->nDoses <= 0))) {
    ReportError (pibIn, RE_LEXEXPECTED | RE_FATAL, "positive-integer", szLex);
    goto Exit_GetSpikes;
  }

  /* iDoseArg is the number of arguments (doses and starting times)
     in Spikes after the first integer, number of doses */
  iDoseArg = 2 * pifn->nDoses;

  if ( !(rgiTypes = InitiVector (iDoseArg)))
    ReportError (pibIn, RE_OUTOFMEM | RE_FATAL, "GetNDoses", NULL);

  if ( !(rgszLex = (PSTRLEX *) malloc (iDoseArg * sizeof(PSTRLEX))))
    ReportError (pibIn, RE_OUTOFMEM | RE_FATAL, "GetNDoses", NULL);

  if ( !(pifn->rgT0s = InitdVector (pifn->nDoses)) ||
       !(pifn->rgMags = InitdVector (pifn->nDoses)))
    ReportError (pibIn, RE_OUTOFMEM | RE_FATAL, "GetNDoses", NULL);

  if (!(pifn->rghT0s =
       (HANDLE *) malloc ((pifn->nDoses) * sizeof(unsigned int))) || 
      !(pifn->rghMags =
       (HANDLE *) malloc (pifn->nDoses * sizeof(unsigned int))))
    ReportError (pibIn, RE_OUTOFMEM | RE_FATAL, "GetNDoses", NULL);

  /* Try to get list: n Mag's, n T0's */

  for (i = 0; i < iDoseArg && bOK; i++) {

    rgiTypes[i] = LX_INTEGER | LX_FLOAT | LX_IDENTIFIER;

    if (!(bOK = GetOptPunct (pibIn, rgszLex[i], ','))) {
      *(rgszLex[i] + 1)  = ',';
      ReportError(pibIn, RE_EXPECTED | RE_FATAL, rgszLex[i], NULL);
      break; /* Error: Stop getting args */
    }

    NextLex (pibIn, rgszLex[i], &iType);
    if (!(bOK &= (iType & rgiTypes[i]) > 0))
      ReportError(pibIn, RE_LEXEXPECTED | RE_FATAL, vrgszLexTypes[rgiTypes[i]],
                  rgszLex[i]);
  } /* for */

  if ((bErr = EGetPunct (pibIn, szLex, CH_RPAREN)))
    goto Exit_GetSpikes;

  /* Try to get each parm to show all errors */

  bOK = TRUE;

  /* magnitudes */
  for (i = 0; i < pifn->nDoses; i++)
    bOK &= DefDepParm (rgszLex[i], pifn->rgMags + i, pifn->rghMags + i);

  /* times */
  for (i = 0; i < pifn->nDoses; i++)
    bOK &= DefDepParm (rgszLex[i+pifn->nDoses], pifn->rgT0s+i, pifn->rghT0s+i);

  if (!bOK) ReportError (pibIn, RE_EXPECTED | RE_FATAL, "input-spec", NULL);

#ifdef ndef 
  /* obsolete code FB 11/6/97 */

  if ( !(pifn->rgT0s  = InitdVector (pifn->nDoses)) ||
       !(pifn->rgMags = InitdVector (pifn->nDoses)))
    ReportError (pibIn, RE_OUTOFMEM | RE_FATAL, "GetSpikes", NULL);

  /* Try to get Spike schedule -- n Mag's, n T0's */

  GetOptPunct (pibIn, szLex, ',');
  bErr = GetNNumbers (pibIn, szLex, pifn->nDoses, pifn->rgMags);

  GetOptPunct (pibIn, szLex, ',');
  if (!bErr)
    bErr = GetNNumbers (pibIn, szLex, pifn->nDoses, pifn->rgT0s);

  if (!bErr)
    bErr = EGetPunct (pibIn, szLex, CH_RPAREN);

#endif

Exit_GetSpikes:

  if (bErr)
    printf ("Syntax: Spikes (nInputs, <n Magnitudes>, <n Times>)\n\n");

  return (!bErr);

} /* GetSpikes */


/* -----------------------------------------------------------------------------
   GetInputFn

   Attempts to define an IFN structure pifn according to the
   input spec in sz if sz is non-NULL, or pibIn if sz is NULL.
   Returns TRUE if the structure is defined.
*/


BOOL GetInputFn (PINPUTBUF pibIn, PSTR sz, PIFN pifn)
{
  INPUTBUF ibDummy;
  PINPUTBUF pibDum = &ibDummy;
  PSTRLEX szLex;
  int iType;
  BOOL bReturn = FALSE;

  if (!pibIn || !pifn)
    return (FALSE);

/*
  If this is being compiled for the model generator, we need to define
  a global variable map so that we can access the symbols.  In the
  simulation compilation, the symbol table is defined already in the
  generated model file.
*/

#ifdef MODGEN
  {
    PINPUTINFO pinfo = (PINPUTINFO) pibIn->pInfo;

    vpvmGlo = pinfo->pvmGloVars;
  } /* block */
#endif

  if (sz)
    MakeStringBuffer (pibIn, pibDum, sz);
  else
    pibDum = pibIn;

  NextLex (pibDum, szLex, &iType);
  switch (iType) {

  default:
  case LX_NULL:
  case LX_PUNCT:
    ReportError (pibIn, RE_LEXEXPECTED, "input-spec", NULL);
    break;

  case LX_FLOAT:
  case LX_INTEGER:
  case LX_IDENTIFIER:
    InitIFN (pifn);

    if (iType == LX_IDENTIFIER) {
      pifn->iType = GetFnType (szLex);
      switch (pifn->iType) {

      case IFN_NDOSES:
        bReturn = GetNDoses (pibDum, szLex, pifn);
        break;

      case IFN_SPIKES:
        bReturn = GetSpikes (pibDum, szLex, pifn);
        break;

      default:
        pifn->iType = IFN_NULL;
        ReportError (pibIn, RE_LEXEXPECTED, "input-spec", szLex);
        break;

      case IFN_PERDOSE:
      case IFN_PERRATE:
      case IFN_PEREXP:
        bReturn = GetInputArgs (pibDum, pifn);
        break;

      } /* switch */
    } /* if identifier */

    else {
      pifn->iType = IFN_CONSTANT;
      pifn->dMag = pifn->dVal = atof (szLex);
      pifn->bOn = TRUE;
      bReturn = TRUE;
    } /* else */
    break;

  } /* switch */

  return (bReturn);
} /* GetInputFn */
