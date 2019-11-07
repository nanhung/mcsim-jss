/* simi.c

   written by Don Maszle
   16 October 1991

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
     Logfile:  SCCS/s.simi.c
    Revision:  1.48
        Date:  14 Nov 1997
     Modtime:  06:39:07
      Author:  @a
   -- SCCS  ---------

   Contains input routines for simulation.

*/

#include <assert.h>
#include <ctype.h>
#include <float.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lexerr.h"
#include "list.h"
#include "simi.h"
#include "siminit.h"
#include "simmonte.h"

PSTRLEX vrgszlexArgs[ARGS_MAX];

/* Keyword Map Structure */

typedef struct tagKM {
  PSTR szKeyword;
  int  iKWCode;   /* Enumeration code of Keyword KM_* */
  WORD fContext;  /* Bit flags of valid context for KW */
} KM, *PKM; /* Keyword Map */

KM vrgkmKeywordMap[] = { /* Global Keyword - code map */

  /* Simulation syntax */

  {"Level",         KM_LEVEL,       CN_GLOBAL},
  {"Experiment",    KM_EXPERIMENT,  CN_GLOBAL},

  {"OutputFile",    KM_OUTPUTFILE,  CN_GLOBAL},    /* Regular out file */

  {"Gibbs",         KM_MCMC,        CN_GLOBAL},    /* Gibbs estim spec */
  {"MCMC",          KM_MCMC,        CN_GLOBAL},    /* Metrop estim spec */
  {"OptimDesign",   KM_OPTDESIGN,   CN_GLOBAL},    /* Optimal design spec */
  {"MonteCarlo",    KM_MONTECARLO,  CN_GLOBAL},    /* Monte Carlo spec */
  {"Distrib",       KM_MCVARY,      CN_GLOBAL},    /* MC Variable mod */
  {"MCVary",        KM_MCVARY,      CN_GLOBAL},    /* Obsolete!! */
  {"SetPoints",     KM_SETPOINTS,   CN_GLOBAL},    /* Forced point runs*/

  {"Integrate",     KM_INTEGRATE,   CN_GLOBAL | CN_EXPERIMENT},
  {"StartTime",     KM_SIMULATE,    CN_GLOBAL | CN_EXPERIMENT},

  {"Print",         KM_PRINT,       CN_EXPERIMENT},
  {"PrintStep",     KM_PRINTSTEP,   CN_EXPERIMENT},
  {"Data",          KM_DATA,        CN_EXPERIMENT},

  /* If a type is not seen before the first section is found, that section
     becomes the type.  Subsequent statements are ignored. */
  {"SimType",       KM_SIMTYPE,     CN_GLOBAL},    /* Optional SimType */

  {"End",           KM_END,         CN_GLOBAL},    /* Optional End statement */
  {"END",           KM_END,         CN_GLOBAL},    /* Optional End statement */

  /* Function arguments */

  {"DefaultSim",    KM_DEFAULTSIM,  CN_FUNCARG},   /* For SimType() only */

  {"No",            KM_NO,          CN_FUNCARG},   /* Use YesNoFromLex() */
  {"Yes",           KM_YES,         CN_FUNCARG},

  {"Uniform",       KM_UNIFORM,     CN_FUNCARG},   /* Use McvFromLex() */
  {"LogUniform",    KM_LOGUNIFORM,  CN_FUNCARG},
  {"Beta",          KM_BETA,        CN_FUNCARG},
  {"Normal",        KM_NORMAL,      CN_FUNCARG},
  {"LogNormal",     KM_LOGNORMAL,   CN_FUNCARG},
  {"TruncNormal",   KM_TRUNCNORMAL, CN_FUNCARG},
  {"TruncLogNormal",KM_TRUNCLOGNORMAL,    CN_FUNCARG},
  {"Chi2",          KM_CHI2,              CN_FUNCARG},
  {"Binomial",      KM_BINOMIAL,          CN_FUNCARG},
  {"Piecewise",     KM_PIECEWISE,         CN_FUNCARG},
  {"Exponential",   KM_EXPONENTIAL,       CN_FUNCARG},
  {"Gamma",         KM_GGAMMA,            CN_FUNCARG},
  {"Poisson",       KM_POISSON,           CN_FUNCARG},
  {"InvGamma",      KM_INVGGAMMA,         CN_FUNCARG},
  {"Normal_v",        KM_NORMALV,         CN_FUNCARG},
  {"LogNormal_v",     KM_LOGNORMALV,      CN_FUNCARG},
  {"TruncNormal_v",   KM_TRUNCNORMALV,    CN_FUNCARG},
  {"TruncLogNormal_v",KM_TRUNCLOGNORMALV, CN_FUNCARG},
  {"BinomialBeta",    KM_BINOMIALBETA,    CN_FUNCARG},
  
  {"Prediction",    KM_PREDICTION,        CN_FUNCARG},

  {"Euler",         KM_EULER,             CN_FUNCARG},
  {"Lsodes",        KM_LSODES,            CN_FUNCARG},

  {"Forward",       KM_FORWARD,           CN_FUNCARG},
  {"Backward",      KM_BACKWARD,          CN_FUNCARG},

  /* Variables names valid in all CN_ */

  {"", 0, CN_ALL} /* End flag */

}; /* vrgkmKeywordMap[] */


/* -----------------------------------------------------------------------------
   GetKeywordCode

   Returns the code of the szKeyword given.  If the string is not
   a valid keyword or abbreviation, returns 0.

   If pfContext is non-NULL, contexts in which the code is valid is
   returned here.
*/

int GetKeywordCode (PSTR szKeyword, PINT pfContext)
{
  PKM pkm = &vrgkmKeywordMap[0];

  while (*pkm->szKeyword && MyStrcmp (szKeyword, pkm->szKeyword))
    pkm++;

  if (pfContext)
    *pfContext = pkm->fContext;        /* Set iContext flag */
  return (pkm->iKWCode);        /* Return Keyword Code or 0 */

} /* GetKeywordCode */


/* -----------------------------------------------------------------------------
   GetKeyword

   Returns the first string of the KM_ keyword map code given.  If the
   code is not a valid keyword code, returns NULL.
*/

PSTR GetKeyword (int iKWCode)
{
  PKM pkm = &vrgkmKeywordMap[0];

  while (*pkm->szKeyword && iKWCode != pkm->iKWCode)
    pkm++;

  return (pkm->szKeyword);        /* Return Keyword Code or 0 */

} /* GetKeyword */


/* -----------------------------------------------------------------------------
   YesNoFromLex

   Converts an string input argument into a Boolean,
   Yes being TRUE and No being FALSE.  Also, a numeric argument
   is converted to Yes if it is non-zero.
*/

BOOL YesNoFromLex (PSTR szLex)
{
  int ikwcode = GetKeywordCode (szLex, NULL);
  BOOL bReturn;

  bReturn = (!isalpha(szLex[0]) ? atoi(szLex)
             : ikwcode == KM_YES ? TRUE
             : ikwcode == KM_NO ? FALSE
             : FALSE);

  return bReturn;

} /* YesNoFromLex */


/* -----------------------------------------------------------------------------
   ImFromLex

   Converts an string input argument into the correct IM_
   integration method.
*/

long ImFromLex (PSTR szLex)
{
  int ikwcode = GetKeywordCode (szLex, NULL);
  long lReturn;

  lReturn = (!isalpha(szLex[0]) ? atoi(szLex)
            : ikwcode == KM_LSODES ? IAL_LSODES
            : ikwcode == KM_EULER  ? IAL_EULER
            : 0);

  if (!lReturn) {
    printf ("Warning: Unknown integrator specification (%s) -\n"
            "         Switching to Lsodes with default options\n\n", szLex);
    lReturn = IAL_DEFAULT;
  }

  return (lReturn);

} /* ImFromLex */


/* -----------------------------------------------------------------------------
   McvFromLex

   Converts a string input argument into the correct MCV_
   Monte Carlo variation distribution type.
*/

int McvFromLex (PSTR szLex)
{
  int ikwcode = GetKeywordCode (szLex, NULL);
  int iReturn;

  iReturn = (ikwcode == KM_UNIFORM          ? MCV_UNIFORM
             : ikwcode == KM_LOGUNIFORM     ? MCV_LOGUNIFORM
             : ikwcode == KM_BETA           ? MCV_BETA
             : ikwcode == KM_NORMAL         ? MCV_NORMAL
             : ikwcode == KM_LOGNORMAL      ? MCV_LOGNORMAL
             : ikwcode == KM_TRUNCNORMAL    ? MCV_TRUNCNORMAL
             : ikwcode == KM_TRUNCLOGNORMAL ? MCV_TRUNCLOGNORMAL
             : ikwcode == KM_CHI2           ? MCV_CHI2
             : ikwcode == KM_BINOMIAL       ? MCV_BINOMIAL
             : ikwcode == KM_PIECEWISE      ? MCV_PIECEWISE
             : ikwcode == KM_EXPONENTIAL    ? MCV_EXPONENTIAL
             : ikwcode == KM_GGAMMA         ? MCV_GGAMMA
             : ikwcode == KM_POISSON        ? MCV_POISSON
             : ikwcode == KM_INVGGAMMA      ? MCV_INVGGAMMA
             : ikwcode == KM_NORMALV        ? MCV_NORMALV
             : ikwcode == KM_LOGNORMALV     ? MCV_LOGNORMALV
             : ikwcode == KM_TRUNCNORMALV   ? MCV_TRUNCNORMALV
             : ikwcode == KM_TRUNCLOGNORMALV? MCV_TRUNCLOGNORMALV
             : ikwcode == KM_BINOMIALBETA   ? MCV_BINOMIALBETA
             : (-1));

  return iReturn;

} /* McvFromLex */


/* ----------------------------------------------------------------------------
   GetTerminator

   Tries to read a statement terminator.  Reports Errors.
*/

int GetTerminator (PINPUTBUF pibIn, PSTR szLex)
{
  int iErr;

  if ((iErr = !GetPunct (pibIn, szLex, CH_STMTTERM))) {
    szLex[1] = CH_STMTTERM;
    ReportError (pibIn, RE_EXPECTED, szLex, NULL);
  }

  return (iErr);

} /* GetTerminator */


/* -----------------------------------------------------------------------------
*/

BOOL GetSimType (PINPUTBUF pibIn)
{
#define NAT_ARGS 1     /* the type */

  static int vrgiAtArgTypes[NAT_ARGS] = {LX_IDENTIFIER};

  PANALYSIS panal = (PANALYSIS) pibIn->pInfo;

  int  iAT = AT_DEFAULTSIM;
  int  iKwCode = 0;
  BOOL bErr=!GetFuncArgs (pibIn, NAT_ARGS, vrgiAtArgTypes, vrgszlexArgs[0]);

  if (!bErr) {
    iKwCode = GetKeywordCode (vrgszlexArgs[0], NULL);
    switch (iKwCode) {

    case KM_MONTECARLO:
      iAT = AT_MONTECARLO;
      break;

    case KM_SETPOINTS:
      iAT = AT_SETPOINTS;
      break;

    case KM_MCMC:
      iAT = AT_MCMC;
      break;

    case KM_OPTDESIGN:
      iAT = AT_OPTDESIGN;
      break;

    case KM_DEFAULTSIM:
      iAT = AT_DEFAULTSIM;
      break;

    default:
      ReportError (pibIn, RE_SPECERR | RE_FATAL, "Unknown SimType ",
                   vrgszlexArgs[0]);
      break;
    } /* switch */
  } /* if */
  else
    printf ("Syntax: %s (Normal | MonteCarlo | SetPoints | MCMC)\n"
         "  -- if not specified, the first spec section will be used.\n\n",
         GetKeyword(KM_SIMTYPE));

  if (!bErr) {
    if (!panal->iType)
      panal->iType = iAT;
    else
      printf ("** Ignoring SimType() specification.\n"
           "   Place SimType() spec before all section specifications.");
  } /* if */

  return (bErr);

} /* GetSimType */


/* -----------------------------------------------------------------------------
   GetIntegrate
 */

BOOL GetIntegrate (PINPUTBUF pibIn, PINTSPEC pis)
{
#define NINT_ARGS 4 /* Four integrator Args at max */

  static int vrgiIntArgTypes[NINT_ARGS]
    = {LX_IDENTIFIER, LX_NUMBER, LX_NUMBER, LX_INTEGER};

  BOOL bErr=!GetFuncArgs (pibIn, NINT_ARGS, vrgiIntArgTypes, vrgszlexArgs[0]);

  if (!bErr) {
    pis->iAlgo = ImFromLex (vrgszlexArgs[0]);

    if (pis->iAlgo  == IAL_LSODES) {
      pis->dRtol = atof(vrgszlexArgs[1]);
      pis->dAtol = atof(vrgszlexArgs[2]);
      pis->iMf   = atoi(vrgszlexArgs[3]);

      /* the following lines change input iMf to orginal lsodes args */
      if (pis->iMf == 0) pis->iMf = 10;
      else
        if (pis->iMf == 1) pis->iMf = 222;
        else {
          printf ("Error: method flag must be 0 or 1 for Lsodes - ");
          printf ("Exiting\n");
          exit (0);
        }

      pis->iDSFlag = 1;
    }
    else {
      if (pis->iAlgo  == IAL_EULER) {
        pis->dTStep = atof(vrgszlexArgs[1]);
        if (pis->dTStep <= 0)
          printf ("Warning: Time step specified is null or negative -\n"
                  "         Resetting to 1\n\n");
      }
      else {
        printf ("Error: Unknown integration method: %s - Exiting\n\n",
                vrgszlexArgs[0]);
        exit (0);
      }
    } /* if */
  } /* if */
  else {
    printf ("Syntax: %s (Lsodes, Relative tolerance, Absolute tolerance, "
            "Method)\n"
            "        or %s (Euler, Time step, 0, 0)\n\n",
            GetKeyword (KM_INTEGRATE), GetKeyword (KM_INTEGRATE));
    exit (0);
  }

  return (bErr);

} /* GetIntegrate */


/* -----------------------------------------------------------------------------
   OneDToArray

   Copies one double from the list to the newly formed array.
   Increments the info pointer which is the pointer into the array.
*/

int OneDToArray (PVOID pData, PVOID pInfo)
{
  PDOUBLE *ppdArrayVal = (PDOUBLE *) pInfo;

  *(*ppdArrayVal)++ = *(PDOUBLE) pData;

  return 0;

} /* OneDToArray */


/* -----------------------------------------------------------------------------
   DListToArray

   Converts a list a doubles to an array of doubles.  *pcDouble is
   the count of doubles in the array, and *ppDouble is the array
   pointer.
*/

void DListToArray (PLIST plist, PINT pcDouble, PDOUBLE *ppDouble)
{
  PDOUBLE pdTmp; /* Temp pointer to get incremented */

  *pcDouble = ListLength(plist);

  if ( !(pdTmp = *ppDouble = InitdVector (*pcDouble)))
    ReportError (NULL, RE_OUTOFMEM | RE_FATAL, "DListToArray", NULL);

  ForAllList (plist, &OneDToArray, (PVOID) &pdTmp);

} /* DListToArray */


/* -----------------------------------------------------------------------------
   GetListOfTimes

   Reads an arbitrary length list of times and closing parenthesis.
   Defines the count and array of times in the PRINTREC structure.
*/

BOOL GetListOfTimes (PINPUTBUF pibIn, int nRecs, PPRINTREC *ppr, PSTR szLex)
{
  PLIST plistTimes = InitList();
  PDOUBLE pdTmp;
  int iNLI, i, j;
  BOOL bErr;

  do {
    if ( !(pdTmp = InitdVector (1)))
      ReportError (NULL, RE_OUTOFMEM | RE_FATAL, "GetListOfTimes", NULL);

    *pdTmp = atof(szLex);
    QueueListItem (plistTimes, (PVOID) pdTmp);
  } while ((iNLI = NextListItem (pibIn, szLex, LX_NUMBER, 1, CH_RPAREN)) > 0);

  if (!iNLI) /* List terminator */
    bErr = EGetPunct (pibIn, szLex, CH_RPAREN) || !ListLength(plistTimes);
  else {
    bErr = TRUE;
    ReportError (pibIn, RE_LEXEXPECTED, "number", szLex);
  }

  if (!bErr) 
    for(i = 0; i < nRecs; ++i)
      DListToArray (plistTimes, &ppr[i]->cTimes, &ppr[i]->pdTimes);

  FreeList (&plistTimes, NULL, TRUE); /* Free list and cells */

  for (i = 1; i < ppr[0]->cTimes && !bErr; i++) /* Verify Times */
    if ((bErr = (*(ppr[0]->pdTimes+i) <= *(ppr[0]->pdTimes+i-1)))) {
      for(j = 0; j < nRecs; ++j)
        free (ppr[j]->pdTimes);
      ReportError (pibIn, RE_SPECERR | RE_FATAL, "Times out of order", NULL);
    } /* if */

  return (bErr);

} /* GetListOfTimes */


/* -----------------------------------------------------------------------------
   GetListOfData

   Reads an arbitrary length list of data and closing parenthesis.
   Defines the count and array of data in the DATAREC structure.
*/

BOOL GetListOfData (PINPUTBUF pibIn, PDATAREC pda, PSTR szLex)
{
  PLIST plistData = InitList();
  PDOUBLE pdTmp;
  int iNLI;
  BOOL bErr;

  while ((iNLI = NextListItem (pibIn, szLex, LX_NUMBER, 1, CH_RPAREN))
         > 0) {
    if ( !(pdTmp = InitdVector (1)))
      ReportError (pibIn, RE_OUTOFMEM | RE_FATAL, "GetListOfData", NULL);

    *pdTmp = atof(szLex);
    QueueListItem (plistData, (PVOID) pdTmp);

  } /* while */

  if (!iNLI) /* List terminator */
    bErr = EGetPunct (pibIn, szLex, CH_RPAREN)
           || !ListLength(plistData);
  else {
    bErr = TRUE;
    ReportError (pibIn, RE_LEXEXPECTED, "number", szLex);
  } /* else */

  if (!bErr) DListToArray (plistData, &pda->cData, &pda->pdData);

  FreeList (&plistData, NULL, TRUE); /* Free list and cells */
  return (bErr);

} /* GetListOfData */


/* -----------------------------------------------------------------------------
   GetPrint

   Gets the arguments to a Print() statement. Put them in
   a list plistPrintRecs of PRINTREC structures
*/

BOOL bGavePrintUsage = FALSE; /* prevent multiple diagnostics */

BOOL GetPrint (PINPUTBUF pibIn, PSTR szLex, POUTSPEC pos)
{
  PPRINTREC pprintrec[MAX_PRINT_VARS];
  BOOL bErr = FALSE;
  HVAR hvar;
  int nVars = 0, n, iLex;

  if (!(bErr = EGetPunct (pibIn, szLex, CH_LPAREN))) {
    for(;;) {
      NextLex(pibIn, szLex, &iLex);
      if (iLex != LX_IDENTIFIER)
        break;
      if(nVars == MAX_PRINT_VARS)
        ReportError(pibIn, RE_TOOMANYPVARS | RE_FATAL, "GetPrint", NULL);
      if ((bErr = !(hvar = GetVarHandle (szLex))))
        ReportError (pibIn, RE_UNDEFINED | RE_FATAL, szLex, NULL);
      else {
        if ( !(pprintrec[nVars] = (PPRINTREC) malloc (sizeof(PRINTREC))) ||
             !(pprintrec[nVars]->szOutputName =
              (PSTR) malloc (MyStrlen(szLex)+1)))
          ReportError (pibIn, RE_OUTOFMEM | RE_FATAL, "GetPrint", NULL);
        MyStrcpy (pprintrec[nVars]->szOutputName, szLex);
        pprintrec[nVars]->hvar = hvar;
        assert(pprintrec[nVars]);
        ++nVars;
      }
      GetOptPunct(pibIn, szLex, ',');
    }

    if (nVars < 1)
      ReportError (pibIn, RE_LEXEXPECTED, "identifier", szLex);

    bErr = GetListOfTimes (pibIn, nVars, pprintrec, szLex);

    if (bErr) {
      for(n = 0; n < nVars; ++n) {
        free (pprintrec[n]->szOutputName);
        free (pprintrec[n]);
      }
    }
    else
      for(n = 0; n < nVars; ++n)
        QueueListItem (pos->plistPrintRecs, (PVOID) pprintrec[n]);
  } /* if */

  if (!bErr) bErr = GetTerminator (pibIn, szLex);
  else {
    if (!bGavePrintUsage) {
      printf ("Syntax: %s (identifier, Time1, Time2, ...)\n\n",
              GetKeyword(KM_PRINT));
      bGavePrintUsage = TRUE;
    }
  } /* else */

  return (bErr);

} /* GetPrint */


/* -----------------------------------------------------------------------------
   GetPrintStep

   Gets the arguments to a PrintStep() statement. They are: an identifier,
   a start time, an end time, a time step. If the time period is not congruent
   with the time step the last step will be shorter.
*/

BOOL bGavePrintStepUsage = FALSE; /* prevent multiple diagnostics */

BOOL GetPrintStep (PINPUTBUF pibIn, PSTR szLex, POUTSPEC pos)
{
  PPRINTREC pprintrec;
  HVAR hvar = 0;
  double dStart = 0, dEnd = 0, dStep = 0;
  long i;

  static int vrgiIntArgTypes[4] /* 3 PrintStep arguments */
             = {LX_IDENTIFIER, LX_NUMBER, LX_NUMBER, LX_NUMBER};

  BOOL bErr =! GetFuncArgs (pibIn, 4, vrgiIntArgTypes, vrgszlexArgs[0]);

  if (!bErr)
    if ((bErr = !(hvar = GetVarHandle (vrgszlexArgs[0]))))
      ReportError (pibIn, RE_UNDEFINED, vrgszlexArgs[0], NULL);
    else {
      dStart = atof(vrgszlexArgs[1]);
      dEnd   = atof(vrgszlexArgs[2]);
      dStep  = atof(vrgszlexArgs[3]);

      /* check times for consistency */
      if ((bErr = (dEnd <= dStart)))
        ReportError (pibIn, RE_SPECERR, "End_time must be > Start_time", NULL);
      else if ((bErr = (dStep > (dEnd - dStart))))
        ReportError (pibIn, RE_SPECERR, "Time_step too large", NULL);
    } /* else */

  if (!bErr) {
    if ( !(pprintrec = (PPRINTREC) malloc (sizeof(PRINTREC))))
      ReportError (pibIn, RE_OUTOFMEM | RE_FATAL, "GetPrintStep", NULL);

    if ( !(pprintrec->szOutputName =
           (PSTR) malloc (MyStrlen(vrgszlexArgs[0])+1)))
      ReportError (pibIn, RE_OUTOFMEM | RE_FATAL, "GetPrintStep", NULL);

    MyStrcpy (pprintrec->szOutputName, vrgszlexArgs[0]);
    assert(pprintrec);

    pprintrec->hvar = hvar;

    pprintrec->cTimes = 1 + ceil((dEnd - dStart) / dStep);

    if ( !(pprintrec->pdTimes = InitdVector (pprintrec->cTimes)))
      ReportError (pibIn, RE_OUTOFMEM | RE_FATAL, "GetPrintStep", NULL);

    for (i = 0; i < pprintrec->cTimes - 1; i++)
      pprintrec->pdTimes[i] = dStart + (i * dStep);

    pprintrec->pdTimes[pprintrec->cTimes - 1] = dEnd;

    QueueListItem (pos->plistPrintRecs, (PVOID) pprintrec);
  }
  else { /* error in the arguments */
    if (!bGavePrintStepUsage) {
      printf ("Syntax: %s (identifier, Start_time, End_time, Time_step)\n\n",
              GetKeyword(KM_PRINTSTEP));
      bGavePrintStepUsage = TRUE;
    } /* if */
  } /* else */

  return (bErr);

} /* GetPrintStep */


/* -----------------------------------------------------------------------------
   GetData

   Gets the arguments to a Data() statement
*/

BOOL bGaveDataUsage = FALSE; /* prevent multiple diagnostics */

BOOL GetData (PINPUTBUF pibIn, PSTR szLex, POUTSPEC pos)
{
  PDATAREC pdatarec;
  BOOL bErr = FALSE;
  HVAR hvar;

  if (!(bErr = EGetPunct (pibIn, szLex, CH_LPAREN))) {
    if (!(bErr = ENextLex (pibIn, szLex, LX_IDENTIFIER))) {

      if ((bErr = !(hvar = GetVarHandle (szLex))))
        ReportError (pibIn, RE_UNDEFINED, szLex, NULL);

      else {
        if ( !(pdatarec = (PDATAREC) malloc (sizeof(DATAREC))))
          ReportError (pibIn, RE_OUTOFMEM | RE_FATAL, "GetData", NULL);

        if ( !(pdatarec->szDataName = (PSTR) malloc (MyStrlen(szLex)+1)))
          ReportError (pibIn, RE_OUTOFMEM | RE_FATAL, "GetData", NULL);

        MyStrcpy (pdatarec->szDataName, szLex);
        assert(pdatarec);

        pdatarec->hvar = hvar;

        bErr = GetListOfData (pibIn, pdatarec, szLex);

        if (bErr) {
          free (pdatarec->szDataName);
          free (pdatarec);
        } /* if */
        else
          QueueListItem (pos->plistDataRecs, (PVOID) pdatarec);
      } /* else */
    } /* if */
  } /* if */

  if (!bErr) bErr = GetTerminator (pibIn, szLex);
  else {
    if (!bGaveDataUsage) {
      printf ("Syntax: %s (identifier, Time1, Time2, ...)\n\n",
               GetKeyword(KM_DATA));
      bGaveDataUsage = TRUE;
    } /* if */
  } /* else */

  return (bErr);

} /* GetData */


/* -----------------------------------------------------------------------------
   GetStringArg

   tries to read a string argument from pibIn and assign it to
   *pszArg.  If pszArg is NULL, the argument is read, but no
   assigment is made.  If pszArg is not NULL, space is allocated for
   argument read.  szLex is a workspace.  If bDelim is TRUE, a
   delimiter is skipped in the input buffer.

   The return value is TRUE for error.  Errors are reported.
*/

BOOL GetStringArg (PINPUTBUF pibIn, PSTR *pszArg, PSTR szLex, BOOL bDelim)
{
  BOOL bErr;

  assert (szLex); /* Workspace must be given */

  if (bDelim)
    GetOptPunct (pibIn, szLex, ',');

  bErr = ENextLex (pibIn, szLex, LX_STRING);

  if (!bErr) {
    if (szLex[0]) {
      /* Allocate and copy the string */
      if ( !(*pszArg = (PSTR) malloc (MyStrlen(szLex) + 1)))
        ReportError (pibIn, RE_OUTOFMEM | RE_FATAL, "GetStringArg", NULL);

      MyStrcpy (*pszArg, szLex);
    } /* if */
    else
      *pszArg = NULL; /* No string given */
  } /* if */

  return (bErr);

} /* GetStringArg */


/* -----------------------------------------------------------------------------
   GetOutputFile

   Use a name different from the default for the regular output
*/

BOOL GetOutputFile (PINPUTBUF pibIn, PSTR szLex, POUTSPEC pos)
{
  BOOL bErr = FALSE;

  bErr = EGetPunct (pibIn, szLex, CH_LPAREN)
         || GetStringArg (pibIn, &pos->szOutfilename, szLex, FALSE);

  if (!bErr)
    bErr = EGetPunct (pibIn, szLex, CH_RPAREN);

  if (!bErr)
    bErr = GetTerminator (pibIn, szLex);
  else
    printf ("Syntax: %s (szOutputFilename)\n\n",
         GetKeyword (KM_OUTPUTFILE));

  return (bErr);

} /* GetOutputFile */


/* ----------------------------------------------------------------------------
*/

BOOL bGaveSimulateUsage = FALSE; /* prevent multiple diagnostics */

BOOL GetSimulate (PINPUTBUF pibIn, PEXPERIMENT pexp)
{
#define NSIM_ARGS 1     /* One Arg */

static int vrgiSimArgTypes[NSIM_ARGS] = {LX_NUMBER};

  BOOL bErr=!GetFuncArgs (pibIn, NSIM_ARGS, vrgiSimArgTypes, vrgszlexArgs[0]);

  if (!bErr) {
    pexp->dT0 = atof(vrgszlexArgs[0]);
  } /* if */
  else {
    if (!bGaveSimulateUsage) {
      printf ("Syntax: %s (InitialTime)\n\n", GetKeyword (KM_SIMULATE));
      bGaveSimulateUsage = TRUE;
    } /* if */
  } /* else */

  return (bErr);

} /* GetSimulate */


/* -----------------------------------------------------------------------------
   GetGibbsSpec

   get the Gibbs specification.
*/

BOOL GetGibbsSpec (PINPUTBUF pibIn, PEXPERIMENT pexp)
{
#define NGIBBS_ARGS 8 /* # Func args to gibbs spec */

static int vrgiGibbsArgTypes[NGIBBS_ARGS] = { LX_STRING, LX_STRING, LX_STRING,
                                              LX_INTEGER, LX_INTEGER, 
                                              LX_INTEGER, LX_INTEGER, 
                                              LX_NUMBER};

  PANALYSIS panal = (PANALYSIS) pibIn->pInfo;

  BOOL bErr= !GetFuncArgs(pibIn, NGIBBS_ARGS,
                          vrgiGibbsArgTypes,vrgszlexArgs[0]);

  static char vszGibbsOutDefault[] = "MCMC.default.out";

  if (!bErr) {
    if (*vrgszlexArgs[0]) { /* Get output Filename */
      if ( !(panal->gd.szGout = (PSTR)malloc(MyStrlen(vrgszlexArgs[0]) + 1)))
        ReportError (pibIn, RE_OUTOFMEM | RE_FATAL, "GetGibbsSpec", NULL);

      MyStrcpy (panal->gd.szGout, vrgszlexArgs[0]);
    }
    else panal->gd.szGout = vszGibbsOutDefault;

    if (*vrgszlexArgs[1]) { /* Get restart file */
      if ( !(panal->gd.szGrestart =
            (PSTR) malloc (MyStrlen(vrgszlexArgs[1]) + 1)))
        ReportError (pibIn, RE_OUTOFMEM | RE_FATAL, "GetGibbsSpec", NULL);

      MyStrcpy (panal->gd.szGrestart, vrgszlexArgs[1]);
    }
    if (panal->gd.szGrestart != NULL &&
        !strcmp(panal->gd.szGout, panal->gd.szGrestart))
      ReportError (pibIn, RE_OUTISRESTART | RE_FATAL, "GetGibbsSpec", NULL);

    if (*vrgszlexArgs[2]) { /* Get Exter Data Filename */
      if ( !(panal->gd.szGdata = (PSTR)malloc(MyStrlen(vrgszlexArgs[2]) + 1)))
        ReportError (pibIn, RE_OUTOFMEM | RE_FATAL, "GetGibbsSpec", NULL);

      MyStrcpy (panal->gd.szGdata, vrgszlexArgs[2]);
    }

    panal->gd.nMaxIter = atol(vrgszlexArgs[3]);
    panal->gd.nInitIter = atol(vrgszlexArgs[4]);
    panal->gd.nPrintFreq = atol(vrgszlexArgs[5]);
    panal->gd.nPrintIter = atol(vrgszlexArgs[6]);

    panal->dSeed = atof(vrgszlexArgs[7]);

    if (panal->gd.nInitIter && (panal->gd.szGrestart == NULL)) {
      printf ("Error: if printPredFlag is not zero a restart file must be "
              "given - Exiting\n\n");
      exit (0);
    }
  } /* if */
  else
    printf ("Syntax: %s (szOut, szRestart, szDat, \n"
            "nMaxIters, [0,1], nPrintFreq, nIterToPrint, dSeed)\n\n",
            GetKeyword (KM_MCMC));

  if (!bErr && !panal->iType)
   panal->iType = AT_MCMC;

  return (!bErr);

} /* GetGibbsSpec */


/* -----------------------------------------------------------------------------
   GetOptDSpec

   get the optimal design specification. It is based on the GetSetPointsSpec
   routine. 
   The modification list is kept in MCVAR variation records, although this is 
   not really a Monte Carlo analysis.  This structure should eventually be 
   changed to reflect a more general variation specification.
*/
BOOL GetOptDSpec (PINPUTBUF pibIn, PANALYSIS  panal, PSTR szLex)
{
  PMCVAR pmcvar;
  HVAR hvar;
  int iErr = 0;
  int iNLI;
  int ikwcode;

  /* Try to get open paren and filenames */
  if ((iErr = EGetPunct (pibIn, szLex, CH_LPAREN)                   || 
              GetStringArg (pibIn, &panal->gd.szGout, szLex, FALSE) ||
              GetStringArg (pibIn, &panal->gd.szGrestart, szLex, TRUE))) {
    goto Exit_GetOptDSpec;
  }

  /* There has to be a restart file */
  if (!panal->gd.szGrestart)
    ReportError (pibIn, RE_SPECERR | RE_FATAL, "Missing restart file", NULL);

  /* Try to get number of parameter samples to read in */
  GetOptPunct (pibIn, szLex, ',');
  if ((iErr = ENextLex (pibIn, szLex, LX_INTEGER)))
    goto Exit_GetOptDSpec;
  panal->mc.nRuns = atol (szLex);

  /* Try to get the random seed */
  GetOptPunct (pibIn, szLex, ',');
  if ((iErr = ENextLex (pibIn, szLex, LX_NUMBER)))
    goto Exit_GetOptDSpec;
  panal->dSeed = atof (szLex);

  /* Try to get the style (Forward or Backward) */
  GetOptPunct (pibIn, szLex, ',');
  if ((iErr = ENextLex (pibIn, szLex, LX_IDENTIFIER)))
    goto Exit_GetOptDSpec;
  ikwcode = GetKeywordCode (szLex, NULL);
  if (ikwcode == KM_FORWARD)
    panal->mc.style = forward;
  else if (ikwcode == KM_BACKWARD)
         panal->mc.style = backward;
       else {
         iErr = TRUE;
         goto Exit_GetOptDSpec;
       }

  /* Try to get identifier list */
  while ((iNLI=NextListItem (pibIn, szLex, LX_IDENTIFIER, 1, CH_RPAREN)) > 0) {
    hvar = GetVarHandle(szLex);
    if ((iErr = (!hvar || IsInput(hvar))))
      break; /* Is this reported ? */

    if ( !(pmcvar = (PMCVAR) malloc (sizeof(MCVAR))))
      ReportError (pibIn, RE_OUTOFMEM | RE_FATAL, "GetOptDSpec", NULL);

    pmcvar->hvar = hvar;
    pmcvar->iType = MCV_SETPOINTS;
    pmcvar->dParm[2] = pmcvar->dParm[3] = 0.0;

    QueueListItem (panal->mc.plistMCVars, pmcvar);

  } /* while */

  panal->mc.nSetParms = ListLength (panal->mc.plistMCVars);

  if (panal->mc.nSetParms == 0) {
    iErr = TRUE;
    printf (
    "\nError: you must specify a list of parameters to read.\n\n");
    goto Exit_GetOptDSpec;
  }
  
  if (!iNLI) /* List terminator */
    iErr = EGetPunct (pibIn, szLex, CH_RPAREN);
  else {
    iErr = TRUE;
    ReportError (pibIn, RE_LEXEXPECTED, "identifier", szLex);
  } /* else */

Exit_GetOptDSpec: ;

  if (iErr) {
    printf ("Syntax:\n"
            "%s (\"Output_File\", \"Param_Sample_File\", nSamples, "
            "random_seed, <Forward or Backward>, "
            "<param-id-list...>)\n\n", GetKeyword (KM_OPTDESIGN));
    printf ("Exiting...\n");
    exit (0);
  }
  else if (!panal->iType)
    panal->iType = AT_OPTDESIGN; /* Flag SetPoints anal if not chosen */

  return (iErr);

} /* GetOptDSpec */


/* ----------------------------------------------------------------------------
   GetMCVarySpec

*/

BOOL bGaveMCVaryUsage = FALSE; /* prevent multiple diagnostics */

int GetMCVarySpec (PINPUTBUF pibIn, PANALYSIS panal, PSTR szLex)
{
  PLIST plist;
  PMCVAR pmcvar = NULL;
  HVAR hvar;
  int n, iErr = 0;
  char varName[256];

  /* If another type is declared already which is not a Monte Carlo
     or a Gibbs type, then ignore this specification.

     NOTE:  This should not set the type since distributions are used
            for multiple SimTypes.
   */

  if (panal->iType && !((panal->iType == AT_MONTECARLO) ||
                        (panal->iType == AT_SETPOINTS)  ||
                        (panal->iType == AT_MCMC))) {
    EatStatement (pibIn);  /* Ignore this Distrib() stmt */
    goto Exit_MCVarySpec;
  } /* if */

  /* Get the Distrib() spec.  Check syntax at each element. */
  
  /* Get the parameter to be varied */
  if ((iErr = (EGetPunct (pibIn, szLex, CH_LPAREN) || 
               ENextLex (pibIn, szLex, LX_IDENTIFIER))))
    goto Done_GetMCVary;

  if ((iErr = (!(hvar = GetVarHandle (szLex)) || /* Invalid variable name? */
               IsInput(hvar))))
  {
    ReportError (pibIn, RE_LEXEXPECTED, "state-or-parameter", szLex);
    goto Done_GetMCVary;
  } /* if */

  /* This conditions the list in which pmcvar will be queued */
  if (IsModelVar (hvar))         /* Distrib is for a likelihood definition */
    plist = panal->plistModelVars;
  else if (panal->iCurrentDepth == 0)            /* not an MCMC simulation */ 
    plist = panal->mc.plistMCVars;
  else                                               /* an MCMC simulation */
    plist = panal->pCurrentLevel[panal->iCurrentDepth-1]->plistMCVars;

  if (!(pmcvar = (PMCVAR) malloc (sizeof(MCVAR))))
    ReportError (pibIn, RE_OUTOFMEM | RE_FATAL, "GetMCVarySpec", NULL);

  if(!(pmcvar->pszName = (PSTR) malloc(strlen (szLex)+1)))
    ReportError (pibIn, RE_OUTOFMEM | RE_FATAL, "GetMCVarySpec", NULL);

  /* Initialize pmcvar */
  strcpy(varName, szLex);
  strcpy(pmcvar->pszName, szLex);
  pmcvar->hvar = hvar;
  pmcvar->iDepth = panal->iCurrentDepth - 1;
  pmcvar->plistDependents = InitList();
  pmcvar->bExptIsDep = FALSE;
  pmcvar->bIsFixed = FALSE;
  pmcvar->lJumps = 0;
  pmcvar->dKernelSD = INIT_KERNELSD;
  pmcvar->bGibbs = FALSE;
  for (n = 0; n < 4; n++) {
    pmcvar->hParm[n] = 0;
    pmcvar->pMCVParent[n] = NULL;
    pmcvar->pdParm[n] = &(pmcvar->dParm[n]);
  }
  pmcvar->cVarParm = 0;

  /* Get the distribution type */
  GetOptPunct (pibIn, szLex, ',');
  iErr |= ENextLex (pibIn, szLex, LX_IDENTIFIER);
  pmcvar->iType = McvFromLex (szLex);
  if (iErr |= pmcvar->iType < 0) {
    ReportError (pibIn, RE_LEXEXPECTED, "distribution-type", szLex);
    goto Done_GetMCVary;
  } 

  /* Get parameters of the distribution. These vary by distribution type.
     No value checking is made because assignements can be symbolic
  */
  switch (pmcvar->iType) {
    /* ----------------------------------------------------------------------*/
    case MCV_UNIFORM:
    case MCV_LOGUNIFORM: /* 2 parameters: min and max */

      if((iErr=GetMCVaryParam(pibIn, szLex, plist, 0, pmcvar)))
        goto Done_GetMCVary;

      if((iErr=GetMCVaryParam(pibIn, szLex, plist, 1, pmcvar)))
        goto Done_GetMCVary;

      pmcvar->dParm[2] = -DBL_MAX;
      pmcvar->dParm[3] = DBL_MAX;

      break;

    /* ----------------------------------------------------------------------*/
    case MCV_NORMAL:
    case MCV_LOGNORMAL:  /* 2 parameters, mean and SD */
    case MCV_NORMALV:
    case MCV_LOGNORMALV: /* 2 parameters, mean and VARIANCE */
      
      if ((iErr = GetMCVaryParam (pibIn, szLex, plist, 0, pmcvar)))
        goto Done_GetMCVary;

      if ((iErr = GetMCVaryParam (pibIn, szLex, plist, 1, pmcvar)))
        goto Done_GetMCVary;

      /* set the range */
      if ((pmcvar->iType == MCV_NORMAL) || (pmcvar->iType == MCV_NORMALV)) {
        pmcvar->dParm[2] = -DBL_MAX;
        pmcvar->dParm[3] =  DBL_MAX;
      }
      else {
        pmcvar->dParm[2] = 0.0;
        pmcvar->dParm[3] = DBL_MAX;
      }

      break; 

    /* ----------------------------------------------------------------------*/
    case MCV_BETA:            /* 2 or 4 parameters */
    case MCV_TRUNCNORMAL:
    case MCV_TRUNCLOGNORMAL:  /* 4 parameters, the last 2 are min and max */
    case MCV_TRUNCNORMALV:
    case MCV_TRUNCLOGNORMALV: /* VARIANCE instead of SD */
      
      if ((iErr = GetMCVaryParam (pibIn, szLex, plist, 0, pmcvar)))
        goto Done_GetMCVary;

      if ((iErr = GetMCVaryParam (pibIn, szLex, plist, 1, pmcvar)))
        goto Done_GetMCVary;

      /* Set min-max range defaults */
      pmcvar->dParm[2] = 0.0; /* Standard range for beta */
      pmcvar->dParm[3] = 1.0;
      if ((pmcvar->iType == MCV_TRUNCNORMAL) ||
          (pmcvar->iType == MCV_TRUNCNORMALV)) {
        pmcvar->dParm[2] = -DBL_MAX;
        pmcvar->dParm[3] =  DBL_MAX;
      }
      else if ((pmcvar->iType == MCV_TRUNCLOGNORMAL) ||
               (pmcvar->iType == MCV_TRUNCLOGNORMALV))
        pmcvar->dParm[3] = DBL_MAX;

      /* Look if a min-max range is included. For truncated types
         it is required. */
      SkipWhitespace (pibIn);
      if ((pmcvar->iType == MCV_BETA) && NextChar (pibIn) == CH_RPAREN)
        break; /* The spec is finished */

      /* Get the min and max */
      if((iErr=GetMCVaryParam(pibIn, szLex, plist, 2, pmcvar)))
        goto Done_GetMCVary;

      if((iErr=GetMCVaryParam(pibIn, szLex, plist, 3, pmcvar)))
        goto Done_GetMCVary;

      break;

    /* ----------------------------------------------------------------------*/
    case MCV_CHI2: /* only one parameter: degrees of freedom */

      if ((iErr = GetMCVaryParam (pibIn, szLex, plist, 0, pmcvar)))
        goto Done_GetMCVary;

      /* set the range */
      pmcvar->dParm[2] = 0.0;
      pmcvar->dParm[3] = DBL_MAX;

      break;

    /* ----------------------------------------------------------------------*/
    case MCV_BINOMIAL: /* 2 parameters, p and n */

      if ((iErr = GetMCVaryParam (pibIn, szLex, plist, 0, pmcvar)))
        goto Done_GetMCVary;

      if((iErr=GetMCVaryParam(pibIn, szLex, plist, 1, pmcvar)))
        goto Done_GetMCVary;

      /* set the range */
      pmcvar->dParm[2] = 0.0;
      if (pmcvar->cVarParm < 2)
        pmcvar->dParm[3] = pmcvar->dParm[1];
      else
        pmcvar->dParm[3] = DBL_MAX; /* FB 18/07/97 */


      break;

    /* ----------------------------------------------------------------------*/
    case MCV_PIECEWISE: /* 4 parameters, note the particular order */

      if ((iErr = GetMCVaryParam (pibIn, szLex, plist, 2, pmcvar)))
        goto Done_GetMCVary;

      if ((iErr = GetMCVaryParam (pibIn, szLex, plist, 0, pmcvar)))
        goto Done_GetMCVary;

      if ((iErr = GetMCVaryParam (pibIn, szLex, plist, 1, pmcvar)))
        goto Done_GetMCVary;

      if ((iErr = GetMCVaryParam (pibIn, szLex, plist, 3, pmcvar)))
        goto Done_GetMCVary;

      break;

    /* ----------------------------------------------------------------------*/
    case MCV_EXPONENTIAL: /* only one parameter: inverse scale */

      if ((iErr = GetMCVaryParam (pibIn, szLex, plist, 0, pmcvar)))
        goto Done_GetMCVary;

      /* set the range */
      pmcvar->dParm[2] = 0.0;
      pmcvar->dParm[3] = DBL_MAX;

      break;

    /* ----------------------------------------------------------------------*/
    case MCV_GGAMMA:   
    case MCV_INVGGAMMA: /* 2 parameter: shape and inverse scale */
      
      if ((iErr = GetMCVaryParam (pibIn, szLex, plist, 0, pmcvar)))
        goto Done_GetMCVary;

      if ((iErr = GetMCVaryParam (pibIn, szLex, plist, 1, pmcvar)))
        goto Done_GetMCVary;

      /* set the range */
      pmcvar->dParm[2] = 0.0;
      pmcvar->dParm[3] = DBL_MAX;

      break;

    /* ----------------------------------------------------------------------*/
    case MCV_POISSON: /* 1 parameter: rate */

      if ((iErr = GetMCVaryParam (pibIn, szLex, plist, 0, pmcvar)))
        goto Done_GetMCVary;

      /* set the range */
      pmcvar->dParm[2] = 0.0;
      pmcvar->dParm[3] = DBL_MAX;

      break;

    /* ----------------------------------------------------------------------*/
    case MCV_BINOMIALBETA: /* 3 parameter: mean, alpha, beta */

      if ((iErr = GetMCVaryParam (pibIn, szLex, plist, 0, pmcvar)))
        goto Done_GetMCVary;

      if ((iErr = GetMCVaryParam (pibIn, szLex, plist, 1, pmcvar)))
        goto Done_GetMCVary;

      if ((iErr = GetMCVaryParam (pibIn, szLex, plist, 2, pmcvar)))
        goto Done_GetMCVary;

      /* set the last parameter, unused */
      pmcvar->dParm[3] = DBL_MAX;

      break;

    /* ----------------------------------------------------------------------*/
    default:
        ReportRunTimeError(panal, RE_UNKNOWNDIST | RE_FATAL, "GetMCVarySpec");
        break;

  } /* switch */

  EGetPunct (pibIn, szLex, CH_RPAREN);

  /* Check for a range error.  If there is a problem, correct it,
     but issue a warning in case this is wrong.
  */
  if (pmcvar->cVarParm < 4 && pmcvar->dParm[3] < pmcvar->dParm[2]) {
    double dTmp = pmcvar->dParm[3];    /* Swap ranges */
    pmcvar->dParm[3] = pmcvar->dParm[2];
    pmcvar->dParm[2] = dTmp;
    ReportError (pibIn, RE_MAXMIN_RANGE | RE_WARNING, NULL, NULL);
  }

  /* If there's no error at this point, queue the variation in
     the Monte Carlo record(s).
  */
  if (!iErr) {
    QueueListItem (plist, pmcvar);
  } /* if */

Done_GetMCVary: ;

  if (iErr) {
    if (pmcvar) free (pmcvar);
    
    if (!bGaveMCVaryUsage) {
      printf ("\nSyntax: Check the syntax of %s.\n", GetKeyword (KM_MCVARY));
      bGaveMCVaryUsage = TRUE;
    }
    
    ReportError(pibIn, RE_SYNTAXERR | RE_FATAL, NULL, NULL);

  }

Exit_MCVarySpec: ;

  return (iErr);

} /* GetMCVarySpec */



/* ----------------------------------------------------------------------------
   GetMCVaryParam

   Determine if argument `n' of the Distrib statement is a variable name or
   a number; set the parameter accordingly

   If the argument is a variable, set a pointer to its MC structure
*/

int GetMCVaryParam (PINPUTBUF pibIn, PSTR szLex,
                    PLIST plist, int n, PMCVAR pmcvar) {
  PANALYSIS panal = (PANALYSIS)pibIn->pInfo;
  int iLex;
  HVAR hvar;

  GetOptPunct (pibIn, szLex, ',');
  if (n != 3)
    NextLex (pibIn, szLex, &iLex);
  else {
    SkipWhitespace (pibIn);
    iLex = LX_NULL;
    if (NextChar (pibIn) != CH_RPAREN)
      NextLex(pibIn, szLex, &iLex);
  }

  if (iLex == LX_IDENTIFIER) {

    if (GetKeywordCode (szLex, NULL) == KM_PREDICTION) {
      if (n > 1) { 
        /* Prediction is allowed only in the first 2 positions */
        ReportError (pibIn, RE_LEXEXPECTED, "id, number, or `)'", szLex);
        return 1;
      }
      if (!IsModelVar (pmcvar->hvar)) { 
        /* only states and outputs can be predicted */
        ReportError (pibIn, RE_LEXEXPECTED, "output or state variable", szLex);
        return 1;
      }
      if (EGetPunct (pibIn, szLex, CH_LPAREN))
        return 1;
      NextLex (pibIn, szLex, &iLex);
      hvar = GetVarHandle (szLex);
      if (n == 0 && (!hvar || hvar != pmcvar->hvar)) {
        /* in position 1, Prediction and data must be have same name */
        ReportError (pibIn, RE_LEXEXPECTED, "correct variable", szLex);
        return 1;
      }
      if (n == 1 && (!hvar || !IsModelVar(hvar))) {
        /* in position 2, Prediction must be state or output */
        ReportError(pibIn, RE_LEXEXPECTED, "state or output variable", szLex);
        return 1;
      }
      if(EGetPunct(pibIn, szLex, CH_RPAREN))
        return 1;
    }
    else /* i.e. not Prediction */
      if (!(hvar = GetVarHandle (szLex)) || IsInput(hvar) || IsOutput(hvar)) {
        /* cannot be input or output, should be parameter, can be state ? */
        ReportError(pibIn, RE_LEXEXPECTED, "parameter", szLex);
        return 1;
      }

    /* Can't have self-dependency at level 0 */
    if((panal->iCurrentDepth == 0 && hvar == pmcvar->hvar)
       || !CheckDistribParam (plist, pmcvar->hvar, hvar)) {
      ReportError(pibIn, RE_LEXEXPECTED, "valid parameter", szLex);
      return 1;
    }
    pmcvar->cVarParm |= MCVP_VARIABLE << n;
    pmcvar->hParm[n] = hvar;

  }
  else /* i.e. not an identifier */
    if (iLex == LX_FLOAT || iLex == LX_INTEGER) {
      pmcvar->cVarParm |= MCVP_FIXED << n;
      pmcvar->dParm[n] = atof(szLex);
    }
    else {
      /* Allow max to be absent - set to default */
      if (n == 3) {
        pmcvar->cVarParm |= MCVP_FIXED << n;
        pmcvar->dParm[n] = DBL_MAX;
      }
      else /* error */
        return 1;
  }

  return 0; /* OK */

} /* GetMCVaryParam */


/* ----------------------------------------------------------------------------
   CheckDistribParam

   We cannot have both Distrib(alpha, ,,,, beta, ...) and
   Distrib(beta, ..., alpha, ...).
*/
BOOL CheckDistribParam(PLIST plist, HVAR hvar1, HVAR hvar2) {
  int n;
  PLISTELEM p = plist->pleHead;
  PMCVAR pmcvar;

  if(plist == NULL) return TRUE;
  for(n = 0; n < plist->iSize; ++n) {
    pmcvar = (PMCVAR)p->pData;
    if(hvar2 == pmcvar->hvar) {
      if((pmcvar->cVarParm & 1) && hvar1 == pmcvar->hParm[0])
        return FALSE;
      if(((pmcvar->cVarParm >> 1) & 1) && hvar1 == pmcvar->hParm[1])
        return FALSE;
      if(((pmcvar->cVarParm >> 2) & 1) && hvar1 == pmcvar->hParm[2])
        return FALSE;
      if(((pmcvar->cVarParm >> 3) & 1) && hvar1 == pmcvar->hParm[3])
        return FALSE;
    }
    p = p->pleNext;
  }
  return TRUE;
}


/* ----------------------------------------------------------------------------
   GetSetPointsSpec

   Reads the SetPoints() arguments. The modification list is kept
   in MCVAR variation records, although this is not really a Monte
   Carlo analysis.  This structure should eventually be changed to
   reflect a more general variation specification.
*/
int GetSetPointsSpec (PINPUTBUF pibIn, PANALYSIS  panal, PSTR szLex)
{
  PMCVAR pmcvar;
  HVAR hvar;
  int iErr = 0;
  int iNLI;

  /* MonteCarlo sampling can be mixed with SetPoints sampling if Distrib
     specs appear after the SetPoint spec */
  if (ListLength (panal->mc.plistMCVars) > 0) {
    printf ("Error: Distrib() statements can only appear after the SetPoints()"
            "specification, not before - Exiting\n\n");
    exit (0);
  }

  /* Try to get open paren and filenames */
  if ((iErr = EGetPunct (pibIn, szLex, CH_LPAREN) || 
              GetStringArg (pibIn, &panal->mc.szMCOutfilename, szLex, FALSE) ||
              GetStringArg (pibIn, &panal->mc.szSetPointsFilename, szLex, 
                            TRUE))) {
    goto Exit_GetSetPointsSpec;
  }

  /* There has to be a restart file */
  if (!panal->mc.szSetPointsFilename)
    ReportError (pibIn, RE_SPECERR | RE_FATAL, "Missing setpoints file", NULL);

  /* Try to get number of runs */
  GetOptPunct (pibIn, szLex, ',');
  if ((iErr = ENextLex (pibIn, szLex, LX_INTEGER)))
    goto Exit_GetSetPointsSpec;
  panal->mc.nRuns = atol (szLex);

  /* Try to get identifier list */

  /* FB 20/11/96 removed this because it gives errors if only a comma
     separates the number of runs from the list of params :
     GetOptPunct (pibIn, szLex, ','); */
  while ((iNLI=NextListItem (pibIn, szLex, LX_IDENTIFIER, 1, CH_RPAREN)) > 0) {
    hvar = GetVarHandle(szLex);
    if ((iErr = (!hvar || IsInput(hvar))))
      break; /* Is this reported ? */

    if ( !(pmcvar = (PMCVAR) malloc (sizeof(MCVAR))))
      ReportError (pibIn, RE_OUTOFMEM | RE_FATAL, "GetSetPointsSpec", NULL);

    pmcvar->hvar = hvar;
    pmcvar->iType = MCV_SETPOINTS;
    pmcvar->dParm[2] = pmcvar->dParm[3] = 0.0;

    QueueListItem (panal->mc.plistMCVars, pmcvar);

  } /* while */

  panal->mc.nSetParms = ListLength (panal->mc.plistMCVars);

  /* FB 19 nov 96 */
  if (panal->mc.nSetParms == 0) {
    iErr = TRUE;
    printf (
    "\nError: you must specify a list of parameters to read.\n\n");
    goto Exit_GetSetPointsSpec;
  }
  
  if (!iNLI) /* List terminator */
    iErr = EGetPunct (pibIn, szLex, CH_RPAREN) || InitSetPoints (&panal->mc);
  else {
    iErr = TRUE;
    ReportError (pibIn, RE_LEXEXPECTED, "identifier", szLex);
  } /* else */

Exit_GetSetPointsSpec: ;

  if (iErr) {
    printf ("Syntax:\n"
             "%s (\"OutputFile\", \"SetPtsFile\", nRuns, "
             "<param-id-list...>)\n\n", GetKeyword (KM_SETPOINTS));
    printf ("Exiting...\n");
    exit (0);
  }
  else if (!panal->iType)
    panal->iType = AT_SETPOINTS; /* Flag SetPoints anal if not chosen */

  return (iErr);

} /* GetSetPointsSpec */


/* -----------------------------------------------------------------------------
   GetMonteCarloSpec
*/
int GetMonteCarloSpec (PINPUTBUF pibIn, PANALYSIS panal, PSTR szLex)
{
#define NMC_ARGS 3     /* 3 MonteCarlo Args */

static int vrgiMCArgTypes[NMC_ARGS] = {LX_STRING, LX_INTEGER, LX_NUMBER};

  int iErr = 0;

  /* If another type is declared already which is not a Monte Carlo
     type, then ignore this specification.
     This should check for duplicate specs! */

  if (panal->iType && panal->iType != AT_MONTECARLO) {
    EatStatement (pibIn);
    goto Exit_GetMonteCarloSpec;
  }

  iErr = !GetFuncArgs (pibIn, NMC_ARGS, vrgiMCArgTypes, vrgszlexArgs[0]);

  if (!iErr) {
    if (*vrgszlexArgs[0]) {
      if ( !(panal->mc.szMCOutfilename =
             (PSTR) malloc(MyStrlen(vrgszlexArgs[0]) + 1)))
        ReportError (pibIn, RE_OUTOFMEM | RE_FATAL, "GetMonteCarloSpec", NULL);

      MyStrcpy (panal->mc.szMCOutfilename, vrgszlexArgs[0]);
    } /* if */

    panal->mc.nRuns = atol (vrgszlexArgs[1]);
    panal->dSeed = atof(vrgszlexArgs[2]);
  } /* if */
  else
    printf ("Syntax: %s (szOutfilename, nRuns, dSeed)\n\n", 
            GetKeyword (KM_MONTECARLO));

  if (!iErr && !panal->iType)
    panal->iType = AT_MONTECARLO; /* Flag as MC if none chosen yet */

Exit_GetMonteCarloSpec: ;

  return (iErr);

} /* GetMonteCarloSpec */


/* -----------------------------------------------------------------------------
   GetParmMod
*/

BOOL GetParmMod (PINPUTBUF pibIn, PSTRLEX szLex, PSTREQN szeqn)
{
  HVAR hvar = GetVarHandle(szLex);
  PANALYSIS panal = (PANALYSIS) pibIn->pInfo;
  PEXPERIMENT pexp = panal->pexpCurrent;

  PSTRLEX szPunct;
  int  iErr;
  PVARMOD pvarmod; /* Pointer to the variable modification */

  if ((iErr = !hvar))
    ReportError (pibIn, RE_LEXEXPECTED, "model-variable", szLex);

  else {
    /* Allocate space and initialize modification */

    if ( !(pvarmod = (PVARMOD) malloc (sizeof(VARMODIFICATION))))
      ReportError (pibIn, RE_OUTOFMEM | RE_FATAL, "GetParmMod", NULL);

    pvarmod->hvar = hvar; /* The variable handle */

    if (!GetOptPunct (pibIn, szPunct, '=')) { /* Try to get '=' */
      iErr = szPunct[1] = '=';
      ReportError (pibIn, RE_EXPECTED, szPunct, NULL);
    } /* if */

    else if (IsInput (hvar)) { /* Process INPUT */
      if ( !(pvarmod->uvar.pifn = (PIFN) malloc (sizeof(IFN))))
        ReportError (pibIn, RE_OUTOFMEM | RE_FATAL, "GetParmMod", NULL);

      iErr = !pvarmod->uvar.pifn
             || !GetInputFn (pibIn, NULL, pvarmod->uvar.pifn);
      if (iErr) {
        free (pvarmod->uvar.pifn); /* Cleanup if error */
        pvarmod->uvar.pifn = NULL;
      } /* if */
    } /* if */

    else { /* PARM, STATE, etc */
      if (!(iErr = ENextLex(pibIn, szLex, LX_NUMBER)))
        pvarmod->uvar.dVal = atof(szLex);
    } /* else */

    if (!iErr) {            /* No errors, add mod to list */
      if(panal->iCurrentDepth == 0 || panal->wContext == CN_EXPERIMENT)
        QueueListItem (pexp->plistParmMods, pvarmod);
      else
        QueueListItem
             (panal->pCurrentLevel[panal->iCurrentDepth-1]->plistVars, pvarmod);
      iErr = GetTerminator (pibIn, szLex);
    } /* if */
    else                /* Invalid mod, cleanup */
      free (pvarmod);

  } /* else valid id */

  return ((BOOL) iErr);

} /* GetParmMod */


/* -----------------------------------------------------------------------------
   NewExperiment

   creates a new experiment in the analysis and copies global defaults.
*/

void NewExperiment (PINPUTBUF pibIn)
{
  PANALYSIS panal = (PANALYSIS)pibIn->pInfo;
  PLEVEL plevel;
  int n;

  /* Allocate new experiment and assign list and current pointers */

  if (panal->iCurrentDepth < 0) { /* something is real wrong - FB 03/08/97 */
     ReportError (pibIn, RE_LEXEXPECTED | RE_FATAL, "Level statement", 
                  "Experiment");
  }

  if (panal->iCurrentDepth == 0) {
    panal->expGlobal.iExp++;    /* Increment number of experiment */
    panal->pexpCurrent = panal->rgpExps[panal->expGlobal.iExp - 1] =
                         (PEXPERIMENT) malloc (sizeof(EXPERIMENT));
    if (!panal->pexpCurrent)
      ReportError (pibIn, RE_OUTOFMEM | RE_FATAL, "NewExperiment()", NULL);

    printf ("Reading experiment %d:\n", panal->expGlobal.iExp);
  }
  else {
    plevel = panal->pLevels[panal->iInstances-1];
    for (n = 0; n < panal->iCurrentDepth-1; ++n) {
      plevel = plevel->pLevels[plevel->iInstances-1];
    }
    if (plevel->iInstances == MAX_INSTANCES - 1)
      ReportError(pibIn, RE_TOOMANYINST | RE_FATAL, "NewExperiment", NULL);
    n = panal->pCurrentLevel[panal->iCurrentDepth-1]->iInstances++;
    if ((plevel = plevel->pLevels[n] = (PLEVEL)malloc(sizeof(LEVEL))) == NULL)
      ReportError(pibIn, RE_OUTOFMEM | RE_FATAL, "NewExperiment", NULL);
    plevel->iInstances = 0;
    plevel->iSequence = n + 1;
    plevel->iDepth = panal->iCurrentDepth;
    panal->pCurrentLevel[panal->iCurrentDepth++] = plevel;
    if (panal->iDepth < panal->iCurrentDepth)
      panal->iDepth = panal->iCurrentDepth;

    plevel->nMCVars = plevel->nFixedVars = 0; /* FB 13 aug 97 */
    plevel->plistVars = InitList();
    plevel->plistMCVars = InitList();

    if ((plevel->pexpt = (PEXPERIMENT)malloc(sizeof(EXPERIMENT))) == NULL)
      ReportError(pibIn, RE_OUTOFMEM | RE_FATAL, "NewExperiment", NULL);

    panal->pexpCurrent = plevel->pexpt;
    panal->pexpCurrent->iExp = panal->expGlobal.iExp = ++panal->iExpts;
    panal->wContext = CN_EXPERIMENT;

    printf ("Experiment %d - depth %d, instance %d\n", panal->iExpts,
            panal->iCurrentDepth,
            panal->pCurrentLevel[panal->iCurrentDepth-2]->iInstances);
  }

  memcpy (panal->pexpCurrent, &panal->expGlobal, sizeof(EXPERIMENT));
  panal->wContext = CN_EXPERIMENT;
  panal->pexpCurrent->plistParmMods = InitList();    /* Local mods */
  panal->pexpCurrent->os.plistPrintRecs = InitList();
  panal->pexpCurrent->os.plistDataRecs = InitList();

} /* NewExperiment */


/* -----------------------------------------------------------------------------
   EndExperiment

   cleans up at the end of defining a new experiment section.
*/
BOOL EndExperiment (PINPUTBUF pibIn, PANALYSIS panal)
{
  BOOL bReturn;

  bReturn = !ErrorsReported (pibIn);

  if (!bReturn) {
    /* Experiment had errors.  Cleanup this space and continue */
    ReportError (pibIn, RE_ERRORSINEXP | RE_FATAL,
         (PSTR) &panal->pexpCurrent->iExp, NULL);
    ClearErrors (pibIn);
    panal->rgpExps[--panal->expGlobal.iExp] = NULL;
    free (panal->pexpCurrent);
  } /* if */

  else {
    /* Create space for outputs and data */
    PrepareOutSpec (panal->pexpCurrent);
  }

  /* Reset current exp to global context. */

  panal->pexpCurrent = &panal->expGlobal;
  panal->wContext = CN_GLOBAL;

  if(panal->iType == AT_MCMC && panal->iCurrentDepth-- == 0)
    return FALSE;

  return (bReturn);

} /* EndExperiment */


/* -----------------------------------------------------------------------------
   SetLevel

   Encountered `Level' keyword, increments number of levels, allocates
   structure, initializes
*/
int SetLevel(PINPUTBUF pibIn) 
{
  PSTRLEX szPunct;
  PANALYSIS panal = (PANALYSIS)pibIn->pInfo;
  PLEVEL plevel;
  BYTE n;

  if (panal->iType != AT_MCMC)
    ReportError(pibIn, RE_TYPENOTMCMC | RE_FATAL, "SetLevel", NULL);

  if (panal->iCurrentDepth == MAX_LEVELS)
    ReportError(pibIn, RE_TOOMANYLEVELS | RE_FATAL, "SetLevel", NULL);

  if (panal->wContext == CN_EXPERIMENT)
    ReportError(pibIn, RE_LEVINEXPT | RE_FATAL, "SetLevel", NULL);

  if (EGetPunct(pibIn, szPunct, CH_LBRACE))
    return 1;

  if (panal->iCurrentDepth == 0) {

    if (panal->iInstances == MAX_INSTANCES - 1)
      ReportError(pibIn, RE_TOOMANYINST | RE_FATAL, "SetLevel", NULL);

    plevel = panal->pLevels[panal->iInstances++] 
           = (PLEVEL) malloc (sizeof (LEVEL));
    if (plevel == NULL)
      ReportError(pibIn, RE_OUTOFMEM | RE_FATAL, "SetLevel", NULL);

    plevel->iSequence = panal->iInstances;
    printf ("New level - depth 1, instance %d\n", panal->iInstances);

  }
  else {

    plevel = panal->pLevels[panal->iInstances-1];

    for (n = 0; n < panal->iCurrentDepth-1; ++n)
      plevel = plevel->pLevels[plevel->iInstances-1];

    if (plevel->iInstances == MAX_INSTANCES - 1)
      ReportError(pibIn, RE_TOOMANYINST | RE_FATAL, "SetLevel", NULL);

    n = panal->pCurrentLevel[panal->iCurrentDepth-1]->iInstances++;

    plevel = plevel->pLevels[n] 
           = (PLEVEL)malloc(sizeof(LEVEL));

    if (plevel == NULL)
      ReportError(pibIn, RE_OUTOFMEM | RE_FATAL, "SetLevel", NULL);

    plevel->iSequence = n + 1;
    printf("New level - depth %d, instance %d\n", panal->iCurrentDepth+1,
           panal->pCurrentLevel[panal->iCurrentDepth-1]->iInstances);
  }

  plevel->iInstances = 0;
  plevel->iDepth = panal->iCurrentDepth;
  panal->pCurrentLevel[panal->iCurrentDepth++] = plevel;

  if (panal->iDepth < panal->iCurrentDepth)
    panal->iDepth = panal->iCurrentDepth;

  plevel->nMCVars = plevel->nFixedVars = 0;
  plevel->plistVars = InitList();
  plevel->plistMCVars = InitList();
  plevel->pexpt = NULL;

  return 0;
  
} /* SetLevel */


/* -----------------------------------------------------------------------------
   EndLevel
*/
BOOL EndLevel (PANALYSIS panal)
{
  if(panal->iCurrentDepth-- == 0)
    return FALSE;
  return TRUE;
} /* EndLevel */


/* -----------------------------------------------------------------------------
   FreeLevels
*/
void FreeLevels (PANALYSIS panal)
{
  BYTE n;
  for (n = 0; n < panal->iInstances; n++)
    if (panal->pLevels[n] != NULL)
      FreeOneLevel (panal->pLevels[n]);
} /* FreeLevels */


/* -----------------------------------------------------------------------------
   FreeMCLists
*/
int FreeMCLists (PVOID pData, PVOID pUserInfo)
{
  PMCVAR pMCVar = (PMCVAR) pData;
  FreeList (&pMCVar->plistDependents, NULL, TRUE);
  /* dummy */
  return 0;
}


/* -----------------------------------------------------------------------------
   FreeOneLevel (recursive)
*/
void FreeOneLevel (PLEVEL plevel)
{
  BYTE n;

  for (n = 0; n < plevel->iInstances; n++)
    if (plevel->pLevels[n] != NULL)
      FreeOneLevel (plevel->pLevels[n]);

  FreeList (&plevel->plistVars, NULL, TRUE);
  ForAllList (plevel->plistMCVars, &FreeMCLists, NULL);
  FreeList (&plevel->plistMCVars, NULL, TRUE);
  if (plevel->pexpt != NULL)
    free (plevel->pexpt);
  if (plevel->nMCVars > 0)
    free (plevel->rgpMCVars);
  if (plevel->nFixedVars > 0)
    free (plevel->rgpFixedVars);
  free(plevel);

} /* FreeOneLevel */



/* -----------------------------------------------------------------------------
   ProcessWord

   processes the word szLex.

   This is the main loop of the interpreter.  It is a big switch that
   recognizes keywords that are specifications and takes the
   appropriate action.

   If the word szLex is not a keyword, ProcessWord() attempts to
   define a parameter specification.
*/
void ProcessWord (PINPUTBUF pibIn, PSTR szLex, PSTR szEqn)
{
  int iErr = 0;
  int iKWCode, fContext;
  PSTRLEX szPunct;
  PANALYSIS panal;

  if (!pibIn || !szLex || !szLex[0] || !szEqn)
    return;

  panal = (PANALYSIS) pibIn->pInfo;

  iKWCode = GetKeywordCode (szLex, &fContext);

  assert(panal->wContext != CN_END);

  if ((iErr =
        (iKWCode                                 /* Is a keyword */
         && !(fContext & panal->wContext))))     /* In invalid context */
    ReportError (pibIn, RE_BADCONTEXT, szLex, NULL);

  else {
    switch (iKWCode) {

    default:
      /* If a keyword is not found, try to get a parameter assignment */
      iErr = GetParmMod (pibIn, szLex, szEqn);
      break;

    /*Process the following keywords */

    case KM_PRINT:
      iErr = GetPrint (pibIn, szLex, &panal->pexpCurrent->os);
      break;

    case KM_PRINTSTEP:
      iErr = GetPrintStep (pibIn, szLex, &panal->pexpCurrent->os);
      break;

    case KM_EXPERIMENT:
      if (!(iErr = EGetPunct (pibIn, szPunct, CH_LBRACE)))
        NewExperiment (pibIn);
      break;

    case KM_LEVEL:
      iErr = SetLevel (pibIn);
      break;

    case KM_MCVARY:
      iErr = GetMCVarySpec (pibIn, panal, szLex);
      break;

    case KM_OUTPUTFILE:
      if (panal->pexpCurrent->os.szOutfilename) {
        ReportError (pibIn, RE_REDEF| RE_WARNING, "OutputFile", "* Ignoring");
        iErr++;
      } /* if */
      else iErr = GetOutputFile (pibIn, szLex, &panal->pexpCurrent->os);
      break;

    case KM_DATA:
      iErr = GetData (pibIn, szLex, &panal->pexpCurrent->os);
      break;

    case KM_INTEGRATE:
      iErr = GetIntegrate (pibIn, &panal->pexpCurrent->is);
      break;

    case KM_MCMC:
      iErr = GetGibbsSpec (pibIn, panal->pexpCurrent);
      break;

    case KM_OPTDESIGN:
      iErr = GetOptDSpec (pibIn, panal, szLex);
      break;

    case KM_MONTECARLO:
      iErr = GetMonteCarloSpec (pibIn, panal, szLex);
      break;

    case KM_SETPOINTS:
      iErr = GetSetPointsSpec (pibIn, panal, szLex);
      break;

    case KM_SIMULATE:
      iErr = GetSimulate (pibIn, panal->pexpCurrent);
      break;

    case KM_SIMTYPE:
      iErr = GetSimType (pibIn);
      break;

    case KM_END:
      panal->wContext = CN_END;
      break;

    } /* switch */
  } /* else */

  if (iErr)
    EatStatement (pibIn);

} /* ProcessWord */


/* -----------------------------------------------------------------------------
*/

BOOL ReadAnalysis (PINPUTBUF pibIn)
{
  PSTRLEX  szLex;    /* Lex elem of MAX_LEX length */
  PSTREQN  szEqn;    /* Equation buffer of MAX_EQN length */
  int      iLexType;

  BOOL      bReturn = TRUE;
  PANALYSIS panal;

  if (!pibIn) return (FALSE);

  panal = (PANALYSIS) pibIn->pInfo;
  panal->iDepth = panal->iCurrentDepth = panal->iInstances = 0;
  panal->mc.plistMCVars = InitList();

  do {
    /* State machine for parsing syntax */
    NextLex (pibIn, szLex, &iLexType);

    switch (iLexType) {

      case LX_NULL:
        if (panal->wContext != CN_GLOBAL)
          ReportError (pibIn, RE_WARNING, NULL, "Unexpected end of file");

        if (panal->wContext == CN_EXPERIMENT)
          bReturn &= EndExperiment (pibIn, panal);
        panal->wContext = CN_END;
        break;

      case LX_IDENTIFIER:
        ProcessWord (pibIn, szLex, szEqn);
        break;

      case LX_PUNCT:
        if (szLex[0] == CH_STMTTERM)
          break;
        else if (szLex[0] == CH_RBRACE) {
          if (panal->wContext & CN_EXPERIMENT) {
            bReturn &= EndExperiment (pibIn, panal);
            break;
          }
          else {
            bReturn &= EndLevel(panal);
            break;
          }
        }
        else
          if (szLex[0] == CH_COMMENT) {
            SkipComment (pibIn);
            break;
          }

        /* else -- fall through! */

      default:
        ReportError (pibIn, RE_UNEXPECTED, szLex, "* Ignoring");
        break;

      case LX_INTEGER:
      case LX_FLOAT:
        ReportError (pibIn, RE_UNEXPNUMBER, szLex, "* Ignoring");
        break;

    } /* switch */

  } while (panal->wContext != CN_END
           && (*pibIn->pbufCur || FillBuffer (pibIn) != EOF));

  if(panal->iCurrentDepth != 0)
    ReportError (pibIn, RE_OPENLEVEL | RE_FATAL, "ReadAnalysis", NULL);
  return (bReturn);

} /* ReadAnalysis */
