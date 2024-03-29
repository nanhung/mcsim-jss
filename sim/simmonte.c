/* simmonte.c

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
     Fr�d�ric Bois / Don Maszle
     BEHS, School of Public Health
     University of California at Berkeley
     Berkeley, CA 94720

     fbois@diana.lbl.gov

   -- Revisions -----
     Logfile:  SCCS/s.simmonte.c
    Revision:  1.26
        Date:  14 Nov 1997
     Modtime:  06:39:09
      Author:  @a
   -- SCCS  ---------

   Handles functions related to Monte Carlo analysis.
*/

#include <assert.h>
#include <ctype.h>
#include <float.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

#include "sim.h"
#include "lex.h"
#include "lexerr.h"
#include "strutil.h"
#include "simmonte.h"


/* -----------------------------------------------------------------------------
   SetParms

   sets the parameters in the rghvar array to the values in the rgdParm
   array.
*/

void SetParms (long cParms, HVAR *rghvar, double *rgdParm)
{
  long i;

  for (i = 0; i < cParms; i++)
    SetVar (rghvar[i], rgdParm[i]);

} /* SetParms */


/* -----------------------------------------------------------------------------
   SetParmsLog

   sets the parameters in the rghvar array to the log-transformed
   values in the rgdParm array.
*/

void SetParmsLog (long cParms, HVAR *rghvar, double *rgdParm)
{
  long i;

  for (i = 0; i < cParms; i++)
    SetVar (rghvar[i], log(rgdParm[i]));

} /* SetParmsLog */


/* -----------------------------------------------------------------------------
   CalculateOneMCParm

   Callback function for CalculateMCParms.
*/

int CalculateOneMCParm (PMCVAR pMCVar)
{
  double dParm1, dParm2, dMin, dMax;

  /* Check pMCVar for dependencies */
  dParm1 = *(pMCVar->pdParm[0]);
  dParm2 = *(pMCVar->pdParm[1]);
  dMin   = *(pMCVar->pdParm[2]);
  dMax   = *(pMCVar->pdParm[3]);

  /* Set variable randomly according to selected distribution */
  switch (pMCVar->iType) {

    default:
    case MCV_UNIFORM:
      pMCVar->dVal = UniformRandom (dParm1, dParm2);
      break;

    case MCV_LOGUNIFORM:
      pMCVar->dVal = LogUniformRandom (dParm1, dParm2);
      break;

    case MCV_BETA:
      pMCVar->dVal = BetaRandom (dParm1, dParm2, dMin, dMax);
      break;

    case MCV_NORMAL:
      pMCVar->dVal = NormalRandom (dParm1, dParm2);
      break;

    case MCV_LOGNORMAL:
      pMCVar->dVal = LogNormalRandom (dParm1, dParm2);
      break;

    case MCV_TRUNCNORMAL:
      pMCVar->dVal = TruncNormalRandom (dParm1, dParm2, dMin, dMax);
      break;

    case MCV_TRUNCLOGNORMAL:
      pMCVar->dVal = TruncLogNormalRandom (dParm1, dParm2, dMin, dMax);
      break;

    case MCV_NORMALV:
      pMCVar->dVal = NormalRandom (dParm1, sqrt (dParm2));
      break;

    case MCV_LOGNORMALV:
      pMCVar->dVal = LogNormalRandom (dParm1, exp(sqrt(dParm2)));
      break;

    case MCV_TRUNCNORMALV:
      pMCVar->dVal = TruncNormalRandom (dParm1, sqrt (dParm2), dMin, dMax);
      break;

    case MCV_TRUNCLOGNORMALV:
      pMCVar->dVal = TruncLogNormalRandom (dParm1, exp(sqrt(dParm2)),
                                           dMin, dMax);
      break;

    case MCV_CHI2:
      pMCVar->dVal = Chi2Random (dParm1);
      break;

    case MCV_BINOMIAL:
      pMCVar->dVal = BinomialRandom (dParm1, (long) dParm2);
      break;

    case MCV_PIECEWISE:
      pMCVar->dVal = PiecewiseRandom (dMin, dParm1, dParm2, dMax);
      break;

    case MCV_EXPONENTIAL:
      pMCVar->dVal = ExpRandom (dParm1);
      break;

    case MCV_GGAMMA:
      pMCVar->dVal = GGammaRandom (dParm1, dParm2);
      break;

    case MCV_INVGGAMMA:
      pMCVar->dVal = InvGGammaRandom (dParm1, dParm2);
      break;

    case MCV_POISSON:
      pMCVar->dVal = PoissonRandom (dParm1);
      break;

    case MCV_BINOMIALBETA: /* dMin is in fact beta */
      pMCVar->dVal = BinomialBetaRandom (dParm1, dParm2, dMin);
      break;

  } /* switch */

  return 0;

} /* CalculateOneMCParm */


/* -----------------------------------------------------------------------------
   CalcMCParms

   calculates random parameters for a Monte Carlo variation.

   This routines uses arrays for the MC vars and distributions.
   It replaces the obsolete CalculateMCParms which used lists.

   The calculated parms are stored in the rgParms[] array.  If this
   array is NULL, the parms are stored in the pMC->rgParms[] array.

   The calculation starts at index iStart.
*/

void CalcMCParms (PMONTECARLO pMC, double rgParms[], long iStart)
{
  long i;

  if (!rgParms)
    rgParms = pMC->rgdParms; /* Put them in the usual place */

  for (i = iStart; i < pMC->nParms; i++) {
    CalculateOneMCParm (pMC->rgpMCVar[i]);
    rgParms[i] = pMC->rgpMCVar[i]->dVal;
  } /* for */

} /* CalcMCParms */


/* -----------------------------------------------------------------------------
   InitSetPoints

   Openn and reads the header of the SetPoint file containing the
   parameters to be tried.

   Returns the file pointer if everything is ok.
*/

BOOL InitSetPoints (PMONTECARLO pMC)
{
  PFILE pfile;

  if (!(pfile = fopen(pMC->szSetPointsFilename, "r")))
    ReportError (NULL, RE_CANNOTOPEN | RE_FATAL,
                 pMC->szSetPointsFilename, NULL);

  pMC->pfileSetPoints = pfile;

  /* Throw away the first line.  This allows a MC output file to be used
     directly as a setpoints file. */
  fscanf (pMC->pfileSetPoints,  "%*[^\n]");  getc(pMC->pfileSetPoints);

  if (feof(pMC->pfileSetPoints))
    ReportError (NULL, RE_INSUF_POINTS | RE_FATAL,
                 pMC->szSetPointsFilename, NULL);

  return (!pfile);

} /* InitSetPoints */


/* -----------------------------------------------------------------------------
   ReadSetPoints

   Reads set points from a file for this run.

   Returns non-zero if a full set of points is read, 0 otherwise.
*/

BOOL ReadSetPoints (PMONTECARLO pMC, double rgParms[])
{
  BOOL bReturn = FALSE; /* Initially, flag no points read */
  long i;

  if (!rgParms)
    rgParms = pMC->rgdParms; /* Put data in the usual place */

  fscanf(pMC->pfileSetPoints, "%*s"); /* Throw away dummy field */

  /* Increment across set point parms list */
  for (i = 0; i < pMC->nSetParms; i++) {

    /* Try to read one data point */

    if (feof(pMC->pfileSetPoints)
    || (fscanf(pMC->pfileSetPoints, "%lg", &pMC->rgpMCVar[i]->dVal)
        == EOF)) {

      if (pMC->nRuns) /* More points expected */
        ReportError (NULL, RE_INSUF_POINTS | RE_FATAL,
                     pMC->szSetPointsFilename, NULL);

      /* If !nRuns, flag that EOF reached without issuing error */

      goto Exit_ReadSetPoints;
    } /* if */

    rgParms[i] = pMC->rgpMCVar[i]->dVal; /* Copy value to user array */
  } /* for */

  bReturn = TRUE; /* Flag that all parms were read */

  /* Throw away remainder of line.  This allows a MC output file to be used
     directly as a setpoints file.  */
  fscanf (pMC->pfileSetPoints,  "%*[^\n]");  getc(pMC->pfileSetPoints);

Exit_ReadSetPoints:
  ;
  return (bReturn);

} /* ReadSetPoints */


/* -----------------------------------------------------------------------------
   GetMCMods

   Calculates random parameter variations or reads a new set of
   modifications from the set points input file.

   Returns TRUE if got modifications.

   FALSE is only returned for a SetPoints analysis where
   the number of runs (nRuns) is set to zero.  In this case the
   simulation continues to set points until end of file is reached,
   and returns FALSE to flag the eof condition.
*/

BOOL GetMCMods (PANALYSIS panal, double rgParms[])
{
  BOOL bOK;

  if (panal->iType == AT_MONTECARLO) { /* Random Monte Carlo mods */
    CalcMCParms (&panal->mc, rgParms, 0); /* start at 0, do them all */
    return TRUE;
  } /* if */

  else if (panal->iType == AT_SETPOINTS) {
    /* read set point mods */
    bOK = ReadSetPoints (&panal->mc, rgParms);
    /* eventually override by Distrib specs, FB - 21/07/97 */
    CalcMCParms (&panal->mc, rgParms, panal->mc.nSetParms);
    return bOK;
  } /* else if */

  return (FALSE);

} /* GetMCMods */


/* -----------------------------------------------------------------------------
   SetParents

   FB 21/07/97: For each Monte Carlo variable, if there are parents referenced
   in hParm for some of the distributionla parameters, pdParms are set to point
   to the parent's dVals. If there is no parent, pdParms already point to their
   own dParms and nothing is done.

   The calculation starts at index iStart.
*/

void SetParents (PMONTECARLO pMC, long iStart)
{
  long i, j, k;
  PMCVAR pMCVar1, pMCVar2;
  BOOL bFound;

  for (i = iStart; i < pMC->nParms; i++) { /* for each pMCVar considered */
    pMCVar1 = pMC->rgpMCVar[i];
    for (j = 0; j < 4; j++) { /* for each of its distrib. param */
      if (pMCVar1->hParm[j] != 0) { 
        /* if there is a parent, find it, but parents must appear before 
           current pMCVar */
        bFound = FALSE;
        for (k = 0; k < i; k++) { 
          pMCVar2 = pMC->rgpMCVar[k];
          if (pMCVar1->hParm[j] == pMCVar2->hvar) {
            /* Point to the parent dVal */
            pMCVar1->pdParm[j] = &(pMCVar2->dVal);
            bFound = TRUE;
          }
        }
        if (!bFound) { /* oops, parent not found, error */
          printf ("\n"
                  "Error: parents must appear in Distrib statements before\n"
                  "       childrens when creating sampling dependencies - Exiting.\n\n");
          exit(0);
        }
      }
    }
  }

} /* SetParents */


/* End */
