/* mh.c

   written by Frederic Bois
   5 January 1996

   Copyright (c) 1996.  Frederic Bois.  All rights reserved.

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
     Logfile:  SCCS/s.mh.c
    Revision:  1.34
        Date:  14 Nov 1997
     Modtime:  06:39:03
      Author:  @a
   -- SCCS  ---------

*/

#include <stdio.h>
#include <string.h>

#include "mh.h"
#include "lsodes.h"
#include "lexerr.h"
#include "simmonte.h"


/* Function -------------------------------------------------------------------
   CalculateTotals

   Find total prior, likelihood for all MC vars
   Called from TraverseLevels
*/

void CalculateTotals (PLEVEL plevel, char **args) 
{
  PANALYSIS panal = (PANALYSIS)args[0];
  double *pdLnPrior = (double*)args[1];

  long n;

  for (n = 0; n < plevel->nMCVars; ++n) {
    *pdLnPrior += LnDensity(plevel->rgpMCVars[n], panal);
  }

} /* CalculateTotals */


/* Function -------------------------------------------------------------------
   CheckForFixed

   It is possible for an MC var to be fixed by a subsequent `=' statement;
   this routine marks such vars
   Called from TraverseLevels
*/

void CheckForFixed (PLEVEL plevel, char **args)
{
  long    n, m;
  PMCVAR  pMCVar;
  PVARMOD pFVar;

  for (n = 0; n < plevel->nMCVars; ++n) {
    pMCVar = plevel->rgpMCVars[n];
    for (m = 0; m < plevel->nFixedVars; ++m) {
      pFVar = plevel->rgpFixedVars[m];
      if (pMCVar->hvar == pFVar->hvar) {

        pMCVar->bIsFixed = TRUE;

        if (IsInput (pFVar->hvar)) {
          printf("Error: a sampled parameter cannot be assigned an input\n");
          exit(0);
        }
        else
          pMCVar->dVal = pFVar->uvar.dVal;
      }
    }
  }

} /* CheckForFixed */


/* Function -------------------------------------------------------------------
   CheckPrintStatements

   Consider statements of type 'Distrib(<var1>, <distrib>, Prediction, <var2>)'
   If var1 and var2 do not have identical 'Print' times, we cannot obtain
   the data likelihood for var1. Here we check these times.
   We also check for two print statements for the same variable, which can
   affect the output under some circumstances.
   Checks also that data statements match print statements, at least for 
   the variables var1 whose likelihood is written. 
   Called from TraverseLevels
*/

void CheckPrintStatements (PLEVEL plevel, char **args)
{
  PANALYSIS panal = (PANALYSIS)args[0];
  POUTSPEC pos;
  PMCVAR pMCVar;
  long n, m, VarIndex = 0, SDIndex;

  if (plevel->pexpt == NULL)
    return;

  pos = &(plevel->pexpt->os);

  /* check that the same var does not appear in 2 or more print statements */
  for (n = 0; n < pos->nOutputs; ++n)
    for (m = n+1; m < pos->nOutputs; ++m)
      if (pos->phvar[n] == pos->phvar[m])
        ReportRunTimeError 
          (panal, RE_DUPVARINPRINT | RE_FATAL, "CheckPrintStatements");

  if (panal->nModelVars == 0) {
    printf ("\nError: you must specify a likelihood function for MCMC "
            "sampling - Exiting.\n\n");
    exit(0);
  }

  for (n = 0; n < panal->nModelVars; ++n) {
    pMCVar = panal->rgpModelVars[n];

    if ((pMCVar->cVarParm >> 1 & MCVP_VARIABLE) &&
        IsModelVar(pMCVar->hParm[1])) {
      SDIndex = -1;

      for (m = 0; m < pos->nOutputs; ++m) {
        if (pMCVar->hvar == pos->phvar[m])
          VarIndex = m;
        if (pMCVar->hParm[1] == pos->phvar[m])
          SDIndex = m;
      }

      if (SDIndex == -1)
        ReportRunTimeError
          (panal, RE_NOPRINTSTATEMENT | RE_FATAL, "CheckPrintStatements");

      if (pos->pcOutputTimes[VarIndex] != pos->pcOutputTimes[SDIndex])
        ReportRunTimeError
          (panal, RE_UNEQUALNUMTIMES | RE_FATAL, "CheckPrintStatements");

      if (!pos->prgdDataVals[VarIndex]) /* FB 01/07/97 */
        ReportRunTimeError 
          (panal, RE_NODATASTATEMENT | RE_FATAL, "CheckPrintStatements");

      for (m = 0; m < pos->pcOutputTimes[VarIndex]; ++m)
        if (pos->prgdOutputTimes[VarIndex][m] !=
            pos->prgdOutputTimes[SDIndex][m])
          ReportRunTimeError
              (panal, RE_UNEQUALTIMES | RE_FATAL, "CheckPrintStatements");

      pMCVar->iParmIndex = SDIndex;
    }
  }

} /* CheckPrintStatements */


/* Function -------------------------------------------------------------------
   CloneMCVars

   Called from TraverseLevels
   For all MC vars in list at given level, add to arrays of all instances of
   next (lower) level
*/
void CloneMCVars (PLEVEL plevel, char **args)
{
  long nMCVars = ListLength(plevel->plistMCVars);
  long n;
  PLEVEL pLower;

  for (n = 0; n < plevel->iInstances; ++n) {
    pLower = plevel->pLevels[n];
    pLower->nMCVars = nMCVars;
    if (nMCVars != 0) {
      if ((pLower->rgpMCVars = (PMCVAR*) malloc (nMCVars * sizeof(PMCVAR))) ==
          NULL)
        ReportError (NULL, RE_OUTOFMEM | RE_FATAL, "CloneMCVars", NULL);
    }
  }

  nMCVars = 0;
  ForAllList3 (plevel->plistMCVars, &CloneMCVarsL, plevel, &nMCVars, NULL);

} /* CloneMCVars */


/* Function -------------------------------------------------------------------
   CloneMCVarsL

   Called from ForAllList3
*/
void CloneMCVarsL (PVOID pData, PVOID pUser1, PVOID pUser2, PVOID pUser3)
{
  PMCVAR pMCVar = (PMCVAR) pData;
  PLEVEL plevel = (PLEVEL) pUser1;
  long *pnMCVars = (long *) pUser2;
  long n;
  PLEVEL pLower;
  PMCVAR pClone;

  ++pMCVar->iDepth;
  for (n = 0; n < plevel->iInstances; ++n) {
    pLower = plevel->pLevels[n];
    if ((pClone = (PMCVAR) malloc (sizeof (MCVAR))) == NULL)
      ReportError (NULL, RE_OUTOFMEM | RE_FATAL, "CloneMCVarsL", NULL);
    memcpy (pClone, pMCVar, sizeof (MCVAR));
    pClone->plistDependents = InitList ();
    pLower->rgpMCVars[*pnMCVars] = pClone;
  }
  ++(*pnMCVars);

} /* CloneMCVarsL */



/* Function -------------------------------------------------------------------
   CloseMarkovFiles

   Closes output files associated with the Markov sampler.
   The restart file has already been closed by the ReadRestart

*/

void CloseMarkovFiles (PANALYSIS panal)
{
  if (panal->gd.pfileOut) {
    fclose (panal->gd.pfileOut);
    printf ("\nWrote results to \"%s\"\n", panal->gd.szGout);
  }

} /* CloseMarkovFiles */


/* Function -------------------------------------------------------------------
   ConvertLists
*/

void ConvertLists(PLEVEL plevel, char **args)
{
  PANALYSIS panal = (PANALYSIS)args[0];
  long n;
  PMCVAR pMCVar;

  ListToPMCArray (panal, panal->plistModelVars, &panal->nModelVars, 
                  &panal->rgpModelVars);

  if (plevel->pexpt == NULL)
    ListToPVArray (panal, plevel->plistVars, &plevel->nFixedVars,
                   &plevel->rgpFixedVars);
  else
    ListToPVArray (panal, plevel->pexpt->plistParmMods, &plevel->nFixedVars,
                   &plevel->rgpFixedVars);

  for (n = 0; n < plevel->nMCVars; ++n) {
    pMCVar = plevel->rgpMCVars[n];
    ListToPMCArray (panal, pMCVar->plistDependents,
                    &pMCVar->nDependents, &pMCVar->rgpDependents);
    if (pMCVar->nDependents == 0)
      pMCVar->bExptIsDep = TRUE;
  }
    

} /*ConvertMCLists */


/* Function -------------------------------------------------------------------
   DoMarkov

   Core routine of the MCMC sampler
*/

void DoMarkov (PANALYSIS panal)
{
  PGIBBSDATA pgd = &panal->gd;
  PLEVEL     pLevel0 = panal->pLevels[0];
  long       nThetas, nUpdateAt, nTotal;
  long       i, iter = 0;
  long       nIter = pgd->nMaxIter;  /* scheduled iterations of the sampler */
  double     *pdMCVarVals  = NULL;   /* read in values of thetas */
  double     *pdSum        = NULL;   /* column sums of read thetas */
  double     **prgdSumProd = NULL;   /* column sums of thetas cross-products */
  double     dLnPrior = 0, dLnData = 0;

  InitRandom (panal->dSeed, TRUE);

  if (pgd->nInitIter == 1)
    printf ("\nDoing analysis -"
            "\nPrinting data and predictions for the last line of the "
            "restart file\n");
  else
    printf ("\nDoing analysis - %ld %s iteration%s\n",
            nIter, "MCMC sampling", (nIter != 1 ? "s " : " "));

  OpenMarkovFiles (panal);

  /* we do not want more than 1 top level; Get out; 
     This should be dealth with earlier, though */
  if (panal->iInstances > 1) {
    printf("Error: only one top level is allowed\nExiting\n");
    exit(0);
  }

  /* MC variables must be placed in arrays at the next lower level */
  TraverseLevels (pLevel0, CloneMCVars, NULL);

  /* Find the parents and dependents of the MC vars */
  TraverseLevels (pLevel0, FindMCParents, panal, NULL);
  TraverseLevels (pLevel0, FindMCDependents, panal, NULL);

  /* Now that we have the MC vars right, write the output file header 
     unless it's just a run for fit checking */
  if (pgd->nInitIter != 1) {
    fprintf (pgd->pfileOut, "iter\t");
    TraverseLevels (pLevel0, WriteHeader, panal, pgd->pfileOut, NULL);
    fprintf (pgd->pfileOut, "LnPrior\tLnData\tLnPosterior\n");
    fflush (pgd->pfileOut);
  }

  /* Convert the rest of the lists to arrays */
  TraverseLevels (pLevel0, ConvertLists, panal, NULL);

  /* Check for MC vars that have been fixed */
  TraverseLevels (pLevel0, CheckForFixed, NULL);

  /* Check variables in statements of type
     'Distrib(<var1>, <distrib>, Prediction, <var2>)'
     for identical 'Print' statements */
  TraverseLevels (pLevel0, CheckPrintStatements, panal, NULL);

  /* Print out the structure for checking */
  if (panal->bDependents) {
    TraverseLevels (pLevel0, PrintDeps, NULL);
    CloseMarkovFiles (panal);
    return;
  }

  /* Change the MC vars hvar pointers from pointing to model parameters to 
     pointing to the parents' dVal */
  TraverseLevels (pLevel0, SetPointers, panal, NULL);

  /* Get the initial values of the MC vars by reading th restart file if
     one is given */
  if (pgd->szGrestart) { 

    /* Read them from the restart file */
    nThetas = 0;
    TraverseLevels (pLevel0, GetNumberOfMCVars, &nThetas, NULL);

    if ( !(pdMCVarVals = InitdVector (nThetas)) ||
         !(pdSum       = InitdVector (nThetas)) ||
         !(prgdSumProd = InitdMatrix (nThetas, nThetas)))
      ReportRunTimeError (panal, RE_OUTOFMEM | RE_FATAL, "DoMarkov");

    /* Read the starting values in the order they are printed and
       close the file when finished */
    ReadRestart (pgd->pfileRestart, nThetas, pdMCVarVals, pdSum, prgdSumProd,
                 &iter);

    /* Set the dVals of the variables to the values read in */
    nThetas = 0;
    if (!TraverseLevels1 (pLevel0, SetMCVars, pdMCVarVals, &nThetas, NULL)) {
      printf ("\nError: some read-in parameters are out of bounds - "
              "Exiting\n\n");
      exit(0);
    }

    /* If nInitIter is 1 print just the predictions and data and exit */
    if (pgd->nInitIter == 1) {
      PrintAllExpts (pLevel0, 0, panal, pgd->pfileOut);
      CloseMarkovFiles (panal);
      return;
    }

    if (pgd->nInitIter == 0) { /* component jumps */
      /* Set the jumping kernel's SD */
      TraverseLevels (pLevel0, SetKernel, 1, pdMCVarVals, NULL);

      /* Free the now useless array */
      free (pdMCVarVals);
    }
    else { /* vector jumping, initialize prior */
      TraverseLevels (pLevel0, CalculateTotals, panal, &dLnPrior, NULL);
    }

    /* Initialize the predictions arrays by running all experiments and
       save the likelihood */
    if (!RunAllExpts (pLevel0, 0, panal, &dLnData)) {
      /* error, cannot compute */
      printf ("\nError: cannot compute at the starting point - Exiting\n\n");
      exit(0);
    }
  }

  else { /* no restart file, init by sampling */

    /* If nInitIter is 1 print an error message and exit */
    if (pgd->nInitIter == 1) {
      printf ("\nError: a restart file must be given to print data and"
              "         predictions - Exiting.\n\n");
      exit (0);
    }    

    /* Set the jumping kernel's SD */
    TraverseLevels (pLevel0, SetKernel, 2, pdMCVarVals, NULL);

    /* Initialize the thetas by sampling, write them out at the same time */
    fprintf (pgd->pfileOut, "0\t");
    TraverseLevels (pLevel0, InitMCVars, panal, pgd->pfileOut, NULL);

    /* Calculate the total prior */
    TraverseLevels (pLevel0, CalculateTotals, panal, &dLnPrior, NULL);
    /* Initilalize the predictions arrays by running all experiments and
       save the data likelihood output */
    if (!RunAllExpts (pLevel0, 0, panal, &dLnData)) {
      /* error, cannot compute */
      printf ("\nError: cannot compute at the starting point - Exiting\n\n");
      exit(0);
    }

    /* Output prior and likelihood */
    fprintf (pgd->pfileOut, "%e\t%e\t%e\n", dLnPrior, dLnData,
             dLnPrior + dLnData);
    fflush (pgd->pfileOut);
  }

  /* Save the data likelihoods */
  TraverseLevels1 (pLevel0, SaveLikelihoods, NULL);

  /* Initializations are finished, let's do the iterations */
  nUpdateAt = UPDATE_AT;
  nTotal = UPDATE_AT + 1;

  while (iter < nIter) {

    /* Output to screen, eventually */
    if (panal->bPrintIter && ((iter+1) % 100 == 0))
      fprintf(stderr, "Iteration %ld\n", iter + 1);

    /* Start output to file, eventually */
    if (((iter + 1) % pgd->nPrintFreq == 0) &&
        (iter >= pgd->nMaxIter - pgd->nPrintIter))
      fprintf (pgd->pfileOut, "%ld\t", iter + 1);

    if (pgd->nInitIter == 0) { /* component jumps */ 

      TraverseLevels (pLevel0, SampleThetas, panal, pgd, &iter, &nUpdateAt, 
                      &nTotal, NULL);

      /* Output log-densities, eventually */
      if (((iter + 1) % pgd->nPrintFreq == 0) &&
          (iter >= pgd->nMaxIter - pgd->nPrintIter)) {  
        dLnPrior = 0.0;
        TraverseLevels (pLevel0, CalculateTotals, panal, &dLnPrior, NULL);
        dLnData = 0.0;     
        TraverseLevels1 (pLevel0, SumAllExpts, &dLnData, NULL);
        fprintf (pgd->pfileOut, "%e\t%e\t%e\n", dLnPrior, dLnData,
               dLnPrior + dLnData);
        fflush (pgd->pfileOut);
      }
    }
    else { /* vector jumps */

      SampleThetaVector (pLevel0, panal, nThetas, pdMCVarVals, pdSum, 
                         prgdSumProd, iter, nUpdateAt, &dLnPrior, &dLnData);

      /* Output log-densities, eventually */
      if (((iter + 1) % pgd->nPrintFreq == 0) &&
          (iter >= pgd->nMaxIter - pgd->nPrintIter)) { 
        for (i = 0; i < nThetas; i++) 
          fprintf(pgd->pfileOut, "%5g\t", pdMCVarVals[i]);
        fprintf (pgd->pfileOut, "%e\t%e\t%e\n", dLnPrior, dLnData,
               dLnPrior + dLnData);
        fflush (pgd->pfileOut);
      }
    }


    /* Adjust the update time eventually */
    if (iter == nUpdateAt) {
      nUpdateAt = nUpdateAt * 1.5;
      nTotal = nUpdateAt - iter;
    }

    /* Increment the iteration counter */
    ++iter;

  } /* while iter */

  CloseMarkovFiles (panal);

} /* DoMarkov */


/* Function -------------------------------------------------------------------
   FindMCParents

   Called from TraverseLevels
   Find the parents of the MC vars at this level by looking at this and
   all previous levels.
*/

void FindMCParents (PLEVEL plevel, char **args)
{
  PANALYSIS panal = (PANALYSIS)args[0];
  long      n, m, l, k;
  PLEVEL    pPrevLev;
  PMCVAR    pMCVar1, pMCVar2;

  /* Set the current level array as we pass through */
  panal->pCurrentLevel[plevel->iDepth] = plevel;

  /* First, this level; Parents must appear before current in the array */
  for (n = 0; n < plevel->nMCVars; ++n) {
    pMCVar1 = plevel->rgpMCVars[n];
    for (m = 0; m < n; ++m) {
      pMCVar2 = plevel->rgpMCVars[m];
      for (l = 0; l < 4; ++l) {
        if (pMCVar1->hParm[l] == pMCVar2->hvar) {
          pMCVar1->pMCVParent[l] = pMCVar2;
        }
      }
    }
  }

  /* Now, all previous levels */
  for (n = plevel->iDepth-1; n >= 0; --n) {
    pPrevLev = panal->pCurrentLevel[n];
    for (m = 0; m < plevel->nMCVars; ++m) {
      pMCVar1 = plevel->rgpMCVars[m];
      for (l = 0; l < pPrevLev->nMCVars; ++l) {
        pMCVar2 = pPrevLev->rgpMCVars[l];
        for (k = 0; k < 4; ++k) {
          if (pMCVar1->pMCVParent[k] == NULL &&
              pMCVar1->hParm[k] == pMCVar2->hvar) {
            pMCVar1->pMCVParent[k] = pMCVar2;
          }
        }
      }
    }
  }

} /* FindMCParents */


/* Function -------------------------------------------------------------------
   FindMCDependents

   Called from TraverseLevels
   Find the direct dependents of the MC vars at this level by looking at this
   and all lower levels
*/

void FindMCDependents (PLEVEL plevel, char **args)
{
  long n, m;

  for (n = 0; n < plevel->nMCVars; ++n)
    for (m = 0; m < 4; ++m)
      if (plevel->rgpMCVars[n]->pMCVParent[m] != NULL &&
          plevel->rgpMCVars[n]->pMCVParent[m]->hvar == 
          plevel->rgpMCVars[n]->hParm[m])
        QueueListItem(plevel->rgpMCVars[n]->pMCVParent[m]->plistDependents,
                      plevel->rgpMCVars[n]);

} /*FindMCDependents */


/* Function -------------------------------------------------------------------
   GetNumberOfMCVars

   Find the total number of MC vars
   Called from TraverseLevels
*/
void GetNumberOfMCVars (PLEVEL plevel, char **args) 
{
  long *pnThetas = (long *) args[0];

  *pnThetas += plevel->nMCVars;

} /* GetNumberOfMCVars */


/* Function -------------------------------------------------------------------
   InitMCVars

   Sample initial values of thetas if not fixed
   Called from TraverseLevels
*/

void InitMCVars(PLEVEL plevel, char **args)
{
  PANALYSIS panal = (PANALYSIS)args[0];
  FILE      *pOutFile = (FILE*)args[1];
  long      n;

  for (n = 0; n < plevel->nMCVars; ++n) 
    if ( !(plevel->rgpMCVars[n]->bIsFixed))
      CalculateOneMCParm (plevel->rgpMCVars[n]); 

  /* Write out the sampled values */
  WriteMCVars (plevel, panal, pOutFile);

} /* InitMCVars */


/* Function -------------------------------------------------------------------
   ListToPMCArray
*/

void ListToPMCArray (PANALYSIS panal, PLIST plist,
                     long *pnMCVars, PMCVAR **rgpMCVars)
{
  if ((*pnMCVars = ListLength(plist)) == 0)
    return;

  if ((*rgpMCVars = (PMCVAR*) malloc (*pnMCVars * sizeof(PMCVAR))) == NULL)
    ReportRunTimeError (panal, RE_OUTOFMEM | RE_FATAL, "ListToPMCArray");

  *pnMCVars = 0;
  ForAllList3 (plist, &ListToPMCArrayL, pnMCVars, *rgpMCVars, NULL);

} /*ListToPMCArray */


/* Function -------------------------------------------------------------------
   ListToPMCArrayL
*/

void ListToPMCArrayL (PVOID pData, PVOID pUser1, PVOID pUser2, PVOID pUser3)
{
  PMCVAR pMCVar = (PMCVAR)pData;
  long *pnMCVars = (long*)pUser1;
  PMCVAR *rgpMCVars = (PMCVAR*)pUser2;

  rgpMCVars[(*pnMCVars)++] = pMCVar;

} /* ListToPMCArrayL */


/* Function -------------------------------------------------------------------
   ListToPVArray
*/

void ListToPVArray (PANALYSIS panal, PLIST plist,
                    long *pnFixedVars, PVARMOD **rgpFixedVars)
{
  if ((*pnFixedVars = ListLength (plist)) == 0)
    return;

  if ((*rgpFixedVars = (PVARMOD*) malloc (*pnFixedVars * sizeof(PVARMOD))) == 
       NULL)
    ReportRunTimeError (panal, RE_OUTOFMEM | RE_FATAL, "ListToPVArray");

  *pnFixedVars = 0;
  ForAllList3 (plist, &ListToPVArrayL, pnFixedVars, *rgpFixedVars, NULL);

} /*ListToPVArray */


/* Function -------------------------------------------------------------------
   ListToPVArrayL
*/

void ListToPVArrayL (PVOID pData, PVOID pUser1, PVOID pUser2, PVOID pUser3)
{
  PVARMOD pVar = (PVARMOD)pData;
  long    *pnFixedVars = (long*)pUser1;
  PVARMOD *rgpFixedVars = (PVARMOD*)pUser2;

  rgpFixedVars[(*pnFixedVars)++] = pVar;

} /* ListToPVArrayL */


/* Function -------------------------------------------------------------------
   LnDensity

   Returns the log of the (exact) density of variate under its distribution. 
*/
#define LNSQRT2PI 9.189385332046E-01

double LnDensity (PMCVAR pMCVar, PANALYSIS panal)
{
  double dTmp, density;
  double dParm1 = *(pMCVar->pdParm[0]);
  double dParm2 = *(pMCVar->pdParm[1]);
  double dMin   = *(pMCVar->pdParm[2]);
  double dMax   = *(pMCVar->pdParm[3]);
  double dTheta = pMCVar->dVal;
  char str[10];

  /* This should take care of all dTheta checking */
  if (pMCVar->iType == MCV_BINOMIALBETA) {
    if (dTheta < 0) {
      printf ("Error: variate out of bounds in LnDensity\n");
      exit (0);
    }
  }
  else {
    if (dTheta > dMax || dTheta < dMin) {
      printf ("Error: variate out of bounds in LnDensity\n");
      exit (0);
    }
  }

  switch (pMCVar->iType) {

    case MCV_UNIFORM:
      if (dTheta > dParm2 || dTheta < dParm1)
        return (BAD_VAL);
      if (dParm2 <= dParm1)
        ReportRunTimeError (panal, RE_BADUNIFORMDIST | RE_FATAL,
                            pMCVar->pszName, "LnDensity");
      return -log(dParm2 - dParm1);

    case MCV_LOGUNIFORM:
      if (dTheta > dParm2 || dTheta < dParm1)
        return (BAD_VAL);
      if (dParm2 <= dParm1)
        ReportRunTimeError (panal, RE_BADUNIFORMDIST | RE_FATAL,
                            pMCVar->pszName, "LnDensity");
      return -log (dTheta * (dParm2 - dParm1));

    case MCV_NORMALV: dParm2 = sqrt (dParm2); /* fall thru */
    case MCV_NORMAL:
      return lnDFNormal (dTheta, dParm1, dParm2);

    case MCV_LOGNORMALV: dParm2 = exp(sqrt(dParm2)); /* fall thru */
    case MCV_LOGNORMAL:
      if (dParm1 <= 0.0) {
        sprintf(str, "%5.2e", dParm1);
        ReportRunTimeError(panal, RE_BADLOGNORMALMEAN | RE_FATAL,
                           pMCVar->pszName, str, "LnDensity");
      }
      return lnDFNormal (log (dTheta), log (dParm1), log (dParm2));

    case MCV_TRUNCNORMALV: dParm2 = sqrt (dParm2); /* fall thru */
    case MCV_TRUNCNORMAL:
      if (dParm2 <= 0.0) {
        sprintf(str, "%5.2e", dParm2);
        ReportRunTimeError(panal, RE_BADNORMALSD | RE_FATAL,
                           pMCVar->pszName, str, "LnDensity");
      }
      return lnDFNormal (dTheta, dParm1, dParm2) / 
             (CDFNormal ((dMax - dParm1) / dParm2) - 
              CDFNormal ((dMin - dParm1) / dParm2));

    case MCV_TRUNCLOGNORMALV: dParm2 = exp(sqrt(dParm2)); /* fall thru */
    case MCV_TRUNCLOGNORMAL:
      if (dParm1 <= 0.0 ) {
        sprintf(str, "%5.2e", dParm1);
        ReportRunTimeError(panal, RE_BADLOGNORMALMEAN | RE_FATAL,
                           pMCVar->pszName, str, "LnDensity");
      }
      if (dParm2 <= 1.0 ) {
        sprintf(str, "%5.2e", dParm2);
        ReportRunTimeError(panal, RE_BADLOGNORMALSD | RE_FATAL,
                           pMCVar->pszName, str, "LnDensity");
      }
      dTmp = log (dParm2);
      return lnDFNormal (log (dTheta), log (dParm1), dTmp) /
             (CDFNormal (log (dMax / dParm1) / dTmp) - 
              CDFNormal (log (dMin / dParm1) / dTmp));

    case MCV_BETA:
      return lnDFBeta (dTheta, dParm1, dParm2, dMin, dMax);

    case MCV_CHI2:
      dTmp = 0.5 * dParm1;
      return (dTmp - 1) * log (dTheta) - 0.5 * dTheta +
             dTmp * (-6.9314718056E-01) - lnGamma (dTmp);

    case MCV_BINOMIAL:
      if ((dParm1 < 0) || (dParm1 > 1)) {
        printf ("Error: bad p for binomial variate in LnDensity\n");
        exit (0);
      }
      if (dTheta > dParm2) {
        printf ("Error: bad N for binomial variate in LnDensity\n");
        exit (0);
      }
      /* log binomial coefficient n! / (x!(n-x)!) */
      dTmp = lnGamma (dParm2 + 1) - lnGamma (dTheta + 1) - 
             lnGamma (dParm2 - dTheta + 1);

      if (dParm1 == 0) { /* revised FB 18/07/97 */
        if (dTheta != 0)
          return (-1e10); /* should be -INF */
      }
      else
        dTmp = dTmp + dTheta * log (dParm1);
      
      if (dParm1 == 1) { /* revised FB 18/07/97 */
        if ((dParm2 - dTheta) == 0) 
          return dTmp; /* because log(0^0) is 0 */
        else
          return (-1e10); /* should be -INF */
      }
      else
        return dTmp + (dParm2 - dTheta) * log (1 - dParm1);

    case MCV_PIECEWISE:
      density = 2 / (dMax + dParm2 - dParm1 - dMin);

      if (dTheta <= dParm1)
        return log (density * (dTheta - dMin) / (dParm1 - dMin));

      else
        if (dTheta <= dParm2)
          return log (density);
        else
          return log (density * (dMax - dTheta) / (dMax - dParm2));

    case MCV_EXPONENTIAL:
      if (dParm1 <= 0) {
        printf ("Error: negative or null inverse scale (%g) for exponential "
                "variate in LnDensity\n", dParm1);
        exit (0);
      }
      return -dTheta * dParm1 * log (dParm1);

    case MCV_GGAMMA:
      if (dParm2 <= 0) {
        printf ("Error: bad inv. scale for gamma variate in LnDensity\n");
        exit (0);
      }
      return (dParm1 - 1) * log (dTheta) - dParm2 * dTheta +
             dParm1 * log (dParm2) - lnGamma (dParm1);

    case MCV_INVGGAMMA:
      if (dParm2 <= 0) {
        printf ("Error: bad scale for inv. gamma variate in LnDensity\n");
        exit (0);
      }
      return (-dParm1 - 1) * log (dTheta) - dParm2 / dTheta +
             dParm1 * log (dParm2) - lnGamma (dParm1);

    case MCV_POISSON:
      if (dParm1 <= 0) {
        printf ("Error: bad rate for Poisson variate in LnDensity\n");
        exit (0);
      }
      return dTheta * log (dParm1) - dParm1 - lnGamma (dTheta + 1);

    case MCV_BINOMIALBETA:
      if (dParm1 < 0) {
        printf ("Error: bad expectation for BinomialBeta variate "
                "in LnDensity\n");
        exit (0);
      }
      if (dParm2 <= 0) {
        printf ("Error: bad alpha for BinomialBeta variate in LnDensity\n");
        exit (0);
      }
      if (dMin <= 0) {
        printf ("Error: bad beta for BinomialBeta variate in LnDensity\n");
        exit (0);
      }
      dTmp = floor (0.5 + dParm1 + dParm1 * dMin / dParm2); /* this is N */
      if (dTheta > dTmp)
        return (-1e10);
      else {
        if ((dParm2 == 0.5) && (dMin == 0.5)) 
          dTmp = lnGamma (0.5 + dTheta) + 
                 lnGamma (0.5 + dTmp - dTheta) -
                 lnGamma (dTheta + 1) - lnGamma (dTmp - dTheta + 1);
        else
          dTmp = lnGamma (dParm2 + dMin) + lnGamma (dTmp + 1) +
                 lnGamma (dParm2 + dTheta) +
                 lnGamma (dMin + dTmp - dTheta) -
                 lnGamma (dTheta + 1) - lnGamma (dTmp - dTheta + 1) -
                 lnGamma (dParm2) - lnGamma(dMin) - 
                 lnGamma (dParm2 + dMin + dTmp);
        return dTmp;
      }

    default:
      ReportRunTimeError(panal, RE_UNKNOWNDIST | RE_FATAL, "LnDensity");

  } /* switch */

  /* Not reached */
  return 0.0 ;

} /* LnDensity */


/* Function -------------------------------------------------------------------
   LnLike

   returns the log-likelihood of the dependents given the parameter specified
   by pMCVar
*/

double LnLike (PMCVAR pMCVar, PANALYSIS panal) 
{
  long n;
  double dDensity, dLnLike = 0.0;

  for (n = 0; n < pMCVar->nDependents; ++n)
    if ((dDensity = LnDensity(pMCVar->rgpDependents[n], panal)) == BAD_VAL)
      return BAD_VAL;
    else
      dLnLike += dDensity;

  return dLnLike;
  
} /* LnLike */


/* Function -------------------------------------------------------------------
   LnLikeData

   Likelihood of the data for one experiment
*/

double LnLikeData (PEXPERIMENT pexpt, PANALYSIS panal) {

  POUTSPEC pos;
  PMCVAR pMCVar;
  long n, m, l;
  double dLnLike = 0.0;
  double dTmp;

  pos = &pexpt->os;

  /* scan all the model states and outputs */
  for (n = 0; n < panal->nModelVars; ++n) {
    pMCVar = panal->rgpModelVars[n];

    /* scan all requested outputs in simulaion file */
    for (m = 0; m < pos->nOutputs; ++m) {
      /* if model variable and output handles match, process */
      if (pos->phvar[m] == pMCVar->hvar) {
        /* for each requested output time */
        for (l = 0; l < pos->pcOutputTimes[m]; ++l) 
          if (pos->prgdDataVals[m][l] != INPUT_MISSING_VALUE) {
            /* if the data is not coded as missing */
            if (pos->prgdOutputVals[m][l] != MISSING_VALUE) {
              /* if the model output is not coded as missing */

              pMCVar->dVal = pos->prgdDataVals[m][l]; /* data value */
              pMCVar->dParm[0] = pos->prgdOutputVals[m][l]; /* predictions */

              /* This finds the SD to use */
              if (pMCVar->cVarParm >> 1 & MCVP_VARIABLE) 
                if (!IsModelVar (pMCVar->hParm[1])) {
                  pMCVar->dParm[1] = GetVarValue (pMCVar->hParm[1]);
                  /* This is not very clean and the dVal of a MCVar structure
                     should be used instead */
                }
                else
                  pMCVar->dParm[1] = 
                    pos->prgdOutputVals[pMCVar->iParmIndex][l];
            
              dTmp = LnDensity (pMCVar, panal);
              if (dTmp == (BAD_VAL))
                return (BAD_VAL);
              else
                dLnLike = dLnLike + dTmp;
            }
            else
              ReportRunTimeError (panal, RE_BADMODEL | RE_FATAL, "LnLikeData");
          }
      }
    }
  }
  return (dLnLike);

} /* LnLikeData */


/* Function -------------------------------------------------------------------
   OpenMarkovFiles

   Opens the output file and the restart file.
*/

void OpenMarkovFiles (PANALYSIS panal)
{
  /* Take care of the output file first */

  /* Use command line spec if given */
  if (panal->expGlobal.os.bCommandLineSpec)
    panal->gd.szGout = panal->expGlobal.os.szOutfilename;

  /* Default if none given */
  else if (!(panal->gd.szGout))
    panal->gd.szGout = "MCMC.default.out";

  /* eventually open the restart file before crushing the output file */
  if (panal->gd.szGrestart)
    if (!(panal->gd.pfileRestart)
      && !(panal->gd.pfileRestart = fopen (panal->gd.szGrestart, "r")))
      ReportRunTimeError(panal, RE_FATAL | RE_CANNOTOPEN,
                         panal->gd.szGrestart, "OpenMarkovFiles");

  if (!(panal->gd.pfileOut)
      && !(panal->gd.pfileOut = fopen (panal->gd.szGout, "w")))
    ReportRunTimeError(panal, RE_FATAL | RE_CANNOTOPEN,
                       panal->gd.szGout, "OpenMarkovFiles");

} /* OpenMarkovFiles */


/* Function -------------------------------------------------------------------
   PrintAllExpts

   Run all experiments at level below `plevel'
*/

void PrintAllExpts (PLEVEL plevel, long nVar, PANALYSIS panal, PFILE pOutFile)
{
  long n;

  for (n = 0; n < plevel->iInstances; ++n)
    TraverseLevels1 (plevel->pLevels[n], PrintExpt, panal, pOutFile, NULL);

} /* PrintAllExpts */


/* Function -------------------------------------------------------------------
   PrintExpt

   Run all experiments and print the level code, experiment number, time, data,
   and predictions to the output file.

   Called from TraverseLevels1
*/

int PrintExpt (PLEVEL plevel, char **args)
{
  PANALYSIS   panal = (PANALYSIS)args[0];
  PFILE       pOutFile = (PFILE)args[1];
  long        l, m, n;
  PEXPERIMENT pExpt = plevel->pexpt;
  POUTSPEC    pos;
  static long printed_head = 0;

  if (!printed_head) {
    fprintf (pOutFile, 
             "Level\tExperiment\tOutput_Var\tTime\tData\tPrediction\n");
    printed_head = 1;
  }

  /* Set level sequence */
  panal->pCurrentLevel[plevel->iDepth] = plevel;

  panal->iInstance[plevel->iDepth] = plevel->iSequence;

  if (pExpt != NULL) {
    InitModel ();

    /* Set the model vars that have been sampled in this experiment and 
       above levels */
    for (n = 0; n <= plevel->iDepth; ++n) {
      SetModelVars (panal->pCurrentLevel[n]);
      SetFixedVars (panal->pCurrentLevel[n]);
    }

    if (!DoOneExperiment (pExpt)) {
      /* Error */
      printf ("Warning: DoOneExperiment failed\n");
      return 0;
    }
    else {
      pos = &pExpt->os;
      for (m = 0; m < pos->nOutputs; ++m) {
        for (l = 0; l < pos->pcOutputTimes[m]; ++l) {
          for (n = 1; n < plevel->iDepth; ++n)
            fprintf (pOutFile, "%d.", panal->iInstance[n]);
          fprintf (pOutFile, "%d\t", panal->iInstance[plevel->iDepth]);
          fprintf (pOutFile, "%d\t%s\t%g\t%g\t%g\n", pExpt->iExp,
                   pos->pszOutputNames[m],
                   pos->prgdOutputTimes[m][l], 
                   pos->prgdDataVals[m][l], pos->prgdOutputVals[m][l]);
        }
      }
    }

  } /* if */

  return (1);

} /* PrintExpt */


/* Function -------------------------------------------------------------------
   PrintDeps

   Called from TraverseLevels

   For debugging, print the variables, parents, and dependencies
*/

void PrintDeps (PLEVEL plevel, char **args)
{
  long n, m;
  PMCVAR pMCVar;

  fprintf (stderr, "Depth %d; Instance %d\n", 
           plevel->iDepth, plevel->iSequence);

  for (n = 0; n < plevel->nMCVars; ++n) {
    pMCVar = plevel->rgpMCVars[n];

    fprintf(stderr, "Variable %s (%d) [%lx]\n",
            pMCVar->pszName, pMCVar->iDepth, (unsigned long) pMCVar);

    for (m = 0; m < 4; ++m)
      if (pMCVar->pMCVParent[m] != NULL)
        fprintf (stderr, "  Parent %ld: %s (%d) [%lx]\n", m,
                 pMCVar->pMCVParent[m]->pszName,
                 pMCVar->pMCVParent[m]->iDepth, 
                 (unsigned long) pMCVar->pMCVParent[m]);

    for (m = 0; m < pMCVar->nDependents; ++m)
      fprintf (stderr, "  Dependent: %s (%d) [%lx]\n",
               pMCVar->rgpDependents[m]->pszName,
               pMCVar->rgpDependents[m]->iDepth, 
               (unsigned long) pMCVar->rgpDependents[m]);

    if (pMCVar->bExptIsDep)
      fprintf(stderr, "  This variable influences experiments directly\n");
  }

} /* PrintDeps */


/* Function -------------------------------------------------------------------
   ReadRestart

   initialize the population and individual parameters by reading them in the 
   restart file.
*/

void ReadRestart (FILE *pfileRestart, long nThetas,
                  double *pdTheta, double *pdSum, double **prgdSumProd, 
                  long *pIter) 
{
  register long i, j;

  *pIter = -1;

  for (i = 0; i < nThetas; i++) {
    pdSum[i] = 0.0;
    for (j = 0; j < nThetas; j++)
      prgdSumProd[i][j] = 0.0;
  }

  /* skip the first line.  This allows a MC output file to be used
     directly as a restart file. */
  fscanf (pfileRestart,  "%*[^\n]");  getc(pfileRestart);

  /* as long as we have not reached the end of the file we keep reading lines
     and overwriting the thetas, they keep only their last value. 
     We throw away first field, and we keep incrementing the global iteration 
     counter iter:
   */
  while (!(feof (pfileRestart) ||
          (fscanf (pfileRestart, "%*s") == EOF))) {
    for (i = 0; i < nThetas; i++) {
      if (fscanf(pfileRestart, "%lg", &(pdTheta[i])) == EOF) {
        printf ("Error: incorrect length for restart file - Exiting\n");
        exit(0);
      }
      else { /* reading ok */
        /* update pdSum, the column sum of pdTheta */
        pdSum[i] = pdSum[i] + pdTheta[i];
      }
    }

    /* Throw away remainder of line. This allows a MC output file to be used
       directly as a restart file. */
    fscanf (pfileRestart,  "%*[^\n]"); getc(pfileRestart);

    /* update prgdSumProd */
    for (i = 0; i < nThetas; i++)
      for (j = 0; j < nThetas; j++)
        prgdSumProd[i][j] = prgdSumProd[i][j] + pdTheta[i] * pdTheta[j];


    /* increment pIter */
    *pIter = *pIter + 1;

  } /* end while */

  /* note that the theta returned is the last parameter set read */

  fclose (pfileRestart);

} /* ReadRestart */


/* Function -------------------------------------------------------------------
   RestoreLikelihoods

   Called from TraverseLevels1
*/

int RestoreLikelihoods (PLEVEL plevel, char **args) 
{
  PEXPERIMENT pExpt = plevel->pexpt;

  if (pExpt != NULL) {
    pExpt->dLnLike = pExpt->dLnLikeSave;
  }

  return (1);

} /* RestoreLikelihoods */


/* Function -------------------------------------------------------------------
   RunAllExpts

   Run all experiments at level below `plevel'
*/

int RunAllExpts (PLEVEL plevel, long nVar, PANALYSIS panal, PDOUBLE pdLnData)
{
  long n;

  for (n = 0; n < plevel->iInstances; ++n) {
    if (!TraverseLevels1 (plevel->pLevels[n], RunExpt, panal, 
                          pdLnData, NULL)) {
      /* error */
      return (0);
    }
  }
  
  /* success */
  return (1);

} /* RunAllExpts */


/* Function -------------------------------------------------------------------
   RunExpt

   If `plevel' has no experiments, modify the variables using its two
   lists; if it has experiments, run them. Return 1 on success and 0 if
   failure

   Called from TraverseLevels1
*/

int RunExpt (PLEVEL plevel, char **args)
{
  PANALYSIS   panal = (PANALYSIS) args[0];
  double      *pdLnData = (double *) args[1];
  long        n;
  PEXPERIMENT pExpt = plevel->pexpt;

  /* Set level sequence */
  panal->pCurrentLevel[plevel->iDepth] = plevel;

  if (pExpt != NULL) {
    InitModel ();

    /* Set the model vars that have been sampled in this experiment and 
       above levels */
    for (n = 0; n <= plevel->iDepth; ++n) {
      SetModelVars (panal->pCurrentLevel[n]);
      SetFixedVars (panal->pCurrentLevel[n]);
    }

    if (!DoOneExperiment (pExpt)) {
      /* Error */
      printf ("Warning: DoOneExperiment failed\n");
      return 0;
    }
    else {
      pExpt->dLnLike = LnLikeData (pExpt, panal);
      *pdLnData += pExpt->dLnLike;
    }

  } /* if */

  return (1);

} /* RunExpt */


/* Function -------------------------------------------------------------------
   SampleTheta
*/

double SampleTheta (PMCVAR pMCVar) {

  /* if the parameter has a discrete distribution, round it - FB 12/06/97 */
  if (pMCVar->iType == MCV_BINOMIAL || pMCVar->iType == MCV_POISSON ) {
   return floor (0.5 + TruncNormalRandom (pMCVar->dVal, pMCVar->dKernelSD,
                                          *(pMCVar->pdParm[2]), 
                                          *(pMCVar->pdParm[3])));
  }
  else { /* FB fixed the uniform case - FB 30/06/97 */
    if (pMCVar->iType == MCV_UNIFORM || pMCVar->iType == MCV_LOGUNIFORM ) {
      return TruncNormalRandom (pMCVar->dVal, pMCVar->dKernelSD, 
                                *(pMCVar->pdParm[0]), *(pMCVar->pdParm[1]));
    }
    else {
      return TruncNormalRandom (pMCVar->dVal, pMCVar->dKernelSD, 
                                *(pMCVar->pdParm[2]), *(pMCVar->pdParm[3]));
    }
  }

} /* SampleTheta */


/* Function -------------------------------------------------------------------
   SampleThetas

   Sample thetas in sequence - test using prior and likelihood -
   restore old values if necessary - write new values to output file
   Called from TraverseLevels
*/

void SampleThetas (PLEVEL plevel, char **args)
{
  PANALYSIS  panal = (PANALYSIS) args[0];
  PGIBBSDATA pgd = (PGIBBSDATA) args[1];
  long *pnIter = (long *) args[2];
  long *pnUpdateAt = (long *) args[3];
  long *pnTotal = (long *) args[4];
  

  double dLnPrior, dLnLike, dLnData;
  double dLnPriorNew, dLnLikeNew, dLnDataNew;
  double dTheta, dJumps, dSum, dVar;
  PMCVAR pMCVar;
  static MCVAR tmpMCVar;
  long i, n;

  /* For all MC vars at this level */
  for (n = 0; n < plevel->nMCVars; ++n) {

    pMCVar = plevel->rgpMCVars[n];

    /* If the MC var is fixed, no sampling is made, just write it out */
    if (pMCVar->bIsFixed)
      goto WriteIt;

    if (pMCVar->bGibbs) {
      /* we can try Gibbs sampling if all dependent are distributed 
         normally around pMCVar. Only the normal case is currently
         implemented */

      /* form the sum of dependents */
      dSum = 0;
      for (i = 0; i < pMCVar->nDependents; ++i) 
        dSum = dSum + pMCVar->rgpDependents[i]->dVal;

      if (pMCVar->iType == MCV_NORMAL) { /* we have to form the variance */
        /* all dependents are supposed to be distributed with the same 
           variance, so use the first one */
        dVar = pow(*(pMCVar->rgpDependents[0]->pdParm[1]), 2);
      }
      else dVar = *(pMCVar->rgpDependents[0]->pdParm[1]);

      tmpMCVar.dParm[0] = (dVar * *(pMCVar->pdParm[0]) + 
                            *(pMCVar->pdParm[1]) * dSum) / 
                           (*(pMCVar->pdParm[1]) * pMCVar->nDependents + dVar);
      tmpMCVar.dParm[1] = 0;
      CalculateOneMCParm (&tmpMCVar);
    }
    else { /* Metropolis-Hastings */

      /* Compute prior and likelihood */
      dLnPrior = LnDensity (pMCVar, panal);
      dLnLike = LnLike (pMCVar, panal);
  
      dLnData = 0.0;
  
      /* If data are dependent compute the data likelihood */
      if (pMCVar->bExptIsDep) {
        /* Form the likelihood of all experiments at this level or beneath.*/
        TraverseLevels1 (plevel, SumAllExpts, &dLnData, NULL);
      }
  
      /* Save current value */
      dTheta = pMCVar->dVal;
  
      /* Adjust the jumping kernel SD, depending on acceptance rates, 
         make sure it does not exceed DBL_MAX or a third of the range */
      if (*pnIter == *pnUpdateAt) {

        dJumps = (double) pMCVar->lJumps / (double) (*pnTotal);

        /* the kernel should not be infinitely increased
        if (pMCVar->iType == MCV_UNIFORM || pMCVar->iType == MCV_LOGUNIFORM )
          dMaxSD = (*(pMCVar->pdParm[1]) / 6.0) - (*(pMCVar->pdParm[0]) / 6.0);
        else 
          dMaxSD = (*(pMCVar->pdParm[3]) / 6.0) - (*(pMCVar->pdParm[2]) / 6.0);
        removed by FB 5/11/1997
        */

        if (dJumps > 0.3) { 
          if (dJumps > 0.99999) { 
            /* pathological case: chances are the parameter has such a 
               small kernel that it does not change values, this leads
               to a "jump" each time. Drastic increase of the kernel is
               attempted */
            if (pMCVar->dKernelSD < sqrt(DBL_MAX))
              pMCVar->dKernelSD = pMCVar->dKernelSD * pMCVar->dKernelSD;
            else
              pMCVar->dKernelSD = DBL_MAX;
          }
          else { /* more normal case */
            /* the kernel should not be infinitely increased */
            if (pMCVar->dKernelSD < DBL_MAX / 2)
              pMCVar->dKernelSD = pMCVar->dKernelSD * 2;
            else
              pMCVar->dKernelSD = DBL_MAX;
          }
        }
        else {
          if (dJumps < 0.00001) { 
            /* pathological case: chances are the parameter has such a 
               big kernel that it will never jump. Drastic decrease of 
               the kernel is attempted, dissymetric from the increase */
            if (pMCVar->dKernelSD > pow(DBL_MIN, 0.45))
              pMCVar->dKernelSD = pow(pMCVar->dKernelSD, 0.45);
            else
              pMCVar->dKernelSD = DBL_MIN;
          }
          else { /* more normal case */
            /* the kernel should not be infinitely decreased; decrease
               is otherwise slighly different from the increase to avoid
               oscillations */
            if (pMCVar->dKernelSD > DBL_MIN / 0.4)
              pMCVar->dKernelSD = pMCVar->dKernelSD * 0.4;
            else
              pMCVar->dKernelSD = DBL_MIN;
          }
        }
  
        pMCVar->lJumps = 0; /* reset the jumps counter */

      }
  
      /* sample a new value */
      pMCVar->dVal = SampleTheta (pMCVar);
  
      /* recompute prior and likelihood */
      dLnPriorNew = LnDensity (pMCVar, panal);
      dLnLikeNew = LnLike (pMCVar, panal);
  
      dLnDataNew = 0.0;
  
      /* If data are dependent compute the data likelihood */
      if (pMCVar->bExptIsDep) {
        /* Run all experiments at this level or beneath.
           We should in fact run only the dependent experiments ! */
        
        if (!TraverseLevels1 (plevel, RunExpt, panal, &dLnDataNew, NULL)) {
          /* If running experiments fails, do not jump */
          pMCVar->dVal = dTheta;
          TraverseLevels1 (plevel, RestoreLikelihoods, NULL);
          goto WriteIt;
        }
      }      
  
      /* Test the results and act accordingly */
      if (!Test (pMCVar->bExptIsDep,  dLnPrior, dLnPriorNew, dLnLike, 
                 dLnLikeNew, dLnData, dLnDataNew)) {
        pMCVar->dVal = dTheta;
  
        if(pMCVar->bExptIsDep)
          TraverseLevels1 (plevel, RestoreLikelihoods, NULL);
      }
      else {
        ++pMCVar->lJumps;
  
        if(pMCVar->bExptIsDep)
          TraverseLevels1 (plevel, SaveLikelihoods, NULL);
      }
      } /* else (Metrolopolis-Hastings */
  
WriteIt: /* Write the MC var value to output file */

    if (((*pnIter+1) % pgd->nPrintFreq == 0) &&
        (*pnIter >= pgd->nMaxIter - pgd->nPrintIter)) {
      fprintf(pgd->pfileOut, "%5g\t", pMCVar->dVal);
    }
  }

} /* SampleThetas */


/* Function -------------------------------------------------------------------
   SampleThetaVector

   Sample thetas in block, do Metropolis test, write the sampled values to 
   the output file
*/

void SampleThetaVector (PLEVEL pLevel, PANALYSIS panal, long nThetas,
                        double *pdTheta, double *pdSum, double **prgdSumProd,
                        long iter, long nUpdateAt, PDOUBLE pdLnPrior, 
                        PDOUBLE pdLnData)
{
  register long i, j;
  double dTmp, dAccept, dLnPrior_old, dLnData_old;

  static long lAccepted = 0;
  static long lOldUpdate = 1;
  static double dJumpSpread;
  static PDOUBLE pdTheta_old = NULL; /* previous model parameters values */
  static PDOUBLE *prgdComponent;
  static PDOUBLE *prgdVariance;
  static PDOUBLE dNormVar; /* storage for nParms normal deviates */

  if ((pdTheta_old == NULL) || (iter == nUpdateAt)) {

    if (pdTheta_old == NULL) { /* initialize */
      if ( !(pdTheta_old = InitdVector (nThetas)) ||
           !(dNormVar    = InitdVector (nThetas)) ||
           !(prgdVariance  = InitdMatrix (nThetas, nThetas)) ||
           !(prgdComponent = InitdMatrix (nThetas, nThetas)))
        ReportRunTimeError (panal, RE_OUTOFMEM | RE_FATAL, 
                            "SampleThetaVector");

      /* initialize dJumpSpread */
      dJumpSpread = 2.4 / sqrt(nThetas); /* Gelman's Normal theory result */
    }
    else {
      /* if some vector samplings have been made, check that the current
         dJumpSpread leads to an acceptation rate of 15 to 30% over the
         last batch of simulations. Adjust eventually. */
      dAccept = ((double) lAccepted) / (double) (nUpdateAt - lOldUpdate);

      if ( dAccept > 0.3) dJumpSpread = dJumpSpread * 1.5;
      else if (dAccept < 0.15) dJumpSpread = dJumpSpread / 1.5;

      printf ("Monitoring: iter\t%ld\t", iter);
      printf ("success rate\t%g\tspread\t%g\n", dAccept, dJumpSpread);
      lAccepted = 0; /* reset the counter */
      lOldUpdate = nUpdateAt;
    }

    /* other updates: */

    /* generate the covariance matrix */
    for (i = 0; i < nThetas; i++)
      for (j = 0; j < nThetas; j++)
        prgdVariance[i][j] = (prgdSumProd[i][j] - 
                              pdSum[i] * pdSum[j] / (long) (iter+1)) / 
                              (long) iter;

    /* do the Cholesky decomposition of the covariance matrix */
    Cholesky (prgdVariance, prgdComponent, nThetas);

    /* do the Cholesky decomposition of the covariance matrix */
    if (!Cholesky (prgdVariance, prgdComponent, nThetas)) {
      /* try to save the computation by zeroing all non-diagonal elements */
      for (i = 0; i < nThetas; i++)
        for (j = 0; j < nThetas; j++)
        	if (i == j)
            prgdVariance[i][j] = prgdSumProd[i][j] / (double) (iter);
          else
            prgdVariance[i][j] = 0.0;

    	/* if is still does not work, exit */
      if (!Cholesky (prgdVariance, prgdComponent, nThetas)) {
      	printf ("Error: impossible to compute a jumping kernel - Exiting."
                "(You should check or change the restart file).\n\n");
      	exit (0);
      }   
    }
  }

  /* keep the value of all thetas */
  for (i = 0; i < nThetas; i++)
    pdTheta_old[i] = pdTheta[i];

  /* keep old prior and old likelihood */
  dLnPrior_old = *pdLnPrior; 
  dLnData_old  = *pdLnData;

  /* generate new pdTheta vector in the range of each element */
  do {
    for (i = 0; i < nThetas; i++) dNormVar[i] = NormalRandom(0, 1);

    for (i = 0; i < nThetas; i++) {
      dTmp = 0;
      for (j = 0; j <= i; j++) /* only the non-zero part of prgdComponent */
        dTmp = dTmp + dNormVar[j] * prgdComponent[i][j];

      pdTheta[i] = pdTheta_old[i] + dJumpSpread * dTmp;
    }

    /* Set the dVals of the variables to the values sampled and check that
       we are within bounds as a while condition */
    nThetas = 0;
  } while (!TraverseLevels1 (pLevel, SetMCVars, pdTheta, &nThetas, NULL));

  /* Calculate the new prior */
  *pdLnPrior = 0.0;
  TraverseLevels (pLevel, CalculateTotals, panal, pdLnPrior, NULL);

  /* compute the model at the newly drawn point and the likelihood */
  *pdLnData = 0.0;
  if (!RunAllExpts (pLevel, 0, panal, pdLnData)) {
    /* computation failed, don't jump, restore */
    for (i = 0; i < nThetas; i++)
      pdTheta[i] = pdTheta_old[i];
    *pdLnPrior = dLnPrior_old; 
    *pdLnData  = dLnData_old;
  }
  else {
    /* Test */
    if (log(Randoms()) > 
        ((*pdLnPrior) + (*pdLnData) - dLnPrior_old - dLnData_old)) { 
      /* don't jump, restore */
      for (i = 0; i < nThetas; i++)
        pdTheta[i] = pdTheta_old[i];
      *pdLnPrior = dLnPrior_old; 
      *pdLnData  = dLnData_old;
    }
    else { /* jump */
      lAccepted++; /* this is used above to adjust the acceptation rate */
    }
  }

  /* update arrays */
  for (i = 0; i < nThetas; i++) {
    pdSum[i] = pdSum[i] + pdTheta[i];
    for (j = 0; j < nThetas; j++)
      prgdSumProd[i][j] = prgdSumProd[i][j] + pdTheta[i] * pdTheta[j];
  }


} /* SampleThetaVector */


/* Function -------------------------------------------------------------------
   SaveLikelihoods

   Called from TraverseLevels1
*/

int SaveLikelihoods (PLEVEL plevel, char **args)
{
  PEXPERIMENT pExpt = plevel->pexpt;

  if (pExpt != NULL) {
    pExpt->dLnLikeSave = pExpt->dLnLike;
  }

  return (1);

} /* SaveLikelihoods */


/* Function -------------------------------------------------------------------
   SetFixedVars

   Set the array of fixed variables
*/

void SetFixedVars (PLEVEL plevel)
{
  long n;
  PVARMOD pFVar;

  for (n = 0; n < plevel->nFixedVars; ++n) {
    pFVar = plevel->rgpFixedVars[n];
    if (IsInput (pFVar->hvar))
      SetInput (pFVar->hvar, pFVar->uvar.pifn);
    else
      SetVar (pFVar->hvar, pFVar->uvar.dVal);
  }

} /* SetFixedVars */


/* Function -------------------------------------------------------------------
   SetKernel

   Set initial values of the MCMC jumping kernel
*/

void SetKernel (PLEVEL plevel, char **args)
{
  long useMCVarVals = (long) args[0]; /* 1 if dVal is to be restored, else 2 */
  double *pdMCVarVals = (double *) args[1];
  double dMin, dMax, dTmp;
  long   n, m;
  static long nThetas;
  PMCVAR pMCVar;

  /* set the jumping kernel's SD: sample 4 variates and take the range */
  for (n = 0; n < plevel->nMCVars; ++n) {

    if ( !(plevel->rgpMCVars[n]->bIsFixed)) {

      pMCVar = plevel->rgpMCVars[n];
      CalculateOneMCParm (pMCVar);

      dMin = dMax = pMCVar->dVal;
      for (m = 0; m < 3; ++m) {
        CalculateOneMCParm (pMCVar);
        dTmp = pMCVar->dVal;
        if (dMin >= dTmp) dMin = dTmp;
        else if (dMax < dTmp) dMax = dTmp;
      }

      /* The range can be at most 2 DBL_MAX, so we halve it in that case 
         to never exceed computational bounds, and we never form the
         dangerous complete range */
      if ((*(pMCVar->pdParm[2]) == -DBL_MAX) || 
          (*(pMCVar->pdParm[3]) == DBL_MAX))
        pMCVar->dKernelSD = (0.5 * dMax) - (0.5 * dMin);
      else
        pMCVar->dKernelSD = dMax - dMin;

      /* but if the range is zero (can happens for discrete variables, then 
         set it to half the prior range - FB 12/06/97 */
      if (pMCVar->dKernelSD == 0) {
        if ((*(pMCVar->pdParm[2]) == -DBL_MAX) || 
            (*(pMCVar->pdParm[3]) == DBL_MAX))
          pMCVar->dKernelSD = (0.5 * *(pMCVar->pdParm[3]) - 
                               0.5 * *(pMCVar->pdParm[2]));
        else 
          pMCVar->dKernelSD = *(pMCVar->pdParm[3]) - *(pMCVar->pdParm[2]);
      }   
    }

    /* restore the value of the variable - FB 02/07/97 */
    if (useMCVarVals == 1)
      plevel->rgpMCVars[n]->dVal = pdMCVarVals[nThetas++];

  }

} /* SetKernel */


/* Function -------------------------------------------------------------------
   SetModelVars

   Sets the array of model variables to the sampled values. Does not set fixed
   variables. That has to be done by SetFixedVars.
*/

void SetModelVars(PLEVEL plevel)
{
  long n;
  PMCVAR  pMCVar;

  for (n = 0; n < plevel->nMCVars; ++n) {
    pMCVar = plevel->rgpMCVars[n];
    if ( !(pMCVar->bIsFixed))
      SetVar (pMCVar->hvar, pMCVar->dVal);
  }

} /* SetModelVars */


/* Function -------------------------------------------------------------------
   SetMCVars

   Set initial values of thetas after reading input file or sampling them.
   Values are assumed to be in proper order.
   It also checks that the ranges are respected.
   Called from TraverseLevels.
*/

int SetMCVars (PLEVEL plevel, char **args)
{
  double *pdMCVarVals = (double *) args[0];
  long   *nThetas = (long *) args[1];
  PMCVAR pMCVar;
  double dVar;
  long   n;

  for (n = 0; n < plevel->nMCVars; ++n) {
    dVar = pdMCVarVals[(*nThetas)++];
    pMCVar = plevel->rgpMCVars[n];
    if ((pMCVar->iType == MCV_UNIFORM) || (pMCVar->iType == MCV_LOGUNIFORM)) {
      if ((dVar < *(pMCVar->pdParm[0])) || (dVar > *(pMCVar->pdParm[1]))) {
        /* error */
        return (0);
      }
    }
    else {
      if ((dVar < *(pMCVar->pdParm[2])) || (dVar > *(pMCVar->pdParm[3]))) {
        /* error */
        return (0);
      }
    }
     
    pMCVar->dVal = dVar;
  }

  /* success */
  return (1);

} /* SetMCVars */


/* Function -------------------------------------------------------------------
   SetPointers

   Called from TraverseLevels

   FB 26 nov 96: For each Monte Carlo variable, pdParms are set to point to the 
   parent's dVals rather than to model parameters. If there is no parent,
   pdParms point to their own dParms
*/

void SetPointers (PLEVEL plevel, char **args)
{
  long n, m;
  PMCVAR pMCVar;

  for (n = 0; n < plevel->nMCVars; ++n) {
    pMCVar = plevel->rgpMCVars[n];

    /* For each distribution parameter possible */
    for (m = 0; m < 4; ++m) {
      if (pMCVar->pMCVParent[m] == NULL) /* Point to its own values */
        pMCVar->pdParm[m] = &(pMCVar->dParm[m]);
      else /* Point to the parent dVal */
        pMCVar->pdParm[m] = &(pMCVar->pMCVParent[m]->dVal);
    }
  }

} /* SetPointers */


/* Function -------------------------------------------------------------------
   SumAllExpts

   If `plevel' has experiments, add the current Likelihood and
   sum-of-squares to the totals

   Called from TraverseLevels1
*/

int SumAllExpts (PLEVEL plevel, char **args)
{
  double      *pdLnData = (double*)args[0];
  PEXPERIMENT pExpt = plevel->pexpt;

  if (pExpt != NULL) {
    *pdLnData += pExpt->dLnLike;
  }
  return (1);

} /* SumAllExpts */


/* Function -------------------------------------------------------------------
   Test

   Test prior, likelihood against random number between 0 and 1
*/

BOOL Test (BOOL bExptIsDep, double dLnPrior, double dLnPriorNew,
           double dLnLike, double dLnLikeNew,
           double dLnData, double dLnDataNew) 
{
  double dPjump;

  if (dLnPriorNew == BAD_VAL || dLnLikeNew == BAD_VAL)
    return FALSE;

  dPjump = dLnPriorNew - dLnPrior + dLnLikeNew - dLnLike;

  if (bExptIsDep)
    dPjump += dLnDataNew - dLnData;

  return ((BOOL) (log(Randoms()) <= dPjump));

} /* Test */


/* Function -------------------------------------------------------------------
   TraverseLevels (recursive)

   Called with variable argument list ending in NULL;
   arguments should be pointers only; if you call this with a value
   that can be zero, you will be very sorry

   Find all allocated levels, execute `routinePtr' for each, starting at the
   top, passing the argument list as char**

   The argument list is saved from the initial call; on recursive calls the
   list is NULL
*/

void TraverseLevels (PLEVEL plevel,
                     void (*routinePtr)(PLEVEL plevel, char **args), ...)
{
  va_list ap;
  static char *arg[MAX_ARGS], **args = arg;
  char *arg1;
  BYTE n, nargs = 0;

  va_start(ap, routinePtr);
  if ((arg1 = va_arg (ap, char*)) != NULL) {
    arg[0] = arg1;
    while ((arg[++nargs] = va_arg(ap, char*)) != NULL) {};
  }
  va_end (ap);

  routinePtr (plevel, args);

  for (n = 0; n < plevel->iInstances; ++n)
    TraverseLevels (plevel->pLevels[n], routinePtr, NULL);

} /* TraverseLevels */


int TraverseLevels1 (PLEVEL plevel,
                     int (*routinePtr)(PLEVEL plevel, char **args), ...)
{
  va_list ap;
  static char *arg[MAX_ARGS], **args = arg;
  char *arg1;
  BYTE n, nargs = 0;

  va_start (ap, routinePtr);
  if ((arg1 = va_arg (ap, char*)) != NULL) {
    arg[0] = arg1;
    while ((arg[++nargs] = va_arg(ap, char*)) != NULL) {};
  }
  va_end (ap);

  if (routinePtr (plevel, args)) {

    for (n = 0; n < plevel->iInstances; ++n) {
      if (!TraverseLevels1(plevel->pLevels[n], routinePtr, NULL)) {
        /* error */
        return (0);
      }
    }
  }
  else /* error */
    return (0);

  /* success */
  return (1);

} /* TraverseLevels1 */


/* Function -------------------------------------------------------------------
   WriteHeader

   Called from Traverse Levels
   Write the names of thetas to output file header
*/

void WriteHeader (PLEVEL plevel, char **args)
{
  PANALYSIS panal = (PANALYSIS)args[0];
  FILE *outFile = (FILE*)args[1];
  long n, m;

  panal->iInstance[plevel->iDepth] = plevel->iSequence;

  for (n = 0; n < plevel->nMCVars; ++n) {
    fprintf (outFile, "%s(", plevel->rgpMCVars[n]->pszName);
    for (m = 1; m < plevel->iDepth; ++m)
      fprintf (outFile, "%d.", panal->iInstance[m]);
    fprintf (outFile, "%d)\t", panal->iInstance[plevel->iDepth]);
  }

  /* fflush (outFile); */

} /* WriteHeader */


/* Function -------------------------------------------------------------------

   WriteMCVars

   Write the values of MC vars for one level to output file
*/

void WriteMCVars (PLEVEL plevel, PANALYSIS panal, PFILE pOutFile)
{
  long n;
  PMCVAR pMCVar;

  for (n = 0; n < plevel->nMCVars; ++n) {
    pMCVar = plevel->rgpMCVars[n];
    fprintf(pOutFile, "%5g\t", pMCVar->dVal);
  }

} /* WriteMCVars */
