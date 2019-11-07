/* siminit.c

   written by Don Maszle
   22 November 1991

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
     Logfile:  SCCS/s.siminit.c
    Revision:  1.17
        Date:  14 Nov 1997
     Modtime:  06:39:09
      Author:  @a
   -- SCCS  ---------

   Contains initialization routines for the simulation
*/

#include <stdlib.h>

#include "lexerr.h"
#include "modelu.h"
#include "random.h"
#include "siminit.h"


/* ----------------------------------------------------------------------------
   GetModelInfo

   Gets information about the model that we are using for all of the
   simulations.
*/

void GetModelInfo (PMODELINFO pmi)
{
  pmi->nModelVars = (long) GetNModelVars ();
  pmi->pdModelVars = GetModelVector ();

  if ((pmi->nStates = (long) GetNStates()) != 0) { /* FB improved 28/07/97 */
    if (!(pmi->pStateHvar = (HVAR*) malloc (pmi->nStates * sizeof(HVAR))))
      ReportError(NULL, RE_OUTOFMEM | RE_FATAL, "GetModelInfo", NULL);

    GetStateHandles (pmi->pStateHvar);
  }
} /* GetModelInfo */


/* -----------------------------------------------------------------------------
   InitIntegratorSpec
*/

void InitIntegratorSpec (PINTSPEC pis)
{
  pis->iAlgo = IAL_DEFAULT;
  pis->iopt  = IOPT_DEFAULT;
  pis->itask = ITASK_DEFAULT;
  pis->itol  = ITOL_DEFAULT;
  pis->dRtol = RTOL_DEFAULT;
  pis->dAtol = RTOL_DEFAULT;
  pis->iMf   = IMF_DEFAULT;
  pis->liw   = LSODES_IWORKSIZE;
  pis->lrw   = LSODES_RWORKSIZE;

  if ( !(pis->iwork = InitlVector (pis->liw)) ||
       !(pis->rwork = InitdVector (pis->lrw)))
    ReportError (NULL, RE_OUTOFMEM | RE_FATAL,
                 "InitIntegratorSpec()", NULL);

  pis->dTStep = TSTEP_DEFAULT;

} /* InitIntegratorSpec */


/* -----------------------------------------------------------------------------
   InitOutputSpec
*/

void InitOutputSpec (POUTSPEC pos)
{
  pos->szOutfilename    = NULL;
  pos->pfileOut         = NULL;
  pos->bCommandLineSpec = FALSE;

  pos->nOutputs         = 0;       /* Init output list to empty */
  pos->pszOutputNames   = NULL;
  pos->phvar = NULL;

  pos->plistPrintRecs   = NULL;    /* List of Print() records to define ... */
  pos->plistDataRecs    = NULL;    /* List of Print() records to define ... */

  pos->pcOutputTimes    = NULL;    /* Count of output times for each var */
  pos->piCurrentOut     = NULL;    /* Index to current output for each var */
  pos->prgdOutputTimes  = NULL;    /* Array of output times for each var */
  pos->prgdOutputVals   = NULL;    /* Array of output values for each var */
  pos->prgdDataVals     = NULL;    /* Array of data values for each var */

  pos->cDistinctTimes   = 0;       /* Init output times list to empty */
  pos->rgdDistinctTimes = NULL;

} /* InitOutputSpec */


/* -----------------------------------------------------------------------------
    InitExperiment

*/

void InitExperiment (PEXPERIMENT pexp, PMODELINFO pmodelinfo)
{
  pexp->iExp = 0; /* Number of current experiment */
  pexp->dT0 = T0_DEFAULT; /* Simulation times */
  pexp->dTfinal = TFINAL_DEFAULT;

  pexp->dTime = 0.0; /* Current time, not used for global */

  pexp->iSubject = 0;

  pexp->pmodelinfo = pmodelinfo; /* Same model for all experiments */
  pexp->plistParmMods = InitList(); /* List of parameter modifications */

  InitIntegratorSpec (&pexp->is);
  InitOutputSpec (&pexp->os);

} /* InitExperiment */


/* -----------------------------------------------------------------------------
   InitMonteCarlo

*/

void InitMonteCarlo (PMONTECARLO pmc)
{
  pmc->nRuns = NSIMULATIONS_DEFAULT;

  pmc->szMCOutfilename = NULL;   /* File name for Monte Carlo output */
  pmc->pfileMCOut = NULL;        /* Output file for Monte Carlo */

  pmc->szSetPointsFilename = NULL;
  pmc->pfileSetPoints = NULL;

  pmc->plistMCVars   = NULL;

} /* InitMonteCarlo */


/* -----------------------------------------------------------------------------
   InitGibbs
*/

void InitGibbs (PGIBBSDATA pgd)
{
  pgd->nMaxIter   = NSIMULATIONS_DEFAULT;
  pgd->nInitIter  = NSIMULATIONS_DEFAULT;
  pgd->nPrintIter = NSIMULATIONS_DEFAULT;
  pgd->nPrintFreq = 1;      /* Print every iteration */

  pgd->szGout = NULL;       /* Filename for Gibbs output */
  pgd->pfileOut = NULL;     /* File for Gibbs output */

  pgd->szGrestart = NULL;   /* Parm file  for restarting Gibbs */
  pgd->pfileRestart = NULL; /* File for Gibbs restart */

  pgd->szGdata = NULL;      /* Filename for Gibbs input data */

} /* InitGibbs */


/* -----------------------------------------------------------------------------
   InitAnalysis

   Initializes an analysis structure and its associated
   sub-structures for integration, output, experiments, and
   Monte Carlo.
*/

void InitAnalysis (PANALYSIS panal)
{
  int i;

  if (!panal) return;

  panal->bDependents = panal->bParams = FALSE;

  panal->iType = AT_NOTSPECD; /* Type of analysis */

  panal->dSeed  = SEED_DEFAULT; /* The seed for everybody */

  panal->plistModelVars = InitList();

  panal->wContext = CN_GLOBAL; /* Begin in global context */
  panal->pexpCurrent = &panal->expGlobal;

  GetModelInfo (&panal->modelinfo); /* The model we are using */

  /* Global experiment settings */
  InitExperiment (&panal->expGlobal, &panal->modelinfo);

  /* Init all experiments NULL */
  panal->iExpts = 0;
  for (i = 0; i < MAX_EXPERIMENTS; i++) panal->rgpExps[i] = NULL;

  InitMonteCarlo (&panal->mc); /* Monte Carlo specifications */
  InitGibbs (&panal->gd); /* Gibbs specifications */

} /* InitAnalysis */


/* -----------------------------------------------------------------------------
   InitOutputs

   Returns TRUE if ok, FALSE otherwise.
*/

BOOL InitOutputs (PEXPERIMENT pexp, PINT piOut, PDOUBLE pdTout)
{
  int j;
  BOOL bReturn = FALSE;

  if (!pexp->os.nOutputs)
    ReportError (NULL, RE_NOOUTPUTS, (PSTR) &pexp->iExp, NULL);

  else {
    *piOut = 0; /* Keep track of out times */
    *pdTout = pexp->os.rgdDistinctTimes[0];
    for (j = 0; j < pexp->os.nOutputs; j++)
      pexp->os.piCurrentOut[j] = 0;

    bReturn = TRUE;
  } /* else */

  return (bReturn);

} /* InitOutputs */


/* -----------------------------------------------------------------------------
   InitOneOutVar

   Initializes information for one output to be printed.  Creates space
   for outputs.
*/

int InitOneOutVar (PVOID pData, PVOID pInfo)
{
  PPRINTREC ppr = (PPRINTREC) pData;
  POUTSPEC pos = (POUTSPEC) pInfo;
  int i = pos->nOutputs++; /* Index of current cell */

  pos->pszOutputNames[i] = ppr->szOutputName;
  pos->phvar[i]          = ppr->hvar;
  pos->pcOutputTimes[i]  = ppr->cTimes;
  pos->piCurrentOut[i]   = 0;

  pos->prgdOutputTimes[i] = ppr->pdTimes;
  pos->prgdOutputVals[i]  = InitdVector (ppr->cTimes);
  if (pos->prgdOutputTimes[i] == NULL || pos->prgdOutputVals[i] == NULL)
    ReportError (NULL, RE_OUTOFMEM | RE_FATAL, "InitOneOutVar()", NULL);

  return 0;

} /* InitOneOutVar */


/* -----------------------------------------------------------------------------
   InitOneDataVar

   Initializes information for one data specification
   Creates space for data and check that they are associated
   with an already created output spec and have matching number
   of time points.
*/

int InitOneDataVar (PVOID pData, PVOID pInfo)
{

  PDATAREC pda = (PDATAREC) pData;
  POUTSPEC pos = (POUTSPEC) pInfo;
  int i;

  /* check that the variable referenced in the data statement
     has a print specification */
  i = 0;
  while ((i < pos->nOutputs) && (pos->phvar[i] != pda->hvar))
   i++;

  /* yes, one was found */
  if (i < pos->nOutputs) { /* n data = n times ? */
    if (pos->pcOutputTimes[i] == pda->cData)
      pos->prgdDataVals[i] = pda->pdData;
    else {
      printf ("Error: There must be as many data in the Data " 
              "statement as times \n" 
              "       in the Print or PrintStep statement for %s - Exiting.\n",
              pda->szDataName);
      exit(0);
    }
  }
  else {
    printf ("Error: The Data statement for %s must have a matching \n",
            pda->szDataName);
    printf ("       Print or PrintStep statement - Exiting.\n");
    exit(0);
  }

  return 0;

} /* InitOneDataVar */


/* -----------------------------------------------------------------------------
   FindNewPoint

   Finds the new point.  Return FALSE if the sort is finished, i.e. no
   new point can be found.
*/

BOOL FindNewPoint (POUTSPEC pos, PINT piPoint)
{
  for (*piPoint = 0; *piPoint < pos->nOutputs; (*piPoint)++)
    if (pos->piCurrentOut[*piPoint] < pos->pcOutputTimes[*piPoint])
      break;

  return (*piPoint < pos->nOutputs ? TRUE : FALSE);

} /* FindNewPoint */


/* -----------------------------------------------------------------------------
   CreateOutputSchedule

   Creates a single output time array from the individual times.  This
   will be used to efficiently schedule the outputs.

   This routines sorts n sorted arrays into 1 array of undetermined
   length omitting duplicate entries.

   The algorithm works as follows.  A counter is kept into each
   array.  A current test "point" is found as the element of the
   first array whose counter is not past the end.  Thus, we start
   with the point at the first element of the first array.

   Looping through the list of arrays, if another array is found
   whose counter element is less than the point, then the point is
   switched to this element.  If no such element is found, the point
   is added to the new list.  If an array element found which is equal
   to the point the counter for that array is incremented.

   The pos->piCurrentOut fields are used for counters.  They are not
   reset, since InitOutputs() must be called before running the
   simulation anyway.
*/

void CreateOutputSchedule (POUTSPEC pos)
{
  int i, cTimes = 0, iPoint;
  BOOL bCont = TRUE;

  for (i = 0; i < pos->nOutputs; i++) /* Find max size of array */
    cTimes += pos->pcOutputTimes[i];

  if ( !(pos->rgdDistinctTimes = InitdVector (cTimes)))
    ReportError (NULL, RE_OUTOFMEM | RE_FATAL, "CreateOutputSchedule()",NULL);

  cTimes = 0;
  FindNewPoint (pos, &iPoint);
  while (bCont) {

    for (i = 0; i < pos->nOutputs; i++)
      if (i != iPoint && pos->piCurrentOut[i] < pos->pcOutputTimes[i]) {

        if (pos->prgdOutputTimes[i][pos->piCurrentOut[i]]
            < pos->prgdOutputTimes[iPoint][pos->piCurrentOut[iPoint]])
          iPoint = i;

         else if (pos->prgdOutputTimes[i][pos->piCurrentOut[i]]
                  == pos->prgdOutputTimes[iPoint][pos->piCurrentOut[iPoint]])
                pos->piCurrentOut[i]++;
      } /* if */

    pos->rgdDistinctTimes[cTimes++] =
    pos->prgdOutputTimes[iPoint][pos->piCurrentOut[iPoint]];

    if (++pos->piCurrentOut[iPoint] >= pos->pcOutputTimes[iPoint])
      bCont = FindNewPoint (pos, &iPoint);

  } /* while */

  pos->cDistinctTimes = cTimes; /* Save number of distinct times */

} /* CreateOutputSchedule */


/* -------------------------------------------------------------------w
   PrepareOutSpec

   Prepares an output spec determined by the plistPrintRecs
   and plistDataRecs specification.
   Adjusts final time eventually to match the last output time
*/

BOOL PrepareOutSpec (PEXPERIMENT pexp)
{
  POUTSPEC pos = &pexp->os;
  BOOL bReturn = FALSE;
  int cOut = ListLength(pos->plistPrintRecs);
  int i;

  if (!cOut)
    ReportError (NULL, RE_NOOUTPUTS, (PSTR) &pexp->iExp, NULL);

  else {
    pos->pszOutputNames = (PSTR *) malloc (cOut * sizeof(PSTR));
    pos->phvar          = (HVAR *) malloc(cOut * sizeof(HVAR));
    pos->pcOutputTimes  = InitiVector (cOut);
    pos->piCurrentOut   = InitiVector (cOut);
    pos->prgdOutputTimes= InitpdVector (cOut);
    pos->prgdOutputVals = InitpdVector (cOut);
    pos->prgdDataVals   = InitpdVector (cOut);

    if (pos->pszOutputNames  == NULL || pos->phvar == NULL          || 
        pos->pcOutputTimes   == NULL || pos->piCurrentOut == NULL   ||    
        pos->prgdOutputTimes == NULL || pos->prgdOutputVals == NULL ||  
        pos->prgdDataVals    == NULL)
      ReportError (NULL, RE_OUTOFMEM | RE_FATAL, "PrepareOutSpec()", NULL);

    else {
      pos->nOutputs = 0;  /* Current cell counter in callback */
      ForAllList (pos->plistPrintRecs, InitOneOutVar, (PVOID) &pexp->os);
      pos->nOutputs = cOut;  /* Set count of outputs for real */

      /* initialize the prgdDataVals array with the data spec list or
         to null.
         This must be called here, after initializing the outputs to
         allow for matching of outputs and data */
      for (i = 0; i < cOut; i++) pos->prgdDataVals[i] = NULL;
      ForAllList (pos->plistDataRecs, InitOneDataVar, (PVOID) &pexp->os);

      FreeList (&pos->plistPrintRecs, NULL, TRUE);
      FreeList (&pos->plistDataRecs, NULL, TRUE);

      CreateOutputSchedule (pos); /* Create single output time list */

      /* Adjust the final time to match the last output time */
      pexp->dTfinal = pos->rgdDistinctTimes[pos->cDistinctTimes - 1];

      if (pexp->dTfinal == pexp->dT0) {
        printf ("\nError: starting and final times are equal in Experiment %d "
                "- Exiting.\n\n", pexp->iExp);
        exit(0);
      }

      bReturn = TRUE;
    } /* else */
  } /* else */

  return (bReturn);

} /* PrepareOutSpec */


BOOL PrintOutSpec (PEXPERIMENT pexp)
{
  POUTSPEC pos = &pexp->os;
  int j, i, cOut = pos->nOutputs;

  printf ("%d Outputs:\n", cOut);
  for (i = 0; i < cOut; i++) {
    printf ("  %#0x  %s: ", pos->phvar[i], pos->pszOutputNames[i]);
    for (j = 0; j < pos->pcOutputTimes[i]; j++)
      printf ("%g ", pos->prgdOutputTimes[i][j]);
    printf ("\n");
  } /* for */
  return 0;
} /* PrintOutSpec */

/* End */
