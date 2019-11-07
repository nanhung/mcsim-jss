/* optdesign.c

   written by Frederic Bois
   16 August 1993

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
     Logfile:  SCCS/s.optdesign.c
    Revision:  1.6
        Date:  14 Nov 1997
     Modtime:  06:39:05
      Author:  @a
   -- SCCS  ---------

*/

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "optdesign.h"
#include "hungtype.h"
#include "lexerr.h"
#include "matutil.h"
#include "simmonte.h"
#include "yourcode.h"

enum {Shannon, Var_Reduction}; /* optimization criterion */

PFILE debfile;

/* ----------------------------------------------------------------------------
   InitDPV

   Count Data statements and initialize some arrays.
   Missing values can be specified.

   Inputs (unmodified):
   panal:      analysis specifications. Used to find how many
               experiments are simulated etc.

   Outputs:
   pdData:     array of data points. It is allocated and filled
               with values given.
   pdPred:     array of predictions. It is allocated and not
               filled.
   pnData:     a pointer to the total number of data points.
               That number has to be > 0.

   pnPred:     a pointer to the total number of predicted points.
               That number has to be > pnData.
*/

void InitDPV (PANALYSIS panal, double ***pdData, int **piData_mask, 
              long *pnData, double ***pdPred, long *pnPred, 
              double **pdVariance, long nMod, long nData_sampled)
{
  BOOL bWarn;
  int  i, j, k;
  OUTSPEC *pos;

  /* read the data from Data statements */

  /* count the data points for allocation */
  bWarn = FALSE;
  *pnPred = *pnData = 0;
  for (i = 0; i < panal->expGlobal.iExp; i++) {
    pos = &panal->rgpExps[i]->os;
    for (j = 0; j < pos->nOutputs; j++) {
      for (k = 0; k < pos->pcOutputTimes[j]; k++) {
        if (pos->prgdDataVals[j]) /* if a Data statement exists... */
          (*pnData)++;
        (*pnPred)++;
      } /* for k */
    } /* for j */
  } /* for i */

  if (*pnData == 0) { /* no data ? */
    printf("Error: you must provide Data Statements ");
    printf("for at least one Experiment - Exiting.\n");
    exit(0);
  }

  if (*pnPred == *pnData) { /* no pure predictions ? */
    printf ("Error: you must provide at least one Experiment ");
    printf ("without Data Statements for pure predictions.\n");
    exit(0);
  }

  /* allocate the pdData arrays */
  if ( !(*pdData = InitdMatrix (nData_sampled, *pnData)))
    ReportError (NULL, RE_OUTOFMEM | RE_FATAL, "InitDPV", NULL);

  /* allocate the plData_mask array  */
  if ( !(*piData_mask = InitiVector (*pnData)))
    ReportError (NULL, RE_OUTOFMEM | RE_FATAL, "InitDPV", NULL);

  /* allocate the pdVariance array  */
  if ( !(*pdVariance = InitdVector (*pnData)))
    ReportError (NULL, RE_OUTOFMEM | RE_FATAL, "InitDPV", NULL);

  /* allocate the pdPred array */
  if ( !(*pdPred = InitdMatrix (nMod, *pnPred)))
    ReportError (NULL, RE_OUTOFMEM | RE_FATAL, "InitDPV", NULL);

} /* InitDPV */


/* ----------------------------------------------------------------------------
   OpenOptFiles

   Opens the output file and the restart file.
*/

void OpenOptFiles (PANALYSIS panal)
{
  /* Take care of the output file first */

  /* Use command line spec if given */
  if (panal->expGlobal.os.bCommandLineSpec)
    panal->gd.szGout = panal->expGlobal.os.szOutfilename;

  /* Default if none given */
  else if (!(panal->gd.szGout))
    panal->gd.szGout = "simopt.default.out";

  if (!(panal->gd.pfileOut)
      && !(panal->gd.pfileOut = fopen (panal->gd.szGout, "w")))
    ReportError (NULL, RE_FATAL | RE_CANNOTOPEN,
                 panal->gd.szGout, "OpenMarkovFiles()");

  /* eventually open the restart file */
  if (panal->gd.szGrestart)
    if (!(panal->gd.pfileRestart)
      && !(panal->gd.pfileRestart = fopen (panal->gd.szGrestart, "r")))
      ReportError (NULL, RE_FATAL | RE_CANNOTOPEN,
                   panal->gd.szGrestart, "OpenMarkovFiles()");

} /* OpenOptFiles */


/* -----------------------------------------------------------------------------
   InitSigma

   inits the pdSigma vector, the experimental standard deviations.
   By default it assigns one SD per separate data statement.
   
   Inputs (unmodified):
   panal         analysis specifications. Used to find how many 
                 experiments are simulated etc.
   nData         total number of data points used to construct the
                 likelihood. Should be > 0.

   Outputs:
   pdSigma:      array of SDs. It is allocated and filled with starting values.
   pnSigma:      number of pdSigma SDs.
   plSigmaIndex: array of size nData which is allocated and holds 
                 the index of the pdSigma attributed to each data point.

   Don't hesitate to tailor this routine to your needs.
   This, in particular, will not work is a data file is used 
   instead of data statements.
   The choice of starting values used should be inconsequential 
   as they are immediately overwritten.
*/

void InitSigma (PANALYSIS panal, long nData, double **pdSigma, 
                 long *pnSigma, long **plSigmaIndex)
{
  long    i, j, k, l, nTmp;
  BOOL    bFound;
  OUTSPEC *pos1, *pos2;
  PINT    piVarIndex;

  /* allocate plSigmaIndex */
  if ( !(*plSigmaIndex = InitlVector (nData)))
    ReportError (NULL, RE_OUTOFMEM | RE_FATAL, "InitSigma", NULL);

  /* find out how many different output variables there are,
     given that the same output variable can be found in different experiments
   */
  *pnSigma = 0;
  for (i = 0; i < panal->expGlobal.iExp; i++) {
    pos1 = &panal->rgpExps[i]->os;
    for (j = 0; j < pos1->nOutputs; j++) {
      if (pos1->prgdDataVals[j]) { /* if a Data statement exists */

        bFound = FALSE;
        k = 0;
        /* now for the current variable (i,j) we scan previous experiments */
        while ((k < i) && (!bFound)) {
          pos2 = &panal->rgpExps[k]->os;
          l = 0;
          /* for all outputs of pos2 */
          while ((l < pos2->nOutputs) && (!bFound)) {
            bFound = !strcmp (pos1->pszOutputNames[j], pos2->pszOutputNames[l]);
            l++;
          }
          k++;
        }
        if (!bFound) {
          /* if that variable has not been found then it is new */        
          (*pnSigma)++;
        }
      } /* if */
    } /* for j */
  } /* for i */

  if (*pnSigma == 0) 
    printf("Warning: no experimental variance defined - Continuing");

  /* now we know that there are *pnSigma different outputs - we allocate a 
     temporary array to index them 
   */
  if ( !(piVarIndex = InitiVector (*pnSigma)))
    ReportError (NULL, RE_OUTOFMEM | RE_FATAL, "InitSigma", NULL);

  /* now we redo the previous scanning and remember the various variables */
  *pnSigma = 0;
  for (i = 0; i < panal->expGlobal.iExp; i++) {
    pos1 = &panal->rgpExps[i]->os;
    for (j = 0; j < pos1->nOutputs; j++) {
      if (pos1->prgdDataVals[j]) { /* if a Data statement exists */

        bFound = FALSE;
        k = 0;
        /* now for the current variable (i,j) we scan previous experiments */
        while ((k < i) && (!bFound)) {
          pos2 = &panal->rgpExps[k]->os;
          l = 0;
          /* for all outputs of pos2 */
          while ((l < pos2->nOutputs) && (!bFound)) {
            bFound = !strcmp (pos1->pszOutputNames[j], pos2->pszOutputNames[l]);
            l++;
          }
          k++;
        }
        if (!bFound) {
          /* hold its id */
          piVarIndex[*pnSigma] = pos1->phvar[j];
          /* update the counter */        
          (*pnSigma)++;
        }
      } /* if */
    } /* for j */
  } /* for i */
  
  /* we build the plSigmaIndex to index the pdSigma associated to each 
     data point.
   */
  for (l = 0; l < *pnSigma; l++) { /* clumsy ? */
    nTmp = 0;
    for (i = 0; i < panal->expGlobal.iExp; i++) {
      pos1 = &panal->rgpExps[i]->os;  
      for (j = 0; j < pos1->nOutputs; j++) {
        if (pos1->prgdDataVals[j]) { /* if a Data statement exists */
          for (k = 0; k < pos1->pcOutputTimes[j]; k++) {
            if (piVarIndex[l] == pos1->phvar[j])
              (*plSigmaIndex)[nTmp] = l;
            nTmp++;
          } /* for k */
        } /* if */
      } /* for j */
    } /* for i */
  } /* for l */

  /* free piVarIndex */
  free (piVarIndex);

} /* InitSigma */

/* ----------------------------------------------------------------------------
   WriteOutHeader

   Prints a tabulated header at the top of the output file.
*/

void WriteOutHeader (PANALYSIS panal, int criterion)
{
  int i, j, k;
  OUTSPEC *pos;
  
  fprintf (panal->gd.pfileOut, "iter\t");

  for (i = 0; i < panal->expGlobal.iExp; i++) {
    pos = &panal->rgpExps[i]->os;
    for (j = 0; j < pos->nOutputs; j++) {
      for (k = 0; k < pos->pcOutputTimes[j]; k++) {
        if (pos->prgdDataVals[j]) /* if a Data statement exists... */
          fprintf (panal->gd.pfileOut, "T%g\t", pos->prgdOutputTimes[j][k]);
      }
    }
  }

  fprintf (panal->gd.pfileOut, "Chosen\t");

  if (criterion == Shannon)
    fprintf (panal->gd.pfileOut, "Shannon\tCost\n");
  else
    fprintf (panal->gd.pfileOut, "Variance\tSD\tCost\n");

  fflush (panal->gd.pfileOut);
  
} /* WriteOutHeader */


/* ----------------------------------------------------------------------------
   Estimate_y

   Calculates y[] for the given conditions by running the model.
   The data is not transformed. Transformations should be coded
   in the likelihood function etc. However it is flattened in the pdPred
   array.

   Note that pdPred may not be completely initialized by this routine if it
   is passed uninitialized.
*/

int Estimate_y (PANALYSIS panal, double *pdTheta, double *pdPred)
{
  int cNPred;
  int i, j, k;
  OUTSPEC *pos;

  /* Run the PBPK model for each experiment assigned to the subject specified
   */
  for (i = 0; i < panal->expGlobal.iExp; i++) {
    InitModel ();

    /* Global modifications */
    ModifyParms (panal->expGlobal.plistParmMods);

    /* Set params to pdTheta values */
    SetParms (panal->mc.nParms, panal->mc.rghvar, pdTheta);

    /* set the Mods for this exp */
    ModifyParms (panal->rgpExps[i]->plistParmMods);

    if (!DoOneExperiment (panal->rgpExps[i])) {
      /* Error */
      printf ("Warning: Can't estimate y with parameters:\n");
      WriteArray (stdout, panal->mc.nParms, pdTheta);
      fputc('\n', stdout);
      return 0;
    } /* if */
  } /* for */

  /* flatten the predictions: this rewrites pdPred, put logs in */
  cNPred = 0;
  for (i = 0; i < panal->expGlobal.iExp; i++) {
    pos = &panal->rgpExps[i]->os;
    for (j = 0; j < pos->nOutputs; j++) {
      for (k = 0; k < pos->pcOutputTimes[j]; k++) {
        pdPred[cNPred] = log (pos->prgdOutputVals[j][k]);
        cNPred++;
      } /* for k */
    } /* for j */
  } /* for i */

  return 1;

} /* Estimate_y */


/* ----------------------------------------------------------------------------
 ReadPSample

   Initialize arrays and reads a parameter sample, obtained for example 
   from a previous MCMC sample.

*/

void ReadPSample (FILE *pfileRestart, long nSubjs, long nParms, 
                  double ***pdTheta, double ***pdSigma, double **pdL, 
                  long **plVectorIndex, long **plVectorIndex_tmp, long nSigma, 
                  long nMod)
{
  long lDummy, iter = 0;
  int i, j;

  /* space allocation */
  if ( !(*pdTheta = InitdMatrix (nMod, nParms)) ||
       !(*pdSigma = InitdMatrix (nMod, nSigma)) ||
       !(*pdL     = InitdVector (nMod)))
    ReportError (NULL, RE_OUTOFMEM | RE_FATAL, "ReadPSample", NULL);

  /* skip the first line.  This allows a MC output file to be used
     directly as a restart file. */
  fscanf (pfileRestart,  "%*[^\n]");  getc(pfileRestart);

  /* as long as we have not reached the end of the file we keep reading lines
     We also keep incrementing the global iteration counter iter:
   */
  while ( !(feof(pfileRestart) ||
           (fscanf(pfileRestart, "%ld", &lDummy) == EOF))) {

    /* read pdTheta */
    for (i = 0; i < nSubjs; i++)
      for (j = 0; j < nParms; j++)
        fscanf (pfileRestart, "%lg", &((*pdTheta)[iter][j]));

    /* read pdSigma */
    for (i = 0; i < nSigma; i++)
      fscanf (pfileRestart, "%lg", &(*pdSigma)[iter][i]);

    /* transform pdSigma */
    for (i = 0; i < nSigma; i++)
      (*pdSigma)[iter][i] = log ((*pdSigma)[iter][i]);

    /* Throw away remainder of line. This allows a MC output file to be used
       directly as a restart file. */
    fscanf (pfileRestart,  "%*[^\n]"); getc(pfileRestart);

    /* increment lIter */
    iter++;

    if (iter >= nMod) {
      printf ("\nNote: the total number of lines in p_sample file exceeds\n");
      printf ("      the number of lines to read (%ld).\n",
              nMod);
      goto Break_while;
    }

  } /* end while */

Break_while:

  fclose (pfileRestart);

} /* ReadPSample */


/* ----------------------------------------------------------------------------
 Fake_data

   Fill in the data array by adding experimental noise to the higest 
   posterior prediction,  in log-space. The highest posterior parameter
   vector must be the first in the parameter file.
*/

void Fake_data (long nData_sampled, long nData, double **pdPred, 
                double **pdSigma, long *plSigmaIndex, double **pdData)
{
  register long i, j;

  for (i = 0; i < nData_sampled; i++)
    for (j = 0; j < nData; j++)
      pdData[i][j] = NormalRandom(pdPred[i][j], pdSigma[i][plSigmaIndex[j]]);

} /* Fake_data */


/* ----------------------------------------------------------------------------
   Likelihood

   returns the expected likelihood of an experiment (containing nData)

   Uses piData_mask to select the data to consider.

   Note that this assume a log-normal distribution of experimental
   errors. Data, Preds and SDs are eventually already log-transformed.
*/

double Likelihood (double *pdPred, double *pdData, long nData, 
                   double *pdSigma, long *plSigmaIndex, int *piData_mask, 
                   int nData_tried)
{
  long k;
  double dLike;
  BOOL bOn;

  dLike = 0.0;

  for (k = 0; k < nData; k++) {

    if (k == nData_tried) /* invert the current mask */
      bOn = ! piData_mask[k];
    else /* straigth mask */
      bOn = piData_mask[k];

    /* form the log-likelihood for an experiment (sum of log-likelihoods */      
    if (bOn)
      dLike = dLike + lnDFNormal(pdData[k], pdPred[k],
                                 pdSigma[plSigmaIndex[k]]);
  }

  return (exp(dLike));
  
} /* Likelihood */


/* ----------------------------------------------------------------------------
   DoVariance

   Compute the total variance for a set of indexed arrays. plDrawn is not 
   used ! no time to do it.
   Only part of the array (from istart to ifinish) is used.
*/

double DoVariance (long nDim, long *pIndex, long *plDrawn, double **pdX,
                   long istart, long ifinish)
{
  long i, j;
  register double ave, ss, dTmp;

  ss = 0;

  /* for all predictions (X between istart and ifinish) */
  for (i = istart; i < ifinish; i++) {
    ave = 0;
    /* Compute the average pdX */
    for (j = 0; j < nDim; j++) {
      ave = ave + pdX[pIndex[j]][i];
    }
    ave = ave / nDim;
  
    /* Compute the SS over pdX */
    for (j = 0; j < nDim; j++) {
      dTmp = pdX[pIndex[j]][i] - ave;
      ss = ss + dTmp * dTmp;
    }
  }

  return (ss / (nDim * (ifinish - istart)));

} /* DoVariance */


/* ----------------------------------------------------------------------------
   DoVariance2

   Compute the total variance of an array of arrays, considering 
   importance ratios.
   Only part of the array (from istart to ifinish) is used.
*/

double DoVariance2 (long nDim, double *pdImpR, double **pdX,
                    long istart, long ifinish)
{
  long i, j;
  register double ave, ss, dTmp;

  ss = 0;

  /* for all predictions (X between istart and ifinish) */
  for (i = istart; i < ifinish; i++) {
    ave = 0;
    /* Compute the average pdX */
    for (j = 0; j < nDim; j++) {
      ave = ave + pdImpR[j] * pdX[j][i];
    }
  
    /* Compute the SS over pdX */
    for (j = 0; j < nDim; j++) {
      dTmp = pdX[j][i] - ave;
      ss = ss + pdImpR[j] * dTmp * dTmp;
    }
  }

  return (ss / (ifinish - istart));

} /* DoVariance2 */


/* ----------------------------------------------------------------------------
   Compute_cost

   Costs are computed for one subject, in French Francs
*/

void Compute_cost (long nData, int *piData_mask, double *dCost)
{
  int nPts = 0;
  int i;
 
  for (i = 0; i < nData; i++)
    if (piData_mask[i]) nPts++;

  /* let say analysis is 40 units per analysis */
  *dCost = 40 * nPts;
  
} /* Compute_cost */


/* ----------------------------------------------------------------------------
   WriteOptimOut

   writes to the output file, print only the significant portions.
*/

void WriteOptimOut (PANALYSIS panal, long iter, long nData, long nMod, 
                    int criterion, double *pdVariance, int *piData_mask, 
                    long iCrit, double dCrit, double dCost)
{
  long i;
 
  fprintf (panal->gd.pfileOut, "%ld\t", iter);

  if (iCrit < nData) { /* if defined */
    for (i = 0; i < nData; i++) {
      if ((&panal->mc)->style == forward) {
        if ((i == iCrit) || !(piData_mask[i]))
          fprintf (panal->gd.pfileOut, "%g\t", pdVariance[i]);
        else 
          fprintf (panal->gd.pfileOut, "\t");
      }
      else { /* backward style, the mask needs to be inverted */
        if (!(piData_mask[i]))
          fprintf (panal->gd.pfileOut, "\t");
        else 
          fprintf (panal->gd.pfileOut, "%g\t", pdVariance[i]);
      }
    } /* end for */

    fprintf (panal->gd.pfileOut, "%ld\t", iCrit+1);
  }
  else
    for (i = 0; i <= nData; i++)
      fprintf (panal->gd.pfileOut, "\t");

  if (criterion == Shannon)
    fprintf (panal->gd.pfileOut, "%g\t%g\t", dCrit, dCost);
  else
    fprintf (panal->gd.pfileOut, "%g\t%g\t%g\n", dCrit, sqrt(dCrit), dCost);

  fflush (panal->gd.pfileOut);
  
} /* WriteOptimOut */


/* ----------------------------------------------------------------------------
   Importance_Resample

   Does just that, updating pIndex1, picking from pIndex0, pdL is destroyed.
*/

void Importance_Resample (long nMod, long *pIndex0, long *pIndex1, 
                          long *plDrawn, double *pdL, double dSumL)
{
  long i, j;

  /* do the weights */
  for (i = 0; i < nMod; i++)
    pdL[i] = pdL[i] / dSumL;

  j = 0;
  do {    
    /* randomly pick a vector among the nMod available */
    i = floor (Randoms() * nMod);

    if (Randoms() < pdL[i]) { /* accept */
      pIndex1[j] = pIndex0[i];
      j++;
      plDrawn[pIndex0[i]]++;
    }
  }
  while (j < nMod);

} /* Importance_Resample */


/* ----------------------------------------------------------------------------
   Do_Importance_Ratios

   Does just that, pdL is destroyed and replaced by the ratios.
*/

void Do_Importance_Ratios (long nMod, double *pdL, double dSumL)
{
  long i;

  /* do the weights */
  for (i = 0; i < nMod; i++)
    pdL[i] = pdL[i] / dSumL;

} /* Do_Importance_Ratios */


/* ----------------------------------------------------------------------------
   CloseOptFiles

   Closes output files associated with the design optimizer.
   The restart file has already been closed by the ReadPSample

*/

void CloseOptFiles (PANALYSIS panal)
{
  if (panal->gd.pfileOut) {
    fclose (panal->gd.pfileOut);
    printf ("\nWrote results to \"%s\"\n", panal->gd.szGout);
  }

} /* CloseOptFiles */


/* ----------------------------------------------------------------------------
   DoOptimalDesign

   Core routine of the experimental design optimizer
*/

void DoOptimalDesign (PANALYSIS panal)
{
  PGIBBSDATA  pgd = &panal->gd;
  PMONTECARLO pmc = &panal->mc;

  long i, j;
  long iData;                 /* index to the current data set */
  long iModel;                /* index to the current param. set */
  long iBest = 0;
  long dim;
  long nData;                 /* # of data points in a data set */
  long nData_sampled;         /* # of fake data sets generated */
  long nPred;                 /* # of predictions */
  long nSigma;                /* # of experimental SDs */
  long nMod = pmc->nRuns;     /* # of model parametrizations tested */
  long *plSigmaIndex;         /* index array for pdSigma computation */
  long *plVectorIndex;        /* index of the currently selected vectors */
  long *plVectorIndex_tmp;    /* index of the currently selected vectors */
  int  *piData_mask;          /* current mask for likelihood computation */
  int   nData_tried;          /* index of the currently tried time point */
  int   criterion;            /* either Shannon's or variance reduction */
  double dBest = 0;           /* best criterion */
  double dCost;               /* total cost of a design */
  double dSumL;               /* sum of likelihoods */
  double dShannon;            /* Shannon's information criterion */
  double *pdL;                /* array of p. vectors' likelihoods */
  double *pdVariance;         /* array of pred. or param. variances */
  double **pdData;            /* simulated observations */
  double **pdPred;            /* predictions of data */
  double **pdSigma;           /* table of experimental standard deviations */
  double **pdTheta;           /* table of prior model parameters */

  /* criterion can be Var_Reduction or Shannon */
  criterion = Var_Reduction;
  if (criterion != Var_Reduction) {
    printf ("Shannon not implemented - exiting\n");
    exit (0);
  }

  InitRandom (panal->dSeed, TRUE);

  /* announce the work to be done */
  printf ("\nDoing analysis - Optimal Design %s %s - %d experiment%c\n",
          (pmc->style == forward ? "forward" : "backward"),
          (criterion == Var_Reduction ? "variance reduction" : "Shannon"),
          panal->expGlobal.iExp, (panal->expGlobal.iExp > 1 ? 's' : ' '));

  /* open restart and output files */
  OpenOptFiles (panal);

  /* decide the number of fake data sets to use: half the number
     of parameter samples */
  nData_sampled = nMod / 2;

  /* Initialize the data and predictions arrays */
  InitDPV (panal, &pdData, &piData_mask, &nData, &pdPred, &nPred, &pdVariance,
           nMod, nData_sampled);

  /* initialize the experimental variances: this is just to count the sigmas */
  InitSigma (panal, nData, &pdSigma[0], &nSigma, &plSigmaIndex);

  /* Initialize the individual parameters to be sampled */
  if (!(pgd->szGrestart)) {
    /* error: we must have a starting prior sample */
    printf ("Error: there must be a parameter sample file - Exiting\n");
    exit (0);
  }
  else {
    /* read the starting values in the order they are printed and
       close the file when finished */
    ReadPSample (pgd->pfileRestart, 1, pmc->nParms, &pdTheta, &pdSigma, &pdL, 
                 &plVectorIndex, &plVectorIndex_tmp, nSigma, nMod);
  }

  /* Write the header line of the output file */
  WriteOutHeader (panal, criterion);

  /* for all experiments, compute the predictions for all parameter vectors
     models, fake measurement noise will eventually be added later */
  for (i = 0; i < panal->expGlobal.iExp; i++)
    for (iModel = 0; iModel < nMod; iModel++)
      Estimate_y (panal, pdTheta[iModel], pdPred[iModel]);

  /* create once for all fake data sets using a number of parameter sets */
  Fake_data (nData_sampled, nData, pdPred, pdSigma, plSigmaIndex, pdData);

  /* turn experimental points on or off */
  if (pmc->style == forward)
    for (i = 0; i < nData; i++) piData_mask[i] = 0; /* all points "off" */
  else
    for (i = 0; i < nData; i++) piData_mask[i] = 1; /* all points "on" */

  /* initialize the array of importance ratios and variance of predictions */
  if (criterion == Var_Reduction) {

    if (pmc->style == backward) { /* all points included, compute the 
      resulting variance reduction */

      /* loop over predicted data to average out the results */
      for (iData = 0; iData < nData_sampled; iData++) {
  
        /* set nData_tried at a value beyond the array bounds so that it is
           not found and does not interfere in the next call to Likelihood */
        nData_tried = nData + 1;

        /* loop over all valid parameter sets and compute the expected 
           likelihood of the current parameter set, given the current fake 
           data set */
        dSumL = 0;
        for (iModel = 0; iModel < nMod; iModel++) {
          pdL[iModel] = Likelihood (pdPred[iModel], pdData[iData], nData,  
                                    pdSigma[iModel], plSigmaIndex, piData_mask, 
                                    nData_tried);

          dSumL = dSumL + pdL[iModel];
        }

        Do_Importance_Ratios (nMod, pdL, dSumL);

        /* sum the variances over the datasets, dBest has been zeroed when
           defined */
        dBest = dBest + DoVariance2 (nMod, pdL, pdPred, nData, nPred);

      } /* for iData */

      /* form the average variance over the datasets */
      dBest = dBest / (double) nData_sampled;

    }
    else { /* forward style is simple */
      for (j = 0; j < nMod; j++)
        pdL[j] = 1 / (double) nMod;

      dBest = DoVariance2 (nMod, pdL, pdPred, nData, nPred);
    }
  }

  iBest = nData + 1; /* out of bounds,  undefined */

  /* compute the cost */
  Compute_cost (nData, piData_mask, &dCost);

  /* output the first line ... */
  WriteOptimOut (panal, 0, nData, nMod, criterion, pdVariance, piData_mask,
                 iBest, dBest, dCost);

  /* start the loop over designs and over data, only some of the previously 
     computed points will be used */
  dim = ((pmc->style == backward) ? nData - 1 : nData);
  for (i = 0; i < dim; i++) {

    /* reset the best criteria */
    if (criterion == Shannon)
      dBest = -DBL_MAX;
    else
      dBest = DBL_MAX;

    for (j = 0; j < nData; j++) {

      /* try this point, if it is not already accepted or deleted.
         (style == backward) is 1 in case of backward style and 0 in
         case of forward style. So if we go backward we skip the point
         if it is already "off" */
      if (piData_mask[j] == (pmc->style == backward)) { 

        nData_tried = j; /* try add or remove point j */

        pdVariance[j] = 0; /* reset */

        /* loop over predicted data to average out the results */
        for (iData = 0; iData < nData_sampled; iData++) {

          /* loop over all valid parameter sets and compute the expected 
             likelihood of the current parameter set, given the current fake 
             data set */
          dSumL = 0;
          for (iModel = 0; iModel < nMod; iModel++) {
            pdL[iModel] = Likelihood (pdPred[iModel], pdData[iData], nData,
                                      pdSigma[iModel], plSigmaIndex, 
                                      piData_mask, nData_tried);

            dSumL = dSumL + pdL[iModel];
          }

          /* check whether this point is the best of the j tried so far */
          if (criterion == Shannon) {
            dShannon = dSumL / nMod; /* expected likeli. over param samples */
            if (dBest < dShannon) { /* if we find a better one record it */
              dBest = dShannon;
              iBest = j;
            }
          }
          else { /* case of variance reduction */

            /* do an importance reweighting to be able to get the variance
               reductions */
            Do_Importance_Ratios (nMod, pdL, dSumL);

            /* sum the variances over the datasets */
            pdVariance[j] = pdVariance[j] + 
                            DoVariance2 (nMod, pdL, pdPred, nData, nPred);

          } /* else */

        } /* for iData */

        /* form the average variance over the datasets */
        pdVariance[j] = pdVariance[j] / (double) nData_sampled;

        /* if forward we want to keep the point giving the best reduction, 
           i.e. the lowest variance; if backward we want to remove the point 
           giving the lowest variance augmentation, which is also the lowest
           variance */
        if (dBest > pdVariance[j]) {
          dBest = pdVariance[j];
          iBest = j;
        }

      } /* if piData_mask */

    } /* for j */
    
    /* set the best point, by inverting it definitely */
    piData_mask[iBest] = !piData_mask[iBest];

    /* compute the cost */
    Compute_cost (nData, piData_mask, &dCost);

    /* output ... */
    WriteOptimOut (panal, i+1, nData, nMod, criterion,  pdVariance, 
                   piData_mask, iBest, dBest, dCost);

    printf ("%ld points design done\n", i+1);
     
  } /* for i */

  /* if style is backward we would still like to know what a no point design
     would do */
  if (pmc->style == backward) {

    /* Importance ratios are reinitialized with the natural weights */
    for (j = 0; j < nMod; j++) pdL[j] = 1 / (double) nMod;

    if (criterion == Var_Reduction) {
      dBest = DoVariance2 (nMod, pdL, pdPred, nData, nPred);
      j = 0;
      while (piData_mask[j] == 0)
        j++;
      iBest = j;
    }
    else
      printf ("need to deal with Shannon\n");

    /* output, note the zero cost */
    WriteOptimOut (panal, nData, nData, nMod, criterion, pdVariance, 
                   piData_mask, iBest, dBest, 0);

    printf ("%ld points design done\n", nData);
  }

  free (plSigmaIndex);
  free (piData_mask);

  CloseOptFiles (panal);

} /* DoOptimalDesign */
