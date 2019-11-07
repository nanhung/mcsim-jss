/* simo.c

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
     Logfile:  SCCS/s.simo.c
    Revision:  1.26
        Date:  14 Nov 1997
     Modtime:  06:39:10
      Author:  @a
   -- SCCS  ---------

   Output routines for the simulation

*/

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lexerr.h"
#include "simo.h"
#include "modelu.h"

static char vszDefOutFilename[] = "sim.out";
static char vszDefMCOutFilename[] = "simmc.out";


/* ----------------------------------------------------------------------------
   SaveOutputs

   Also saves states
*/

void SaveOutputs (PEXPERIMENT pexp, PDOUBLE pdTout)
{
  #define SO_EPSILON (1e-100) /* Smaller values are zeroed  */

  static     PDOUBLE  rgdInterpStates, rgdInterpDeriv;
  int        i, j, index;
  PMODELINFO pmod = pexp->pmodelinfo;
  POUTSPEC   pos = &pexp->os;
  extern     IFN vrgInputs[]; /* Input Function records */

  if (!(rgdInterpStates) || !(rgdInterpDeriv))
    if ( !(rgdInterpStates = InitdVector (GetNModelVars ())) ||
         !(rgdInterpDeriv  = InitdVector (GetNModelVars ())))
      ReportError (NULL, RE_OUTOFMEM | RE_FATAL, "SaveOutputs", NULL);

  memcpy (rgdInterpStates, pmod->pdModelVars,
          pmod->nModelVars*sizeof(double));

  /* Update inputs and outputs defined only in Dynamics */
  CalcDeriv(rgdInterpStates, rgdInterpDeriv, pdTout);

  /* Update output scaling */
  CalcOutputs (rgdInterpStates, rgdInterpDeriv, pdTout);

  for (i = 0; i < pos->nOutputs; i++) {

    /* Save interpolated value if there are still times to output
       for this variable, and if this time is scheduled */

    if (pos->piCurrentOut[i] < pos->pcOutputTimes[i]
        && *pdTout == pos->prgdOutputTimes[i][pos->piCurrentOut[i]]) {
      double dTmp = 0;

      if (IsModelVar(pos->phvar[i]))  /* Use interp'd model value */
        dTmp = rgdInterpStates[ ModelIndex(pos->phvar[i])];

      else { /* Use current parm/input value */
        index = HINDEX(pos->phvar[i]);
        j = vrgInputs[index].iDoseCur;
        if (IsInput(pos->phvar[i]) && 
            (vrgInputs[index].iType == IFN_SPIKES)) {
 
          /* FB 11/6/97 */
          if ((vrgInputs[index].rgT0s[j] == pexp->dTime) &&
              (j < vrgInputs[index].nDoses))
            dTmp = vrgInputs[index].rgMags[j];
          else 
            dTmp = 0;

        }
        else
          dTmp = GetVarValue (pos->phvar[i]);
      }

      if (fabs(dTmp) < SO_EPSILON) /* Avoid silly little numbers  */
        dTmp = 0.0;

      pos->prgdOutputVals[i][pos->piCurrentOut[i]++] = dTmp;

    } /* if */
  } /* for */

} /* SaveOutputs */


/* -----------------------------------------------------------------------------
   NextOutputTime

   Returns in pdTout,the next time, pdTout, at which an variable is
   to be output.
*/

void NextOutputTime (PEXPERIMENT pexp, PDOUBLE pdTout, PINT piOut)
{
  if (pexp->dTime < pexp->dTfinal)
    if (++*piOut < pexp->os.cDistinctTimes)
      *pdTout = pexp->os.rgdDistinctTimes[*piOut];
    else
      *pdTout = pexp->dTfinal;

} /* NextOutputTime */


/* -----------------------------------------------------------------------------
   WriteOneMod

   writes one parameter modification from the list.   Inputs are *not*
   written.
*/

int WriteOneMod (PVOID pData, PVOID pInfo)
{
  PMCVAR pmcvar = (PMCVAR) pData;
  PFILE pfile = (PFILE) pInfo;

  if (!IsInput (pmcvar->hvar))
    fprintf(pfile, "%g\t", pmcvar->dVal);

  return 0;

} /* WriteOneMod */


/* -----------------------------------------------------------------------------
   WriteMCHeader

   Write a tabulated text header with the run number, the list of parameters 
   and outputs.
*/

void WriteMCHeader (PFILE pfileOut, PANALYSIS panal)
{
  long i, j, k;
  PMONTECARLO pmc = &panal->mc;
  OUTSPEC *pos;

  fprintf (pfileOut, "Iter");

  for (i = 0; i < pmc->nParms; i++)
   fprintf (pfileOut, "\t%s", GetVarName(pmc->rgpMCVar[i]->hvar));
  
  /* print the outputs as they come with experiment and time code */
  for (i = 0; i < panal->expGlobal.iExp; i++) {
    pos = &panal->rgpExps[i]->os;
    for (j = 0; j < pos->nOutputs; j++) {
      for (k = 0; k < pos->pcOutputTimes[j]; k++)
         fprintf (pfileOut, "\t%s_%ld.%ld", pos->pszOutputNames[j], i+1, k+1);
    } /* for j */
  } /* for i */

  fprintf (pfileOut, "\n");

  fflush (pfileOut);

} /* WriteMCHeader */


/* -----------------------------------------------------------------------------
   OpenMCFiles

   Open all the files written to be WriteMCOutput()

   Return non-NULL on error;
*/

int OpenMCFiles (PANALYSIS panal)
{
  int iErr = 0;
  PMONTECARLO pmc = &panal->mc;

  /* Use command line spec if given */
  if (panal->expGlobal.os.bCommandLineSpec)
    pmc->szMCOutfilename = panal->expGlobal.os.szOutfilename;
  else 
    if (!(pmc->szMCOutfilename))  /* Default if none given */
      pmc->szMCOutfilename = vszDefMCOutFilename;

  if (!pmc->pfileMCOut
      && !(pmc->pfileMCOut = fopen (pmc->szMCOutfilename, "w"))) {
    iErr++;
    ReportError (NULL, RE_FATAL | RE_CANNOTOPEN, pmc->szMCOutfilename,
                 "OpenMCFiles()");
  }

  WriteMCHeader (pmc->pfileMCOut, panal);

  return (iErr);

} /* OpenMCFiles */


/* -----------------------------------------------------------------------------
   CloseMCFiles

   Closes output files associated with Monte Carlo and set points runs
*/

void CloseMCFiles (PANALYSIS panal)
{
  fclose (panal->mc.pfileMCOut);
  printf ("\nWrote results to \"%s\"\n", panal->mc.szMCOutfilename);

} /* CloseMCFiles */


/* -----------------------------------------------------------------------------
   WriteMCOutput

   Output the parameters for this run and the results of the
   simulation (passed through TransformPred).
*/

void WriteMCOutput (PANALYSIS panal, PMCPREDOUT pmcpredout)
{
  PFILE pfileMC;
  PMONTECARLO pmc = &panal->mc;

  pfileMC = pmc->pfileMCOut;

  fprintf (pfileMC, "%ld\t", panal->mc.lRun);

  /* Include parameter values for that run */
  WriteArray (pfileMC, panal->mc.nParms, panal->mc.rgdParms);
  fprintf (pfileMC, "\t");

  /* write the flattened and eventually transformed predictions */
  WriteArray (pfileMC, pmcpredout->nbrdy, pmcpredout->pred);
  fprintf (pfileMC, "\n");

  fflush (pfileMC);

} /* WriteMCOutput */


/* -----------------------------------------------------------------------------
   WriteNormalOutput

   Write the results in the output file. This procedure is
   called only from time to time in order to save storage space
*/

void WriteNormalOutput (PANALYSIS panal, PEXPERIMENT pexp)
{
  int i, j;
  PFILE pfile;
  POUTSPEC posGlo, pos;

  if (!panal) return;

  posGlo = &panal->expGlobal.os;
  pos = &pexp->os;

  if (!posGlo->szOutfilename)
    posGlo->szOutfilename = vszDefOutFilename;

  if (!(posGlo->pfileOut))
    if (!(posGlo->pfileOut = fopen (posGlo->szOutfilename, "w")))
      ReportError (NULL, RE_CANNOTOPEN | RE_FATAL,
               posGlo->szOutfilename, NULL);

  pfile = posGlo->pfileOut;
  fprintf (pfile, "Results of Experiment %d\n\n", pexp->iExp);

  /* Vertical output:  Formatted  Time1    Out_Var1  Out_Var2 ... */

  fprintf (pfile, "Time");

  for (i = 0; i < pos->nOutputs; i++)
    fprintf (pfile, "\t%s", pos->pszOutputNames[i]);
  fprintf (pfile, "\n");

  for (j = 0; j < pos->nOutputs; j++)
    pos->piCurrentOut[j] = 0;

  for (i = 0; i < pos->cDistinctTimes; i++) {
    fprintf (pfile, "%g", pos->rgdDistinctTimes[i]);
    for (j = 0; j < pos->nOutputs; j++) {

      if (pos->piCurrentOut[j] < pos->pcOutputTimes[j]
          && pos->rgdDistinctTimes[i]
          == pos->prgdOutputTimes[j][pos->piCurrentOut[j]])
        fprintf (pfile, "\t%g",
                 pos->prgdOutputVals[j][pos->piCurrentOut[j]++]);

      else
        fprintf (pfile, "\t");

    } /* for */

    fprintf (pfile, "\n");

  } /* for */

  fprintf (pfile, "\n");

} /* WriteNormalOutput */
