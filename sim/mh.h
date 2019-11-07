/* mh.h

   originally written by Frederic Bois
   
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
     Logfile:  SCCS/s.mh.h
    Revision:  1.12
        Date:  14 Nov 1997
     Modtime:  06:39:17
      Author:  @a
   -- SCCS  ---------

   Header file for mh.c
*/

#ifndef _MH_H_
#define _MH_H_

/* ----------------------------------------------------------------------------
   Inclusions
*/

#include <float.h>  /* Floating point limits */
#include <stdarg.h>

#include "sim.h"

#define UPDATE_AT 20	/* Change MH criterion */

/* ----------------------------------------------------------------------------
   Global/External variables
*/


/* ----------------------------------------------------------------------------
   Definitions
*/

#define MAX_ARGS              25

#define BAD_VAL               1.0E+100
#define MISSING_VALUE         (-DBL_MAX)
#define INPUT_MISSING_VALUE   -1

/* ----------------------------------------------------------------------------
   Typedefs
*/


/* ----------------------------------------------------------------------------
   Prototypes
*/

void CalculateTotals (PLEVEL plevel, char **args);

void CheckForFixed (PLEVEL plevel, char **args);
void CheckPrintStatements (PLEVEL plevel, char **args);

void CloneMCVars (PLEVEL plevel, char **args);
void CloneMCVarsL (PVOID pData, PVOID pUser1, PVOID pUser2, PVOID pUser3);

void CloseMarkovFiles (PANALYSIS panal);

void ConvertLists (PLEVEL plevel, char **args);

void DoMarkov (PANALYSIS panal);

void FindMCParents (PLEVEL plevel, char **args);

void FindMCDependents(PLEVEL plevel, char **args);

void GetNumberOfMCVars (PLEVEL plevel, char **args);

void InitArrays (long lDim, long nSubjs, double ***pdSum, 
                 double ****prgdSumProd);

void InitMCVars (PLEVEL plevel, char **args);

/* double L2 (PEXPERIMENT pexpt, PANALYSIS panal); */

void ListToPMCArray (PANALYSIS panal, PLIST plist,
                     long *nMCVars, PMCVAR **rgpMCVars);
void ListToPMCArrayL (PVOID pData, PVOID pUser1, PVOID pUser2, PVOID pUser3);
void ListToPVArray (PANALYSIS panal, PLIST plist,
                    long *nFixedVars, PVARMOD **rgpFixedVars);
void ListToPVArrayL (PVOID pData, PVOID pUser1, PVOID pUser2, PVOID pUser3);
double LnDensity (PMCVAR pMCVar, PANALYSIS panal);
double LnLike (PMCVAR pMCVar, PANALYSIS panal);
double LnLikeData (PEXPERIMENT pexpt, PANALYSIS panal);
void   OpenMarkovFiles (PANALYSIS panal);
void   PrintAllExpts (PLEVEL plevel, long nVar, PANALYSIS panal,
                      PFILE pOutFile);
void   PrintDeps (PLEVEL plevel, char **args);
int    PrintExpt (PLEVEL plevel, char **args);
void   ReadRestart (FILE *pfileRestart, long nThetas,
                    double *pdTheta, double *pdSum, double **prgdSumProd, 
                    long *pIter);
int    RestoreLikelihoods (PLEVEL plevel, char **args);
int    RunAllExpts(PLEVEL plevel, long nVar, PANALYSIS panal, 
       PDOUBLE pdLnData);
int    RunExpt (PLEVEL plevel, char **args);
double SampleTheta (PMCVAR pMCVar);
void   SampleThetas (PLEVEL plevel, char **args);
void   SampleThetaVector (PLEVEL pLevel, PANALYSIS panal, long nThetas,
                          double *pdTheta, double *pdSum, double **prgdSumProd,
                          long iter, long nUpdateAt, PDOUBLE pdLnPrior, 
                          PDOUBLE pdLnData);
int    SaveLikelihoods (PLEVEL plevel, char **args);
void   SetFixedVars (PLEVEL plevel);
void   SetKernel (PLEVEL plevel, char **args);
void   SetModelVars (PLEVEL plevel);
int    SetMCVars (PLEVEL plevel, char **args);
int    SumAllExpts (PLEVEL plevel, char **args);
void   SetPointers (PLEVEL plevel, char **args);
BOOL Test (BOOL bExptIsDep, double dLnPrior, double dLnPriorNew,
           double dLnLike, double dLnLikeNew,
           double dLnData, double dLnDataNew);
void   TraverseLevels (PLEVEL plevel,
                       void (*routinePtr)(PLEVEL plevel, char **args), ...);
int    TraverseLevels1 (PLEVEL plevel,
                        int (*routinePtr)(PLEVEL plevel, char **args), ...);
void   WriteHeader (PLEVEL plevel, char **args);
void   WriteMCVars (PLEVEL plevel, PANALYSIS panal, PFILE pOutFile);

#endif /* _MH_H_ */

/* End */
