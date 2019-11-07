/* optdesign.h

   originally written by Frederic Bois
   
   Copyright (c) 1997.  Don Maszle, Frederic Bois.  All rights reserved.

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
     Logfile:  SCCS/s.optdesign.h
    Revision:  1.4
        Date:  14 Nov 1997
     Modtime:  06:39:18
      Author:  @a
   -- SCCS  ---------

   Header file for optdesign.c
*/

#ifndef _OPTDESIGN_H_
#define _OPTDESIGN_H_

/* -----------------------------------------------------------------------------
   Inclusions
*/

#include <float.h>  /* Floating point limits */

#include "sim.h"


/* -----------------------------------------------------------------------------
   Prototypes
*/

void CloseOptFiles (PANALYSIS panal);

void Compute_cost (long nData, int *piData_mask, double *dCost);

void Do_Importance_Ratios (long nMod, double *pdL, double dSumL);

void DoOptimalDesign (PANALYSIS panal);

double DoVariance (long nDim, long *pIndex, long *plDrawn, double **pdX,
                   long istart, long ifinish);

double DoVariance2 (long nDim, double *pdImpR, double **pdX,
                    long istart, long ifinish);

int  Estimate_y (PANALYSIS panal, double *pdTheta, double *pdPred);

void Fake_data (long nData_sampled, long nData, double **pdPred, 
                double **pdSigma, long *plSigmaIndex, double **pdData);

void Importance_Resample (long nMod, long *pIndex0, long *pIndex1, 
                          long *plDrawn, double *pdLL, double dSumL);

void InitDPV (PANALYSIS panal, double ***pdData, int **piData_mask, 
              long *pnData, double ***pdPred, long *pnPred, 
              double **pdVariance, long nMod, long nData_sampled);

void InitSigma (PANALYSIS panal, long nData, double **pdSigma, 
                 long *pnSigma, long **plSigmaIndex);

double Likelihood (double *pdPred, double *pdData, long nData, 
                   double *pdSigma, long *plSigmaIndex, int *piData_mask, 
                   int nData_tried);

void OpenOptFiles (PANALYSIS panal);

void ReadPSample (FILE *pfileRestart, long nSubjs, long nParms, 
                  double ***pdTheta, double ***pdSigma, double **pdLL, 
                  long **plVectorIndex, long **plVectorIndex_tmp,
                  long nSigma, long nMod);

void WriteOptimOut (PANALYSIS panal, long iter, long nData, long nMod, 
                    int criterion, double *pdVariance, int *piData_mask, 
                    long iCrit, double dCrit, double dCost);

void WriteOutHeader (PANALYSIS panal, int criterion);


#endif /* _OPTDESIGN_H_ */

/* End */

