/* yourcode.h

   written by Frederic Bois
   
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
     Logfile:  SCCS/s.yourcode.h
    Revision:  1.7
        Date:  14 Nov 1997
     Modtime:  06:39:23
      Author:  @a
   -- SCCS  ---------

   Header file for the "yourcode" file, containing customizable routines.
*/

#ifndef _YOURCODE_H_
#define _YOURCODE_H_

/* -----------------------------------------------------------------------------
   Inclusions
*/

#include "sim.h"


/* -----------------------------------------------------------------------------
   Typedefs
*/

typedef struct tagMCPREDOUT {
  long nbrdy;    /* number of kinetic ys */
  double *pred;  /* pointer to the data */
  int passflag;  /* typically a pass/fail flag */
} MCPREDOUT, *PMCPREDOUT;


/* -----------------------------------------------------------------------------
   Prototypes  */

void DoStep_by_Step (void /* double t, long *neq, double *y */);
void OutspecToLinearArray (PANALYSIS panal, PMCPREDOUT pMCPredOut);
void TransformPred (PANALYSIS, PMCPREDOUT);
double Definite_Integral (double (*Function)(double), double dFrom, double dTo);
void Interpolate_Poly (double rgdX[], double rgdY[], int n, double x, 
                       double *pdY, double *pdDY);
double Trapezes (double (*Function)(double x), double dFrom, double dTo, 
                 int nSteps);

#endif /* _YOURCODE_H_ */

/* End */

