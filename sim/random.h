/* random.h

   written by Frederic Bois
   modifications by Don Robert Maszle
   8 January 1992

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
     Logfile:  SCCS/s.random.h
    Revision:  1.21
        Date:  14 Nov 1997
     Modtime:  06:39:19
      Author:  @a
   -- SCCS  ---------

   Header for random number generator.  See random.c for extensive
   documentation.

   Gives prototypes for random functions.
*/

#ifndef _RANDOM_H_

#include <math.h>

/* ----------------------------------------------------------------------------
   Constants  */

#define SEED_MIN     1.0
#define SEED_MAX     2147483646.0
#define SEED_DEFAULT 314159265.3589793
#define PI           3.1415926535897932384626433
#define INV_SQRT_2PI 0.398942280401433


/* ----------------------------------------------------------------------------
   Prototypes  */

/* Initialize the random generators, optional but recommended */
void InitRandom (double dSeed, int bWarmUp);

/* Two random generators */
/*     one that shuffles its output, */
double RandomShuffle (void);

/*     and one that doesn't */
double Randoms (void);


/* Several types of random variates */

double BetaRandom (double alpha, double beta, double a, double b);
double BinomialBetaRandom (double Expectation, double alpha, double beta);
double BinomialRandom (double p, long n);
double Chi2Random (double dof);
double ExpRandom (double beta);
double InvGGammaRandom (double alpha, double beta);
double GammaRandom (double alpha);
double GGammaRandom (double alpha, double beta);
double LogNormalRandom (double dMean, double dStdDev);
double LogUniformRandom (double a, double b);
double NormalRandom (double dMean, double dStdDev);
double PiecewiseRandom (double min, double a, double b, double max);
double PiecewiseVariate (long cDim, double rg_x[], double rg_pdf[],
                         double rg_Cdf[], int iOrder, double *pVal_pdf);
long   PoissonRandom (double mu);
double TruncLogNormalRandom (double dMean, double dStdDev, double a, double b);
double TruncNormalRandom (double dMean, double dStdDev, double a, double b);
double UniformRandom (double a, double b);
void   Multinomial (long n, int dim, double *p, double *x);
void   WishartRandom (long n, long p, double *t, double *w, double *work);


/* ----------------------------------------------------------------------------
   Utility functions */

double CDFNormal(double z);
double InterpolateX (double rgX[], double rgY[], long lLower, double dY);
double erfc(double x);
double lnDFNormal(double x, double mu, double sd);
double lnGamma (double x);
double lnDFBeta (double x, double alpha, double beta, double min, double max);
void   CalcCumulative (long cDim, double *rg_x, double *rg_pdf,
                       double *rg_Cdf, int  iOrder);
void   SetSeed (double dSeed);
 
#define _RANDOM_H_
#endif

/* End */

