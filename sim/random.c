/* random.c

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
     Fr�d�ric Bois / Don Maszle
     BEHS, School of Public Health
     University of California at Berkeley
     Berkeley, CA 94720

     fbois@diana.lbl.gov

   -- Revisions -----
     Logfile:  SCCS/s.random.c
    Revision:  1.29
        Date:  14 Nov 1997
     Modtime:  06:39:06
      Author:  @a
   -- SCCS  ---------

 * Random number generator module:  provides two basic random number
   generators:

   Randoms()        yielding uniform random variates between 0 and 1, and
   RandomShuffle()  which shuffles the output of the first generator.

 * Also available are several other types of random variates:

   BetaRandom(alpha, beta, a, b)      -- Beta(alpha, beta) over [a, b]
   BinomialBetaRandom(E, a, b)        -- BetaBinomial (n = E + E * a / b)
   BinomialRandom(p, n)               -- Binomial of n trials, P(each) = p
   Chi2Random(dof)                    -- Chi-squared w/dof degrees of freedom
   ExpRandom(beta)                    -- Exponential of inverse scale beta
   InvGGammaRandom (alpha, beta)      -- General inverse gamma variate
   GammaRandom (alpha)                -- Gamma variate
   GGammaRandom (alpha, beta)         -- General gamma variate
   LogNormalRandom (m, s)             -- exp (Normal)
   LogUniformRandom(a, b)             -- LogUniform over [a, b]
   Multinomial(...)                   -- Multinomial variates
   NormalRandom (m, s)                -- General Normal
   PiecewiseRandom (min, a, b, max)            -- Draws from a mayan pyramid !
   PiecewiseVariate (n, x[], p[], Cdf[], o, p) -- Draws from a tabulated PDF
   PoissonRandom(mu)                  -- Poisson with rate mu
   TruncLogNormalRandom (m, s, a, b)  -- Truncated log normal
   TruncNormalRandom (m, s, a, b)     -- Truncated general normal
   UniformRandom(a, b)                -- Uniform over [a, b]
   WishartRandom(...)                 -- Wishart (matrix) variates

 * And utility functions:

   CDFNormal(z)                                -- Integral of the normal at z
   CalcCumulative (n, x[], p[], Cdf[], o)      -- Constructs a CDF given a PDF
   erfc(x)                                     -- Error function
   lnDFNormal(x, mu, sd)                       -- Log of the normal density
   lnGamma(x)                                  -- Natural log of gamma function
   lnDFBeta (x, alpha, beta, min, max)         -- Log of beta density
   SetSeed(seed)                               -- Sets the seed of Randoms
   InitRandom(seed, f)                         -- Initializes the package

 * The random number generator must be initialized by providing a seed
   to InitRandom().  A non-zero second argument to this function
   instructs the random number generator to "warm up" by filling a
   memory array used by RandomShuffle().  This also initializes a flag
   used by the Normal() routine.

   If the random number generator is not initialized before any of its
   routines are called, it initializes itself with a default seed.

*/

#include <float.h>
#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "hungtype.h"
#include "lex.h"
#include "lexerr.h"
#include "random.h"


typedef struct tagRANDREC {
  double seed, last;
  double mem[50];
  long switchG;
  double memGauss;

} RANDREC, *PRANDREC;


static vbNoSeed = TRUE;         /* Flag to prevent use without seed */
static vbNotInitd = TRUE;       /* Flag to prevent use without initializing */
static RANDREC vRandRec;        /* Global random informatino shared by */
                                /* all random number functions */

void SetSeed (double dSeed)
{
  int bCorrected = 0;

  if (dSeed == 0.0) {
    dSeed = SEED_DEFAULT;
    bCorrected++;
  }

  if (dSeed < 0)
    dSeed = -dSeed; /* Don't announce this correction */

  if (dSeed < SEED_MIN) {
    dSeed = SEED_MIN + (dSeed/SEED_MIN) / (SEED_MAX-SEED_MIN);
    bCorrected++;
  }

  if (dSeed > SEED_MAX) {
    dSeed = SEED_MIN + (SEED_MAX/dSeed) / (SEED_MAX-SEED_MIN);
    bCorrected++;
  }

  assert ((/* Invalid Seed */ dSeed >= SEED_MIN && dSeed <= SEED_MAX));

  /* Assign valid seed */

  if (bCorrected)
    printf ("SetSeed():  corrected out of range random number seed\n"
            "Seed must lie in the range [%g, %g]\n"
            "New seed --> %g\n", SEED_MIN, SEED_MAX, dSeed);

  vRandRec.seed = dSeed;
  vbNoSeed = FALSE; /* Flag that seed has been set */

} /* SetSeed */


/* -----------------------------------------------------------------------------
   InitRandom

   initializes the random generator with the given seed.
   If an invalid seed is given, SetSeed() silently corrects it.

   If the boolean bWarmUp is non-zero, the random number generator is
   "warmed up" by running it a number of times.  After this, a memory
   array is filled from which shuffled random values will be drawn.
   Also, a flag used by the Normal() routine is initialized.
*/

void InitRandom (double dSeed, int bWarmUp)
{
  long i;

  /* Prevent nuking user's seed if not initd */
  if (vbNoSeed || dSeed != SEED_DEFAULT) 
    SetSeed (dSeed);

  if (bWarmUp) {
    /* Warm up generator */
    for (i = 0; i < 50; i++) (void) Randoms ();

    /* Fill the shuffle array */
    for (i = 0; i < 50; i++) vRandRec.mem[i] = Randoms ();

    vRandRec.last = Randoms (); /* Draw first number */
    vRandRec.switchG = 0;       /* Flag as first Normal */
    vbNotInitd = FALSE;         /* Flag as initialized */
  } /* if */

} /* InitRandom */


/* -----------------------------------------------------------------------------
   Randoms

   An alternative random number generator, so you don't have to use
   the (probably not so good) system supplied standard C version.

   Randoms() returns random numbers between 0 and 1. The minimum
   returned value is 1/m and the maximum 1 - 1/m. The generator can
   be initialized with InitRandom(). 

   This generator should be correct on any system for which the
   representattion of reals uses at least a 32-bit mantissa, including
   the sign bit.

   From PARK SK, MILLER KW: Random number generators: good ones are
   hard to find.  Commun. ACM 1988; 31: 1192. (Version Real 2).
*/

double Randoms (void)
{
#define a  16807.0
#define m  2147483647.0
#define q  127773.0   /* m Div a */
#define r  2836.0     /* m Mod a */

  double hi, test;

  if (vbNoSeed)
    SetSeed (SEED_DEFAULT);

  hi = (long)(vRandRec.seed / q);
  test = a * (vRandRec.seed - q * hi) - r * hi;

  if (test > 0.0)
    vRandRec.seed = test;
  else
    vRandRec.seed = test + m;

  return (vRandRec.seed / m);

#undef a
#undef m
#undef q
#undef r

} /* Randoms */


/* -----------------------------------------------------------------------------
   RandomShuffle

   assumes that the random number generator is Randoms ().
   RandomShuffle() shuffles the output of this generator.

   The routine is initialized by calling InitRandom().  A check is
   done to assure that initialization is performed.

   Adapted from the algorithm described in the book Numerical Recipes by
   Press et al. 
*/

double RandomShuffle (void)
{
  long i;

  if (vbNotInitd)
    InitRandom (SEED_DEFAULT, TRUE);

  i = (long) (50.0 * vRandRec.last); /* Randomly shuffle output */
  vRandRec.last = vRandRec.mem[i];
  vRandRec.mem[i] = Randoms ();

  return (vRandRec.last);

} /* RandomShuffle */


/* -----------------------------------------------------------------------------
   NormalRandom

   Returns a Normal random variate based on a unit variate,
   using a random generator as a source of uniform deviates.
   Adapted from the algorithm described in the book Numerical Recipes by
   Press et al. 

   Programs using Normal should initialize the random number
   generator with InitRandom().
*/

double NormalRandom (double dMean, double dStdDev)
{
  double dRacine, dTemp1, dTemp2, dTemp3;

  if (vbNotInitd)
    InitRandom (SEED_DEFAULT, TRUE);

  if (vRandRec.switchG != 0) {
    vRandRec.switchG = 0;
    return (dMean + dStdDev * (vRandRec.memGauss));
  } /* if */

  do {
    dTemp1 = 2 * RandomShuffle () - 1;
    dTemp2 = 2 * RandomShuffle () - 1;
    dRacine = dTemp1 * dTemp1 + dTemp2 * dTemp2;
  } while ((dRacine >= 1) || (dRacine == 0));

  dTemp3 = sqrt(-2 * log(dRacine) / dRacine);
  vRandRec.memGauss = dTemp1 * dTemp3;
  vRandRec.switchG = 1;
  return (dMean + dStdDev * (dTemp2 * dTemp3));

} /* NormalRandom */


/* -----------------------------------------------------------------------------
   LogNormalRandom

   returns a variate such that the log of the variate is normally
   distributed.
*/

double LogNormalRandom (double dMean, double dStdDev)
{

  if (dMean <= 0) {
    char str[10];
    sprintf(str, "%5.2e", dMean);
    ReportRunTimeError(NULL, RE_BADLOGNORMALMEAN | RE_FATAL,
                       "", str, "LogNormalRandom");
  }
  else 
    if (dStdDev < 1) {
      char str[10];
      sprintf(str, "%5.2e", dStdDev);
      ReportRunTimeError(NULL, RE_BADLOGNORMALSD | RE_FATAL,
                         "", str, "LogNormalRandom");
    }

  return exp (NormalRandom (log (dMean), log (dStdDev)));

} /* LogNormalRandom */


/* -----------------------------------------------------------------------------
   TruncNormalRandom

   returns a truncated Normal variate in the range [a, b].
*/

double TruncNormalRandom (double dMean, double dStdDev, double a, double b)
{
  double X = 0.0;
  int    iter = 0;

  if (a >= b) {
    printf ("Error: TruncNormalRandom: min >= max  [%g %g]\n", a, b);
    exit (0);
  }

  else do {
    if(++iter == 25) {
      printf("Warning: TruncNormalRandom: problem with range: ");
      printf("min %g, max %g, ave %g, sd %g\n", a, b, dMean, dStdDev);
    }
    X = NormalRandom(dMean, dStdDev);
  }
  while (X < a || X > b);

  return X;

} /* TruncNormalRandom */


/* -----------------------------------------------------------------------------
   TruncLogNormalRandom

   returns a truncated LogNormal variate in the range [a, b].
*/

double TruncLogNormalRandom (double dMean, double dStdDev, double a, double b)
{
  double X = 0.0;
  int    iter = 0;

  if (a >= b)
    printf ("TruncLogNormalRandom: min >= max  [%g %g]\n", a, b);

  else do {
    if(++iter == 25) {
      printf("TruncLogNormalRandom: problem with range: ");
      printf("min %g, max %g, ave %g, sd %g\n", a, b, dMean, dStdDev);
    }
    X = LogNormalRandom(dMean, dStdDev);
  }
  while (X < a || X > b);

  return X;

} /* TruncLogNormalRandom */


/* ----------------------------------------------------------------------------
   ExpRandom

   returns an exponential variate with inverse scale beta

   Algorithm 3.2 from Ripley "Stochastic Simulations" Wiley 1987, p. 55.
*/

double ExpRandom (double beta)
{

  if (beta <= 0) {
    printf ("Error: negative or null inverse scale for an exponential variate "
            "- Exiting\n\n");
    exit (0);
  }

  return -log(Randoms()) / beta;

} /* ExpRandom */


/* ----------------------------------------------------------------------------
   GammaRandom

   returns a gamma distributed random variate with shape parameter
   alpha.

   If alpha < 1 uses algorithm 3.19 of Ripley; if alpha > 1 uses
   algorithm 3.20 of Ripley; if alpha is 1 uses returns an
   ExpRandom variate.

   Reference:
   - Ripley, Stochastic Simulations, John Wiley and Sons, 1987, pp 88-90.
*/

double GammaRandom (double alpha)
{
#define E 2.718281828459
  static double aprev = 0.0, c1, c2, c3, c4, c5;
  double b, u1, u2, w, x;

  if (alpha <= 0) {
    printf ("Error: negative or null shape parameter for a gamma variate "
            "- Exiting\n\n");
    exit (0);
  }
  else if (alpha < 1) {

    b = (alpha + E) / E;

    do {
      u1 = b * Randoms();
      if (u1 <= 1.0) {
        x = pow(u1, 1./alpha);
        /* problem: if alpha is too small, x will be about zero and that's
           bad, particularly for inverse-gamma variates. 
           Fixed by blocking zeros - FB 5/11/1997 */
        if ((x > DBL_MIN) && (x <= -log(Randoms()))) 
          return(x);
      }
      else {
        x = -log((b - u1) / alpha);
        if (pow(x, alpha - 1) >= Randoms()) return(x);
      }
    } while (1);

  } /* end if alpha < 1 */

  else {
    if (alpha > 1) {

      if (alpha != aprev) {
        /* initialize */
        aprev = alpha;
        c1 = alpha - 1;
        b = 1.0 / c1;
        c2 = b * (alpha - (1 / (6.0 * alpha)));
        c3 = 2 * b;
        c4 = c3 + 2.0;
        if (alpha > 2.5)
          c5 = 1.0 / sqrt(alpha);
      }

      do {
        do {
          u1 = Randoms();
          u2 = Randoms();
          if (alpha > 2.5)
            u1 = u2 + c5 * (1 - 1.86 * u1);
        } while ((u1 >= 1) || ( u1 <= 0));

        w = c2 * u2 / u1;
        if (((c3 * u1 + w + 1 / w) <= c4) ||
            ((c3 * log(u1) - log(w) + w) < 1))
          return(c1 * w);
      } while (1);

    }
    else
      return ExpRandom(1.0);
  }

  #undef E

} /* GammaRandom */


/* ----------------------------------------------------------------------------
   GGammaRandom

   Returns a gamma distributed random variate with shaping parameter
   alpha and inverse scale parameter beta.
*/

double GGammaRandom (double alpha, double beta)
{

  if (beta <= 0) {
    printf ("Error: negative or null inverse scale for a gamma variate "
            "- Exiting\n\n");
    exit (0);
  }
  return GammaRandom(alpha) / beta;

} /* GGammaRandom */


/* ----------------------------------------------------------------------------
   InvGGammaRandom

   Returns an inverse gamma distributed random variate with shaping parameter
   alpha and scale parameter beta.
   This just gets a general gamma variate and returns its inverse
   See Gelman et al. "Bayesian Data Analysis"
*/

double InvGGammaRandom (double alpha, double beta)
{

  /* parameters will be checked in GGammaRandom */
  if (beta <= 0) {
    printf ("Error: negative or null scale for an inverse gamma variate "
            "- Exiting\n\n");
    exit (0);
  }
  
  return beta / GammaRandom(alpha);

} /* InvGGammaRandom */


/* ----------------------------------------------------------------------------
   Chi2Random

   returns a chi-squared random variate, which is a gamma(dof/2, 1/2).
*/

double Chi2Random (double dof)
{

  return (GGammaRandom (dof / 2.0, 0.5));

} /* Chi2Random */


/* ----------------------------------------------------------------------------
   PoissonRandom

   returns a Poisson random variate, with rate mu.

   If mu is less than 60, uses inversion; otherwise uses the rejection
   method of Atkinson (as presented by Ripley "Stochastic Simulations",
   Wiley 1987, p 79).
*/

long PoissonRandom(double mu)
{

  double u1, x, u2, lnfact, s, t;
  static double prev_mu = 0, c, beta, alpha, k;
  long n = 0;

  if (mu <= 0) {
    printf("Error: negative or null rate for a Poisson variate "
           "- Exiting\n\n");
    exit(0);
  }

  if (mu <= 60) {
    /* inversion */
    s = 1;
    t = 1;
    u1 = Randoms() * exp(mu);
    while(s < u1){
      n++;
      t = t * mu / n;
      s = s + t;
    }
  }
  else {
    /* rejection */
    if (mu != prev_mu) {
      c = 0.767 - 3.36 / mu;
      beta = PI / sqrt(3 * mu);
      alpha = beta * mu;
      k = log(c) - mu - log(beta);
    }

    do {
      do {
        u1 = Randoms();
        x = (alpha - log((1 - u1) / u1)) / beta;
      } while (x <= -0.5);

      n = (long)(x + 0.5);
      u2 = Randoms();

      /* calculate log n factorial using Stirling's formula */
      lnfact = 0.918938533 - n + (n + 0.5) * log(n);
    } while (alpha - beta * x + log(u2 / pow((1 + exp(alpha - beta * x)), 2))
             > k + n * log(mu) - lnfact);
  }

  return n;

} /* PoissonRandom */


/* -----------------------------------------------------------------------------
   BinomialRandom

   Return as a double floating-point number an integer value that is a random
   deviate drawn from a binomial distribution of n trials each of
   probability p, using Randoms () as a source of uniform random deviates.
   Adapted from the algorithm described in the book Numerical Recipes by
   Press et al. 
*/

double BinomialRandom (double p, long N)
{
  long j;
  static long iOldN = -1;
  double dAngle, dDeviate, dMean, dPtemp, dSqrt, dTangent, dTemp1, dTemp2;
  static double dLnFactN, dPold = -1, dLnP, dQ, dLnQ;

  if (p < 0 || p > 1 || N < 0) {
    printf ("Error: negative or null parameters for a binomial variate "
            "- Exiting\n\n");
    exit (0);
  }

  dPtemp = ( p <= 0.5 ? p : 1 - p);
  dMean = N * dPtemp;  /* mean of the deviate to be produced. */

  /* Use the direct method if N is not too large.
     This can require up to 25 calls to random */

  if (N < 25) {
    dDeviate = 0;
    for (j = 0; j < N; j++)
      if (Randoms () < dPtemp)
        dDeviate = dDeviate + 1;
  }
  else
    if (dMean < 1) {
      /* if less than one event is expected out of 25 or more trials,then the
         distribution is quite accurately Poisson. Use direct method. */
      dTemp1 = exp(-dMean);
      dTemp2 = 1.0;
      for (j = 0; j <= N; j++) {
        dTemp2 = dTemp2 * Randoms ();
        if (dTemp2 < dTemp1) break;
      }

      dDeviate = (j <= N ? j : N);
    }
    else { /* Use rejection */

      if (N != iOldN) { 
        /* if N has changed or it's the first call, initialize */
        dLnFactN = lnGamma((double) N + 1);
        iOldN = N;
      }

      if (dPtemp != dPold) { 
        /* if dPtemp has changed or it's the first call, initialize. */
        dPold = dPtemp;
        dQ = 1 - dPtemp;
        dLnP = log(dPtemp);
        dLnQ = log(dQ);
      } /* if */

      dSqrt = sqrt(2 * dMean * dQ);

      /* Rejection method with a Lorentzian comparison function. */

      do {
        do {
          dAngle = PI * Randoms ();
          dTangent = tan(dAngle);
          dTemp1 = dSqrt * dTangent + dMean;
        } while (dTemp1 < 0 || dTemp1 >= (N + 1)); /* Reject */

        dTemp1 = floor(dTemp1); /* discrete distribution */

        dTemp2 = 1.2 * dSqrt * (1 + dTangent * dTangent) *
                 exp(dLnFactN - lnGamma(dTemp1 + 1) - 
                     lnGamma(N - dTemp1 + 1) +
                     dTemp1 * dLnP + (N - dTemp1) * dLnQ);

      } while (Randoms () > dTemp2);

      /* Reject on average about 1.5 time per deviate */

      dDeviate = dTemp1;

    } /* else */  /* end of rejection */

  if (dPtemp != p)
    dDeviate = N - dDeviate; /* undo the symmetry tranformation */

  return (dDeviate);

} /* BinomialRandom */


/* ----------------------------------------------------------------------------
   BinomialBetaRandom

   Return as a double floating-point number an integer value that is a random
   deviate drawn from a binomial distribution of n trials each of
   probability p, p being beta distributed with parameters alpha and beta.
   I use the expectation in input. The classical N is equal to 
   E + E * beta / alpha.
   See Bernardo & Smith "Bayesian Theory"
*/

double BinomialBetaRandom (double Expectation, double alpha, double beta)
{

  /* parameters will be checked in BinomialRandom */

  return BinomialRandom (BetaRandom (alpha, beta, 0, 1),
                         Expectation + Expectation * beta / alpha);

} /* BinomialBetaRandom */


/* -----------------------------------------------------------------------------
   Multinomial

   A procedure to compute return multinomial deviates.
   N is the number of trials,
   p the array of probabilities,
   dim the dimension of the array (number of possible events),
   x the array of event occurences which is returned.

   From Devroye "Non-Uniform Random Numbers...".
*/

void Multinomial (long n, int dim, double *p, double *x)
{
  int i;
  double sum, ptemp;

  sum = 1;

  for (i = 1; i <= dim; i++) {
    if (p[i]) {
      ptemp = p[i] / sum;
      x[i] = BinomialRandom (ptemp, n);
      n = n - (long)x[i];
      sum = sum - p[i];
    }
    else x[i] = 0.0;

  } /* for */

} /* Multinomial */


/* -----------------------------------------------------------------------------
   Wishart

   samples a matrix according to the Wishart distribution by the method
   of Odell and Feiveson (1966).

   Paramters are:
   n (degrees of freedom); p (dimension of Wishart matrix);
   t (pointer to a Cholesky decomposition of a covariance matrix);
   w (pointer to the sampled Wishart matrix, in
   triangular form; work (pointer to a work space, of length p*p).

   Triangular matrices are stored in order
   0 1 3
     2 4
       5 etc.
*/

void WishartRandom (long n, long p, double *t, double *w, double *work)
{
  double eta, sum;
  long i, j, k, m, k1, k2, k3;

  printf ("WishartRandom not tested - Exiting...");
  exit(0);

  /* generate random quantities for Bartlett's decomposition */
  for (j = 0, k = 0; j < p; j++) {
    for (i = 0; i < j; i++)
      w[k++] = NormalRandom(0, 1);

    /* Chi-square with n-i degrees of freedom */
    w[k++] = GGammaRandom((n - i) / 2.0, 0.5);
  }

  /* generate a standard Wishart */
  for (j = p - 1, m = k - 1, k2 = (p * (p - 1)) / 2; j >= 0; k2 = k2 - (j--)) {
    eta = w[m];
    for (i = j, k1 = (i * (i + 1)) / 2; i >= 0; k1 = k1 - (i--), m--) {
      for (k = 0, sum = 0.0; k < i; k++)
        sum = sum + w[k1+k] * w[k2+k];

      if (i == j)
        w[m] = sum + eta;
      else
        w[m] = sum + sqrt(eta) * w[m];
    }
  }

  /* form product L * W * L' */
  for (i = 0, k1 = 0, m = 0; i < p; k1 = k1 + (++i)) {
    for (j = 0, k2 = 0; j < p; k2 = k2 + (++j), m++) {
      for (k = 0, sum = 0.0; k < j; k++)
        sum = sum + t[k1+k] * w[k2+k];

      for (k = j, k3 = j; k <= i; k3 = k3 + (++k))
        sum = sum + t[k1+k] * w[k2+k3];

      work[m] = sum;
    }
  }

  for (i = 0, m = 0, k1 = 0; i < p; i++, k1 = k1 + p) {
    for (j = 0, k2 = 0; j <= i; k2 = k2 + (++j), m++) {
      for (k = 0, sum = 0.0; k <= j; k++)
        sum = sum + work[k1+k] * t[k2+k];

      w[m] = sum;
    }
  }

} /* WishartRandom */


/* -----------------------------------------------------------------------------
   UniformRandom

   returns a variate that is uniformly distributed on the interval [a,b].
*/

double UniformRandom (double a, double b)
{

  if (b < a) {
    printf ("Error: bad range a for uniform variate - Exiting\n\n");
    exit (0);
  }

  return (Randoms() * (b - a) + a);

} /* UniformRandom */


/* -----------------------------------------------------------------------------
   LogUniformRandom

   returns a variate that is log-uniformly distributed on the interval
   [a,b].
*/

double LogUniformRandom (double a, double b)
{

  if (b < a) {
    printf ("Error: bad range a for uniform variate - Exiting\n\n");
    exit (0);
  }

  return ( a * pow(b/a, Randoms()) );

} /* LogUniformRandom */


/* -----------------------------------------------------------------------------
   BetaRandom

   returns a variate that is Beta distributed on the interval [a,b]
   with shape parameters alpha and beta.

   The Beta function has two shaping parameters, alpha and beta.
   Setting these parameters to 1.5 and 1.5 yields a normal-like
   distribution, but without tails. If alpha and beta are equal to
   1 it is a uniform distribution.

   If alpha and beta are less than 1, use a rejection algorithm;
   Otherwise use the fact that if x is distributed Gamma(alpha) and y
   Gamma(beta) then x/(x+y) is Beta(alpha, beta).

   The rejection algorithm first a Beta variate is found over the
   interval [0, 1] with not the most efficient algorithm.  This is then
   scaled at the end to desired range.

   It may be tempting to re-use the second number drawn as the first
   random number of the next iteration, and simply draw one more.
   *** Don't do it.  You will produce an incorrect distribution.  You
   must draw two new numbers for the rejection sampling to be correct.

   References:
   - Ripley, Stochastic Simulations, John Wiley and Sons, 1987, p 90.
   - J.H.Maindonald, Statistical Computation, John Wiley and Sons,
     1984, p 370.
*/
double BetaRandom (double alpha, double beta, double a, double b)
{
  double u1, u2, w;

  if (b <= a || alpha <= 0 || beta <= 0) {
    printf ("Error: bad shape or range for a beta variate - Exiting\n\n");
    exit (0);
  }

  if ((alpha < 1) && (beta < 1))
    /* use rejection */
    do {
      u1 = Randoms(); /* Draw two numbers */
      u2 = Randoms();

      u1 = pow(u1, 1/alpha); /* alpha and beta are > 0 */
      u2 = pow(u2, 1/beta);

      w = u1 + u2;

    } while (w > 1.0);

  else {
    /* use relation to Gamma */
    u1 = GammaRandom(alpha);
    u2 = GammaRandom(beta);
    w  = u1 + u2;
  }

  return (a + (u1/w) * (b - a)); /* Scale to interval [a, b] */

} /* BetaRandom */


/* -----------------------------------------------------------------------------
   Utility functions
*/

/* -----------------------------------------------------------------------------
   erfc

   the error function of z.

   Adapted from the algorithm described in the book Numerical Recipes by
   Press et al. 

*/

double erfc (double x)
{
  double dAbsX, t, dVal;

  dAbsX = fabs(x);
  t = 1 / (1 + 0.5 * dAbsX);
  dVal = t * exp(-dAbsX*dAbsX - 1.26551223 + t*(1.00002368 + t*(0.37409196 +
         t*(0.09678418 + t*(-0.18628806 + t*(0.27886807 + t*(-1.13520398 +
         t*(1.48851587 + t*(-0.82215223 + t*(0.17087277))))))))));
  return ( x >= 0 ? dVal : 2 - dVal );
}


/* -----------------------------------------------------------------------------
   CDFNormal

   the probability for [-inf;Z] under the normal distribution
*/

double CDFNormal (double z)
{
  return ( 0.5 * (2 - erfc(z/sqrt(2))) );
}


/* -----------------------------------------------------------------------------
   lnDFNormal
   the log of the normal density function
   0.9189385332046 is log(sqrt(2*PI))
*/

double lnDFNormal (double x, double mu, double sd)
{
  if (sd <= 0.0) {
    printf ("Error: negative or null SD in lnDFNormal\n");
    exit (0);
  }

  return ( -0.9189385332046 - log (sd) - 0.5 * pow ((mu - x)/sd, 2) );
}


/* -----------------------------------------------------------------------------
   lnDFBeta
   the log of the beta density function
   FB 08/07/97
*/

double lnDFBeta (double x, double alpha, double beta, double min, double max)
{
  if (max <= min) {
    printf ("Error: bad range for beta variate in lnDFBeta\n");
    exit (0);
  }
  if (alpha <= 0) {
    printf ("Error: bad alpha for beta variate in LnDensity\n");
    exit (0);
  }
  if (beta <= 0) {
    printf ("Error: bad beta for beta variate in LnDensity\n");
    exit (0);
  }

  x = (x - min) / (max - min); 
  return (alpha - 1) * log (x) + (beta - 1) * log (1 - x) +
         lnGamma (alpha + beta) - lnGamma (alpha) - lnGamma(beta) - 
         log (max - min);
}


/* -----------------------------------------------------------------------------
   lnGamma

   A function to return the natural log of the Gamma function of x.
   Adapted from the algorithm described in the book Numerical Recipes by
   Press et al. 
   It can be used to compute factorials since ln(n!) = lnGamma(n + 1)
*/

double lnGamma (double x)
{
  double dSeries, dTemp;

  if (x <= 0.0) {
    printf ("Error: negative or null parameter for lnGamma function\n");
    exit (0);
  }
  
  dSeries = 1.000000000190015 +
            76.18009172947146   /  x      -
            86.50532032141677   / (x + 1) +
            24.01409824083091   / (x + 2) -
            1.231739572450155   / (x + 3) +
            1.20865097386617E-3 / (x + 4) -
            5.39523938495E-6    / (x + 5);

  dTemp = x + 4.5;
  dTemp = -dTemp + (x - 0.5) * log (dTemp) + log (2.50662827465 * dSeries);
  return dTemp;

} /* lnGamma */


/* -----------------------------------------------------------------------------
   InterpolateX

   Do a linear interpolation to return x
*/

double InterpolateX (double rgX[], double rgY[], long lLower, double dY)
{
  return rgX[lLower] + (dY - rgY[lLower]) *
         (rgX[lLower + 1] - rgX[lLower]) /
         (rgY[lLower + 1] - rgY[lLower]);

} /* InterpolateX */


/* -----------------------------------------------------------------------------
   CalcCumulative

   Approximates to an iOrder the cumulative distribution rg_Cdf
   given a sampling grid (rg_x) of dimension cDim, and the
   sampled pdf (rg_pdf) points.

   Supports piecewise-constant (order 0) and piecewise-linear
   (order 1) pdfs.
*/

void CalcCumulative (long cDim, double *rg_x, double *rg_pdf,
                     double *rg_Cdf, int  iOrder)
{
  long i;                /* Index for the samples */

  if (iOrder > 1) {
    printf ("CalcCumulative: Order %d not supported"
                     "-> using piecewise-linear\n", iOrder);
    iOrder = 1;
  }

  rg_Cdf[0] = 0.0;  /* Cumulative starts at 0.0 */
  switch (iOrder) {

  /* Piecewise Constant: sum of rectangles */
  case 0:
    for (i = 1; i < cDim; i++)
      rg_Cdf[i] = rg_Cdf[i-1] + rg_pdf[i]*(rg_x[i] - rg_x[i-1]);
    break;

  /* Piecewise Linear: sum of trapezoids */
  case 1:
    for (i = 1; i < cDim; i++)
      rg_Cdf[i] = rg_Cdf[i-1] + ((rg_x[i] - rg_x[i - 1]) *
                  (rg_pdf[i] + rg_pdf[i - 1]) / 2);
    break;

  default:
    assert (0); /* This is an error condition */
    break;

  } /* switch */

} /* CalcCumulative */


/* -----------------------------------------------------------------------------
   PiecewiseVariate

   Returns a variate drawn by tabulated inversion from the
   cumulative, Cdf[], calculated to order iOrder
   (0 = piecewise-constant, etc.)

   Inputs: dimension of the table, x values, cdf values at x,
   pdf values at x, order of the interpolation.

   Returns the sampled variate as its value. If a pointer is given,
   the value of pdf[] at the sampled variate is returned in *pVal_pdf.

   Note: For piecewise-constant variates, the grid is corrected with
         CorrectPWConstantGrid() to center the intervals around
         sampled points.
*/

double PiecewiseVariate (long cDim, double rg_x[], double rg_pdf[],
                         double rg_Cdf[], int iOrder, double *pVal_pdf)
{
  double dPWVariate; /* the variate chosen */
  double dValPdf;    /* the value of the pdf at variate */
  double dUniform = UniformRandom(0, rg_Cdf[cDim - 1]);
  long   lUpper, lLower, lIndex;

  if (iOrder > 1) {
    printf ("CalcCumulative: Order %d not supported"
                     "-> using piecewise-linear\n", iOrder);
    iOrder = 1;
  }

  /* Find bounding Xs by a binary search of the ordered rg_x's
   */
  lUpper = cDim;
  lLower = 0;
  lIndex = 0;

  while (lUpper - lLower > 1) {
    lIndex = (lUpper + lLower)/2;

    if (dUniform > rg_Cdf[lIndex]) lLower = lIndex; /* Move to right half */
    else if (dUniform < rg_Cdf[lIndex]) lUpper = lIndex; /* Move to left half */
         else lUpper = lLower = lIndex;
  }

  /* If we are exactly on a cumulative data point (unlikely),
     the value of the pdf is known and the variate is a grid point.
   */
  if (lUpper == lLower) {
    dValPdf = rg_pdf[lLower];
    dPWVariate = rg_x[lLower];
  }

  /* Otherwise we do the appropriate interpolation
   */
  else
    switch (iOrder) {

    /* Piecewise-constant pdf: the Cdf is piecewise-linear
     */
    case 0:
      dValPdf = rg_pdf[lLower];
      dPWVariate = InterpolateX (rg_x, rg_Cdf, lLower, dUniform);
      break;

    /* Piecewise-linear pdf: the Cdf is piecewise-quadratic
     */
    case 1: {

      if (rg_pdf[lLower] == rg_pdf[lUpper]) { /* A linear segment */
        dValPdf = rg_pdf[lLower];
        dPWVariate = InterpolateX (rg_x, rg_Cdf, lLower, dUniform);
      }

      else { /* Interpolate a quadratic */

        double a, b, c, dRadical;

        /* Find a, b, and c from the quadratic equation.
           a is guaranteed not zero from if() above. */

        a = (rg_pdf[lUpper] - rg_pdf[lLower]) /(rg_x[lUpper] - rg_x[lLower]);

        b = rg_pdf[lLower] - a*rg_x[lLower];

        c = rg_Cdf[lLower] - (a*rg_x[lLower]/2.0 + b) * rg_x[lLower];

        dRadical = sqrt(b*b - 2*a*(c - dUniform));

        dPWVariate = (-b + dRadical) / a;

        assert (dPWVariate >= rg_x[lLower] && dPWVariate <= rg_x[lUpper]);

        dValPdf = a * dPWVariate + b;

        if (a > 0)
          assert (dValPdf >= rg_pdf[lLower] && dValPdf <= rg_pdf[lUpper]);
        else
          assert (dValPdf <= rg_pdf[lLower] && dValPdf >= rg_pdf[lUpper]);

      } /* else */

    } /* case block */
    break;

    default:
      dValPdf = 0;
      dPWVariate = 0;
      assert(0);
      break;

  } /* switch */

  if (pVal_pdf)    *pVal_pdf = dValPdf; /* Return the value if requested */

  return dPWVariate;

} /* PiecewiseVariate */


/* -----------------------------------------------------------------------------
   PiecewiseRandom
                                                  __
   returns a variate that is distributed along a /  \ shaped distribution.
*/

double PiecewiseRandom (double min, double a, double b, double max)
{
  double dTemp;
  static double Grille[4];
  static double densite[4];
  static double densiteCum[4];
  double nvlle_densite;

  Grille[0] = min;
  Grille[1] = a;
  Grille[2] = b;
  Grille[3] = max;
  densite[0] = 0;
  densite[1] = 1/(max/2+b/2-a/2-min/2);
  densite[2] = 1/(max/2+b/2-a/2-min/2);
  densite[3] = 0;

  CalcCumulative (4, Grille, densite, densiteCum, 1);

  dTemp = PiecewiseVariate (4, Grille, densite, densiteCum, 1,
                            &nvlle_densite);
  return (dTemp);

} /* PiecewiseRandom */


/* End of random module */
