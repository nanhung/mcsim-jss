/* model.c
   ___________________________________________________

   Model File:  linear.model

   Date:  Fri Nov 14 08:12:29 1997

   Created by:  "mod" v4.0
    -- a model preprocessor by Don Maszle
   ___________________________________________________

   Copyright (c) 1993-96.  Don Maszle, Frederic Bois.  All rights reserved.

  Model calculations for compartmental model:

    0 States:

    1 Outputs:
      y 	-> 0.0

    0 Inputs:

    4 Parameters:
      A 	:= 0
      B 	:= 1
      SD_true 	:= 0
      SD_esti 	:= 0
*/


#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <float.h>
#include "modelu.h"
#include "random.h"

/*----- Indices to Global Variables */

/* Model variables: States and other outputs */
#define ID_y	0x0000

/* Inputs */

/* Parameters */
#define ID_A	0x0001
#define ID_B	0x0002
#define ID_SD_true	0x0003
#define ID_SD_esti	0x0004

/*----- Global Variables */

  /* For export.  Keep track of who we are. */
char szModelDescFilename[] = "linear.model";
char szModelSourceFilename[] = __FILE__;
char szModelGenAndVersion[] = "mod-v4.0";

  /* Externs */
extern BOOL vbModelReinitd;

  /* Model Dimensions */
int vnStates	= 0;
int vnOutputs	= 1;
int vnModelVars	= 1;
int vnInputs	= 0;
int vnParms	= 4;

  /* States and Outputs*/
double vrgModelVars[1];

  /* Inputs */
IFN vrgInputs[1];

/* Parameters */
  double A;
  double B;
  double SD_true;
  double SD_esti;


/*----- Global Variable Map */
VMMAPSTRCT vrgvmGlo[] = {

    {"y",	(PVOID) &vrgModelVars[ID_y], 	ID_OUTPUT | ID_y},


    {"A",	(PVOID) &A, 	ID_PARM | ID_A},
    {"B",	(PVOID) &B, 	ID_PARM | ID_B},
    {"SD_true",	(PVOID) &SD_true, 	ID_PARM | ID_SD_true},
    {"SD_esti",	(PVOID) &SD_esti, 	ID_PARM | ID_SD_esti},
    {"",	NULL,	0} /* End flag */
};  /* vrgpvmGlo[] */

/* InitModel

   Should be called to initialize model variables at
   the beginning of experiment before reading
   variants from the simulation spec file.
*/

void InitModel()
{
/*-- Initialize things in the order that they appear in
     model definition file so that dependencies are
     handled correctly. */

  vrgModelVars[ID_y] = 0.0;
  A = 0;
  B = 1;
  SD_true = 0;
  SD_esti = 0;

  vbModelReinitd = TRUE;  /* Flag is initialized */
} /* InitModel */

/* Dynamics section */
void CalcDeriv (double  rgModelVars[],
   double  rgDerivs[],
   PDOUBLE pdTime)
{
  /* Local variables */

  CalcInputs (pdTime);    /* Get new input vals */
} /* CalcDeriv */
/* Model scaling */

void ScaleModel (void)
{
  /* Local variables */

} /* ScaleModel */
/* Jacobian calculation */


void CalcJacob (double rgModelVars[], double *rgdSave[], PDOUBLE pdTime)
{
  /* Local variables */

} /* CalcJacob */
/* Outputs calculation */

void CalcOutputs (double  rgModelVars[],
      double  rgDerivs[],
      PDOUBLE pdTime)
{
  /* Local variables */


  rgModelVars[ID_y] = A + B * (*pdTime) + NormalRandom ( 0 , SD_true ) ;
} /* CalcOutputs */
