/* mod.c

   written by Don Maszle
   15 September 1991
   
   Copyright (c) 1993.  Don Maszle, Frederic Bois.  All rights reserved.

   -- Revisions -----
     Logfile:  %F%
    Revision:  %I%
        Date:  %G%
     Modtime:  %U%
      Author:  @a
   -- SCCS  ---------

   Entry point for model code generator.

   Calls ReadModel() to define the model and WriteModel() to create
   the output file.
   
   The INPUTINFO struct is defined as follows:
     
     wContext	:	A flag for the current context of the input. 

     pvmGloVars :       Vars of type ID_LOCAL* are accessible
                        only within the respective section.  If
		        states are given a value outside of the
			Dynamics section, it is used as an INITIAL
			value, otherwise they are initialized to 0.0. 

     pvmDynEqns:	List of equations to go into the CalcDeriv(),
     pvmJacobEqns:	Jacobian(), and ScaleModel() routines, respectively.
     pvmScaleEqns:	The LHS of equations of state variables in Dynamics
			are actually dt().  The hType field gives the type of
			the LHS and also a flag for space in the uppermost
			bit.
     pvmCalcOutEqns:    List of equations to into CalcOutputs().  These
                        can be used to scale output variables *only*.
                        The routinine is called just be outputting
                        values specified in Print().
*/

#ifdef __LOCAL_HDR__
#include "stdlib.h"
#include "stdio.h"
#include "string.h"

#else
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#endif

#include "mod.h"


/***********************************************************************
   main -- Entry point for the PBPK simulation model preprocessor
   
*/

int main (int nArg, PSTR rgszArg[])
{
  INPUTINFO info;

  fprintf (stderr, "\nModel Generator Program " VSZ_VERSION "\n");
  fprintf (stderr, VSZ_COPYRIGHT "\n\n");

#ifdef _MACOS_
  /* for the Macintosh there are no command line parameters 
     so we input them at the console
   */
  rgszArg[1] = (PSTR) calloc (1, 80);
  printf ("Model definition file? ");
  scanf ("%[^:\f\r\v\n]", rgszArg[1]);
  nArg = 2;
#endif

  if (nArg == 2 || nArg == 3 ) {
    ReadModel (&info, rgszArg[1]);
    WriteModel (&info, nArg, rgszArg);    
  }
  else {
    printf ("Usage: %s  [input-file  [output-file]]\n", rgszArg[0]);
    printf ("  Creates file 'model.c' according to input-file specs\n");
  }

  return 0;
  
} /* main */
