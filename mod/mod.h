/* mod.h

   written by Don Maszle
   26 September 1991
   
   Copyright (c) 1993.  Don Maszle, Frederic Bois.  All rights reserved.

   -- Revisions -----
     Logfile:  %F%
    Revision:  %I%
        Date:  %G%
     Modtime:  %U%
      Author:  @a
   -- SCCS  ---------
*/

#ifndef _MOD_INCLUDED

/* ----- Inclusions  */

#include "lex.h"
#include "hungtype.h"

/* ----- Constants  */

/* Version and copyright */
#define VSZ_VERSION "v4.0"
#define VSZ_COPYRIGHT "Copyright (c) 1993-96.  Don Maszle, Frederic Bois.  All rights reserved."

/* The time variable accessible to the user */
#define VSZ_TIME "t"


/* Keyword Map Constants */

#define KM_NULL        0
#define KM_STATES      1
#define KM_INPUTS      2
#define KM_OUTPUTS     3
#define KM_DYNAMICS    4
#define KM_SCALE       5
#define KM_JACOB       6
#define KM_CALCOUTPUTS 7
#define KM_DXDT        20
#define KM_END         100

/* Context Types bit flags */
/*
   These are used to keep track of what context the parser is in,
   i.e. defining global vars or defining dynamics.
   
   They are also used as bit flags in the keyword list to indicate
   a valid context for keywords, var names.
*/

#define CN_GLOBAL       0x0001
#define CN_DYNAMICS     0x0002
#define CN_SCALE        0x0003
#define CN_JACOB        0x0004
#define CN_CALCOUTPUTS  0x0005
#define CN_INPUTDEF     0x0100
#define CN_END          0x4000

#define CN_ALL          0xFFFF  /* All Contexts */


/* Identifier Types -- Stored as upper nybble so that the variable
   can be indexed in modo.c to create a handle to the variable
*/

#define ID_TYPEMASK     0xF000 /* Allow up to 15 variable types */
#define ID_SPACEFLAG    0x0800 /* To flag for formatting eqns */
#define ID_INDEXMASK    0x07FF /* Index for symbol table */
#define MAX_VARS        (ID_SPACEFLAG) /* Max number of vars of a type */

#define ID_NULL         0x0000
#define ID_STATE        0x1000 /* Model state variables -- dynamics */
#define ID_INPUT        0x2000 /* Model input -- type IFN */
#define ID_OUTPUT       0x3000 /* Model output -- for observation only */
#define ID_PARM         0x4000 /* Global parameters */
#define ID_LOCALDYN     0x5000 /* Local variables in Dynamics */
#define ID_LOCALSCALE   0x6000 /* Local variables in Scale    */
#define ID_LOCALJACOB   0x7000 /* Local variables in Jacobian */
#define ID_LOCALCALCOUT 0x8000 /* Local variables in CalcOutputs */
#define ID_DERIV        0x9000 /* Derivative eqn in CalcDeriv */

/* ----- Enumerations */

/* ----- Typedefs */


/* The VMMAPSTRCT, Variable Map structure, used for maintaining a
   list of variables and of dynamics equations. */

typedef HANDLE HVAR;

typedef struct tagVM {
  PSTR szName;    /* Identifier */
  PSTR szEqn;     /* Def'ing eqn to be created and copied */
  HANDLE hType;   /* ID_type of identifier */

  struct tagVM *pvmNextVar; /* Var list is a stack */
} VMMAPSTRCT, *PVMMAPSTRCT; /* Variable Map */


typedef struct tagINPUTINFO {
  WORD wContext;
  PSTR szInputFilename;
  
  PVMMAPSTRCT  pvmGloVars;
  PVMMAPSTRCT  pvmDynEqns;
  PVMMAPSTRCT  pvmScaleEqns;
  PVMMAPSTRCT  pvmJacobEqns;
  PVMMAPSTRCT  pvmCalcOutEqns;

} INPUTINFO, *PINPUTINFO; /* tagINPUTINFO */
 

/* ----- Macros */

#define TYPE(pvm) (pvm ? (pvm)->hType & ID_TYPEMASK : ID_NULL)
#define INDEX(pvm) (pvm ? (pvm)->hType & ID_INDEXMASK : ID_NULL)

#define KM_TO_CN(kmCode) ((kmCode) == KM_CALCOUTPUTS ? CN_CALCOUTPUTS \
     : (kmCode) == KM_JACOB  ? CN_JACOB \
     : (kmCode) == KM_SCALE ? CN_SCALE \
     : (kmCode) == KM_DYNAMICS ? CN_DYNAMICS \
     : 0)


/* ----- Globals/Externals */


/* ----- Prototypes */

void DeclareModelVar (PINPUTBUF, PSTR, int);
void DefineVariable (PINPUTBUF, PSTR, PSTR, int);
int  GetKeywordCode (PSTR, PINT);
int  GetVarType (PVMMAPSTRCT, PSTR);
PVMMAPSTRCT  GetVarPTR (PVMMAPSTRCT, PSTR);
void ReadModel (PINPUTINFO, PSTR);
void WriteModel (PINPUTINFO, int, PSTR *);

#define _MOD_INCLUDED
#endif
