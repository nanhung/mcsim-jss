/* ----------------------------------------------------------------------------
   modo.c

   written by Don Maszle
   21 September 1991
   
   Copyright (c) 1993.  Don Maszle, Frederic Bois.  All rights reserved.

   -- Revisions -----
     Logfile:  %F%
    Revision:  %I%
        Date:  %G%
     Modtime:  %U%
      Author:  @a
   -- SCCS  ---------

   Output the model.c file.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <time.h>

#include "mod.h"
#include "lexfn.h"
#include "lexerr.h"


/* Extra Constants, typedefs, prototypes */

#define ALL_VARS (0)

#ifndef FILENAME_MAX
#define FILENAME_MAX 40
#endif

#define WriteIndexName(pfile, pvm)  (fprintf ((pfile), "ID_%s", (pvm)->szName))

typedef int (*PFI_CALLBACK) (PFILE, PVMMAPSTRCT, PVOID);

static char *vszModelFilename = NULL;
static char *vszModGenName = NULL;
  
static char vszModelArrayName[] = "vrgModelVars";
static char vszInputArrayName[] = "vrgInputs";
extern char vszHasInitializer[]; /* decl'd in modd.c */

PVMMAPSTRCT vpvmGloVarList;

char *vszIFNTypes[] = { /* Must match defines in lexfn.h */
  "IFN_NULL /* ?? */",
  "IFN_CONSTANT",
  "IFN_PERDOSE",
  "IFN_PERRATE",
  "IFN_PEREXP",
  "IFN_NDOSES"
}; /* vszIFNTypes[] = */


/* Global Variables */

int vnStates, vnOutputs, vnInputs, vnParms, vnModelVars;

/* ----------------------------------------------------------------------------
   ForAllVar
   
   Takes a pfile, a pvm list head and a callback function which is called
   for all variables in the list if hType == ALL_VARS or only for
   only those that match hType otherwise.
*/
int ForAllVar (PFILE pfile, PVMMAPSTRCT pvm, PFI_CALLBACK pfiFunc,
               HANDLE hType, PVOID pinfo)
{
  int iTotal = 0;
  
  while (pvm) {
    if (hType == ALL_VARS          /* Do for all ...*/
        || TYPE(pvm) == hType)     /* ..or do only for vars of hType */
      if (pfiFunc)
        iTotal += (*pfiFunc) (pfile, pvm, pinfo);
      else
        iTotal++; /* No func! just count */

    pvm = pvm->pvmNextVar;
  } /* while */

  return (iTotal);

} /* ForAllVar */


/* ----------------------------------------------------------------------------
   CountOneDecl
   
   Counts declarations for variables.  Note that each variable may only
   be declared and assigned *once*.  This is a problem and should be fixed.

   Callback for ForAllVar().
*/
int CountOneDecl (PFILE pfile, PVMMAPSTRCT pvm, PVOID pInfo)
{
  if (pvm->szEqn != vszHasInitializer) { /* These appear later */
    return (1);
  }
  return (0);

} /* CountOneDecl */


/* ----------------------------------------------------------------------------
   WriteOneName
   
   Writes a name and nominal value for the given variable of pvm.

   Callback for ForAllVar().
*/
int WriteOneName (PFILE pfile, PVMMAPSTRCT pvm, PVOID pInfo)
{
  if (pvm->szEqn != vszHasInitializer) { /* These appear later */
    fprintf (pfile, "      %s", pvm->szName);
    if (TYPE(pvm) != ID_INPUT)
      fprintf (pfile, " \t%s %s\n", (pvm->szEqn ? ":=" : "->"),
               (pvm->szEqn ? pvm->szEqn : "0.0"));
    else
      fprintf (pfile, " is an input function\n");

    return (1);
  } /* if */

  return (0);

} /* WriteOneName */

 
/* ----------------------------------------------------------------------------
   WriteHeader
   
   That it does do dashingly.  Includes a nice comment at the
   beginning (hopefully!) of the file replete with filename,
   timestamp, and a list of model variables. 
*/
void WriteHeader (PFILE pfile, PSTR szName, PVMMAPSTRCT pvmGlo)
{
  struct tm *ptm; /* Local time struct */
  time_t ttTime;
  
  time (&ttTime);
  ptm = localtime (&ttTime);

  fprintf (pfile, "/* %s\n", szName);
  fprintf(pfile, "   ___________________________________________________\n\n");
  fprintf (pfile, "   Model File:  %s\n\n", vszModelFilename);
  fprintf (pfile, "   Date:  %s\n", ctime(&ttTime));
  fprintf (pfile, "   Created by:  \"%s-%s\"\n", vszModGenName, VSZ_VERSION);
  fprintf (pfile, "    -- a model preprocessor by Don Maszle\n");
  fprintf(pfile, "   ___________________________________________________\n\n");

  fprintf (pfile, "   " VSZ_COPYRIGHT "\n");

  fprintf (pfile, "\n  Model calculations for compartmental model:\n\n");

  fprintf (pfile, "    %d States:\n", vnStates);
  ForAllVar (pfile, pvmGlo, &WriteOneName, ID_STATE, NULL);

  fprintf (pfile, "\n    %d Outputs:\n", vnOutputs);
  ForAllVar (pfile, pvmGlo, &WriteOneName, ID_OUTPUT, NULL);
    
  fprintf (pfile, "\n    %d Inputs:\n", vnInputs);
  ForAllVar (pfile, pvmGlo, &WriteOneName, ID_INPUT, NULL);

  fprintf (pfile, "\n    %d Parameters:\n", vnParms);
  ForAllVar (pfile, pvmGlo, &WriteOneName, ID_PARM, NULL);
  
  fprintf (pfile, "*/\n\n");
  
} /* WriteHeader */


/* ----------------------------------------------------------------------------
*/
void WriteIncludes (PFILE pfile)
{
  fprintf (pfile, "\n#include <stdlib.h>\n");
  fprintf (pfile, "#include <stdio.h>\n");
  fprintf (pfile, "#include <math.h>\n");
  fprintf (pfile, "#include <string.h>\n");
  fprintf (pfile, "#include <float.h>\n");
  
  fprintf (pfile, "#include \"modelu.h\"\n");
  fprintf (pfile, "#include \"random.h\"\n");

} /* WriteIncludes */


/* ----------------------------------------------------------------------------
   WriteOneDecl
  
   Write one global or local declaration.  Callback for ForAllVar().
*/
int WriteOneDecl (PFILE pfile, PVMMAPSTRCT pvm, PVOID pInfo)
{
  assert(TYPE(pvm) != ID_INPUT);
  assert(TYPE(pvm) != ID_OUTPUT);
  assert(TYPE(pvm) != ID_STATE);

  if (TYPE(pvm) & (ID_LOCALDYN | ID_LOCALSCALE | ID_LOCALJACOB))
    fprintf (pfile, "  ");

  fprintf (pfile, "double %s;\n", pvm->szName);
  return (1);

} /* WriteOneDecl */


/* ----------------------------------------------------------------------------
*/
int WriteOneIndexDefine (PFILE pfile, PVMMAPSTRCT pvm, PVOID pInfo)
{
  if (pvm->szEqn != vszHasInitializer) { /* These appear later */
    fprintf (pfile, "#define ");
    WriteIndexName(pfile, pvm);
    if (INDEX(pvm))
      fprintf (pfile, "\t0x%04x\n", INDEX(pvm));
    else
      fprintf (pfile, "\t0x0000\n");

    return 1;
  }  /* if */

  return 0;

} /* WriteOneIndexDefine */


/* ----------------------------------------------------------------------------
*/
void WriteDecls (PFILE pfile, PVMMAPSTRCT pvmGlo)
{
  fprintf (pfile, "\n/*----- Indices to Global Variables */\n");

  fprintf (pfile, "\n/* Model variables: States and other outputs */\n");
  ForAllVar (pfile, pvmGlo, &WriteOneIndexDefine, ID_STATE, NULL);
  ForAllVar (pfile, pvmGlo, &WriteOneIndexDefine, ID_OUTPUT, NULL);

  fprintf (pfile, "\n/* Inputs */\n");
  ForAllVar (pfile, pvmGlo, &WriteOneIndexDefine, ID_INPUT, NULL);
  fprintf (pfile, "\n/* Parameters */\n");
  ForAllVar (pfile, pvmGlo, &WriteOneIndexDefine, ID_PARM, NULL);
  
  fprintf (pfile, "\n/*----- Global Variables */\n");

  fprintf (pfile, "\n  /* For export.  Keep track of who we are. */\n");
  fprintf (pfile, "char szModelDescFilename[] = \"%s\";\n", vszModelFilename);
  fprintf (pfile, "char szModelSourceFilename[] = __FILE__;\n");
  fprintf (pfile, "char szModelGenAndVersion[] = \"%s-%s\";\n",
    vszModGenName, VSZ_VERSION);

  fprintf (pfile, "\n  /* Externs */\n");
  fprintf (pfile, "extern BOOL vbModelReinitd;\n");

  fprintf (pfile, "\n  /* Model Dimensions */\n");
  fprintf (pfile, "int vnStates\t= %d;\n", vnStates);
  fprintf (pfile, "int vnOutputs\t= %d;\n", vnOutputs);
  fprintf (pfile, "int vnModelVars\t= %d;\n", vnModelVars);
  fprintf (pfile, "int vnInputs\t= %d;\n", vnInputs);
  fprintf (pfile, "int vnParms\t= %d;\n", vnParms);

  fprintf (pfile, "\n  /* States and Outputs*/\n");
  fprintf (pfile, "double %s[%d];\n", vszModelArrayName, vnModelVars);

  fprintf (pfile, "\n  /* Inputs */\n");
  /* if vnInputs is zero put a dummy 1 for array size */
  fprintf (pfile, "IFN %s[%d];\n", vszInputArrayName, 
           (vnInputs > 0 ? vnInputs : 1));

  fprintf (pfile, "\n/* Parameters */\n");
  ForAllVar (pfile, pvmGlo, &WriteOneDecl, ID_PARM, NULL);

} /* WriteDecls */


/* ----------------------------------------------------------------------------
   GetName 
   
   returns a string to the name of the pvm variable given.  The
   string is static and must be used immediately or copied.  It will
   be changed on the next call of this function.
   
   szModelVarName and szDerivName are names to be used for
   state variables and derivatives arrays, resp.

   The name is determined by hType if hType is non-NULL.  If hType is
   NULL, then the type is taken to be the hType field of pvm.
*/
PSTR GetName (PVMMAPSTRCT pvm, PSTR szModelVarName, PSTR szDerivName, HANDLE hType)
{
static PSTRLEX vszVarName;
  HANDLE hTypeToUse = (hType ? hType : TYPE(pvm));

  switch (hTypeToUse) {

  case ID_INPUT:
    sprintf (vszVarName, "vrgInputs[ID_%s]", pvm->szName);
    break;

  case ID_STATE:
  case ID_OUTPUT:
    if (szModelVarName)
      sprintf (vszVarName, "%s[ID_%s]", szModelVarName, pvm->szName);
    else
      sprintf (vszVarName, "vrgModelVars[ID_%s]", pvm->szName);
    break;

  case ID_DERIV:
    assert (szDerivName);
    sprintf (vszVarName, "%s[ID_%s]", szDerivName, pvm->szName);
    break;

  default:    /* Parms and local variables */
    sprintf (vszVarName, "%s", pvm->szName);
    break;
  }  /* switch */

  return (vszVarName);

} /* GetName */


/* ----------------------------------------------------------------------------
   WriteOneVMEntry

   prints a single entry for the variable map symbol table.

   Callback for ForAllList.
*/
int WriteOneVMEntry (PFILE pfile, PVMMAPSTRCT pvm, PVOID pInfo)
{
  int iType = TYPE(pvm);

  if (!pvm) {
    fprintf (pfile, "    {\"\",\tNULL,\t0} /* End flag */\n");
    return 0;
  }
    
  assert(iType && \
    iType != ID_LOCALDYN && iType != ID_LOCALSCALE && iType != ID_LOCALJACOB);

  if (pvm->szEqn != vszHasInitializer) { /* These appear later */
    fprintf (pfile, "    {\"%s\",\t(PVOID) &%s", pvm->szName,
             GetName(pvm, vszModelArrayName, NULL, ID_NULL));
    
    fprintf (pfile, ", \tID_%s | ID_%s},\n", 
             (iType == ID_PARM ? "PARM"
              : (iType == ID_INPUT ? "INPUT"
              : (iType == ID_OUTPUT ? "OUTPUT"
              : "STATE"))), pvm->szName);

    return (1);

  }  /* if */

  return 0;

} /* WriteOneVMEntry */


/* ----------------------------------------------------------------------------
*/
void WriteVarMap (PFILE pfile, PVMMAPSTRCT pvmGlo)
{
  fprintf (pfile, "\n\n/*----- Global Variable Map */\n");
  fprintf (pfile, "VMMAPSTRCT vrgvmGlo[] = {\n");
  ForAllVar (pfile, pvmGlo, &WriteOneVMEntry, ID_STATE, NULL);  
  fprintf (pfile, "\n");

  ForAllVar (pfile, pvmGlo, &WriteOneVMEntry, ID_OUTPUT, NULL);
  fprintf (pfile, "\n");

  ForAllVar (pfile, pvmGlo, &WriteOneVMEntry, ID_INPUT, NULL);
  fprintf (pfile, "\n");

  ForAllVar (pfile, pvmGlo, &WriteOneVMEntry, ID_PARM, NULL);
  WriteOneVMEntry (pfile, NULL, NULL); /* Include end flag */
  fprintf (pfile, "};  /* vrgpvmGlo[] */\n");

}  /* WriteVarMap */


/* ----------------------------------------------------------------------------
*/
int WriteOneInit (PFILE pfile, PVMMAPSTRCT pvm, PVOID pInfo)
{
  IFN ifnNull = {IFN_CONSTANT}; /* Init other fields to zero */
  PSTR szVarName = GetName (pvm, NULL, NULL, ID_NULL);

  if (pvm->szEqn != vszHasInitializer /* These appear later */
      && TYPE(pvm) <= ID_PARM) { /* No Locals!  */
    
    if (TYPE(pvm) == ID_INPUT) {
      PIFN pifn = (PIFN) pvm->szEqn;
      
      if (!pifn) /* No eqn, init to zero */
        pifn = &ifnNull;
      
      fprintf (pfile, "\n  %s.iType = %s;\n",
               szVarName, vszIFNTypes[pifn->iType]);
      fprintf (pfile, "  %s.dTStartPeriod = 0.0;\n", szVarName);
      fprintf (pfile, "  %s.bOn  = FALSE;\n", szVarName);
      fprintf (pfile, "  %s.dMag = %f;\n", szVarName, pifn->dMag);
      fprintf (pfile, "  %s.dT0  = %f;\n", szVarName, pifn->dT0);
      fprintf (pfile, "  %s.dTexp = %f;\n", szVarName, pifn->dTexp);
      fprintf (pfile, "  %s.dDecay  = %f;\n", szVarName, pifn->dDecay);
      fprintf (pfile, "  %s.dTper = %f;\n", szVarName, pifn->dTper);
      
      fprintf (pfile, "  %s.hMag = %#x;\n", szVarName, pifn->hMag);
      fprintf (pfile, "  %s.hT0  = %#x;\n", szVarName, pifn->hT0);
      fprintf (pfile, "  %s.hTexp = %#x;\n", szVarName, pifn->hTexp);
      fprintf (pfile, "  %s.hDecay  = %#x;\n", szVarName, pifn->hDecay);
      fprintf (pfile, "  %s.hTper = %#x;\n", szVarName, pifn->hTper);
      
      fprintf (pfile, "  %s.dVal  = 0.0;\n", szVarName);
      fprintf (pfile, "  %s.nDoses = 0;\n", szVarName);
    } /* if */
    else 
      fprintf (pfile, "  %s = %s;\n", szVarName,
               (pvm->szEqn ? pvm->szEqn : "0.0"));

    return (1);
  } /* if */

  return (0);

} /* WriteOneInit */


/* ----------------------------------------------------------------------------
   WriteInitModel
   
   Writes the routine to initialize the model variables.
*/
void WriteInitModel (PFILE pfile, PVMMAPSTRCT pvmGlo)
{
  fprintf (pfile, "\n/* InitModel\n\n");
  fprintf (pfile, "   Should be called to initialize model variables at\n");
  fprintf (pfile, "   the beginning of experiment before reading\n");
  fprintf (pfile, "   variants from the simulation spec file.\n*/\n\n");
  fprintf (pfile, "void InitModel()\n{\n");
  fprintf (pfile, "/*-- Initialize things in the order that they appear in\n"
      "     model definition file so that dependencies are\n"
      "     handled correctly. */\n\n");
  ForAllVar (pfile, pvmGlo, &WriteOneInit, ALL_VARS, NULL);
  fprintf (pfile, "\n  vbModelReinitd = TRUE;  /* Flag is initialized */\n");
  fprintf (pfile, "} /* InitModel */\n\n");

} /* WriteInitModel */


#ifdef ndef
/* ----------------------------------------------------------------------------
   WriteInitExpt
   
   Writes the routine to initialize certain model variables.
*/

void WriteInitExpt (PFILE pfile, PVMMAPSTRCT pvmGlo)
{
  fprintf (pfile, "\n/* InitExpt\n\n");
  fprintf (pfile, "   Should be called to initialize model variables at\n");
  fprintf (pfile, "   the beginning of experiment after reading\n");
  fprintf (pfile, "   variants from the simulation spec file.\n*/\n\n");
  fprintf (pfile, "void InitExpt()\n{\n");
  fprintf (pfile, "/*-- Initialize things in the order that they appear in\n"
      "     model definition file so that dependencies are\n"
      "     handled correctly. */\n\n");
  ForAllVar (pfile, pvmGlo, &WriteOneInit, ID_STATE, NULL);
  ForAllVar (pfile, pvmGlo, &WriteOneInit, ID_INPUT, NULL);
  ForAllVar (pfile, pvmGlo, &WriteOneInit, ID_OUTPUT, NULL);
  fprintf (pfile, "\n  vbModelReinitd = TRUE;  /* Flag is initialized */\n");
  fprintf (pfile, "} /* InitExpt */\n\n");
} /* WriteInitExpt */
#endif


/* ----------------------------------------------------------------------------
   TranslateID
   
   Writes an equation id if it declared and in a valid context.
   Errors are reported if necessary.
*/

void TranslateID (PINPUTBUF pibDum, PFILE pfile, PSTR szLex, int iEqType)
{
  int iKWCode, fContext;
    
  iKWCode = GetKeywordCode (szLex, &fContext);
  switch (iKWCode) {

  case KM_DXDT: {
    int iArg = LX_IDENTIFIER;
    PVMMAPSTRCT pvm = NULL;
    
    if (GetFuncArgs (pibDum, 1, &iArg, szLex)
        && (pvm = GetVarPTR(vpvmGloVarList, szLex))
        && TYPE(pvm) == ID_STATE)
      fprintf (pfile, "%s", GetName (pvm, NULL, "rgDerivs", ID_DERIV));
    else
      ReportError (pibDum, RE_BADSTATE | RE_FATAL, (pvm? szLex : NULL), NULL);
    } /* KM_DXDT: */
    break;

  case KM_NULL: {
    PVMMAPSTRCT pvm = GetVarPTR(vpvmGloVarList, szLex);

    /* Handle undeclared ids */
    if (!pvm) {
      if ((iEqType == KM_DYNAMICS || iEqType == KM_CALCOUTPUTS) && 
          !strcmp (szLex, VSZ_TIME))
        /* If this is the time variable, convert to the correct formal arg */
        fprintf(pfile, "(*pdTime)");
      else
        /* otherwise output id exactly as is */
        fprintf(pfile, "%s", szLex);
    } /* if */
 
    else {
      fprintf(pfile, "%s", GetName(pvm, "rgModelVars", NULL, ID_NULL));
      if (TYPE(pvm) == ID_INPUT) {
        if (iEqType == KM_SCALE)
          fprintf(pfile, ".dMag"); /* Can only scale with magnitude */
        else
          fprintf(pfile, ".dVal"); /* Otherwise use current value */
      } /* if */
    } /* else */

    } /* KM_NULL: */
    break;

  default: /* No other keywords allowed in equations */

    /* Allow for C keywords here, including math library functions */
    ReportError (pibDum, RE_BADCONTEXT | RE_FATAL, szLex, NULL);
    break;

  } /* switch */
  
} /* TranslateID */


/* ----------------------------------------------------------------------------
   TranslateEquation
   
   Writes an equation to the output file substituting model variable
   names, inputs names, derivative names, etc.
   
   Tries to do some hack formatting.
*/

void TranslateEquation (PFILE pfile, PSTR szEqn, int iEqType)
{
#define RMARGIN 65

  INPUTBUF ibDum;
  PINPUTBUF pibDum = &ibDum;
  PSTRLEX szLex;
  int iType;
  
  MakeStringBuffer (NULL, pibDum, szEqn);
  
  NextLex (pibDum, szLex, &iType);
  if (!iType) {
    fprintf (pfile, "0.0;  /* NULL EQN!?? */");
    return;
  } /* if */

  do {
    if (iType == LX_IDENTIFIER)   /* Process Identifier */
      TranslateID (pibDum, pfile, szLex, iEqType);
    
    else if ((iType == LX_EQNPUNCT || iType == LX_PUNCT) && IsComment(szLex)) {
      while (*pibDum->pbufCur && *pibDum->pbufCur != CH_EOLN)
        pibDum->pbufCur++;

      fprintf (pfile, "\n");
    } /* if */

    else /* Spew everything else */
      fprintf (pfile, "%s", szLex);

    fprintf (pfile, " ");
    NextLex (pibDum, szLex, &iType);

  } while (iType);

  fprintf (pfile, ";\n");
  
} /* TranslateEquation */


/* ----------------------------------------------------------------------------
   WriteOneEquation
   
   Writes one equation of a function definition.  Uses the hType
   flag for spacing set by DefineEquation().
   
   Callback function for ForAllVar().
*/
int WriteOneEquation (PFILE pfile, PVMMAPSTRCT pvm, PVOID pInfo)
{
  int iType = (int) pInfo;
  
  if (pvm->hType & ID_SPACEFLAG) /* Flag for space between equations */
    fprintf (pfile, "\n");

  switch (iType) {
    default:
    case KM_SCALE: /* Scale Global Names */

      /* Inputs not allowed anymore in Scale section - FB 7/12/96 */
      if (TYPE(pvm) == ID_INPUT) {
        printf ("Error: input '%s' used in Scale context.\n", 
                pvm->szName);
        exit(0);
      }

      fprintf (pfile, "  %s%s = ", GetName (pvm, NULL, NULL, ID_NULL),
        (TYPE(pvm) == ID_INPUT ? ".dMag" : "") );
      break;

    case KM_CALCOUTPUTS:
    case KM_DYNAMICS:
    case KM_JACOB:
      fprintf (pfile, "  %s = ", GetName (pvm, "rgModelVars", "rgDerivs", 
               ID_NULL));
      break;

  } /* switch */

  TranslateEquation (pfile, pvm->szEqn, iType);
  return 1;

} /* WriteOneEquation */


/* ----------------------------------------------------------------------------
   WriteCalcDeriv
   
   Writes the CalcDeriv() function.  Writes dynamics equations in 
   the order they appeared in the model definition file.
*/

void WriteCalcDeriv (PFILE pfile, PVMMAPSTRCT pvmGlo, PVMMAPSTRCT pvmDyn)
{
  if (!pvmDyn)
    printf ("No Dynamics{} equations.\n");

  fprintf (pfile, "/* Dynamics section */\n");
  fprintf (pfile, "void CalcDeriv (double  rgModelVars[],\n");
  fprintf (pfile, "   double  rgDerivs[],\n");
  fprintf (pfile, "   PDOUBLE pdTime)\n");
  fprintf (pfile, "{\n");
  fprintf (pfile, "  /* Local variables */\n");

  ForAllVar (pfile, pvmGlo, &WriteOneDecl, ID_LOCALDYN, NULL);

  fprintf (pfile, "\n");
  fprintf (pfile, "  CalcInputs (pdTime);    /* Get new input vals */\n");

  ForAllVar (pfile, pvmDyn, &WriteOneEquation, ALL_VARS, (PVOID) KM_DYNAMICS); 
 
  fprintf (pfile, "} /* CalcDeriv */\n");

} /* WriteCalcDeriv */



/* ----------------------------------------------------------------------------
   WriteScale
*/
void WriteScale (PFILE pfile, PVMMAPSTRCT pvmGlo, PVMMAPSTRCT pvmScale)
{
  if (!pvmScale)
    printf ("No Scale{} equations. Null function defined.\n");
    
  fprintf (pfile, "/* Model scaling */\n");
  fprintf (pfile, "\nvoid ScaleModel (void)\n");
  fprintf (pfile, "{\n");
  fprintf (pfile, "  /* Local variables */\n");

  ForAllVar (pfile, pvmGlo, &WriteOneDecl, ID_LOCALSCALE, NULL);

  fprintf (pfile, "\n");

  ForAllVar (pfile, pvmScale, &WriteOneEquation, ALL_VARS, (PVOID) KM_SCALE);  

  fprintf (pfile, "} /* ScaleModel */\n");

} /* WriteScale */


/* ----------------------------------------------------------------------------
   WriteCalcJacob
*/
void WriteCalcJacob (PFILE pfile, PVMMAPSTRCT pvmGlo, PVMMAPSTRCT pvmJacob)
{
  fprintf (pfile, "/* Jacobian calculation */\n\n");
  fprintf (pfile, "\nvoid CalcJacob (double rgModelVars[], double *rgdSave[]");
  fprintf (pfile, ", PDOUBLE pdTime)\n");
  fprintf (pfile, "{\n");
  fprintf (pfile, "  /* Local variables */\n");
  ForAllVar (pfile, pvmGlo, &WriteOneDecl, ID_LOCALJACOB, NULL);
  fprintf (pfile, "\n");
  ForAllVar (pfile, pvmJacob, &WriteOneEquation, ALL_VARS, (PVOID) KM_JACOB);  
  fprintf (pfile, "} /* CalcJacob */\n");
} /* WriteCalcJacob */


/* ----------------------------------------------------------------------------
   WriteCalcOutputs
*/
void WriteCalcOutputs (PFILE pfile, PVMMAPSTRCT pvmGlo, PVMMAPSTRCT pvmCalcOut)
{
  if (!pvmCalcOut)
    printf ("No CalcOutput{} equations. Null function defined.\n");
    
  fprintf (pfile, "/* Outputs calculation */\n");
  fprintf (pfile, "\nvoid CalcOutputs (double  rgModelVars[],\n");
  fprintf (pfile, "      double  rgDerivs[],\n");
  fprintf (pfile, "      PDOUBLE pdTime)\n");
  fprintf (pfile, "{\n");
  fprintf (pfile, "  /* Local variables */\n");

  ForAllVar (pfile, pvmGlo, &WriteOneDecl, ID_LOCALCALCOUT, NULL);

  fprintf (pfile, "\n");

  ForAllVar (pfile, pvmCalcOut, &WriteOneEquation, ALL_VARS, 
             (PVOID) KM_CALCOUTPUTS); 
 
  fprintf (pfile, "} /* CalcOutputs */\n");

} /* WriteCalcOutputs */


/* ----------------------------------------------------------------------------
   IndexOneVar
   
   ORs the index passed through the info pointer, a PINT,
   and increments the value of the pointer.

   The handle is corrected for use in the simulation.  For this, the
   TYPE part of the handle is shifted left 4 bits.
   
   Callback function for ForAllVar().
*/
int IndexOneVar (PFILE pfile, PVMMAPSTRCT pvm, PVOID pInfo)
{
  if (pvm->szEqn != vszHasInitializer) { /* Handled later */
    pvm->hType |= (*((PINT) pInfo))++;
    return 1;
  }  /* if */
  return 0;

} /* IndexOneVar */


/* ----------------------------------------------------------------------------
   IndexVariables

   Creates indices and counts all the model variables.
   
   The model variables (states and outputs) are indexed
   together, since they will be stored in one array.
   
   The indices to the parmeters are into the global var map.
   
   The indices to inputs are into the array of inputs.
*/
void IndexVariables (PVMMAPSTRCT pvmGlo)
{
  int iIndex, iMax = MAX_VARS;
  
  /* Get counts */
  vnStates  = ForAllVar (NULL, pvmGlo, &CountOneDecl, ID_STATE, NULL);
  vnOutputs = ForAllVar (NULL, pvmGlo, &CountOneDecl, ID_OUTPUT, NULL);
  vnInputs  = ForAllVar (NULL, pvmGlo, &CountOneDecl, ID_INPUT, NULL);
  vnParms   = ForAllVar (NULL, pvmGlo, &CountOneDecl, ID_PARM, NULL);
  vnModelVars = vnStates + vnOutputs;
  
  /* Report all errors */
  if (vnStates > MAX_VARS)
    ReportError (NULL, RE_TOOMANYVARS, "state", (PSTR) &iMax);
  if (vnOutputs > MAX_VARS)
    ReportError (NULL, RE_TOOMANYVARS, "input", (PSTR) &iMax);
  if (vnInputs > MAX_VARS)
    ReportError (NULL, RE_TOOMANYVARS, "output", (PSTR) &iMax);
  if (vnParms > (iMax = MAX_VARS - vnModelVars))
    ReportError (NULL, RE_TOOMANYVARS, "parameter", (PSTR) &iMax);

  if (vnStates > MAX_VARS
      || vnInputs > MAX_VARS
      || vnOutputs > MAX_VARS
      || vnParms > iMax)
    ReportError (NULL, RE_FATAL, NULL, NULL); /* Abort generation */

  /* Set indices */
  iIndex = 0;
  ForAllVar (NULL, pvmGlo, &IndexOneVar, ID_STATE, (PVOID) &iIndex);
  ForAllVar (NULL, pvmGlo, &IndexOneVar, ID_OUTPUT, (PVOID) &iIndex);

  iIndex = 0;
  ForAllVar (NULL, pvmGlo, &IndexOneVar, ID_INPUT, (PVOID) &iIndex);
  
  iIndex = vnStates + vnOutputs + vnInputs;
  ForAllVar (NULL, pvmGlo, &IndexOneVar, ID_PARM, (PVOID) &iIndex);
  
} /* IndexVariables */


/* ----------------------------------------------------------------------------
   AdjustOneVar
   
   Increments the dependent parameter handles of an input by the
   iOffset given through the info pointer.
   
   Callback function for ForAllVar().
*/
int AdjustOneVar (PFILE pfile, PVMMAPSTRCT pvm, PVOID pInfo)
{
  PIFN pifn = (PIFN) pvm->szEqn;  
  WORD wOffset = *(PWORD) pInfo;

  if (!pifn)
    return 1; /* No eqn!  No dependent parm! */
  
  if (pifn->hMag)
    pifn->hMag += wOffset;
  if (pifn->hTper)
    pifn->hTper += wOffset;
  if (pifn->hT0)
    pifn->hT0 += wOffset;
  if (pifn->hTexp)
    pifn->hTexp += wOffset;
  if (pifn->hDecay)
    pifn->hDecay += wOffset;

  return 1;

} /* AdjustOneVar */


/* ----------------------------------------------------------------------------
   AdjustVarHandles
   
   Adjusts the variable handles on input definitions.  They must be
   incremented by the beginning offset of the parameter section of
   the global variable map.
*/
void AdjustVarHandles (PVMMAPSTRCT pvmGlo)
{
  WORD wOffset = (WORD) vnInputs + vnStates + vnOutputs;

  ForAllVar (NULL, pvmGlo, &AdjustOneVar, ID_INPUT, (PVOID) &wOffset);
  
} /* AdjustVarHandles */


/* ----------------------------------------------------------------------------
   ReversePointers
   
   Flips the pointer on the var-list so that the head is the tail
   and the tail is the head and things aren't what they seem to be.
   
   The dynamic equation list must be reversed so that the equations
   appear in the right order since they were created as a stack.
*/
void ReversePointers (PVMMAPSTRCT *ppvm)
{
  PVMMAPSTRCT pvmPrev, pvmNext;

  if (!ppvm || !(*ppvm)
      || !(*ppvm)->pvmNextVar) /* List of one is already reversed! */
    return;

  pvmPrev = NULL;  
  while ((pvmNext = (*ppvm)->pvmNextVar)) {
    (*ppvm)->pvmNextVar = pvmPrev;
    pvmPrev = (*ppvm);
    *ppvm = pvmNext;
  } /* while */

  (*ppvm)->pvmNextVar = pvmPrev; /* Link new head to reversed list */

} /* ReversePointers */


/* ----------------------------------------------------------------------------
   AssertExistsEqn
   
   Check that equation exists.
   
   Uses info pointer of ForAllVar's callback as a second eqn list.
     
   (1) If the list is second list is NULL, checks for initialization
   of pvm.
   
   (2) If the list in non-NULL, checks the second-list for
   definition.
   
   Note that case (1) and (2) apply to inputs and dynamics eqns respectively.
   Further assertion will have to be handled otherwise.  Parms are
   assured to be initialized when declared, and outputs can be initialized
   to 0 automatically.
   
   The errors are not reported as fatal so that all errors can
   be discovered.  They will cause exit subsequently.
*/
int AssertExistsEqn (PFILE pfile, PVMMAPSTRCT pvm, PVOID pInfo)
{
  int iReturn = 0;
  PVMMAPSTRCT pvmDyn = (PVMMAPSTRCT) pInfo;

  if (pvm->szEqn != vszHasInitializer) { /* Don't count these! */
    if (pvmDyn) {
      if (!(iReturn = (int) GetVarPTR (pvmDyn, pvm->szName)))
        ReportError (NULL, RE_NODYNEQN, pvm->szName, NULL);
    }
    else
      if (!(iReturn = (int) pvm->szEqn))
        ReportError (NULL, RE_NOINPDEF, pvm->szName, NULL);
  } /* if */

  /* If HasInitializer, no error to report */
  return (iReturn ? 1 : 0);

} /* AssertExistsEqn */


/* ----------------------------------------------------------------------------
*/
void VerifyEqns (PVMMAPSTRCT pvmGlo, PVMMAPSTRCT pvmDyn)
{
  BOOL bStatesOK;

  bStatesOK = (vnStates == ForAllVar (NULL, pvmGlo, &AssertExistsEqn,
                                      ID_STATE, (PVOID) pvmDyn));

  if (!bStatesOK)
    ReportError (NULL, RE_FATAL, NULL, "State equations missing");

} /* VerifyEqns */

void MakeCodeFilename (PSTR szNewName, PSTR szName)
{
  strcpy (szNewName, szName);

#ifdef _DOS_
  /* In Dos, you can only have one '.' in a filename */
  strtok (szNewName, ".");  /* Remove any extension */
  strcat (szNewName, ".c");  /* Create ".c" filename */

#else
  {
  int cchName;

  cchName = strlen (szNewName);
  if (!((cchName > 1)
      && (szNewName[cchName-1] == 'c')
      && (szNewName[cchName-2] == '.'))) /* Look for current extension */
    strcat (szNewName, ".c"); /* Create ".c" filename */
  } /* block */

#endif

} /* MakeCodeFilename */
  

/* ----------------------------------------------------------------------------
   WriteModel
   
   Writes the model calculation file szOutFilename for the parameters
   and dynamic equations given.
*/
void WriteModel (PINPUTINFO pinfo, int nArg, PSTR rgszArgs[])
{
  PFILE pfile;
  char szFileWithExt[FILENAME_MAX];
  static char vszFilenameDefault[] = "model.c";

  if (!pinfo->pvmGloVars || (!pinfo->pvmDynEqns && !pinfo->pvmCalcOutEqns)) {
    printf ("Error: No Dynamics, no outputs or no global variables defined\n");
    return;
  }
  
  ReversePointers (&pinfo->pvmGloVars);
  ReversePointers (&pinfo->pvmDynEqns);
  ReversePointers (&pinfo->pvmScaleEqns);
  ReversePointers (&pinfo->pvmCalcOutEqns);
  ReversePointers (&pinfo->pvmJacobEqns);
  vpvmGloVarList = pinfo->pvmGloVars;

  IndexVariables (pinfo->pvmGloVars);
  AdjustVarHandles (pinfo->pvmGloVars);
  VerifyEqns (pinfo->pvmGloVars, pinfo->pvmDynEqns);

  MakeCodeFilename (szFileWithExt, 
                    (nArg > 2 ? rgszArgs[2] : vszFilenameDefault));

  if ((pfile = fopen (szFileWithExt, "w"))) {

    /* Keep track of the model description file and generator name */
    vszModelFilename = pinfo->szInputFilename;
#ifdef _MACOS_
    vszModGenName = "Mod";
#else
    vszModGenName = rgszArgs[0];
#endif
    
    WriteHeader (pfile, szFileWithExt, pinfo->pvmGloVars);

    WriteIncludes (pfile);
    WriteDecls (pfile, pinfo->pvmGloVars);
    WriteVarMap (pfile, pinfo->pvmGloVars);

    WriteInitModel (pfile, pinfo->pvmGloVars);
/*  WriteInitExpt (pfile, pinfo->pvmGloVars); FB 7/12/96 commented out */
    WriteCalcDeriv (pfile, pinfo->pvmGloVars, pinfo->pvmDynEqns);
    WriteScale (pfile, pinfo->pvmGloVars, pinfo->pvmScaleEqns);
    WriteCalcJacob (pfile, pinfo->pvmGloVars, pinfo->pvmJacobEqns);
    WriteCalcOutputs (pfile, pinfo->pvmGloVars, pinfo->pvmCalcOutEqns);

    fclose (pfile);
    printf ("\n* Created model file '%s'\n", szFileWithExt);

  } /* if */

  if (!pfile)
    ReportError (NULL, RE_CANNOTOPEN | RE_FATAL,
                 szFileWithExt, "...in WriteModel ()");

} /* WriteModel */

