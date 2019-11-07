/* modi.c

   written by Don Robert Maszle
   3 September 1991
   
   Copyright (c) 1993.  Don Maszle, Frederic Bois.  All rights reserved.

   -- Revisions -----
     Logfile:  %F%
    Revision:  %I%
        Date:  %G%
     Modtime:  %U%
      Author:  @a
   -- SCCS  ---------

   Handles parsing input of the Model Definition File.
*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "lexerr.h"
#include "mod.h"

  
/* Keyword Map Structure */

typedef struct tagKM {
  PSTR szKeyword;
  int  iKWCode;   /* Enumeration code of Keyword KM_* */
  WORD  fContext; /* Bit flags of valid context for KW */
} KM, *PKM; /* Keyword Map */

KM vrgkmKeywordMap[] = { /* Global Keyword - code map */
  {"States",  KM_STATES,   CN_GLOBAL},  /* Special syntax */
  {"Inputs",  KM_INPUTS,   CN_GLOBAL},
  {"Outputs", KM_OUTPUTS,  CN_GLOBAL},

  {"Dynamics",   KM_DYNAMICS, CN_GLOBAL},/* Sections */
  {"Scale",      KM_SCALE,    CN_GLOBAL},
  {"Jacob",      KM_JACOB,    CN_GLOBAL},
  {"CalcOutputs",KM_CALCOUTPUTS,CN_CALCOUTPUTS},

  /* Can be LHS only in Dynamics */
  {"dt", KM_DXDT,  CN_DYNAMICS | CN_INPUTDEF},
  
  {"End", KM_END,  CN_GLOBAL}, /* Optional End statement */
  

  /* Variables names valid in all CN_ */
  {"", 0, CN_ALL}  /* End flag */
  
}; /* vrgkmKeywordMap[] */


/* GetKeywordCode
   
   Returns the code of the szKeyword given.  If the string is not
   a valid keyword or abbreviation, returns 0.
*/

int GetKeywordCode (PSTR szKeyword, PINT pfContext)
{
  PKM pkm = &vrgkmKeywordMap[0];
  
  while (*pkm->szKeyword && strcmp (szKeyword, pkm->szKeyword))
    pkm++;
  
  if (pfContext)
    *pfContext = pkm->fContext;  /* Set iContext flag */

  return (pkm->iKWCode);  /* Return Keyword Code or 0 */

}  /* GetKeywordCode */


/* GetKeyword
   
   Returns a pointer to the keyword string given the code.  If the
   code is not valid, returns empty string.
*/

PSTR GetKeyword (int iCode)
{
  PKM pkm = &vrgkmKeywordMap[0];
  
  while (*pkm->szKeyword && pkm->iKWCode != iCode)
    pkm++;
  
  return (pkm->szKeyword); /* Return Keyword Code or null str */

}  /* GetKeyword */



void InitInfo (PINPUTINFO pinfo, PSTR szInputFilename)
{
  pinfo->pvmGloVars = NULL;
  pinfo->pvmDynEqns = NULL;
  pinfo->pvmScaleEqns = NULL;
  pinfo->pvmJacobEqns = NULL;
  pinfo->pvmCalcOutEqns = NULL;
  pinfo->wContext = CN_GLOBAL;
  pinfo->szInputFilename = szInputFilename;
  
}  /* InitInfo */


/* ProcessWord
   
   Processes the word szLex.
*/

void ProcessWord (PINPUTBUF pibIn, PSTR szLex, PSTR szEqn)
{
  int iErr = 0;
  int iLexType, iKWCode, fContext;
  PSTRLEX szPunct;
  PINPUTINFO pinfo;
  
  if (!pibIn || !szLex || !szLex[0] || !szEqn)
    return;

  pinfo = (PINPUTINFO) pibIn->pInfo;
  
  iKWCode = GetKeywordCode (szLex, &fContext);

  assert(pinfo->wContext != CN_END);

  if (pinfo->wContext == CN_END             /* Beyond valid input ! */
      || (iKWCode                           /* Is a keyword */
      && !(fContext & pinfo->wContext)))    /* In a valid context */
    ReportError (pibIn, RE_BADCONTEXT, szLex, NULL);

  else {
    switch (iKWCode) {
      case KM_END:
        pinfo->wContext = CN_END;
        break;
      
      case KM_STATES:
      case KM_INPUTS:
      case KM_OUTPUTS: {
        int cItem = 0;
        int iNLI;

        if (GetOptPunct (pibIn, szPunct, '='))
          if (GetPunct (pibIn, szPunct, CH_LBRACE)) { /* Read model var-list */
            while ((iNLI = NextListItem (pibIn, szPunct, LX_IDENTIFIER,
                                         cItem++, CH_RBRACE)) > 0)
              DeclareModelVar (pibIn, szPunct, iKWCode);

              if (!iNLI)
                NextLex (pibIn, szPunct, &iLexType);
              if (szPunct[0] != CH_RBRACE)
                iErr = szPunct[1] = CH_RBRACE;

          } /* if get LBRACE */
       
          else
            iErr = szPunct[1] = CH_LBRACE;
        else
          iErr = szPunct[1] = '=';

        if (iErr)
          ReportError (pibIn, RE_EXPECTED, szPunct, NULL);

        } /* case States, Inputs, Outputs block */
        break;
      

      case KM_CALCOUTPUTS:
      case KM_JACOB:
      case KM_SCALE:
      case KM_DYNAMICS:
        if (!GetPunct (pibIn, szPunct, CH_LBRACE)) {
          szPunct[1] = CH_LBRACE;
          ReportError (pibIn, RE_EXPECTED | RE_FATAL, szPunct,
                       "* Section must be delimited by curly braces.");
        } /* if */

        else
          pinfo->wContext = KM_TO_CN(iKWCode);
        break;
      

      case KM_DXDT: { /* State equation definition */    
        int iArgType = LX_IDENTIFIER;
 
        if ((iErr = !GetFuncArgs (pibIn, 1, &iArgType, szLex)))
          break;

        /* check this is a declared state ! FB 8/12/96 */
        if (GetVarPTR (pinfo->pvmGloVars, szLex) == NULL)
          ReportError (pibIn, RE_BADSTATE | RE_FATAL, szLex, NULL);

        } /* block */
  
        /* else Fall through ! */

      default: /* Not a keyword, process identifier */
        if (!GetPunct (pibIn, szPunct, '=')) {
          szPunct[1] = '=';
          ReportError (pibIn, RE_EXPECTED, szPunct, NULL);
        } /* if */
        else {
          GetStatement (pibIn, szEqn);
          DefineVariable (pibIn, szLex, szEqn, iKWCode);
          if (!GetPunct (pibIn, szLex, CH_STMTTERM)) {
            szLex[1] = CH_STMTTERM;
            ReportError (pibIn, RE_EXPECTED, szLex, NULL);
          } /* if */
        } /* else */
        break;

    } /* switch */

    if (iErr)
      EatStatement (pibIn); /* Err in stmt, eat to terminator ';' */

  } /* else */

} /* ProcessWord */


/* ReadModel
   
   Read the model definition in the given szFullPathname according to
   the syntax described above and in the documentation.
*/

void ReadModel (PINPUTINFO pinfo, PSTR szFullPathname)
{
  INPUTBUF ibIn;
  PSTRLEX  szLex; /* Lex elem of MAX_LEX length */
  PSTREQN  szEqn; /* Equation buffer of MAX_EQN length */
  int      iLexType;
  
  if (!InitBuffer (&ibIn, szFullPathname))
    ReportError (&ibIn, RE_INIT | RE_FATAL, "ReadModel", NULL);

  InitInfo (pinfo, szFullPathname);
  ibIn.pInfo = (PVOID) pinfo; /* Attach info rec to input buffer */

  do {

    /* State machine for parsing syntax */
    NextLex (&ibIn, szLex, &iLexType);
    switch (iLexType) {
      case LX_NULL:
        pinfo->wContext = CN_END;
        break;

      case LX_IDENTIFIER:
        ProcessWord (&ibIn, szLex, szEqn);
        break;
 
      case LX_PUNCT: case LX_EQNPUNCT:
        if (szLex[0] == CH_STMTTERM)
          break;
        else 
          if (szLex[0] == CH_RBRACE && 
             (pinfo->wContext & (CN_DYNAMICS | CN_JACOB | CN_SCALE))) {
           pinfo->wContext = CN_GLOBAL;
           break;
         } /* if */
          else 
            if (szLex[0] == CH_COMMENT) {
              SkipComment (&ibIn);
              break;
            } /* if */

        /* else -- fall through! */
 
      default:
        ReportError (&ibIn, RE_UNEXPECTED, szLex, "* Ignoring");
        break;
 
      case LX_INTEGER:
      case LX_FLOAT:
        ReportError (&ibIn, RE_UNEXPNUMBER, szLex, "* Ignoring");
        break; 
 
    }  /* switch */
    
  } while (pinfo->wContext != CN_END && 
           (*ibIn.pbufCur || FillBuffer (&ibIn) != EOF));

  pinfo->wContext = CN_END;
  
  fclose (ibIn.pfileIn);

}  /* ReadModel */
  

  
  
