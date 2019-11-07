/* lexerr.c

   written by Don Maszle
   15 September 1991

   Copyright (c) 1993.  Don Maszle, Frederic Bois.  All rights reserved.

   -- Revisions -----
     Logfile:  SCCS/s.lexerr.c
    Revision:  1.4
        Date:  03 Sep 1995
     Modtime:  10:20:26
      Author:  @a
   -- SCCS  ---------

   Reports errors and exits program if fatal.
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "lexerr.h"


/* -----------------------------------------------------------------------------
   ReportError

   Reports error iCode to terminal (one of RE_) and optional
   szMessage.   If iSeverity is set to RE_FATAL, exits program.
*/

void ReportError (PINPUTBUF pibIn, WORD wCode, PSTR szMsg, PSTR szAltMsg)
{
  char cNull = '\0';
  BOOL bFatal   = wCode & RE_FATAL;
  BOOL bWarning = wCode & RE_WARNING;

  wCode &= ~(RE_FATAL | RE_WARNING);

  if (!szMsg)
    szMsg = &cNull;

  if (wCode) {
    if (bWarning)
      printf ("Warning: ");
    else {
      printf ("Error: ");
      bFatal |= (pibIn && (pibIn->cErrors++ > MAX_ERRORS));
    } /* else */
  } /* if */

  if (pibIn)
    if (pibIn->pfileIn || pibIn->iLNPrev) /* Line number is valid */
      printf ("line %d: ", pibIn->iLineNum);

    else if (wCode != RE_FILENOTFOUND) { /* Dummy pibIn, show buffer */
      PSTRLEX szTmp;
      szTmp[MAX_LEX-1] = '\0';
      printf ("'%s'...\n  ", strncpy (szTmp, pibIn->pbufOrg, MAX_LEX-1));
    } /* if */

  switch (wCode) {

  case 0:
    break;

  default:
    printf ("Unknown error code %x: %s", wCode, szMsg);

  case RE_INIT:
    printf ("Initialization error.");
    break;

  case RE_FILENOTFOUND:
    printf ("File not found \"%s\".", szMsg);
    break;

  case RE_CANNOTOPEN:
    printf ("Cannot open file \"%s\".", szMsg);
    break;

  case RE_UNEXPECTED:
    printf ("Unexpected character '%c' in input file.", *szMsg);
    break;

  case RE_UNEXPNUMBER:
    printf ("Unexpected number %s in input file.", szMsg);
    break;

  case RE_EXPECTED:
    printf ("Expected '%c' before '%c'.", szMsg[1], szMsg[0]);
    break;

  case RE_LEXEXPECTED:
    printf ("Expected <%s>", szMsg);
    if (szAltMsg)
      printf (" before '%s'", szAltMsg);
    break;

  /* USER error handling -- Add user error reporting below */

  /* Model generator errors */

  case RE_BADCONTEXT:
    printf ("'%s' used in invalid context.", szMsg);
    break;

  case RE_DUPDECL:
    printf ("Duplicate declaration of model variable '%s'.", szMsg);
    break;

  case RE_OUTOFMEM:
    printf ("Out of memory in %s() !", szMsg);
    break;

  case RE_REDEF:
    printf ("'%s' redefined.", szMsg);
    break;

  case RE_EQNTOOLONG:
    printf ("Equation is too long.  Possibly missing terminator.");
    break;

  case RE_BADSTATE:
    printf ("Invalid state identifier '%s'.", szMsg);
    break;

  case RE_UNDEFINED:
    printf ("Undefined identifier '%s'.", szMsg);
    break;

  case RE_NOINPDEF:
    printf ("Input '%s' is not initialized.", szMsg);
    break;

  case RE_NODYNEQN:
    printf ("State variable '%s' has no dynamics.", szMsg);
    break;

  case RE_TOOMANYVARS:
    printf ("Too many %s declarations. Limit is %d.\n",
             szMsg, *(PINT)szAltMsg);
    break;

  /* Simulation input errors */

  case RE_ERRORSINEXP:
    printf ("Bad definition of experiment %d\n", *(PINT)szMsg);
    break;

  case RE_NOOUTPUTS:
    printf ("Experiment %d has no outputs specified\n", *(PINT)szMsg);
    break;

  case RE_NOOUTTIMES:
    printf ("Experiment %d has no output times specified\n", *(PINT)szMsg);
    break;

  case RE_INTEGERR:
    printf ("DifSub() returned error %ld: %s\n", *(PLONG) szMsg, szAltMsg);
    break;

  case RE_SPECERR:
    printf ("in specification: %s\n", szMsg);
    break;

  case RE_INSUF_POINTS:
    printf ("Insufficient points in file \"%s\"\n", szMsg);
    break;

  case RE_DEFOTHER:
    printf ("Already defining other experiment %s\n", szMsg);
    break;

  case RE_MAXMIN_RANGE:
    printf ("Max is less than min\n");
  } /* switch */

  printf ("\n");
  if (szAltMsg && wCode != RE_LEXEXPECTED)
    printf ("%s\n", szAltMsg);

  if (bFatal) {
    printf ("One or more fatal errors: Exiting...\n");

#ifdef _MACOS_
    getchar();
#endif

    exit (wCode);
  } /* if */

} /* ReportError */


