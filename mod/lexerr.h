/* lex.h

   written by Don Maszle
   13 October 1991
   
   Copyright (c) 1993.  Don Maszle, Frederic Bois.  All rights reserved.

   -- Revisions -----
     Logfile:  SCCS/s.lexerr.h
    Revision:  1.1
        Date:  01 Feb 1995
     Modtime:  05:12:34
      Author:  @a
   -- SCCS  ---------

   Header file for error reporting routine of lexerr.c
*/

#ifndef _LEXERR_H_

/* -----------------------------------------------------------------------------
   Inclusions  */

#include "hungtype.h"
#include "lex.h"

/* -----------------------------------------------------------------------------
   Prototypes */

void ReportError (PINPUTBUF, WORD, PSTR, PSTR);

#define _LEXERR_H_
#endif

/* End */


