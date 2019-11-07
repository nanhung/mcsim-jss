/* strutil.h

   written by Don Robert Maszle
   27 January 1992
   
   Copyright (c) 1993.  Don Maszle, Frederic Bois.  All rights reserved.

   -- Revisions -----
     Logfile:  SCCS/s.strutil.h
    Revision:  1.2
        Date:  20 Dec 1994
     Modtime:  03:22:55
      Author:  @a
   -- SCCS  ---------

   Alternate string routines header file.  Routines for when NULL
   pointer dereferencing is a no-no.

   These handle NULL string pointer cases in a reasonable manner and
   ultimately call the standard library routines.
*/

#define  MyStrcpy(szDest, szSource) \
  ((szDest) && (szSource) ? strcpy((szDest), (szSource)) : NULL)

#define  MyStrlen(sz) \
  ((sz) ? strlen((sz)) : (int) 0)

#define  MyStrchr(sz, iChar) \
  ((sz) ? strchr((sz), (iChar)) : NULL)

#define  MyStrtok(sz, szToken) \
  ((sz) && (szToken) ? strtok((sz), (szToken)) : NULL)

/* -----------------------------------------------------------------------------
   Prototypes */

int MyStrcmp(const char* sz1, const char* sz2);

/* End */

