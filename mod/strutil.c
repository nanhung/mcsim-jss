/* strutil.c

   written by Don Robert Maszle
   12 January 1992
   
   Copyright (c) 1993.  Don Maszle, Frederic Bois.  All rights reserved.

   -- Revisions -----
     Logfile:  SCCS/s.strutil.c
    Revision:  1.3
        Date:  20 Dec 1994
     Modtime:  03:22:54
      Author:  @a
   -- SCCS  ---------


   The following routines are provided as alternates to the standard
   library routines.  They handle NULL string pointer cases in a
   reasonable manner and ultimately call the standard library routines
   for non-NULL cases.  Some routines are implemented as macros.

   Use the header file "strutil.h"

   Currently supported are:

   MyStrlen(), MyStrcpy(), MyStrcmp(), MyStrchr(), MyStrtok()
*/

#include <string.h>

#include "strutil.h"

int MyStrcmp(const char* sz1, const char* sz2)
{
  if (!sz1) {
    if (sz2)
      return (-1);  /* NULL comes before the -something- */
    else
      return (0);   /* Two NULL strings compare equal */
  } /* if */

  else { /* assert (sz1) */
    if (sz2)
      return (strcmp(sz1, sz2));  /* Normal comparison */
    else
      return (1);   /* -Something- comes after the NULL */
  } /* else */

  /* Prevent compiler complaints */
  return 0; /* Never reached! */

} /* MyStrcmp */


/* End */
