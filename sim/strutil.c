/* strutil.c

   written by Don Robert Maszle
   12 January 1992

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
     Frédéric Bois / Don Maszle
     BEHS, School of Public Health
     University of California at Berkeley
     Berkeley, CA 94720

     fbois@diana.lbl.gov

   -- Revisions -----
     Logfile:  SCCS/s.strutil.c
    Revision:  1.8
        Date:  14 Nov 1997
     Modtime:  06:39:11
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

