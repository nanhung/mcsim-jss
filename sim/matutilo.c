/* matutilo.c

   written by Don Robert Maszle
   18 September 1992

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
     Logfile:  SCCS/s.matutilo.c
    Revision:  1.8
        Date:  14 Nov 1997
     Modtime:  06:39:02
      Author:  @a
   -- SCCS  ---------

   Matrix utilitites.  Output routines.
*/

#include <stdio.h>
#include <math.h>

#include "matutilo.h"

/* -----------------------------------------------------------------------------
   WriteArray

   writes the elements of an array, tab separated, to a specified file.

   The trailing tab is not printed, nor is  carriage return.
*/

void WriteArray (FILE *pfile, long cElems, double *rg)
{
  register long i;
  register long cElems_minus_1 = cElems - 1;

  for (i = 0; i < cElems; i++) {
    fprintf(pfile, "%g", rg[i]);
    if (i < cElems_minus_1) fputc ('\t', pfile);
  } /* for */
} /* WriteArray */


void WriteArrayExp (FILE *pfile, long cElems, double *rg)
{
  register long i;
  register long cElems_minus_1 = cElems - 1;

  for (i = 0; i < cElems; i++) {
    fprintf(pfile, "%g", exp(rg[i]));
    if (i < cElems_minus_1) fputc ('\t', pfile);
  } /* for */
} /* WriteArrayExp */


void _walog (long cElems, double *rg)
{
  int i;
  double dSum = 0.0;
  printf ("{");
  for (i = 0; i < cElems; i++) {
    dSum += exp(rg[i]);
    printf("%s%g", (i ? ", " : ""), exp(rg[i]));
  } /* for */
  printf ("} => %g [%g]\n", dSum, 1.0-dSum);
} /* _walog */

