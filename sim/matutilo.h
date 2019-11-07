/* matutil.h

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
     Logfile:  SCCS/s.matutilo.h
    Revision:  1.6
        Date:  14 Nov 1997
     Modtime:  06:39:16
      Author:  @a
   -- SCCS  ---------

*/


/* Prototypes */

/* Write an array to a file */
void WriteArray (FILE *pfile, long cElems, double *rg);

/* Write the array, exp() transformed, to a file, i.e. exp(a[.])*/
void WriteArrayExp (FILE *pfile, long cElems, double *rg);

/* For debugging */
void _walog (long cElems, double *rg);

/* End */

