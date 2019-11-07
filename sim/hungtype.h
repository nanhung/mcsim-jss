/* hungtype.h

   written by Don Robert Maszle
   10 September 1991

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
     Logfile:  SCCS/s.hungtype.h
    Revision:  1.7
        Date:  02 Jul 1997
     Modtime:  08:36:55
      Author:  @a
   -- SCCS  ---------

   Hungarian style type defs.
*/

#ifndef _HUNGTYPE_H_

/* -----------------------------------------------------------------------------
   Inclusions
*/

#include <stdio.h>


/* -----------------------------------------------------------------------------
   Typedefs
*/

typedef int            BOOL;
typedef unsigned char  BYTE;
typedef unsigned int   WORD;
typedef unsigned long  DWORD;
typedef WORD           HANDLE;

#ifdef IBM /* IBM non-ANSI definitions */
typedef char  near      *PSTR;
typedef char  near      *NPSTR;
typedef char  far       *LPSTR;
typedef BYTE  near      *PBYTE;
typedef BYTE  far       *LPBYTE;
typedef int   near      *PINT;
typedef int   far       *LPINT;
typedef WORD  near      *PWORD;
typedef WORD  far       *LPWORD;
typedef long  near      *PLONG;
typedef long  far       *LPLONG;
typedef DWORD near      *PDWORD;
typedef DWORD far       *LPDWORD;
typedef void  far       *LPVOID;

#else
typedef char     *PSTR;
typedef BYTE     *PBYTE;
typedef int      *PINT;
typedef WORD     *PWORD;
typedef long     *PLONG;
typedef DWORD    *PDWORD;
typedef void     *PVOID;
#endif /* else */

typedef float    *PFLOAT;
typedef double   *PDOUBLE;

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

typedef FILE *PFILE;

#define _HUNGTYPE_H_
#endif

/* End */

