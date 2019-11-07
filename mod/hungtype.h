/* hungtype.h

   written by Don Robert Maszle
   10 September 1991

   Copyright (c) 1993.  Don Maszle, Frederic Bois.  All rights reserved.

   -- Revisions -----
     Logfile:  %F%
    Revision:  %I%
        Date:  %G%
     Modtime:  %U%
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

