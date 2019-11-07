/* modelu.h

   written by Don Maszle
   7 October 1991
   
   Copyright (c) 1993.  Don Maszle, Frederic Bois.  All rights reserved.

   -- Revisions -----
     Logfile:  %F%
    Revision:  %I%
        Date:  %G%
     Modtime:  %U%
      Author:  @a
   -- SCCS  ---------

   Model utilities include file.
   
   Utility prototypes and structures used by the generated model
   file.  The model typedefs need to be defined here so that
   'modelu.c' can use them.
   
*/


#ifndef _MODELU_INCLUDED

/* ----- Inclusions  */

#include "modiface.h"

/* ----- Constants  */

#define ID_NULL		0x0000
#define ID_STATE	0x1000
#define ID_INPUT	0x2000
#define ID_OUTPUT	0x3000
#define ID_PARM		0x4000	/* Global parameter */

#define HV_TYPE_MASK	0xF000	      /* Handle to variable is a WORD */
#define HV_INDEX_MASK	0x0FFF	      /* == 0xtiii, type and index */


/* ----- Typedefs  */

  /* Global Variable Map */

typedef struct tagVM {
    PSTR szName;     /* Name of the variable */
    PVOID pVar;	     /* Ptr to C variable */
    HVAR hvar;       /* Handle to variable: ID_TYPE | index */

} VM, *PVM; /* Variable Map element */


/* ----- Macros  */

#define TYPE(pvm)	((pvm) ? (pvm)->hvar & HV_TYPE_MASK : ID_NULL)
#define INDEX(pvm)	((pvm) ? (pvm)->hvar & HV_INDEX_MASK: ID_NULL)

#define HTYPE(hvar)	((hvar) & HV_TYPE_MASK)
#define HINDEX(hvar)	((int) ((hvar) & HV_INDEX_MASK))

/* ----- Globals/Externals  */

/* ----- Prototypes  */

#define _MODELU_INCLUDED
#endif
