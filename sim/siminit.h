/* sim.h

   originally written by Don Maszle

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
     Logfile:  SCCS/s.siminit.h
    Revision:  1.9
        Date:  14 Nov 1997
     Modtime:  06:39:21
      Author:  @a
   -- SCCS  ---------

   Header file for simulation
*/

#ifndef _SIMINIT_H_
#define _SIMINIT_H_

/* -----------------------------------------------------------------------------
   Inclusions
*/

#include "sim.h"


/* -----------------------------------------------------------------------------
   Prototypes
*/

void CreateOutputSchedule (POUTSPEC pos);
BOOL FindNewPoint (POUTSPEC pos, PINT piPoint);
void GetModelInfo (PMODELINFO pmi);
void InitAnalysis (PANALYSIS panal);
void InitExperiment (PEXPERIMENT pexp, PMODELINFO pmodelinfo);
void InitIntegratorSpec (PINTSPEC pis);
void InitMonteCarlo (PMONTECARLO pmc);
void InitGibbs (PGIBBSDATA pgd);
int  InitOneOutVar (PVOID pData, PVOID pInfo);
int  InitOneDataVar (PVOID pData, PVOID pInfo);
BOOL InitOutputs (PEXPERIMENT pexp, PINT piOut, PDOUBLE pdTout);
void InitOutputSpec (POUTSPEC pos);
BOOL PrepareOutSpec (PEXPERIMENT pexp);
BOOL PrintOutSpec (PEXPERIMENT pexp);

#endif /* _SIMINIT_H_ */

/* End */

