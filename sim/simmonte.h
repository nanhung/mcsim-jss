/* simmonte.h

   originally written by Frederic Bois

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
     Logfile:  SCCS/s.simmonte.h
    Revision:  1.12
        Date:  14 Nov 1997
     Modtime:  06:39:21
      Author:  @a
   -- SCCS  ---------

   Header file for simmonte.c
*/

/* -----------------------------------------------------------------------------
   Prototypes */

void CalcMCParms (PMONTECARLO pMC, double rgParms[], long iStart);
int  CalculateOneMCParm (PMCVAR pMCVar);
double GetParm (PMCVAR pMCVar, int iIndex);
BOOL GetMCMods (PANALYSIS panal, double rgdOptionalParms[]);
BOOL InitSetPoints (PMONTECARLO pMC);
BOOL ReadSetPoints (PMONTECARLO pMC, double rgParms[]);
void SetParents (PMONTECARLO pMC, long iStart);
void SetParms (long cParms, HVAR *rghvar, double *rgdParm);
void SetParmsLog (long cParms, HVAR *rghvar, double *rgdParm);

/* End */

