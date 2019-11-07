/* simi.h

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
     Logfile:  SCCS/s.simi.h
    Revision:  1.10
        Date:  14 Nov 1997
     Modtime:  06:39:20
      Author:  @a
   -- SCCS  ---------

   Header file for simi.c
*/

#ifndef _SIMI_H_
#define _SIMI_H_

/* ----------------------------------------------------------------------------
   Inclusions
*/

#include "sim.h"
#include "strutil.h"


#define INIT_KERNELSD 1.0  /* Initial value of MH std dev divisor */

/* ----------------------------------------------------------------------------
   Prototypes
*/

BOOL CheckDistribParam(PLIST plist, HVAR hvar1, HVAR hvar2);
void DListToArray (PLIST plist, PINT pcDouble, PDOUBLE *ppDouble);
BOOL EndExperiment (PINPUTBUF pibIn, PANALYSIS panal);
BOOL EndLevel (PANALYSIS panal);
void FreeLevels (PANALYSIS panal);
int  FreeMCLists (PVOID pData, PVOID pUserInfo);
void FreeOneLevel (PLEVEL plevel);
BOOL GetData (PINPUTBUF pibIn, PSTR szLex, POUTSPEC pos);
BOOL GetGibbsSpec (PINPUTBUF pibIn, PEXPERIMENT pexp);
BOOL GetIntegrate (PINPUTBUF pibIn, PINTSPEC pis);
PSTR GetKeyword (int iKWCode);
int  GetKeywordCode (PSTR szKeyword, PINT pfContext);
BOOL GetListOfData (PINPUTBUF pibIn, PDATAREC pda, PSTR szLex);
BOOL GetListOfTimes (PINPUTBUF pibIn, int nRecs, PPRINTREC *ppr, PSTR szLex);
int  GetMCVaryParam(PINPUTBUF pibIn, PSTR szLex,
                    PLIST plist, int n, PMCVAR pmcvar);
int  GetMCVarySpec (PINPUTBUF pibIn, PANALYSIS panal, PSTR szLex);
int  GetMonteCarloSpec (PINPUTBUF pibIn, PANALYSIS panal, PSTR szLex);
BOOL GetOptDSpec (PINPUTBUF pibIn, PANALYSIS  panal, PSTR szLex);
BOOL GetOutputFile (PINPUTBUF pibIn, PSTR szLex, POUTSPEC pos);
BOOL GetParmMod (PINPUTBUF pibIn, PSTRLEX szLex, PSTREQN szeqn);
BOOL GetPrint (PINPUTBUF pibIn, PSTR szLex, POUTSPEC pos);
BOOL GetPrintStep (PINPUTBUF pibIn, PSTR szLex, POUTSPEC pos);
int  GetSetPointsSpec (PINPUTBUF pibIn, PANALYSIS  panal, PSTR szLex);
BOOL GetSimType (PINPUTBUF pibIn);
BOOL GetSimulate (PINPUTBUF pibIn, PEXPERIMENT pexp);
BOOL GetStringArg (PINPUTBUF pibIn, PSTR *pszArg, PSTR szLex, BOOL bDelim);
int  GetTerminator (PINPUTBUF pibIn, PSTR szLex);
long ImFromLex (PSTR szLex);
int  McvFromLex (PSTR szLex);
void NewExperiment (PINPUTBUF pibIn);
int  OneDToArray (PVOID pData, PVOID pInfo);
void ProcessWord (PINPUTBUF pibIn, PSTR szLex, PSTR szEqn);
BOOL ReadAnalysis (PINPUTBUF);
int  SetLevel (PINPUTBUF pibIn);
BOOL YesNoFromLex (PSTR szLex);

#endif /* _SIMI_H_ */

/* End */

