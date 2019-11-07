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
     Logfile:  SCCS/s.sim.h
    Revision:  1.35
        Date:  14 Nov 1997
     Modtime:  06:39:20
      Author:  @a
   -- SCCS  ---------

   Header file for simulation

   Modified the obsolete "Options Flags"
*/

#ifndef _SIM_H_
#define _SIM_H_

/* ----------------------------------------------------------------------------
   Inclusions
*/

#include "modiface.h"
#include "list.h"
#include "matutil.h"
#include "random.h"


/* ----------------------------------------------------------------------------
   Constants
*/

/* These are potential array size problems.
   Other maximum sizes are MAX_EQN and MAX_LEX in lex.h */

#define MAX_LEVELS            10   /* for now; actual possible is 255 */
#define MAX_INSTANCES         200  /* for now; allowable instances of `Level'
                                      at any depth */
#define MAX_EXPERIMENTS       200
#define LSODES_IWORKSIZE      300  /* Lsodes tells if this is not big enough */
#define LSODES_RWORKSIZE      1200 /* Idem */
#define ARGS_MAX              8    /* Maximum number of args to lex */
                                   /* Used in allocating argument strings */
#define MAX_PRINT_VARS        10   /* Arbitrary */

/* Keyword Map constants */

#define KM_INTEGRATE       1
#define KM_SIMULATE        2
#define KM_PRINT           5
#define KM_PRINTSTEP       6
#define KM_DATA            7

#define KM_SIMTYPE         8  /* For simulation type specification */
#define KM_DEFAULTSIM      9  /* 'Normal' keyword: normal sims */

#define KM_EXPERIMENT      10
#define KM_MONTECARLO      11
#define KM_MCVARY          12
#define KM_SETPOINTS       13
#define KM_OUTPUTFILE      14
#define KM_MCMC            15
#define KM_LEVEL           16
#define KM_OPTDESIGN       17

#define KM_END             100

/* Function argument keywords */

#define KM_YES             200
#define KM_NO              201

#define KM_UNIFORM         210
#define KM_LOGUNIFORM      211
#define KM_BETA            212
#define KM_NORMAL          213
#define KM_LOGNORMAL       214
#define KM_TRUNCNORMAL     215
#define KM_TRUNCLOGNORMAL  216
#define KM_CHI2            217
#define KM_BINOMIAL        218
#define KM_PIECEWISE       219
#define KM_EXPONENTIAL     220
#define KM_GGAMMA          221
#define KM_POISSON         222
#define KM_INVGGAMMA       223
#define KM_NORMALV         224
#define KM_LOGNORMALV      225
#define KM_TRUNCNORMALV    226
#define KM_TRUNCLOGNORMALV 227
#define KM_BINOMIALBETA    228

#define KM_PREDICTION      300

#define KM_LSODES          600
#define KM_EULER           601

#define KM_FORWARD         700
#define KM_BACKWARD        701

/* Context bit flags */

#define CN_END             0x0000    /* No context */

#define CN_GLOBAL          0x0001
#define CN_EXPERIMENT      0x0002
#define CN_FUNCARG         0x0100

#define CN_ALL             0xFFFF    /* All contexts */

/* Analysis Types */

#define AT_NOTSPECD     0    /* Not yet specified */
#define AT_DEFAULTSIM   1    /* Normal simulation */
#define AT_MONTECARLO   2    /* Monte Carlo variations */
#define AT_SETPOINTS    3    /* Set points simulation */
#define AT_MCMC         4    /* Metropolis or Gibbs estimation */
#define AT_OPTDESIGN    5    /* Metropolis or Gibbs estimation */

/* Monte Carlo Variation types */

#define MCV_SETPOINTS       (-1) /* Not really Monte Carlo */
#define MCV_UNIFORM         0
#define MCV_LOGUNIFORM      1
#define MCV_BETA            2
#define MCV_NORMAL          3
#define MCV_LOGNORMAL       4
#define MCV_TRUNCNORMAL     5
#define MCV_TRUNCLOGNORMAL  6
#define MCV_CHI2            7
#define MCV_BINOMIAL        8
#define MCV_PIECEWISE       9
#define MCV_EXPONENTIAL     10	
#define MCV_GGAMMA          11
#define MCV_POISSON         12
#define MCV_INVGGAMMA       13
#define MCV_NORMALV         14
#define MCV_LOGNORMALV      15
#define MCV_TRUNCNORMALV    16
#define MCV_TRUNCLOGNORMALV 17
#define MCV_BINOMIALBETA    18

/* Integration Method types */

#define IAL_EULER  2  /* Euler algorithm */
#define IAL_LSODES 3  /* lsodes algorithm */

/* Integrator spec defaults */

#define IAL_DEFAULT     IAL_LSODES
#define IOPT_DEFAULT    (0)
#define ITOL_DEFAULT    (1)
#define ITASK_DEFAULT   (4)   /* do not overshoot - FB 01/07/97 */
#define RTOL_DEFAULT    (1.0e-5)
#define ATOL_DEFAULT    (1.0e-7)
#define IMF_DEFAULT     (222) /* stiff */
#define TSTEP_DEFAULT   (1)

/* Simulation specification defaults */

#define T0_DEFAULT            0.0
#define TFINAL_DEFAULT        0.0
#define NSIMULATIONS_DEFAULT  0

/* Defs for Distrib statement */

#define MCVP_FIXED    0
#define MCVP_VARIABLE 1


/* ----------------------------------------------------------------------------
   Typedefs
*/

/* Union of two types of variables: constants and input fns */

typedef union tagUVAR {
  double dVal;
  PIFN pifn;
} UVAR; /* tagUVAR */


/* Modification specification for one variable */

typedef struct tagVARMODIFICATION {
  HVAR hvar; /* Handle to the variable */
  UVAR uvar; /* Union of variable value or input function spec */
} VARMODIFICATION, *PVARMOD; /* tagVARMODIFICATION */


/* Specification of integrator settings */

typedef struct tagINTSPEC {
  int     iAlgo;          /* one of IM_ types */
  long    iopt;           /* optional inputs flag */
  long    itask;          /* type of work */
  long    itol;           /* type of error checking */
  double  dRtol;          /* relative error tolerance */
  double  dAtol;          /* aboslute error tolerance */
  long    iMf;            /* 0 = nonstiff, 1 = stiff */
  long    iDSFlag;        /* lsodes return flag */
  long    liw;            /* length of iwork array */
  long    lrw;            /* length of rwork array */
  PLONG   iwork;          /* working array pointer */
  PDOUBLE rwork;          /* working array pointer */

  double  dTStep;         /* time step for Euler */
} INTSPEC, *PINTSPEC; /* tagINTSPEC */


/* Print Record: for info from a Print() statement */

typedef struct tagPRINTREC {
  PSTR szOutputName;
  HVAR hvar;
  int  cTimes;
  PDOUBLE pdTimes;
} PRINTREC, *PPRINTREC; /* tagPRINTREC */


/* Data record: for info from a Data() statement */

typedef struct tagDATAREC {
  PSTR szDataName;
  HVAR hvar;
  int  cData;
  PDOUBLE pdData;
} DATAREC, *PDATAREC; /* tagDATAREC */


/* Output specification */

typedef struct tagOUTSPEC {
  PSTR  szOutfilename;      /* Name of file for regular output */
  PFILE pfileOut;           /* Pointer to file */
  BOOL  bCommandLineSpec;   /* Output file specified on command line */

  int   nOutputs;           /* Number of outputs */
  PSTR  *pszOutputNames;    /* Array of output names */
  HVAR  *phvar;             /* Array of handles to outputs */

  PLIST plistPrintRecs;     /* List of records from Print()'s */
  PLIST plistDataRecs;      /* List of records from Data()'s */

  /* The lists are converted into the following */

  PINT    pcOutputTimes;    /* Count of output times for each var */
  PINT    piCurrentOut;     /* Index to current output for each var */
  PINT    piSigmas;         /* Array of SDs for each var */
  PDOUBLE *prgdOutputTimes; /* Array of output times for each var */
  PDOUBLE *prgdOutputVals;  /* Array of output values for each var */
  PDOUBLE *prgdDataVals;    /* Array of data values for each var */

  int cDistinctTimes;       /* Count of distinct output times */
  PDOUBLE rgdDistinctTimes; /* Array of distinct output times */

} OUTSPEC, *POUTSPEC; /* tagOUTSPEC */


/* Monte Carlo Variation for one parameter */

typedef struct tagMCVAR {
  PSTR    pszName;          /* Model variableariable name */
  HVAR    hvar;             /* Handle to the model variable to be modified */
  int     iDepth;           /* Level (to distinguish vars with same hvar) */
  int     iType;            /* One of MCV_ distribution types */
  char    cVarParm;         /* Flag for variable parameters */
  HVAR    hParm[4];         /* pointers to model vars for 4 distrib. params */
  double  dParm[4];         /* values of distribution parameters */
  PDOUBLE pdParm[4];        /* pointers to distribution parameters */
  double  dVal;             /* Value for this run */
  struct  tagMCVAR *pMCVParent[4]; /* Pointers to parents of this var (vars on
                                      which this var depends) */
  PLIST   plistDependents;  /* List of MCvars depending directly on this one */
  long    nDependents;
  struct  tagMCVAR **rgpDependents;
  BOOL    bExptIsDep;       /* True if experiment is dependent on this var */
  int     iParmIndex;       /* Index in OUTSPEC when Parm[1] is ModelVar */
  BOOL    bIsFixed;         /* True if var is fixed */
  BOOL    bGibbs;           /* True if its conditional distrib. is known */
  long    lJumps;           /* Number of MH jumps for this param */
  double  dKernelSD;        /* MCMC jumping kernel SD */

} MCVAR, *PMCVAR; /* tagMCVAR */


typedef struct tagGIBBSDATA {
  long nMaxIter;        /* Number of iterations */
  long nInitIter;       /* Number of iterations prior to vector sampling */
  long nPrintFreq;      /* To request output every nPrintFreq iterations */
  long nPrintIter;      /* Number of final iterations to print */

  PSTR szGout;          /* Filename for output */
  PFILE pfileOut;       /* File pointer for output */

  PSTR szGrestart;      /* Filename for restart parameter vectors */
  PFILE pfileRestart;   /* File pointer for restart */

  PSTR szGdata;         /* Filename for input data */

} GIBBSDATA, *PGIBBSDATA; /* tagGIBBSDATA */


/* Specification for Monte Carlo type experiment */
enum {forward, backward};

typedef struct tagMONTECARLO {
  long nRuns;               /* Number of Monte Carlo runs */
  long lRun;                /* Number of current Run */

  PSTR  szMCOutfilename;    /* File name for Monte Carlo output */
  PFILE pfileMCOut;         /* File for Monte Carlo output */

  PSTR  szSetPointsFilename;/* File name for set points */
  PFILE pfileSetPoints;     /* File of set points */

  PLIST plistMCVars;        /* List of MCVAR record, variation specs */

  /* The list is converted to the following */
  long   nParms;            /* Count of parameters */
  double *rgdParms;         /* The actually used parameter vector */
  HVAR   *rghvar;           /* Array of handles to the parameters */
  MCVAR  **rgpMCVar;        /* A priori distributions for each */

  long nSetParms;           /* Count of setpoint parameters */

  int  style;               /* either forward or backward for optimal design */

} MONTECARLO, *PMONTECARLO; /* tagMONTECARLO */


/* Record of info about the model */

typedef struct tagMODELINFO {
  long      nStates;
  long      nModelVars;

  HVAR      *pStateHvar;	/* hvars of state variables */

  PDOUBLE   pdModelVars;

} MODELINFO, *PMODELINFO; /* tagMODELINFO */


/* Record of information for one experiment.
   An experiment specifies a set of experimental
   conditions, parameter settings, input levels, etc.
 */

typedef struct tagEXPERIMENT {
  int iExp;                 /* Number of this experiment */

  double dT0;               /* Time limits */
  double dTfinal;

  double dTime;             /* Current Time -- not used for global sim */

  int iSubject;             /* Subject #, eventually */

  double dLnLike;           /* Log-likelihood */
  double dLnLikeSave;

  PMODELINFO pmodelinfo;    /* Pointer to the model information */
  PLIST plistParmMods;      /* List of parameter mods (elt = PVARMOD) */
  INTSPEC is;               /* Integrator spec, this experiment */
  OUTSPEC os;               /* Output spec, this experiment */

} EXPERIMENT, *PEXPERIMENT; /* tagEXPERIMENT */


/* Information for each instance of a level */

typedef struct tagLEVEL {
  int    iDepth;                           /* Depth of this level */
  int    iSequence;                        /* Instance # of this level */
  int    iInstances;                       /* # of instances of next level */
  struct tagLEVEL *pLevels[MAX_INSTANCES]; /* Pointers to sublevels */

  PLIST  plistVars;    /* Vars, other than those in Distrib */
  long   nFixedVars;
  PVARMOD *rgpFixedVars;

  PLIST   plistMCVars; /* List of MCVAR record, variation specs */
  long    nMCVars;     /* Count of parameters */
  PMCVAR *rgpMCVars;   /* A priori distributions for each */

  PEXPERIMENT pexpt;   /* Ptr to expt struct, NULL if not expt
				                  EXPERIMENT is used for compatibility */ 
} LEVEL, *PLEVEL; /* tagLEVEL */


/* Defines an analysis for an input file */

typedef struct tagANALYSIS {

  BOOL bDependents;	    /* Debug flag for printing dependents to stderr */
  BOOL bParams;		      /* Debug flag for printing params of MC vars to file */
  BOOL bPrintIter;	    /* Debug flag for printing iteration numbers */

  int iType;            /* Type of analysis. One of AT_ types */

  WORD wContext;        /* Context flag used during input processing */
  double dSeed;         /* Random seed used for all analyses */

  MODELINFO   modelinfo;/* The model we are using */

  int iDepth;		        /* Depth of levels */
  int iCurrentDepth;
  int iInstances;       /* Number of instances of level 1 */
  int iExpts;           /* Total number of experiments at all levels */

  PLEVEL pLevels[MAX_INSTANCES];    /* Pointer to level 1 structures */
  PLEVEL pCurrentLevel[MAX_LEVELS]; /* Pointers to currently nested structs */
  int iInstance[MAX_LEVELS];        /* Sequence of instances, e.g., toplevel 1,
                                       subject 2, experiment 3 */

  EXPERIMENT  expGlobal;            /* Global experiment settings */

  PEXPERIMENT rgpExps[MAX_EXPERIMENTS];/* List of pointer to experiments */
  PEXPERIMENT pexpCurrent;             /* Experiment being currently defined */

  PLIST plistVars;	    /* Global variables to set */

  PLIST plistModelVars; /* List of output variables to randomize after expt */
  long nModelVars;
  PMCVAR *rgpModelVars;

  MONTECARLO    mc;     /* Monte Carlo specification */
  GIBBSDATA     gd;     /* Gibbs estimate specification */

} ANALYSIS, *PANALYSIS; /* tagANALYSIS */


/* -----------------------------------------------------------------------------
   Globals/Externals
*/

extern PSTRLEX vrgszlexArgs[];


/* ----------------------------------------------------------------------------
   Prototypes
*/

void AnnounceProgram (void);
void CorrectInputToTransition (PEXPERIMENT, PDOUBLE);

int  DoOneExperiment (PEXPERIMENT pexp);

void DoAnalysis (PANALYSIS panal);
void DoMonteCarlo (PANALYSIS panal);
void DoNormal (PANALYSIS panal);
int  DoOneMCExp (PANALYSIS panal, PEXPERIMENT pexp);
int  DoOneNormalExp (PANALYSIS panal, PEXPERIMENT pexp);

int  Euler (long neq, double *y, double *t, double tout, double dTStep);

void FreeVarMod (PVOID pData);

void GetCmdLineArgs (int cArg, char *const *rgszArg, PSTR *pszFileIn,
                     PSTR *pszFileOut, PANALYSIS panal);
void GetOutputFlagOption (PANALYSIS panal, char *optarg);

int  MCVarListToArray (PVOID pv_pMCVar, PVOID pv_Null);
int  ModifyOneParm (PVOID pData, PVOID pNullInfo);
void ModifyParms (PLIST plistParmMods);

void PrepAnalysis (PANALYSIS panal);
int  ProcessMonteCarlo (PINPUTBUF, PANALYSIS, PSTR, int);
void PromptFilenames (PSTR *pszFileIn, PSTR *pszFileOut);

char *SansPath (char *szFullPathname);

void WriteArray (FILE *pfile, long cElems, double *rg);
void WriteArrayLog (FILE *pfile, long cElems, double *rg);

#endif /* _SIM_H_ */

/* End */

