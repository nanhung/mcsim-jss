/* sim.c

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
     Fr�d�ric Bois / Don Maszle
     BEHS, School of Public Health
     University of California at Berkeley
     Berkeley, CA 94720

     fbois@diana.lbl.gov

   -- Revisions -----
     Logfile:  SCCS/s.sim.c
    Revision:  1.48
        Date:  14 Nov 1997
     Modtime:  06:39:06
      Author:  @a
   -- SCCS  ---------

   Entry point and main simulation routines for 'sim' program.

*/

#ifdef _MACOSLEVEL2_
#include <PPC Macheaders>
#include "mac.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <assert.h>

#include "yourcode.h"
#include "getopt.h"
#include "mh.h"
#include "optdesign.h"
#include "lexerr.h"
#include "lsodes.h"
#include "sim.h"
/* #include "simhelp.h" */
#include "simi.h"
#include "siminit.h"
#include "simo.h"
#include "simmonte.h"
#include "strutil.h"


static char vszVersion[] = "v4.2.0"; /* Version of program */


/* -----------------------------------------------------------------------------
   CorrectInputToTransition

   resets the integrator and inputs when an input transition occurs.

   returns the simulation time pexp->dTime and input values to
   the input discontinuity, or transition point *pdTtrans.

   The inputs are updated to reflect their state just after the
   transition.  The integrator is initialized for a new segment.

   This does NOT affect state and output definitions.
*/

void CorrectInputToTransition (PEXPERIMENT pexp, PDOUBLE pdTtrans)
{
  pexp->dTime = *pdTtrans;
  UpdateInputs (&pexp->dTime, pdTtrans);

} /* CorrectInputToTransition */


/* -----------------------------------------------------------------------------
   Euler

   Simple Euler integrator.

*/

int Euler (long neq, double *y, double *t, double tout, double dTStep)
{
  static PDOUBLE rgdDeriv;
  double dTmp_step;
  long   i;

  if (!(rgdDeriv))
    if ( !(rgdDeriv = InitdVector (neq)))
      ReportError (NULL, RE_OUTOFMEM | RE_FATAL, "Euler", NULL);

  /* Iterate through time out to prescrebed output */
  while (*t < tout) {

    /* Compute derivatives at current time */
    CalcDeriv (y, rgdDeriv, t);

    /* Update time */
    *t = *t + dTStep;

    /* But do not exceed prescribed tout */
    if (*t > tout) {
      dTmp_step = tout - (*t - dTStep);
      *t = tout;
    }
    else
      dTmp_step = dTStep;

    /* Update the state variables */
    for (i = 0; i < neq; i++)
      y[i] = y[i] + dTmp_step * rgdDeriv[i];

    DoStep_by_Step();
  }

  /* Calculate the derivatives at t = tout for cleanliness */
  CalcDeriv (y, rgdDeriv, t);

  return (0);

} /* Euler */


/* -----------------------------------------------------------------------------
   FreeVarMod

   Callback for FreeList().

   Frees the memory for one parameter modification.  If the parameter
   is an input, the memory allocated for the input function is also
   free'd.  Note that FreeList() will pass the data as a PVOID which
   needs to be re-cast.
*/

void FreeVarMod (PVOID pData)
{
  PVARMOD pvarmod = (PVARMOD) pData;

  if (IsInput (pvarmod->hvar))
    if (pvarmod->uvar.pifn) free (pvarmod->uvar.pifn);

  free (pvarmod);

} /* FreeVarMod */


/* -----------------------------------------------------------------------------
   ModifyOneParm

   Callback function for ModifyParms.
*/

int ModifyOneParm (PVOID pData, PVOID pNullInfo)
{
  PVARMOD pvarmod = (PVARMOD) pData;

  if (IsInput(pvarmod->hvar))
    SetInput (pvarmod->hvar, pvarmod->uvar.pifn);
  else
    SetVar (pvarmod->hvar, pvarmod->uvar.dVal);

  return 0;

} /* ModifyOneParm */


/* -----------------------------------------------------------------------------
   ModifyParms

   Modifies the parameters in the plistParmMods LIST of the experiment
   spec by call ForAllList to increment through the list.
*/

void ModifyParms (PLIST plistParmMods)
{

  assert (plistParmMods);
  ForAllList (plistParmMods, &ModifyOneParm, NULL);

} /* ModifyParms */


/* ----------------------------------------------------------------------------
   DoOneExperiment

   Runs one experiment - return 1 on success and 0 in case of errors
*/

int DoOneExperiment (PEXPERIMENT pexp)
{

  double dTout;     /* next output time */
  double dTtrans;   /* next exposure transition time */
  double dTup;      /* the smaller one of dTout or dTtrans*/
  int    iOut;      /* index to next output time */
  PMODELINFO pmod;  /* pointer to the current model info */
  PINTSPEC   pis;   /* pointer to the integrator specs */

  if (!pexp) return 0;

  pmod = pexp->pmodelinfo;
  pis  = &(pexp->is);

  if (!InitOutputs (pexp, &iOut, &dTout)) return 0;

  UpdateInputs (&pexp->dT0, &dTtrans); /* Resolve dependent inputs */

  if (pexp->dT0 > dTtrans) {
    printf ("\nError: starting time is greater than first discontinuity,"
            "       check your inputs - Exiting.\n");
    exit (0);
  }

  if (pexp->dT0 > dTout) {
    printf ("\nError: starting time is greater than first output time,"
            "       check your outputs - Exiting.\n");
    exit (0);
  }

  pexp->dTime = pexp->dT0;

  /* set lsodes return flag to 1 for first call */
  pis->iDSFlag = 1;

  /* Iterate to final time */
  while (pexp->dTime < pexp->dTfinal) {

#ifdef _MACOSLEVEL2_
    /* this allows background processing on the Macintosh */
    HandleEvent();
#endif

    /* If dynamics equations are defined */
    if (pmod->nStates > 0) {

      /* the upper limit of integration dTup should be either dTout
         or dTtrans, whichever is smaller */
      dTup = (dTout < dTtrans) ? dTout : dTtrans;

      if (pis->iAlgo == IAL_LSODES) { /* Lsodes algorithm */

        pis->rwork[0] = dTup; /* do not overshoot dTup - FB 01/07/97 */

        lsodes_(&pmod->nStates, pmod->pdModelVars, &(pexp)->dTime,
                &dTup, &pis->itol, &pis->dRtol, &pis->dAtol,
                &pis->itask, &pis->iDSFlag, &pis->iopt, pis->rwork,
                &pis->lrw, pis->iwork, &pis->liw, &pis->iMf);

        /* Handle error returns : FB 25/11/96 : */
        if (pis->iDSFlag < 0) {
          /* We cannot guarantee the accuracy of the results, exit the routine
             with an error flag */
          return (0);
        }
      }
      else {
        if (pis->iAlgo == IAL_EULER) { /* Euler algorithm */
          Euler (pmod->nStates, pmod->pdModelVars, &(pexp)->dTime, dTup,
                 pis->dTStep);
        }
      }
    }
    else {
      /* We still need to advance the time */
      pexp->dTime = (dTout < dTtrans) ? dTout : dTtrans;
    }

    if (dTtrans <= dTout) {
      /* dTime == dTtrans <= dTout: we are at a discontinuity.
         This point belongs to the NEW period UNLESS we are at
         the final time */
      if (dTtrans < dTout) {
        if (dTtrans < pexp->dTfinal) {
          CorrectInputToTransition (pexp, &dTtrans);
          pis->iDSFlag = 1;
        }
      }
      else {
        /* dTtrans == dTout */
        if (dTtrans < pexp->dTfinal) {
          CorrectInputToTransition (pexp, &dTtrans);
          pis->iDSFlag = 1;
        }
        SaveOutputs (pexp, &dTout);
        NextOutputTime (pexp, &dTout, &iOut);
      }
    }
    else {
      /* dTime == dTout < dTtrans: */
      SaveOutputs (pexp, &dTout);
      NextOutputTime (pexp, &dTout, &iOut);
    }

  } /* while dTime < final time */

  /* success */
  return 1;

} /* DoOneExperiment */


/* ----------------------------------------------------------------------------
   DoOneNormalExp

   Does one AT_DEFAULTSIM simulation.

   Return 1 on success and 0 in case of failure
*/

int DoOneNormalExp (PANALYSIS panal, PEXPERIMENT pexp)
{
  printf (" %d", pexp->iExp); /* Show what experiment it is */

  InitModel ();
  ModifyParms (panal->expGlobal.plistParmMods); /* Global modifications */
  ModifyParms (pexp->plistParmMods); /* Mods for this experiment */
  if (!DoOneExperiment (pexp)) {
    /* Error */
    return 0;
  }

  printf ("\n");

  return (1);

} /* DoOneNormalExp */


/* ----------------------------------------------------------------------------
   DoOneMCExp

   Does one AT_MONTECARLO simulation.

   Can maybe merge this with DoOneNormalExp() in the future.

   The major issue is the order of setting parameters.  For each
   experiment in a Monte Carlo run of an analysis, the order must be
   as follows:

   Each Run
    calc mc mods

     Each Experiment
     1)  Init the model
     2)  Global parm mods
     3)  Monte Carlo mods
     4)  Local mods override everything

   The problem becomes that for the simulation to be started over
   again, the inputs have to be told to initialize and parm mods for
   the current experiment must be made (body weight, etc).  This
   currently won't happen unless the model is init'd.  Maybe create a
   ResetInputs() with starting time which will do the funky stuff
   done by the global variable right now.

   Return 1 on success and 0 in case of failure
*/

int DoOneMCExp (PANALYSIS panal, PEXPERIMENT pexp)
{
  register MONTECARLO *pmc = &panal->mc;

  InitModel ();
  ModifyParms (panal->expGlobal.plistParmMods); /* Global modifications */
  SetParms (pmc->nParms, pmc->rghvar, pmc->rgdParms); /* MC mods */
  ModifyParms (pexp->plistParmMods); /* Mods for this experiment */
  if (!DoOneExperiment (pexp)) {
    /* Error */
    return 0;
  }

  return (1);

} /* DoOneMCExp */


/* -----------------------------------------------------------------------------
   DoNormal

   Does a normal analysis
*/

void DoNormal (PANALYSIS panal)
{
  int nExps = panal->expGlobal.iExp;
  int i;

  printf ("\nDoing analysis - %d normal experiment%c\n", nExps,
       (nExps > 1 ? 's' : ' '));

  for (i = 0; i < nExps; i++) {
    if (DoOneNormalExp (panal, panal->rgpExps[i])) {
      /* if successfull write out the results */
      WriteNormalOutput (panal, panal->rgpExps[i]);
    }
    else
      printf ("Warning: Integration failed - No output generated\n");
  }

} /* DoNormal */


/* ----------------------------------------------------------------------------
   DoMonteCarlo

   Does a Monte Carlo analysis or a Set Points analysis.  The latter is
   handled here because the looping is basically the same, with one
   difference.

   If the number of runs (nRuns) for SetPoints() analysis is
   specified to be zero, set points are read from the set points file
   until end of file is reached.  Otherwise, the number of runs
   explicity stated are read.  Not having enough points in the file
   in this latter case yields an error.

   If nRuns == 0, the test at the end of the while{} loop is not
   used, and the decision to continue is made by the return value of
   GetMCMods().  Since for MonteCarlo analyses, this return value is
   always TRUE (i.e. you can always pick another random number),
   nRuns is locally modified to 1, if it has been spec'd to zero,
   thus preventing the the Monte Carlo's from accidentaly running
   forever.
*/

void DoMonteCarlo (PANALYSIS panal)
{
  int nExps = panal->expGlobal.iExp;
  long nRuns = panal->mc.nRuns;
  MCPREDOUT mcpredout;
  BOOL bOK = FALSE, bNotDone; /* Not finished with analysis */
  int i;

  mcpredout.pred = NULL;
  InitRandom (panal->dSeed, TRUE);
  if (panal->iType == AT_MONTECARLO && nRuns <= 0)
    nRuns = 1; /* Don't let MonteCarlo run forever */

  /* if cannot open files, Abort */
  if (OpenMCFiles (panal)) exit(0);

  printf ("\nDoing analysis - %ld %s run%c... %d experiment%c%s\n",
          nRuns,
          (panal->iType == AT_MONTECARLO ? "Monte Carlo" : "Set point"),
          (nRuns != 1 ? 's' : ' '),
          nExps, (nExps > 1 ? 's' : ' '),
          (nRuns != 1 ? " each" : " "));

  if (!nRuns)
    printf ("0 runs specified for SetPoint().  Reading entire file.\n\n");

  /* FB 21/07/97: dependencies between parameters are handled here. Pointers 
     to the parent parameters are stored in hparm of the pmcvar structure for
     each parameter. We now have to make pdParms point to the parent's dVals. */
  if (panal->iType == AT_MONTECARLO) {
    SetParents (&panal->mc, 0); /* start at 0, do them all */
  }
  else { /* panal->iType == AT_SETPOINTS */
    SetParents (&panal->mc, panal->mc.nSetParms);
  }

  panal->mc.lRun = 0; /* First run */
  bNotDone = TRUE;

  while (bNotDone) {

    bNotDone = GetMCMods (panal, NULL); /* Mods for this run */

    if (bNotDone) {
      /* Do analysis if not finished */
      for (i = 0; i < nExps; i++) { /* Do all experiments */
        bOK = DoOneMCExp (panal, panal->rgpExps[i]);
        if (!bOK) break;
      }

      if (bOK) {
        /* If successful write results */
        TransformPred (panal, &mcpredout); /* transform output run */
        WriteMCOutput (panal, &mcpredout);
      }
      else
        printf ("Warning: Integration failed on iteration %ld, experiment %d:\n"
                "         No output generated\n", panal->mc.lRun+1, i+1);
    } /* if bNotDone */

    panal->mc.lRun++; /* Next run */
    if (nRuns) /* If a number of runs spec'd... */
      bNotDone = (panal->mc.lRun < nRuns);

  } /* while */

  CloseMCFiles (panal);

} /* DoMonteCarlo */


/* -----------------------------------------------------------------------------
   DoAnalysis

   Does the analysis in the given specification.
*/

void DoAnalysis (PANALYSIS panal)
{

  switch (panal->iType) {

    default:
    case AT_DEFAULTSIM:
      DoNormal (panal);
      break;

    case AT_SETPOINTS: /* Not really Monte Carlo */
    case AT_MONTECARLO:
      DoMonteCarlo (panal);
      break;

    case AT_MCMC:
      DoMarkov (panal);
      break;

    case AT_OPTDESIGN:
      DoOptimalDesign (panal);
      break;

  } /* switch */

  if (panal->expGlobal.os.pfileOut) {
    fclose (panal->expGlobal.os.pfileOut);
    printf ("Wrote output file \"%s\"\n", panal->expGlobal.os.szOutfilename);
  }

} /* DoAnalysis */


/* -----------------------------------------------------------------------------
   MCVarListToArray

   converts a list of MCVAR to an array.  This must be a callback for
   ForAllList() since we are making the change here that will let us
   not to be forced to use list traversal in the future.
*/

MCVAR **vrgpMCVar; /* Avoid hairy pointers in here */
int   viMCVar;     /* Index to the array */

int MCVarListToArray (PVOID pv_pMCVar, PVOID pv_Null)
{

  vrgpMCVar[viMCVar] = (MCVAR *) pv_pMCVar; /* Copy the pointer and.. */
  viMCVar++; /* Advance to next element of array */
  return 1;

} /* MCVarListToArray */


/* ----------------------------------------------------------------------------
   PrepAnalysis

   makes the ANALYSIS structure easier to work with in the simulation
   code.  Specifically, changes lists to arrays.
*/

void PrepAnalysis (PANALYSIS panal)
{
  register MONTECARLO *pmc = &panal->mc;
  register int l;

  pmc->nParms = ListLength (pmc->plistMCVars);
  /* avoid zero pmc->nParms which can confuse some implementations of
     malloc. If pmc->nParms is zero  no use is going to be made of these
     arrays anyway */
  if (pmc->nParms == 0) return;
  
  pmc->rgdParms = InitdVector (pmc->nParms);
  pmc->rgpMCVar = (MCVAR **) malloc((pmc->nParms)*sizeof(MCVAR *));
  if (!(pmc->rgdParms && pmc->rgpMCVar))
    ReportError (NULL, RE_OUTOFMEM | RE_FATAL, "PrepAnalysis", NULL);

  /* Address of the first pointer */
  vrgpMCVar = &pmc->rgpMCVar[0];

  /* Initialize global array index */
  viMCVar = 0;
  ForAllList (pmc->plistMCVars, MCVarListToArray, (PVOID) NULL);
  FreeList (&pmc->plistMCVars, NULL, FALSE);

  /* Make a handle vector for theta */
  pmc->rghvar = (HVAR *) malloc((pmc->nParms)*sizeof(HVAR));
  if (pmc->rghvar) {
    for (l = 0; l < pmc->nParms; l++)
      pmc->rghvar[l] = pmc->rgpMCVar[l]->hvar;
  }
  else
    ReportError (NULL, RE_OUTOFMEM | RE_FATAL, "PrepAnalysis", NULL);

} /* PrepAnalysis */


/* Get the command line argument stuff */

/* -----------------------------------------------------------------------------
   SansPath

   returns a pointer to just the filename of a full path.
*/
/*
char *SansPath (char *szFullPathname)
{
  register char *szFile;

  if ((szFile = szFullPathname))
    while (*szFullPathname) {
      if (*szFullPathname == '/')
        szFile = szFullPathname+1;
      szFullPathname++;
    }

  return szFile;

} */ /* SansPath */


/* -----------------------------------------------------------------------------
   PromptFilenames

   prompts for both input and output file names.  The space allocated
   for inputting the files is reallocated to their actual size.
*/

void PromptFilenames (PSTR *pszFileIn, PSTR *pszFileOut)
{
  *pszFileIn = (PSTR) calloc (1, 80);
  *pszFileOut = (PSTR) calloc (1, 80);

  printf ("Input filename? ");
#ifdef _MACOSLEVEL1_
  scanf ("%[^:\f\r\v\n]", *pszFileIn);
  getchar();
#else
  gets (*pszFileIn);
  *pszFileIn = strtok (*pszFileIn, " \t");
#endif

  if (!(*pszFileIn)) /* Nothing entered, quit */
    return;

  if ((*pszFileIn)[0]) { /* Input file specified */
    printf ("Output filename? ");
#ifdef _MACOSLEVEL1_
    scanf ("%[^:\f\r\v\n]", *pszFileOut);
#else
    gets (*pszFileOut);
    *pszFileOut = strtok (*pszFileOut, " \t");
#endif
  }

  if (!(*pszFileOut) || !(*pszFileOut)[0]) { /* If no output specified */
    free (*pszFileOut);                      /* .. use default later */
    *pszFileOut = NULL;
  }
  else {
    *pszFileIn = (PSTR) realloc (*pszFileIn, MyStrlen(*pszFileIn) + 1);
    *pszFileOut = (PSTR) realloc (*pszFileOut, MyStrlen(*pszFileOut) + 1);
  }

} /* PromptFilenames */


/* -----------------------------------------------------------------------------
   Command Line Parsing

*/

/*
#define WarnArgumentReqd(szOption, szArg) \
  printf ("* Command-line option \"%s\" requires an argument \"%s\"\n",\
          szOption, szArg);

#define WarnUnknownArg(szOption, szArg) \
  printf ( "* Unknown argument \"%s\" to option \"%s\"\n", szArg, szOption);


void GetOutputFlagOption (PANALYSIS panal, char *optarg)
{
  WarnUnknownArg ("-O", optarg);

} */ /* GetOutputFlagOption */


/* -----------------------------------------------------------------------------
   GetFilenames

   retrieves options and filenames from the command line arguments passed to
   the program.

   The command line syntax is:

     mcsim [-options | --options] [input-file [output-file]]

   If the output filename is not given a default is used.
   If neither the input, nor output filenames are given, the
   program prompts for them both.

   The options can appear anywhere in the line and in any order.

   The options are parsed with _getopt(). After _getopt() is called,
   the args in rgszArg have been permuted so that non-option args are
   first, which in this case means the filenames.

   Uses the following globals:

     char *optarg;    -- Contains the string argument to each option in turn
     int   optind;    -- Index in ARGV of the next elem to be scanned
     char *nextchar;  -- The next char to be scanned in the option-element
     int   opterr;    -- 0 value flags to inhibit GNU error messages

*/

static char vszOptions[] = "h:H:D:";

void GetCmdLineArgs (int cArg, char *const *rgszArg, PSTR *pszFileIn, 
                     PSTR *pszFileOut, PANALYSIS panal)
{
  int c;

  *pszFileIn = *pszFileOut = (PSTR) NULL;

  while (1) {

    c = _getopt (cArg, rgszArg, vszOptions);
    if (c == EOF) /* Finish with option args */
      break;

    switch (c) {
      case '?':
        optarg = 0;
        /* Fall through! */

      case 'H':
      case 'h':
        /* if (optarg && *optarg) disabled for now, not up to date
          ShowHelp (optarg);
        else
          ShowHelpMessage (SansPath (rgszArg[0])); */
        exit (-1);
        break;

      case 'D':
        printf (">> Debug mode: Using option '%s'\n", optarg);
        /* Could setup to run with certain debug flags, not enabled  */
        break;

      default:
        printf ("Unknown option in command-line, %c = code 0%o ?\n", c, c);
        break;

    } /* switch */

  } /* while */

  switch (cArg - optind) { /* Remaining args are  filenames */
    case 2: /* Output and input file specificed */
      *pszFileOut = rgszArg[optind + 1];

      /* Fall through! */

    case 1: /* Input file specificed */
      *pszFileIn = rgszArg[optind];
      break;

    case 0: /* No file names specified */
      PromptFilenames (pszFileIn, pszFileOut);
      break;

    default:
      /* ShowHelp ("Usage"); */ /* disabled for now, not updated */
      exit (-1);
      break;
  } /* switch */

  while (*pszFileIn && (*pszFileIn)[0] &&      /* Files specified   */
         !MyStrcmp(*pszFileIn, *pszFileOut)) { /* and not different */

    printf ("\n** Input and output filename must be different.\n");
    PromptFilenames (pszFileIn, pszFileOut);

  } /* while */

  if (!(*pszFileIn && (*pszFileIn)[0])) { /* no input name given is an error */
    printf ("Error: an input file name must be specified - Exiting\n\n");
    exit (-1);
  }

} /* GetCmdLineArgs */


/* -----------------------------------------------------------------------------
*/
void AnnounceProgram (void)
{
  printf ("\n________________________________________\n");
  printf ("\nMCSim %s\n\n", vszVersion);
  printf ("Copyright (c) 1993-1997 by F. Bois & D. Maszle. "
          "All rights reserved.\n\n");

  printf ("MCSim comes with ABSOLUTELY NO WARRANTY;\n"
          "This is free software, and you are welcome to redistribute it\n"
          "under certain conditions; see the GNU General Public License.\n\n");

  printf ("* Using `%s' model in file \"%s\" created by %s\n\n",
          szModelDescFilename, szModelSourceFilename, szModelGenAndVersion);

} /* AnnounceProgram */


/* -----------------------------------------------------------------------------
   main

   Entry point for simulation and analysis program.
*/

int main (int nArg, char *const *rgszArg)
{
  PSTR szFileIn, szFileOut;
  INPUTBUF ibIn;
  PANALYSIS panal = (PANALYSIS) malloc (sizeof(ANALYSIS));

#ifdef _MACOSLEVEL2_
  InitMacintosh ();
  while (1) HandleEvent();
#endif

  AnnounceProgram ();

  if (!panal)
    ReportError (NULL, RE_OUTOFMEM | RE_FATAL,
                 "ANALYSIS specification too large", NULL);

  InitAnalysis (panal);
  GetCmdLineArgs (nArg, rgszArg, &szFileIn, &szFileOut, panal);

  /* Define the output file as the global experiment default  */
  panal->expGlobal.os.szOutfilename = szFileOut;
  szFileOut == NULL ? (panal->expGlobal.os.bCommandLineSpec = FALSE) :
                      (panal->expGlobal.os.bCommandLineSpec = TRUE);

  if (!InitBuffer (&ibIn, szFileIn))
    ReportError (&ibIn, RE_INIT | RE_FATAL, "ReadInput", NULL);

  ibIn.pInfo = (PVOID) panal; /* Attach analysis specification to input */

  if (ReadAnalysis (&ibIn)) {
    PrepAnalysis (panal);
    DoAnalysis (panal);
  } /* if */
  FreeLevels(panal);

#ifdef _MACOSLEVEL1_
  printf ("Done.(Hit Return)\n\n");
#endif

  return 0;

} /* main */
