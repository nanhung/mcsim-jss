/* lex.h

   written by Don Maszle
   13 October 1991

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
     Logfile:  SCCS/s.lex.h
    Revision:  1.14
        Date:  02 Jul 1997
     Modtime:  08:36:57
      Author:  @a
   -- SCCS  ---------

   Header file for Lexical parsing routines.

   Modified 21/3/97 by FB set MAX_ERRORS to zero.

*/

#ifndef _LEX_H_

/* ----------------------------------------------------------------------------
   Inclusions  */

#include "hungtype.h"

/* ----------------------------------------------------------------------------
   Constants  */

#define BUFFER_SIZE    0x1000     /* Size of input data buffer */
#define MAX_LEX        255        /* Max size of Lexical Element */
#define MAX_EQN        0x03FF     /* Max size of a string eqn */

/* ----------------------------------------------------------------------------
   Lexical types */

#define LX_NULL       0x0000
#define LX_IDENTIFIER 0x0001
#define LX_INTEGER    0x0002
#define LX_FLOAT      0x0004
#define LX_NUMBER     (LX_INTEGER | LX_FLOAT)
#define LX_PUNCT      0x0008
#define LX_STRING     0x0010

/* To avoid unmatched delimeters confusions in editor */

#define CH_LPAREN       ('(')
#define CH_RPAREN       (')')    
#define CH_LBRACKET     ('[')
#define CH_RBRACKET     (']')
#define CH_LBRACE       ('{')
#define CH_RBRACE       ('}')

/* Character constants for convenience */

#define CH_EOLN        ('\n')   /* End of line character */
#define CH_COMMENT     ('#')    /* One line Comment Char */
#define CH_STRDELIM    ('\"')   /* String delimiter */
#define CH_STMTTERM    (';')    /* Statement terminator */

/* Report Error constants -- Lex errors */

#define MAX_ERRORS 0

#define RE_FATAL        0x8000  /* Can be ORd to iCode to cause exit(1) */
#define RE_WARNING      0x4000  /* can be ORd to issue Warning instead */

#define RE_UNKNOWN      0x0000  /* Unspecified error */
#define RE_INIT         0x0001  /* Error during initialization */
#define RE_FILENOTFOUND 0x0002  /* Error opening file for I/O */
#define RE_CANNOTOPEN   0x0003  /* Cannot open file */
#define RE_OUTOFMEM     0x0004  /* Error allocating memory */

#define RE_UNEXPECTED   0x0011  /* Unexpected char in input */
#define RE_UNEXPNUMBER  0x0012  /* Unexpected number in input */
#define RE_EXPECTED     0x0013  /* Expected character szMsg[0] */
#define RE_LEXEXPECTED  0x0014  /* Expected szMsg lexical element */
#define RE_SYNTAXERR    0x0015  /* Let's make syntax errors fatal */

/* User defined errors starting with 0x0100 */

#define RE_USERERROR    0x0100  /* User Error prefix */

/* Model Generator Errors */

#define RE_MODERROR      0x0100             /* Model Generator error prefix */
#define RE_BADCONTEXT    (RE_MODERROR + 1)  /* Invalid context for ident */
#define RE_DUPDECL       (RE_MODERROR + 2)  /* Duplicate declaration */
#define RE_REDEF         (RE_MODERROR + 3)  /* Redefinition of parm */
#define RE_EQNTOOLONG    (RE_MODERROR + 4)  /* Equation too long for buffer */
#define RE_BADSTATE      (RE_MODERROR + 5)  /* Invalid state id */
#define RE_UNDEFINED     (RE_MODERROR + 6)  /* Undefined identifier */
#define RE_NODYNEQN      (RE_MODERROR + 7)  /* Missing dyn eqn for szMsg */
#define RE_NOINPDEF      (RE_MODERROR + 8)  /* Input not init'd */
#define RE_TOOMANYVARS   (RE_MODERROR + 9)  /* Too many variable decls */
#define RE_TOOMANYLEVELS (RE_MODERROR + 10) /* Too many levels of distrib dependency */
#define RE_TOOMANYINST   (RE_MODERROR + 11) /* Too many instances at given level */
#define RE_OPENLEVEL     (RE_MODERROR + 12) /* Unclosed level or experiment block */
#define RE_LEVINEXPT     (RE_MODERROR + 13) /* Level statement enclosed in experiment */
#define RE_BADDEPTH      (RE_MODERROR + 14) /* Experiment statement not lowest depth */
#define RE_BADOUTPUTVAR  (RE_MODERROR + 15) /* Bad syntax in Distrib(OutputVar, ...) */
#define RE_TYPENOTMCMC   (RE_MODERROR + 16) /* Level statement only allowed for MCMC */
#define RE_TOOMANYPVARS  (RE_MODERROR + 17) /* Too many variables in Print statement */
#define RE_NOPRINTSTATEMENT (RE_MODERROR + 18) /* No 'Print' for model var in 'Distrib' */
#define RE_UNEQUALNUMTIMES  (RE_MODERROR + 19) /* Model vars in 'Distrib' statement have != # 
                                                  of times in 'Print' statements */
#define RE_UNEQUALTIMES     (RE_MODERROR + 20) /* Model variables in 'Distrib' statement have
                                                  != times in 'Print' statements */
#define RE_DUPVARINPRINT    (RE_MODERROR + 21) /* Same var appears in 2 or more 'Print' statements */
#define RE_NODATASTATEMENT  (RE_MODERROR + 22) /* No 'Data' for model var in 'Distrib' */

#define RE_SIMERROR         0x0200             /* Simulation error prefix */
#define RE_ERRORSINEXP      (RE_SIMERROR + 1)  /* Errors reported, skipping exp */
#define RE_NOOUTPUTS        (RE_SIMERROR + 2)  /* No outputs specified */
#define RE_NOOUTTIMES       (RE_SIMERROR + 3)  /* No output times specified */
#define RE_INTEGERR         (RE_SIMERROR + 4)  /* Integrator reports error */
#define RE_SPECERR          (RE_SIMERROR + 5)  /* Errors in specification */
#define RE_DEFOTHER         (RE_SIMERROR + 6)  /* Defining other experiment */
#define RE_INSUF_POINTS     (RE_SIMERROR + 8)  /* Insufficient forced points */
#define RE_MAXMIN_RANGE     (RE_SIMERROR + 9)  /* Max < min */
#define RE_OUTISRESTART     (RE_SIMERROR + 10) /* Output and restart files have same name */

/* Run-time Errors */

#define RE_RUNTIMEERROR     0x0300
#define RE_BADNORMALSD      (RE_RUNTIMEERROR + 1)
#define RE_BADLOGNORMALSD   (RE_RUNTIMEERROR + 2)
#define RE_BADLOGNORMALMEAN (RE_RUNTIMEERROR + 3)
#define RE_BADUNIFORMDIST   (RE_RUNTIMEERROR + 4)
#define RE_UNKNOWNDIST      (RE_RUNTIMEERROR + 5)
#define RE_SAMPLEFAILURE    (RE_RUNTIMEERROR + 6)
#define RE_BADMODEL         (RE_RUNTIMEERROR + 7)

/* -----------------------------------------------------------------------------
   Typedefs */

  /* The INPUTBUF structure which is used for file I/O buffering */

typedef PSTR PBUF;


typedef struct tagINPUTBUF {
  PFILE pfileIn;    /* DOS file pointer */
  PBUF  pbufOrg;    /* Pointers for buffer Origin */
  PBUF  pbufCur;    /* ... Current point */
  int   iLineNum;   /* Line number in file */
  int   iLNPrev;    /* Prev line num.  For formatting Dynamics eqns */
  int   cErrors;    /* Count of Errors */

  PVOID    pInfo; /* Pointer to private user information */

} INPUTBUF, * PINPUTBUF;


typedef char PSTRLEX[MAX_LEX]; /* String of a lexical element */
typedef char PSTREQN[MAX_EQN]; /* String of an equation */


/* -----------------------------------------------------------------------------
   Macros */

#define EOB(pib) (!(pib)\
                  || ((!(pib)->pbufCur || !*(pib)->pbufCur)\
              && (!(pib)->pfileIn || feof((pib)->pfileIn))))

#define IsUnderscore(c)    ((c) == '_')
#define IsSign(c)    ((c) == '+' || (c) == '-')
#define IsComment(szLex) ((szLex) ? (*(szLex) == CH_COMMENT) : (0) )
#define IsString(szLex) ((szLex) ? (*(szLex) == CH_STRDELIM) : (0) )

#define ErrorsReported(pib) ((pib)->cErrors)
#define ClearErrors(pib) ((pib) ? (pib)->cErrors = 0 : 0)


/* -----------------------------------------------------------------------------
   Prototypes */

void    EatStatement (PINPUTBUF pib);
int     EGetPunct (PINPUTBUF pibIn, PSTR szLex, char chPunct);
int     ENextLex (PINPUTBUF, PSTRLEX, int);

int     FillBuffer (PINPUTBUF pibIn);
void    FlushBuffer (PINPUTBUF pibIn);

BOOL    GetFuncArgs (PINPUTBUF, int, PINT, PSTR);
void    GetIdentifier (PINPUTBUF pibIn, PSTR szLex);
void    GetNumber (PINPUTBUF pibIn, PSTR szLex, PINT piLexType);
int     GetOptPunct (PINPUTBUF, PSTR, char);
int     GetPunct (PINPUTBUF pibIn, PSTR szLex, char chPunct);
void    GetStatement (PINPUTBUF pibIn, PSTR szStmt);
void    GetaString (PINPUTBUF pibIn, PSTR szLex);

BOOL    InitBuffer (PINPUTBUF pibIn, PSTR szFullPathname);

void    MakeStringBuffer (PINPUTBUF pBuf, PINPUTBUF pStrBuf, PSTR sz);

char    NextChar (PINPUTBUF pibIn);
void    NextLex    (PINPUTBUF, PSTRLEX, PINT);
int     NextListItem (PINPUTBUF, PSTR, int, int, char);

void    PreventLexSplit (PINPUTBUF pibIn, int iOffset);

void    SkipComment (PINPUTBUF);
int     SkipWhitespace (PINPUTBUF pibIn);

#define _LEX_H_
#endif

/* End */


