/* Copyright 2026 Michael Sherman
 * Copyright 1989-2025 PTC Inc.; 1984-1988 Symbolic Dynamics, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy
 * of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include <assert.h>
#include <ctype.h>
#include "libs.h"
#include "libsprot.h"
#include "words.h"

/*======================================================================*/
/* Procedures used for parsing and processing the input file (System). */
/*======================================================================*/

static int SKIP_BLANKS_ETC(FILE *System);
static int GETCH(FILE *System);

/*========*/
/* ASSERT */
/*========*/

void ASSERT(int cond,
       int num,
       char *pname)
{
    if (!cond) {
        fprintf(stderr, "\n***ASSERTION %d failed in %s\n", num, pname);
        abort();
    }
}

/*==========*/
/* USER_ERR */
/*==========*/

void USER_ERR(void)
{
    /* Prepare to issue user error. */

    fflush(stdout);
    fprintf(stderr, "\n\n*** ERROR: ");        /* begin all errors like this */
}

/*===========*/
/* USER_WARN */
/*===========*/

void USER_WARN(void)
{
    /* Prepare to issue user warning. */

    fflush(stdout);
    fprintf(stderr, "\n\n*** WARNING: ");  /* begin all warnings like this */
}

/*==============*/
/* ILLEGAL_CHAR */
/*==============*/

static char ILLEGAL_CHAR(int Ch)
{
    /* This routine defines the particular characters which are NEVER allowed
       in an input file.  Mainly this is a guard against accidental control
       characters. */

    return Ch != EOF && (!isascii(Ch) || !isprint(Ch) && !isspace(Ch));
}

/*=======*/
/* GETCH */
/*=======*/

static int GETCH(FILE *System)
{
    /* Get char.  We check for illegal characters here and bomb if we
       find one.  Normally just unprintable characters should be illegal. */
    int ch;

    ch = getc(System);
    if (ILLEGAL_CHAR(ch)) {
        USER_ERR();
        fprintf(stderr, "Illegal character (%d) found in input file.\n", ch);
        return 0;
    } else
        return ch;
}

/*=================*/
/* SKIP_BLANKS_ETC */
/*=================*/

static int SKIP_BLANKS_ETC(FILE *System)
{
    /* Scan to the first non-whitespace, non-comment following in */
    /* the file. */
    register int ch;

    for (;;) {
        if (ILLEGAL_CHAR(ch = GETCH(System)))
            return 0;
        if (feof(System))
            return EOF;
        if (!isspace(ch))
            if (ch == '#') {
                do
                    if (sdfast_opt.verbose > 1)
                        putchar(ch);
                while ((ch = GETCH(System)) != '\n' && !feof(System));
                if (feof(System))
                    return 0;
            } else
                break;
        if (sdfast_opt.verbose > 1)
            putchar(ch);
    }
    return ch;
}

/*============*/
/* WHATS_NEXT */
/*============*/

char WHATS_NEXT(FILE *System,
                ItemType_t *Next)
{
    /* skips blanks, tells what's coming up in the input */
    char ch;

    if (!(ch = SKIP_BLANKS_ETC(System)))
        return 0;
    if (feof(System))
        *Next = cEndOfFileNext;
    else {
        switch (ch) {
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
            case '-':
            case '+':
            case '.':
                *Next = cNumberNext;
                break;
            case '=': *Next = cEqualNext;
                break;
            case '?': *Next = cQuestionNext;
                break;
            default: *Next = cIdentNext;
                break;
        }
        ungetc(ch, System);
    }
    return 1;
}

/*==============*/
/* NUMERIC_NEXT */
/*==============*/

char NUMERIC_NEXT(FILE *System,
                  int *IsNumeric)
{
    /* true if the next thing is a number or a question mark. */
    /* Functional return is status.                           */
    ItemType_t NextThing;

    if (WHATS_NEXT(System, &NextThing)) {
        *IsNumeric = NextThing == cNumberNext || NextThing == cQuestionNext;
        return 1;
    } else
        return 0;
}

/*============*/
/* EQUAL_NEXT */
/*============*/

char EQUAL_NEXT(FILE *System,
                int *IsEqual)
{
    /* true if the next thing is an equal sign. */
    /* Functional return is status.             */
    ItemType_t NextThing;

    if (WHATS_NEXT(System, &NextThing)) {
        *IsEqual = NextThing == cEqualNext;
        return 1;
    } else
        return 0;
}

/*=========*/
/* READ_ID */
/*=========*/

char READ_ID(FILE *System,
             string32 Ident)
{
    /* reads an identifier from file "System" */
    char *cp = Ident;
    register int ch;
    int err = 0; /*assume failure*/

    if (!(ch = SKIP_BLANKS_ETC(System)))
        goto done;

    for (;;) {
        if (feof(System))
            break;
        if (isspace(ch) || ch == '=' || ch == '#') {
            ungetc(ch, System);
            break;
        }
        if (sdfast_opt.verbose > 1)
            putchar(ch);
        if ((int)(cp - Ident) < 32)
            *cp++ = ch;
        if (ILLEGAL_CHAR(ch = GETCH(System)))
            goto done;
    }

    err = 1; /* success */

  done:
    *cp = '\0';
    return err;
}

/*============*/
/* FIND_EQUAL */
/*============*/

char FIND_EQUAL(FILE *System)
{
    /* read whitespace from System until we see an equal sign. */
    register char ch;

    if (!(ch = SKIP_BLANKS_ETC(System)))
        return 0;
    if (feof(System)) {
        USER_ERR();
        fprintf(stderr, "Found eof while looking for an \"=\".\n");
        return 0;
    }
    if (ch != '=') {
        USER_ERR();
        fprintf(stderr, "Found character \"%c\" while looking for \"=\".\n",
          ch);
        return 0;
    }
    if (sdfast_opt.verbose > 1)
        putchar(ch);
    return 1;
}

/*==========*/
/* GET_INT  */
/*==========*/

char GET_INT(FILE *System,
             long *IntVal)
{
    char ch;

    if (!(ch = SKIP_BLANKS_ETC(System)))
        return 0;
    if (feof(System)) {
        USER_ERR();
        fprintf(stderr, "Found eof while looking for an int.\n");
        return 0;
    }
    ungetc(ch, System);
    int n = fscanf(System, "%ld", IntVal);
    assert(n == 1);
    if (sdfast_opt.verbose > 1)
        printf("%ld", *IntVal);
    return 1;
}

/*==========*/
/* GET_REAL */
/*==========*/

char GET_REAL(FILE *System,
              double *RealVal)
{
    char ch, ostr[50];

    if (!(ch = SKIP_BLANKS_ETC(System)))
        return 0;
    if (feof(System)) {
        USER_ERR();
        fprintf(stderr, "Found eof while looking for a number.\n");
        return 0;
    }
    ungetc(ch, System);
    int n = fscanf(System, "%lf", RealVal);
    assert(n == 1);

    NiceDTOC(*RealVal, ostr);
    if (sdfast_opt.verbose > 1)
        fputs(ostr, stdout);
    return 1;
}


/* GET_REAL_EXPR 
 *
 * If the next token is just a number, return the expression 
 * SC(GET_REAL) with QuesFlg=0.
 * If it is just a question mark, we return expression SC(0.0) 
 * with QuesFlg=ISQUESFLG.
 * If it's a number immediately followed by a question mark, we
 * return SC(GET_REAL) with QuesFlg=ISQUESFLG|HASNOMFLG.
 */
char GET_REAL_EXPR(FILE *System,
                   expr *Rexpr,
                   flags_t *QuesFlg)
{
    double RealVal;
    ItemType_t NextThing;
    int ch;

    if (!WHATS_NEXT(System, &NextThing)) return 0;
    switch (NextThing) {
        case cNumberNext:
            if (!GET_REAL(System, &RealVal)) return 0;
            *Rexpr = PERM(SC(RealVal));
            ch = getc(System);
            if (ch == '?') {
                if (sdfast_opt.verbose > 1)
                    putchar(ch);
                *QuesFlg = ISQUESFLG|HASNOMFLG;
            } else {
                ungetc(ch, System);
                *QuesFlg = 0;
            }
            break;
        case cQuestionNext:
            *Rexpr = SCALAR_ZERO();
            if (sdfast_opt.verbose > 1)
                putchar(getc(System));        /* absorb '?' */
            else
                (void) getc(System);        /* absorb '?' */
            *QuesFlg = ISQUESFLG;
            break;
        default:
            USER_ERR();
            fprintf(stderr, "Expected a number or a question mark.\n");
            return 0;
    }

    return 1;
}

/*========*/
/* LOOKUP */
/*========*/

int LOOKUP(char *ReservedWords[],
           char *Id)
{
    /* look up name in ReservedWords, return number or 0 if not found */
    string32 UpName;
    register Index_t idno;

    strcpy(UpName, Id);
    UPSTR(UpName);
    for (idno = 0; idno < cNumReservedWords; idno++)
        if (!strcmp(ReservedWords[idno], UpName))
            return idno + 1;
    return 0;
}
