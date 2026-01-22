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

#include "calc.h"
#include "calcprot.h"
#include "language.h"

#include <stdio.h>
#include <ctype.h>

/*
eprintf(format [, arg] ... )
char *format;

efprintf(stream, format [, arg] ... )
FILE *stream;
char *format;

char *esprintf(s, format [, arg] ... )
char *s, *format;

eprintf/efprintf/esprintf is an extended printf useful for multi-language
program generators.  The format string may contain standard printf()
specifications and extensions described below.  Before each formatted string or
group of characters between format specifications or \n is output, a
continuation to the next line is output if the characters will not fit on the
current line.

Output is in two modes, normal and comment.  The current mode may change the
type of continuation to use, and whether text is converted to upper case.  The
mode and current lexical level are saved between calls to efprintf (but not
separately for each stream).

The current language struct determines several output characteristics,
including:

        Maximum line length
        Line prefixes, suffixes, and indents for normal and comment mode
        Conversion to upper-case
        Subscript offset (0 or 1)
        Various predefined strings

The printf() flag, field width, and precision characters:
        0-9,.,-,+, ,#,*,l,h
and specifications:
        c,d,e,E,f,g,G,o,s,u,x,X,%,i,p,n

are passed through to printf().  Each converted output string (except for the
special case of a simple %s) must be less than 200 characters.  The following
extended flag characters are recognized:

        @        With an integer, causes the subscript offset to be added to the
                number.  With %s, forces the string never to be converted to
                uppercase.  With any other type of conversion, @ is ignored.
                This is useful for making sure that conversions don't get
                expanded into SCCS strings.
        :x        where x is [H-W], "define" the output of %H thru %W.
                Argument(s) (possible * arguments and the data) are consumed,
                but nothing is immediately output.  Definitions are static, and
                therefore saved between calls to efprintf.  Upper-case
                conversion is determined at this time.

The following additional format specifications are recognized.  These must
appear without any flags (except for @), field width, or precision:

        %H - %W        Output the string defined with the : flag.
        %A      Outputs the external name prefix, e.g. `sd'.
        %B        A string argument is output at column 1, followed by any
                remaining indent (useful for FORTRAN statement labels).
        %D        Output math function prefix (d for double in FORTRAN)
        %(        Output a subscript opening delimiter
        %,        Output a between subscript delimiter
        %)        Output a subscript closing delimiter
        %{        Start a comment and enter comment mode
        %}        End a comment, including end-of-line, and return to normal mode
        %;        Output a statement terminator, NOT including end-of-line.
        %=        Output an assignment operator
        %r        Language and precision-appropriate real number conversion
        %t        Language and precision-appropriate real number type name
        %>        Increase lexical level (amount of indent).  Statement mode
                defaults to 1, while comment mode defaults to 0.  A %>
                after a %{ on the same line will be aligned with statements at
                the default lexical level (useful for commented out statements).
        %<        Decrease lexical level.  Below 0 is treated like 0.
        %&        Outputs nothing; used for separating tokens for line breaks

This routine is dependent on a character set in which 'H' thru 'Z' are
contiguous.
*/
extern int sd_check_for_interrupt(void);

static int linepos, cmtmode, stmt_lexlev;
int lexlev;
static FILE *stream;
static char *output_str;

/* CMT_MODE
 *
 * Returns non-zero if we're currently in a comment.
 */
int 
CMT_MODE(void)
{
    return (cmtmode > 0);
}

static void
do_eprintf(
    void (*out_func)(char *, int, int, int),
    register char *fmt,
    va_list argptr)
{
    char ofmt[50], *percent_pos, outtmp[200], *outstr, *deststr;
    static char macros['W' - 'H' + 1][200];

    while (*fmt) {
        if (percent_pos = strchr(fmt, '%')) {
            int nindir, otype, subs_offset = 0, leave_lc = 1, begin_line = 0;
            register char *ofmtp, *cp;

            (*out_func)(fmt, (int)(percent_pos - fmt), 0, 0);
            fmt = percent_pos + 1;
            outstr = deststr = outtmp;
            outtmp[0] = '\0';        /* print nothing by default */
            if (*fmt == '@') {        /* special case of @ here */
                subs_offset = Lang->subs_offset;
                leave_lc = 2;        /* force %s to lower-case */
                fmt++;
            }
            switch (*fmt++) {
            case 'A':
                outstr = gPrefix;
                leave_lc = 0;
                break;
            case 'B':
                outstr = va_arg(argptr, char *);
                begin_line = 1;
                break;
            case 'D':
                if (!gSinglePrecision) {
                    outstr = Lang->dbl_prefix;
                    leave_lc = 0;
                }
                break;
            case '(':
                outstr = Lang->begin_subs;
                break;
            case ',':
                outstr = Lang->between_subs;
                break;
            case ')':
                outstr = Lang->end_subs;
                break;
            case ';':
                outstr = Lang->end_stmt;
                break;
            case '=':
                outstr = Lang->assign_op;
                break;
            case '{':
                cmtmode = 2;
                stmt_lexlev = lexlev;
                lexlev = 0;
                break;
            case '}':
                if (cmtmode) {
                    outstr = Lang->end_cmt +
                      (!linepos && *Lang->end_cmt == ' ');
                    lexlev = stmt_lexlev;
                } else
                    outstr = "\n";
                cmtmode = -1;
                break;
            case '>':
                lexlev++;
                break;
            case '<':
                lexlev--;
                break;
            case '&':
                break;
            case 'r':        /* real number conversion */
                NiceDTOC(va_arg(argptr, double), outtmp);
                break;
            case 's':        /* special case of 's' conversion to allow for long
                           strings and avoid copying */
                outstr = va_arg(argptr, char *);
                leave_lc--;        /* 0 only if no @ seen */
                break;
            case 't':
                outstr = gSinglePrecision ? Lang->single_decl :
                  Lang->double_decl;
                leave_lc = 0;
                break;
            default:
                if (fmt[-1] >= 'H' && fmt[-1] <= 'W') {
                    outstr = macros[fmt[-1] - 'H'];
                    break;
                }

                /* must be a standard printf() conversion character */
                fmt--;
                nindir = otype = 0;
                ofmt[0] = '%';
                ofmtp = ofmt + 1;
                while (otype < 2) {
                    *ofmtp++ = *fmt;
                    switch (*fmt++) {
                    case '*':
                        nindir++;
                        break;

                    case 'l':
                        otype = 1;
                        break;

                    case 'c':
                    case 'x':
                        leave_lc = 0;
                    case 'd':
                    case 'o':
                    case 'u':
                    case 'X':
                    case 'i':
                        otype = otype ? 3 : 2;        /* int or long types */
                        break;

                    case 'e':
                    case 'g':
                        leave_lc = 0;
                    case 'E':
                    case 'f':
                    case 'G':
                        otype = 4;        /* double types */
                        break;

                    case 's':
                        leave_lc--;        /* 0 only if no @ seen */
                    case 'p':
                    case 'n':
                        otype = 5;        /* pointer types */
                        break;

                    case '%':
                    case '\0':
                        otype = 6;        /* no argument consumed */
                        break;
 
                    case '@':
                        subs_offset = Lang->subs_offset;
                        leave_lc = 2;        /* force %s to lower-case */
                        ofmtp--;        /* don't send to sprintf() */
                        break;

                    case ':':
                        ofmtp--;        /* don't send to sprintf() */
                        deststr = macros[*fmt++ - 'H'];
                        break;
                    }
                }
                *ofmtp = '\0';

                /* this stuff is really sick, but I don't know of a better
                   portable method */
                if (!nindir) {
                    switch (otype) {
                    case 2:
                        sprintf(deststr, ofmt, va_arg(argptr, int) + subs_offset);
                        break;
                    case 3:
                        sprintf(deststr, ofmt, va_arg(argptr, long));
                        break;
                    case 4:
                        sprintf(deststr, ofmt, va_arg(argptr, double));
                        break;
                    case 5:
                        sprintf(deststr, ofmt, va_arg(argptr, void *));
                        break;
                    case 6:
                        sprintf(deststr, ofmt, ""); // dummy arg to kill warning
                        break;
                    }
                } else if (nindir == 1) {
                    int width = va_arg(argptr, int);

                    switch (otype) {
                    case 2:
                        sprintf(deststr, ofmt, width,
                          va_arg(argptr, int) + subs_offset);
                        break;
                    case 3:
                        sprintf(deststr, ofmt, width, va_arg(argptr, long));
                        break;
                    case 4:
                        sprintf(deststr, ofmt, width, va_arg(argptr, double));
                        break;
                    case 5:
                        sprintf(deststr, ofmt, width, va_arg(argptr, void *));
                        break;
                    case 6:
                        sprintf(deststr, ofmt, width);
                        break;
                    }
                } else {        /* indir must (better) be == 2 */
                    int width = va_arg(argptr, int);
                    int prec = va_arg(argptr, int);

                    switch (otype) {
                    case 2:
                        sprintf(deststr, ofmt, width, prec,
                          va_arg(argptr, int) + subs_offset);
                        break;
                    case 3:
                        sprintf(deststr, ofmt, width, prec, va_arg(argptr, long));
                        break;
                    case 4:
                        sprintf(deststr, ofmt, width, prec, va_arg(argptr, double));
                        break;
                    case 5:
                        sprintf(deststr, ofmt, width, prec, va_arg(argptr, void *));
                        break;
                    case 6:
                        sprintf(deststr, ofmt, width, prec);
                        break;
                    }
                }
            }        /* switch first char after % */
            if (!leave_lc && outstr == outtmp &&
              Lang->flags & (cmtmode < 1 ? LANG_STMT_UPPER : LANG_CMT_UPPER)) {
                for (cp = deststr; *cp; cp++)
                    if (islower(*cp))
                        *cp = toupper(*cp);
            }
            if (deststr == outtmp)        /* not a macro definition */
                (*out_func)(outstr, (int)strlen(outstr),
                  leave_lc || outstr == outtmp, begin_line);
        } else {        /* no % found */
            (*out_func)(fmt, (int)strlen(fmt), 0, 0);
            fmt = "";        /* make while loop get out */
        }
        if (cmtmode < 0)        /* flag for not indenting comment terminator */
            cmtmode++;
    }        /* while not end of format string */
    va_end(argptr);
}

static void
foutput_str(char *s, int len, int leave_lc, int begin_line)
{
    int indent = (lexlev > 0 ? lexlev : 0) * Lang->indent;

    /* This routine won't return if an interrupt is detected.  (Only 
     * does anything if we're in Applied Motion mode, that is, linked
     * in with the ApM engine.)
     */
    sd_check_for_interrupt();

    if (begin_line && !cmtmode && !linepos) {
        /* handle the special case of %B conversion.  It is assumed the string
           contains no newlines and is not longer than begin_stmt */
        linepos = fprintfcnt(stream, "%s%s%*s", s, Lang->begin_stmt + len,
          indent, "");
        return;
    }
    while (len) {
        register char *nlpos;
        uintptr_t sublen;

        if (*s == '\n') {
            s++;
            len--;
            if (!linepos)
                if (cmtmode == 2 && !(Lang->flags & LANG_LINE_COMMENTS)) {
                    char *cp;

                    /* start comment; assume first space is trailing */
                    for (cp = Lang->begin_cmt; *cp && *cp != ' '; cp++)
                        putc(*cp, stream);
                    cmtmode = 1;
                } else
                    fputs(Lang->spacer, stream);
            putc('\n', stream);
            linepos = 0;
        }
        for (nlpos = s; nlpos < s + len; nlpos++)
            if (*nlpos == '\n') {
                break;
            }
        if (!(sublen = nlpos - s))
            continue;
        if (cmtmode == 0 && linepos && Lang->max_line &&
          linepos + sublen > Lang->max_line) {
            fputs(Lang->cont_stmt, stream);
            putc('\n', stream);
            linepos = fprintfcnt(stream, "%s%*s", Lang->next_stmt, indent, "");
        } else if (!linepos && cmtmode > 0) {
            if (cmtmode == 2 || Lang->flags & LANG_LINE_COMMENTS)
                linepos = fprintfcnt(stream, "%s%*s", Lang->begin_cmt,
                  indent + (indent ? Lang->cmt_indent : 0), "");
            else
                linepos = fprintfcnt(stream, "%*s", indent, "");
        } else if (!linepos && cmtmode == 0) {
            linepos = fprintfcnt(stream, "%s%*s", Lang->begin_stmt, indent, "");
        }
        len -= (int)sublen;
        linepos += (int)sublen;
        if (!leave_lc &&
          Lang->flags & (cmtmode < 1 ? LANG_STMT_UPPER : LANG_CMT_UPPER)) {
            while (sublen--) {
                register char ch = islower(*s) ? toupper(*s) : *s;
                putc(ch, stream);
                s++;
            }
        } else {
            fwrite(s, sublen, 1, stream);
            s += sublen;
        }
    }
}

static void
soutput_str(char *s, int len, int leave_lc, int begin_line)
{
    while (len) {
        register char *nlpos;
        uintptr_t sublen;

        if (*s == '\n') {
            s++;
            len--;
            *output_str++ = '\n';
            *output_str = '\0';
        }
        for (nlpos = s; nlpos < s + len; nlpos++)
            if (*nlpos == '\n')
                break;
        if (!(sublen = nlpos - s))
            continue;
        len -= (int)sublen;
        if (!leave_lc &&
          Lang->flags & (cmtmode < 1 ? LANG_STMT_UPPER : LANG_CMT_UPPER)) {
            while (sublen--) {
                *output_str++ = islower(*s) ? toupper(*s) : *s;
                s++;
            }
        } else {
            memcpy(output_str, s, sublen);
            output_str += sublen;
            s += sublen;
        }
        *output_str = '\0';
    }
}

/*
 * The fprintf's in different Unix implementations assign 
 * different meanings to their return values.  In particular,
 * they don't always return a count of the number of bytes
 * output.  This routine acts like fprintf, but is guaranteed
 * to return a count in all environments (because vfprintf only
 * has one definition.)
 */
/*VARARGS*/
int 
fprintfcnt(FILE *stream, char *fmt, ...)
{
    va_list argptr;

    va_start(argptr, fmt);
    return vfprintf(stream, fmt, argptr);
}

/*VARARGS*/
void 
eprintf(char *fmt, ...)
{
    va_list argptr;

    va_start(argptr, fmt);
    stream = stdout;
    do_eprintf(foutput_str, fmt, argptr);
}

/*VARARGS*/
void 
efprintf(FILE *F, char *fmt, ...)
{
    va_list argptr;

    va_start(argptr, fmt);
    stream = F;
    do_eprintf(foutput_str, fmt, argptr);
}

/*VARARGS*/
char *
esprintf(char *ostr, char *fmt, ...)
{
    va_list argptr;

    va_start(argptr, fmt);
    output_str = ostr;
    output_str[0] = '\0';
    do_eprintf(soutput_str, fmt, argptr);
    return ostr;
}
