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

/* in this file, bcopy() is used for overlapping moves */
#define bcopy(src, dest, len)        memmove(dest, src, len)

/* this is the number of digits to output when writing a constant */
#define DBL_DIGITS        15

int NiceDTOC(scalar r,
             char *Out)
{
    register char *sp;
    char *dp, *ep, *zp, expchar;

    if (Lang->flags & LANG_STMT_UPPER) {
        sprintf(Out, "%.*G", DBL_DIGITS, r);
        expchar = 'E';
    } else {
        sprintf(Out, "%.*g", DBL_DIGITS, r);
        expchar = 'e';
    }

    /* If there's a decimal point, remove unnecessary trailing zeroes.
     */
    if (dp = strchr(Out, '.')) {
         zp = &Out[strlen(Out)];           /* pts to zero at end */
        /* find last digit before exponent */
        if (!(ep = strchr(Out, expchar))) 
             ep = zp;
        sp = ep - 1;
        while (sp != dp && *sp == '0')
            /* sp moves back to point at 1st non-zero or dp */
            --sp;
        /* if this leaves a trailing decimal pt, remove it */
        if (sp == dp) --sp;
        /* now copy the exponent (or null) after the 1st non-zero */
        bcopy(ep, sp + 1, (zp-ep)+1);
    }

    if (Lang == &Pascal_language || Lang == &Ada_language) {
        /* insert leading zero before decimal point if not present already */
        if (Out[0] == '.') {
            bcopy(Out, Out + 1, strlen(Out) + 1);
            Out[0] = '0';
        } else if (Out[0] == '-' && Out[1] == '.') {
            bcopy(Out + 1, Out + 2, strlen(Out + 1) + 1);
            Out[1] = '0';
        }
    } else {
        if (Out[0] == '0' && Out[1] == '.')
            /* delete 0 before . */
            bcopy(Out + 1, Out, strlen(Out + 1) + 1);
        else if (Out[0] == '-' && Out[1] == '0')
            /* delete 0 before -0. */
            bcopy(Out + 2, Out + 1, strlen(Out + 2) + 1);
    }
    if (sp = strchr(Out, expchar)) {
        if ((Lang == &Pascal_language || Lang == &Ada_language) &&
          !strchr(Out, '.')) {
            /* if no ., add .0 before exponent */
            bcopy(sp, sp + 2, strlen(sp) + 1);
            *sp++ = '.';
            *sp++ = '0';
        }
        if (!gSinglePrecision &&
          (Lang == &FORTRAN_language || Lang == &ADSIM_language))
            *sp += 'D' - 'E';
        dp = ++sp;        /* past E or D */
        if (*dp == '-')
                dp++;        /* keep negative exponent sign */
        while (*++sp == '0' && sp[1]) ;                /* skip leading zeros */
        bcopy(sp, dp, strlen(sp) + 1);
        return (int)(dp - Out) + strlen(dp);
    } else {
        int len = strlen(Out);

        if (gSinglePrecision ||
          Lang != &FORTRAN_language && Lang != &ADSIM_language) {
            if (!strchr(Out, '.')) {        /* if no '.', append one */
                if (Lang == &Pascal_language || Lang == &Ada_language) {
                    /* brain-damaged languages need digits before and after . */
                    strcpy(Out + len, ".0");
                    return len + 2;
                } else {
                    strcpy(Out + len, ".");
                    return len + 1;
                }
            } else
                return len;
        } else {
            strcpy(Out + len, "D0");
            return len + 2;
        }
    }
}
