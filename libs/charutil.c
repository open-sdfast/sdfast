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

#include "libs.h"
#include "libsprot.h"
#include <ctype.h>

/*=======*/
/* UPSTR */
/*=======*/

void UPSTR(char *S)
{
    /* upshifts the supplied string into itself */
    register char *cp;

    for (cp = S; *cp; cp++)
        if (islower(*cp))
            *cp = toupper(*cp);
}

/*============*/
/* TIME_STAMP */
/*============*/

void TIME_STAMP(string32 DateAndTime)
{
    /* Returns a string containing current date and time.  */
    /* The returned string may contain lower case letters. */
    string11 CurDate, CurTime;

    GETDATE(CurDate);
    GETTIME(CurTime);
    sprintf(DateAndTime, "%s %s", CurDate, CurTime);
}

/* ADD_EXT_TO_FILENAME
 *
 * If the passed-in file name does not have an explicit extension, that is,
 * a `.' occurring in the last segment of the pathname, we'll add the 
 * supplied extension to it unless it won't fit.  In case we *should*
 * add the extension but can't, we return 1.  Otherwise we return 0.
 *
 * We won't make S any longer than maxlen characters (that is, we assume
 * S is declared S[maxlen+1]).
 */

int
ADD_EXT_TO_FILENAME(char *S,
                    int maxlen,
                    char *Ext)
{
    char *slash;

    if ((slash = strrchr(S, PATHNAME_DELIM1)) || 
        (slash = strrchr(S, PATHNAME_DELIM2)) ||
        (slash = strrchr(S, PATHNAME_DELIM3)))
        slash++;
    else
        slash = S;

    /* slash now points to the first char in the last segment */

    if (!strchr(slash, '.')) {
        /* should add the extension */
        if (strlen(S)+strlen(Ext) > maxlen)
            return 1;
        strcat(slash, Ext);
    } 
    return 0;
}

/* GET_BASE_NAME 
 * Remove the directory and last extension on instr and put the result in 
 * outstr.  If this is VMS, the version number will get chopped off as well.
 */

void GET_BASE_NAME(char *instr,
              char *outstr)
{
    char *cp;

    if ((cp = strrchr(instr, PATHNAME_DELIM1)) ||
        (cp = strrchr(instr, PATHNAME_DELIM2)) ||
        (cp = strrchr(instr, PATHNAME_DELIM3)))
        cp++;
    else
        cp = instr;

    /* cp points to the first char in the last segment */

    strcpy(outstr, cp);
    if (cp = strrchr(outstr, '.'))
        *cp = '\0';
}

/* READSTR
 * Read a length-limited string of chars from a file.  
 * The rest of the line (beyond len chars) is discarded. 
 * The string `s' must be dimensioned s[len+1] or larger.
 *
 * If we hit EOF before seeing a newline we'll return 1,
 * otherwise 0.  You don't have to check the return value if
 * you don't care -- the returned string will just be NULL if
 * we hit EOF without seeing any characters.
 */
int
READSTR(FILE *F,
        int len,
        string32 s)
{
    register int i;

    if (!fgets(s, len+1, F)) {
        s[0] = '\0';
        return 1;
    }
    if (s[i = (int)strlen(s) - 1] == '\n')
        s[i] = '\0';
    else {
        while ((i = getc(F)) != EOF && i != '\n') ;
        if (i == EOF)
            return 1;
    }
    return 0;
}

/* READSTR32
 * Read string of up to 32 chars from a file.  See READSTR for other info.
 */

int
READSTR32(FILE *F,
          string32 s)
{
    return READSTR(F, 32, s);
}

