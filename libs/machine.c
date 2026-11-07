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

#ifdef _WIN32
#define _CRT_SECURE_NO_WARNINGS
#endif

/*==========================================================================*/
/*                     MACHINE DEPENDENT ROUTINES                           */
/*==========================================================================*/

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#ifndef _WIN32
#include <sys/resource.h>
#endif

#include "libs.h"
#include "libsprot.h"

/*===================================*/
/*            GETDATE                */
/* Return date in format DD-MON-YYYY */
/* DD is ' D' if day < 10.           */
/*===================================*/

void GETDATE(register string11 s)
{
    time_t clock;
    register char *t;

    time(&clock);
    t = ctime(&clock);
    s[0] = t[8];
    s[1] = t[9];
    s[2] = '-';
    s[3] = t[4];
    s[4] = t[5];
    s[5] = t[6];
    s[6] = '-';
    s[7] = t[20];
    s[8] = t[21];
    s[9] = t[22];
    s[10] = t[23];
    s[11] = '\0';
}

/*=====================================*/
/*              GETTIME                */
/* Return time in format "hh:mm:ss"    */
/*=====================================*/

void GETTIME(string11 s)
{
    time_t clock;
    char *t;

    time(&clock);
    t = ctime(&clock);
    strncpy(s, t + 11, 8);
    s[8] = '\0';
}

/* GETNUMTIME
 *
 * Get time as an integer hhmmss, e.g. 142512 means
 * "25 minutes and 12 seconds after 2 pm".
 */
long
GETNUMTIME(void)
{
    struct tm *tm;
    time_t clock;

    time(&clock);
    tm = localtime(&clock);
    return tm->tm_hour*10000L + tm->tm_min*100L + tm->tm_sec;
}

/*========================================*/
/*                openr                   */
/* Open an existing file for read access. */
/* Return 1 if we fail, else 0.           */
/*========================================*/

int
openr(FILE **f,
      char *fn)
{
    if (!(*f = fopen(fn, "r"))) {
        perror(fn);
        return 1;
    }
    return 0;
}

/*===================================*/
/*               openw               */
/* Open a file for write access and  */
/* create it if necessary.           */
/* Return 1 if we fail, else 0.      */
/*===================================*/

int
openw(FILE **f,
      char *fn)
{
    if (!(*f = fopen(fn, "w"))) {
        perror(fn);
        return 1;
    }
    return 0;
}

/*==============================*/
/*      CLOSE_FILE              */
/* Close a file.                */
/* Return 1 if we fail, else 0. */
/*==============================*/

int CLOSE_FILE(FILE *f)
{
    if (fclose(f)) {
        perror("close");
        return 1;
    }
    return 0;
}

/*=================================================*/
/*                 CPU_SECONDS                     */
/* Returns the number of CPU seconds               */
/* since some unspecified time in the past.        */
/* Should be called once at start of program, then */
/* all later times should be taken relative to     */
/* the start time.                                 */
/*=================================================*/

double CPU_SECONDS(void)
{
#ifdef _WIN32
    return clock() / (double)CLOCKS_PER_SEC;
#else
    struct rusage usage;

    if (getrusage(RUSAGE_SELF, &usage)) {
        perror("getrusage");
        exit(4);
    }
    return usage.ru_utime.tv_sec + usage.ru_utime.tv_usec / 1000000.;
#endif
}
