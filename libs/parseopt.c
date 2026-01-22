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
#include "../calc/language.h"
#include <stdio.h>

options_t sdfast_opt;

/* These are the default settings for all the options which have
 * simple defaults.
 */
options_t sdfast_optdef = {
    /*formulation*/     OPT_KANE,
    /*precision*/       OPT_DOUBLE,
    /*verbose*/         0,
    /*breakup*/         0,
    /*magic_no*/        0,
    /*language*/        &FORTRAN_language,
    /*prefix*/          "sd",
    /*infile*/          OPT_STRDEFAULT, /* ask user */
    /*basename*/        OPT_STRDEFAULT, /* extract from infile */
    /*gendyn*/          1,
    /*geninfo*/         1,
    /*gensar*/          1,
    /*genlib*/          0,
    /*dynname*/         OPT_STRDEFAULT, /* construct from basename */
    /*infoname*/        OPT_STRDEFAULT, /* construct from basename */
    /*sarname*/         OPT_STRDEFAULT, /* construct from basename */
    /*libname*/         OPT_STRDEFAULT, /* construct from LIBNAME */
    /*progname*/        OPT_STRDEFAULT, /* always comes from argv[0] */
};

static void usage(void)
{
    fprintf(stderr, "\n\
usage: %s [-sdknvb] [-l language] [-p prefix] [-g disle] [infile [basename]]\n\
    -s -d  Use single or double precision\n\
    -k -n  Use Kane's or Order(N) formulation\n\
       -v  Verbose: output roadmap, stats, etc.; -vv echos input file also\n\
       -b  Break up Dynamics file into multiple smaller files\n",
#if defined(vms) || defined(_WIN32)
    "sdfast"        /* the returned name is too ugly under VMS or WIN32 */
#else
    sdfast_opt.progname
#endif
    );

    fprintf(stderr, "\
       -l  Specify output language: fortran, c\n\
       -p  Specify prefix to be used for all generated external symbol names\n\
       -g  1-4 letters to generate dyn,info,sar,lib or everything (def: dis)\n\
   infile  Input File name\n\
 basename  Base to use in forming output file names\n");

    exit(1);
}


/* Given pointers to argc and argv, parse the command line and
 * modify the sdfast_opt structure to reflect what we find there.
 */
void parse_cmdline(int argc,
              char *argv[])
{
    char ch, ch2, *arg, *namep;

    /* First initialize the sdfast_opt structure so we can tell which
     * things have been left unspecified.  These will become the options as 
     * desired by the user.  First, options
     * provided on the command line will be filled in.  If anything typed
     * in by the user conflicts, the typed-in values will be written on top
     * of the command line values.  Next, the input file will be read in
     * and any options specified in the input will be filled in here as long
     * as the slot still says `use default'.  If there are any defaults 
     * remaining after that, they'll be filled in from sdfast_optdef below.
     */
    sdfast_opt.formulation =        OPT_DEFAULT;
    sdfast_opt.precision   =        OPT_DEFAULT;
    sdfast_opt.verbose     =        0;
    sdfast_opt.breakup     =        0;
    sdfast_opt.magic_no    =        0;
    sdfast_opt.lang        =        OPT_LANGDEFAULT;
    sdfast_opt.prefix      =        OPT_STRDEFAULT;
    sdfast_opt.infile      =        OPT_STRDEFAULT;
    sdfast_opt.basename    =        OPT_STRDEFAULT;
    sdfast_opt.gendyn      =        OPT_DEFAULT;
    sdfast_opt.geninfo     =        OPT_DEFAULT;
    sdfast_opt.gensar      =        OPT_DEFAULT;
    sdfast_opt.genlib      =        OPT_DEFAULT;
    sdfast_opt.dynname     =        OPT_STRDEFAULT;
    sdfast_opt.infoname    =        OPT_STRDEFAULT;
    sdfast_opt.sarname     =        OPT_STRDEFAULT;
    sdfast_opt.libname     =        OPT_STRDEFAULT;
    sdfast_opt.progname    =    argv[0];

    while (--argc) {

        arg = *++argv;

        if (*arg == OPT_CHAR) {
            /* an option letter follows */
            while (ch = *++arg) {
                switch (ch) {
                case 's':
                    sdfast_opt.precision = OPT_SINGLE;
                    break;

                case 'd':
                    sdfast_opt.precision = OPT_DOUBLE;
                    break;

                case 'k':
                    sdfast_opt.formulation = OPT_KANE;
                    break;

                case 'n':
                    sdfast_opt.formulation = OPT_ORDERN;
                    break;

                case 'x': /* secret */
                    sdfast_opt.formulation = OPT_EXP;
                    break;

                case '2': /* secret */
                    sdfast_opt.formulation = OPT_EXP2;
                    break;

                case 'm': /* secret */
                    if (arg[1])
                        namep = arg + 1;
                    else {
                      if (argc > 1 && **(argv+1) != OPT_CHAR) {
                          --argc;
                          namep = *++argv;
                      } else {
                          /* No number is treated the same as -m0, that is,
                           * no security.
                           */
                              sdfast_opt.magic_no = 0;
                          goto nextArg;
                      }
                    }
                    sscanf(namep, "%d", &sdfast_opt.magic_no);
                    goto nextArg;
                    /* break; */

                case 'v':
                    sdfast_opt.verbose++;        /* 1 and 2 are recognized */
                    break;

                case 'b':
                    sdfast_opt.breakup = 1;
                    break;

                case 'l':
                    if (arg[1])
                        namep = arg + 1;
                    else {
                      if (argc > 1 && **(argv+1) != OPT_CHAR) {
                          --argc;
                          namep = *++argv;
                      } else {
                          fprintf(stderr, "Language expected after -c\n");
                          usage();
                      }
                    }
                    if (!strcmp_ci("fortran", namep) || !strcmp_ci("f", namep)) 
                        sdfast_opt.lang = &FORTRAN_language;
                    else if (!strcmp_ci("krc", namep))
                        sdfast_opt.lang = &KRC_language;
                    else if (!strcmp_ci("ansic", namep) || !strcmp_ci("c", namep))
                        sdfast_opt.lang = &ANSIC_language;
                    else if (!strcmp_ci("cpp", namep) || !strcmp_ci("c++", namep))
                        sdfast_opt.lang = &Cpp_language;
                    else {
                        fprintf(stderr, "Bad language specified\n");
                        usage();
                    }
                    goto nextArg;
                    /* break; */

                case 'p':
                    if (arg[1])
                        sdfast_opt.prefix = arg + 1;
                    else {
                      if (argc > 1 && **(argv+1) != OPT_CHAR) {
                          --argc;
                          sdfast_opt.prefix = *++argv;
                          if (sdfast_opt.prefix[0] == '\0') {
                              fprintf(stderr, "Null prefix not allowed\n");
                              usage();
                          }
                      } else {
                          fprintf(stderr, "Prefix expected after -p\n");
                          usage();
                      }
                    }
                    goto nextArg;
                    /* break; */
                
                case 'g':
                    sdfast_opt.gendyn  = 0;
                    sdfast_opt.geninfo = 0;
                    sdfast_opt.gensar  = 0;
                    sdfast_opt.genlib  = 0;
                    if (arg[1])
                        namep = arg + 1;
                    else {
                      if (argc > 1 && **(argv+1) != OPT_CHAR) {
                          --argc;
                          namep = *++argv;
                      } else
                          namep = "";
                    }
                    while (ch2 = *namep++) {
                        switch (ch2) {
                        case 'd': case 'D':
                            sdfast_opt.gendyn  = 1; break;
                        case 'i': case 'I':
                            sdfast_opt.geninfo = 1; break;
                        case 's': case 'S':
                            sdfast_opt.gensar  = 1; break;
                        case 'l': case 'L':
                            sdfast_opt.genlib  = 1; break;
                        case 'e': case 'E':
                            sdfast_opt.gendyn = sdfast_opt.geninfo = 
                            sdfast_opt.gensar = sdfast_opt.genlib = 1;
                            break;
                        default:
                            fprintf(stderr, 
                                "Only d,i,s,l and e allowed after -g\n");
                            usage();
                        }
                    }
                    goto nextArg;
                    /* break; */

                /* Secret -f{d,i,s,l} <filename> option for use by Applied
                 * Motion in specifying specific file names.
                 */
                case 'f':
                    if (!arg[1]) {
                        fprintf(stderr, 
                            "d,i,s or l expected immediately after -f\n");
                        exit(1);
                    }
                    switch (arg[1]) {
                    case 'd': case 'D':
                        sdfast_opt.gendyn = 1;
                        if (arg[2])
                            sdfast_opt.dynname = arg + 2;
                        else {
                          if (argc > 1 && **(argv+1) != OPT_CHAR) {
                              --argc;
                              sdfast_opt.dynname = *++argv;
                          } else {
                              fprintf(stderr, 
                                  "Dynamics File name expected after -fd\n");
                              exit(1);
                          }
                        }
                        break;
                    case 'i': case 'I':
                        sdfast_opt.geninfo = 1;
                        if (arg[2])
                            sdfast_opt.infoname = arg + 2;
                        else {
                          if (argc > 1 && **(argv+1) != OPT_CHAR) {
                              --argc;
                              sdfast_opt.infoname = *++argv;
                          } else {
                              fprintf(stderr, 
                                  "Info File name expected after -fi\n");
                              exit(1);
                          }
                        }
                        break;
                    case 's': case 'S':
                        sdfast_opt.gensar = 1;
                        if (arg[2])
                            sdfast_opt.sarname = arg + 2;
                        else {
                          if (argc > 1 && **(argv+1) != OPT_CHAR) {
                              --argc;
                              sdfast_opt.sarname = *++argv;
                          } else {
                              fprintf(stderr, 
                          "Simplified Analysis File name expected after -fs\n");
                              exit(1);
                          }
                        }
                        break;
                    case 'l': case 'L':
                        sdfast_opt.genlib = 1;
                        if (arg[2])
                            sdfast_opt.libname = arg + 2;
                        else {
                          if (argc > 1 && **(argv+1) != OPT_CHAR) {
                              --argc;
                              sdfast_opt.libname = *++argv;
                          } else {
                              fprintf(stderr, 
                                  "Library File name expected after -fl\n");
                              exit(1);
                          }
                        }
                        break;
                    default:
                        fprintf(stderr, "Only d,i,s, or l allowed after -f\n");
                        exit(1);
                    }
                    goto nextArg;
                    /* break; */

                default:
                    fprintf(stderr, "-%c: Illegal option\n", ch);
                    usage();
                }
            }
        } else {
            /* this must be the infile name or the basename */
            if (sdfast_opt.infile != OPT_STRDEFAULT) {
                if (sdfast_opt.basename != OPT_STRDEFAULT) {
                    fprintf(stderr, "Too many arguments\n");
                    usage();
                }
                sdfast_opt.basename = arg;
            } else
                sdfast_opt.infile = arg;
        }

    nextArg: /*here to skip to next whitespace-separated arg*/;
    }

    /* In case there is no input file specified, and we're being asked
     * to generate a file that depends on an input file, then we'll have
     * to have a dialog with the user.  In that case, turn up the 
     * verbosity to full blast.
     */
    if (sdfast_opt.infile == OPT_STRDEFAULT && 
        (sdfast_opt.gendyn || sdfast_opt.geninfo || sdfast_opt.gensar))
        sdfast_opt.verbose = 2;

/*XXX dump_options(stderr, "command line", &sdfast_opt); */
}

#ifdef notdef
dump_options(FILE *F,
             char *str,
             options_t *opt)
{
    char tmp[100];

    fprintf(F, "%s options:\n",str);
    fprintf(F, "  progname=%s\n", opt->progname);
    fprintf(F, "  formulation=%s\n", 
      opt->formulation==OPT_DEFAULT?"use def":
      opt->formulation==OPT_KANE?"kane":
      opt->formulation==OPT_EXP?"experimental":
      opt->formulation==OPT_ORDERN?"ordern": "???");
    fprintf(F, "  precision=%s\n", 
      opt->precision==OPT_DEFAULT?"use def":
      opt->precision==OPT_SINGLE?"single":
      opt->precision==OPT_DOUBLE?"double": "???");
    sprintf(tmp, "%d", opt->verbose);
    fprintf(F, "  verbose=%s\n",
      opt->verbose==OPT_DEFAULT?"use def":tmp);
    sprintf(tmp, "%d", opt->breakup);
    fprintf(F, "  breakup=%s\n",
      opt->breakup==OPT_DEFAULT?"use def":tmp);
    if (opt->magic_no)
        fprintf(F, "  magic=%d\n", opt->magic_no);
    fprintf(F, "  language=%s\n", 
      opt->lang==OPT_LANGDEFAULT?"use def":
      opt->lang==&FORTRAN_language?"fortran":
      opt->lang==&KRC_language?"k&r c":
      opt->lang==&ANSIC_language?"ansi c":
      opt->lang==&Cpp_language?"c++": "???");
    fprintf(F, "  prefix=%s\n", 
      opt->prefix==OPT_STRDEFAULT?"use def": opt->prefix);
    fprintf(F, "  infile=%s\n", 
      opt->infile==OPT_STRDEFAULT?"use def": opt->infile);
    fprintf(F, "  basename=%s\n", 
      opt->infile==OPT_STRDEFAULT?"use def": opt->basename);
    sprintf(tmp, "%d", opt->gendyn);
    fprintf(F, "  gendyn=%s\n",
      opt->gendyn==OPT_DEFAULT?"use def":tmp);
    sprintf(tmp, "%d", opt->geninfo);
    fprintf(F, "  geninfo=%s\n",
      opt->geninfo==OPT_DEFAULT?"use def":tmp);
    sprintf(tmp, "%d", opt->gensar);
    fprintf(F, "  gensar=%s\n",
      opt->gensar==OPT_DEFAULT?"use def":tmp);
    sprintf(tmp, "%d", opt->genlib);
    fprintf(F, "  genlib=%s\n",
      opt->genlib==OPT_DEFAULT?"use def":tmp);
    fprintf(F, "  dynname=%s\n", 
      opt->dynname==OPT_STRDEFAULT?"use def": opt->dynname);
    fprintf(F, "  infoname=%s\n", 
      opt->infoname==OPT_STRDEFAULT?"use def": opt->infoname);
    fprintf(F, "  sarname=%s\n", 
      opt->sarname==OPT_STRDEFAULT?"use def": opt->sarname);
    fprintf(F, "  libname=%s\n", 
      opt->libname==OPT_STRDEFAULT?"use def": opt->libname);
}
#endif

/* GET_INPUTS
 *
 * Reads the Input File, returns 0 if it's OK, else 1.
 * Space will have been allocated for tables in `sys' if we
 * return successfully, otherwise no space is allocated.
 */
int 
GET_INPUTS(SystemInfo_t *sys)
{
    FILE *Systemf;
    int needinfile,prompting,st;
    static char basename[128];
    static char InFname[128];
    static char DynFname[128];
    static char InfoFname[128];
    static char SarFname[128];
    static char LibFname[128];

    /* If user supplied an input file name, or if we don't need to
     * see an input file (e.g. library generation only), we'll just
     * proceed.  Otherwise we'll have to ask the user a few questions. 
     */

    needinfile = !(sdfast_opt.gendyn==0 && sdfast_opt.geninfo==0 
                   && sdfast_opt.gensar==0);
    if (sdfast_opt.infile == OPT_STRDEFAULT && needinfile) {
        printf("System description file: ");
        READSTR(stdin, 127, InFname);
        printf("\n%s\n", InFname);
        sdfast_opt.infile = InFname;
        prompting = 1;
    } else
        prompting = 0;

    /* Initialize the calculator to supplied options or to defaults. 
     * These setting may be changed when we read in the input file. 
     */
    SET_LANGUAGE(sdfast_opt.lang == OPT_LANGDEFAULT ?
                 sdfast_optdef.lang : sdfast_opt.lang);
    SET_PRECISION(sdfast_opt.precision == OPT_DEFAULT ?
                        sdfast_optdef.precision == OPT_SINGLE 
                      : sdfast_opt.precision == OPT_SINGLE);
    SET_PREFIX(sdfast_opt.prefix == OPT_STRDEFAULT ?
                 sdfast_optdef.prefix : sdfast_opt.prefix);
        
    /* If we have an infile, we'll read it even if we aren't going to
       need it.  User might just want it checked. */
    if (sdfast_opt.infile != OPT_STRDEFAULT) {
        if (openr(&Systemf, sdfast_opt.infile))
            return 1;
        st = READ_SYSTEM(Systemf, sys);
        CLOSE_FILE(Systemf);
        if (st)
            return 1;        /* error return */
        if (sdfast_opt.verbose)
            printf("\nInput file successfully processed.\n\n");
    }

/*XXX dump_options(stderr, "after input file,", &sdfast_opt); */

    /* Now set unspecified options to the defaults. */
    if (sdfast_opt.formulation == OPT_DEFAULT)
        sdfast_opt.formulation = sdfast_optdef.formulation;
    if (sdfast_opt.precision == OPT_DEFAULT)
        sdfast_opt.precision = sdfast_optdef.precision;
    if (sdfast_opt.verbose == OPT_DEFAULT)
        sdfast_opt.verbose = sdfast_optdef.verbose;
    if (sdfast_opt.lang == OPT_LANGDEFAULT)
        sdfast_opt.lang = sdfast_optdef.lang;
    if (sdfast_opt.prefix == OPT_STRDEFAULT)
        sdfast_opt.prefix = sdfast_optdef.prefix;
    if (sdfast_opt.gendyn == OPT_DEFAULT)
        sdfast_opt.gendyn = sdfast_optdef.gendyn;
    if (sdfast_opt.geninfo == OPT_DEFAULT)
        sdfast_opt.geninfo = sdfast_optdef.geninfo;
    if (sdfast_opt.gensar == OPT_DEFAULT)
        sdfast_opt.gensar = sdfast_optdef.gensar;
    if (sdfast_opt.genlib == OPT_DEFAULT)
        sdfast_opt.genlib = sdfast_optdef.genlib;

    if (!(sdfast_opt.gendyn || sdfast_opt.geninfo 
          || sdfast_opt.gensar || sdfast_opt.genlib))
        fprintf(stderr, "Warning: no output files will be generated\n");

    /* Generate the output file names if needed.  
     */
    if (needinfile) {
        if (sdfast_opt.basename == OPT_STRDEFAULT) {
            GET_BASE_NAME(sdfast_opt.infile, basename);
            sdfast_opt.basename = basename;
        }
        if (sdfast_opt.gendyn && sdfast_opt.dynname == OPT_STRDEFAULT) {
            genname(prompting,"Dynamics","",sdfast_opt.basename,
                    DYNSUF,Lang->file_suffix,DynFname);
            sdfast_opt.dynname = DynFname;
        }
        if (sdfast_opt.geninfo && sdfast_opt.infoname == OPT_STRDEFAULT) {
            genname(prompting,"Information","",sdfast_opt.basename, 
                    INFOSUF,
#ifdef vms
                    ".txt",
#else
                    "",
#endif
                    InfoFname);
            sdfast_opt.infoname = InfoFname;
        }
        if (sdfast_opt.gensar && sdfast_opt.sarname == OPT_STRDEFAULT) {
            genname(prompting,"Simplified Analysis","",sdfast_opt.basename,
                    SARSUF,Lang->file_suffix,SarFname);
            sdfast_opt.sarname = SarFname;
        }
    }
    if (sdfast_opt.genlib && sdfast_opt.libname == OPT_STRDEFAULT) {
        genname(prompting,"Library",sdfast_opt.prefix,LIBNAME, "",
                Lang->file_suffix,LibFname);
        sdfast_opt.libname = LibFname;
    }

/*XXX dump_options(stderr, "final", &sdfast_opt); */

    return 0;        /* success */
}

/* Generate the output file names when needed.  If the user types
 * a name in to a prompt, we won't add the `_dyn' or whatever
 * part of the suffix, and we'll only add the `.f' if there is
 * no `.' in the name the user typed.  If we're not prompting,
 * or the user hits RETURN to a prompt, we'll generate the output
 * file name from the input file name and the appropriate 
 * prefix, extension and suffix.
 *
 * `name' should have room for 127 characters plus a null byte.
 */
void genname(int prompting,
        char *prompt,
        char *prefix,
        char *basename,
        char *ext,
        char *langsuf,
        char *name)
{
    char response[128], *np;

    /* put the default name in `name' */

    strcpy(name, prefix);
    np = name + strlen(prefix);
    strcpy(np, basename);
    np = np + strlen(basename);
    strcpy(np, ext);
    np = np + strlen(ext);
    strcpy(np, langsuf);

    if (prompting) {
    retry:
        printf("Name for generated %s File: (%s) ", prompt, name);
        READSTR(stdin, 127, response);
        if (*response) {
            strcpy(name, response);
            if (ADD_EXT_TO_FILENAME(name, 127, langsuf)) {
                fprintf(stderr, 
                    "File name too long -- can't add extension\n");
                goto retry;
            }
        }
        printf("%s\n", name);
    } 
}
