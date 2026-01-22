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

#ifdef THINK_C
#include <Storage.h>
#endif
#include "calc.h"
#include "language.h"
#include "decl.h"
#include "calcprot.h"

static char *
skip_decl(unsigned    vtype,
          va_list     *argptr,
          packedvar_t *pvar,
          unsigned    decl_flags);

static void
do_declare_vars1(FILE        *F,
                 unsigned    vtype,
                 packedvar_t *pvar,
                 va_list     *argptr,
                 unsigned    decl_flags,
                 unsigned    num_suffix);

static void
do_declare_vars2(FILE        *F,
                 unsigned    next_vtype,
                 packedvar_t *pvar,
                 va_list     *argptr,
                 unsigned    decl_flags,
                 unsigned    num_suffix);

static void
get_dims(unsigned    vtype,
         dim_t       *dimu,
         va_list     *argptr,
         packedvar_t *pvar,
         unsigned    decl_flags);

static void
declare_sym(unsigned vtype,
            char     *vname,
            dim_t    *dimu,
            register pSym *S,
            unsigned decl_flags);

pSym newsym(SymbolKind_t kind)
{
    /* maybe someday bother with variant allocation (for 8 bytes?) */
    pSym S;

    if (!(S = (pSym)malloc(sizeof(struct tSymTabEntry)))) {
        perror("fatal");
        exit(2);
        /*NOTREACHED*/
    } else
        return S;
}

/*===========*/
/* DECL_FUNC */
/*===========*/

void
DECL_FUNC(pSym *F,
          char *Name,
          enum tKnownFunction Which)
{
    pSym tmp;

    tmp = newsym(cFunctionSym);
    *F = tmp;
    (*F)->SymbolKind = cFunctionSym;
    strcpy((*F)->PrintName, Name);
    SCALAR_TYPE(&(*F)->SymValueType);
    (*F)->WhichFunction = Which;
}

/*===========*/
/* DECL_TEMP */
/*===========*/

void
DECL_TEMP(FILE *F)
{
    /* Prints out declaration of the TEMP symbol.  The actual declaration */
    /* is handled in INIT_CALC.                                           */

    declare_vars(F, 0, VT_ARRAY, gTempSym->PrintName, gMaxTemps, 0, 0);
}


/* declare_vars(F, decl_flags, [num_suffix,] {type_declaration} ...)
FILE *F;

unsigned type, decl_flags, cond;
int num_suffix;
char *name;
int dimension;
struct user_type *user_type;

Declares variables in a language-independent way, and may optionally also
declare corresponding internal symbols.  If VT_DUP is used when possible and
variables of like base types (integer, real, or user) are grouped together,
declarations generated will be compressed when possible.

F is a stream on which to output declarations.  decl_flags may specify the
following:
        DECL_PROC        Used when declaring formal parameters (only used by
                        declare_proc()).
        DECL_GLOBAL        Used when declaring global variables.  No effect on
                        declare_vars().
        DECL_EXTERN        Used when declaring external references.  For C this
                        will cause an "extern" prefix.
        DECL_STATIC        Used when declaring static variables.  For C this
                        will cause a "static" prefix.
        DECL_NODSYM        Any symbols specified with VT_DSYM will not be
                        declared.
        DECL_NOPRINT        Output of the declaration to file F is suppressed.
                        This can be used in combination with VT_DSYM to
                        declare internal symbols without producing output.
        DECL_NUMSUFFIX        The next integer parameter plus the current subscript
                        offset is appended to the name of each variable or
                        symbol declared.
        DECL_INIT        This variable is going to be initialized.  In some
                        languages (e.g. C) this must be done as part of
                        the declaration, so this causes suppression of the
                        final `;'.
        DECL_PACKED        Each passed-in argument is a pointer to a heap-
                        allocated structure containing the type declaration
                        for a single variable.  This is used to reduce
                        the number of arguments to the routine, since some
                        compilers have limits.

The following type declarations may be specified.  VT_REAL is the default base
type:

VT_INTEGER, name
VT_REAL, name
VT_TYPENAME, typename, name
VT_PROCNAME, name,    
VT_INTEGER|VT_ARRAY, name {,dimension} ... , 0        int array, VT_BYREF implied
VT_ARRAY, name {,dimension} ... , 0                real array, VT_BYREF implied
These array types take array dimensions as strings rather than numbers:
VT_INTEGER|VT_SARRAY, name {,dimension} ... , NULL  int array, VT_BYREF implied
VT_SARRAY, name {,dimension} ..., NULL                real array, VT_BYREF implied

Flags (one or neither):
VT_VECTOR        a x3 vector or a {dims} array of vectors (VT_BYREF implied)
VT_MATRIX        a 3x3 matrix or a {dims} array of matrices (VT_BYREF implied)

Special types (VT_VECTOR and VT_MATRIX may not be specified):
VT_USER, user_type, name  Type is referenced user type (struct user_type *)
VT_DUP, name                Identical to previous declaration, as defined above
0                        End of parameter list; finish any pending declaration
VT_PAUSE                End of parameter list; declaration may still be pending

If "name" is NULL, no variable is declared but the type is still "saved" for
the purpose of VT_DUP.  This is useful with VT_PAUSE before a loop that calls
declare_vars() (but be sure at least 1 variable is declared, or an empty
declaration may be output).

Special flags (may be used with regular types plus VT_DUP and VT_USER)
VT_BYREF         force formal parameter declarations to be by reference
                (only useful with declare_proc())
VT_DSYM                declare internal symbol of indicated type (pSym * follows
                other parameters).  (This can be suppressed with DECL_NODSYM.)
VT_COND                conditionally declare this variable.  An integer parameter
                immediately follows the type.  If 0 and the type is not
                VT_DUP, the variable and any following duplicates are not
                declared.  If the type is VT_DUP, only the duplicate is not
                declared (exactly the same as specifying a NULL name). 

Note:  This function probably does too much.  Its parameter syntax is
reminiscent of SunView. (TE)
I disagree -- I think it's great! (MS)
*/


/* This routine takes a single variable description (beginning with
 * VT_ flags) and packs it into a single data structure.  This is
 * used to cut down on the number of arguments passed to declare_proc
 * or declare_vars to avoid compiler limits.
 *
 * We return a pointer to the data structure, which is heap-allocated
 * here and also linked onto a list of these structures so that they
 * can be freed when no longer needed.
 * Don't forget to free it when you're done.
 */

#define PVNULL ((packedvar_t *)NULL)

/* This list should be built up by packvar() calls.  Then, any routine
 * which can accept packed arguments should be sure to free everything
 * on it before exiting, and then reset the list head to PVNULL.  It
 * doesn't matter in what order packedvar's are put in the list -- we're
 * just using it to track them for later freeing.
 */
static packedvar_t *pv_alloclist = PVNULL;

packedvar_t *
packvar(unsigned vtype, ...)
{
    va_list     argptr;
    packedvar_t *pvar;

    va_start(argptr, vtype);

    pvar = (packedvar_t *)malloc(sizeof(packedvar_t));
    pvar->pv_flags = vtype;
    pvar->pv_name = pvar->pv_typename = NULL;
    pvar->pv_cond = 0;
    pvar->pv_sym = NULL;
    pvar->pv_usertype = NULL;
    pvar->pv_next = pv_alloclist;
    pv_alloclist = pvar;

    if (!vtype || vtype == VT_PAUSE) 
        goto allDone;

    if ((vtype & VT_BASETYPE) > VT_PAUSE) {
        fprintf(stderr, "packvar: bad type %#x\n", vtype);
        abort();
    }

    if (vtype & VT_COND)
        pvar->pv_cond = va_arg(argptr, unsigned);

    if ((vtype & VT_BASETYPE) == VT_USER)
        pvar->pv_usertype = va_arg(argptr, struct user_type *);

    if ((vtype & VT_BASETYPE) == VT_TYPENAME)
        pvar->pv_typename = va_arg(argptr, char *);

    pvar->pv_name = va_arg(argptr, char *);

    if (vtype & VT_ARRAY) {
        uintptr_t *dimp = pvar->pv_dimu.ut_dim;
        while (*dimp++ = va_arg(argptr, unsigned));
    } else if (vtype & VT_SARRAY) {
        char **sdimp = pvar->pv_dimu.ut_sdim;
        while (*sdimp++ = va_arg(argptr, char *));
    }

    if (vtype & VT_DSYM)
        pvar->pv_sym = va_arg(argptr, pSym *);

  allDone:
    va_end(argptr);
    return pvar;
}

static void
free_packedvars( )
{
    register packedvar_t *pvar;

    while(pv_alloclist) {
        pvar = pv_alloclist->pv_next;
        free(pv_alloclist);
        pv_alloclist = pvar;
    }
}

/* This function gets the next packed variable (if we're packing) and then
 * returns its type.  If we're not packing, then the next vararg is
 * simply the type, and it is returned.
 */
static unsigned
GETPVAR(va_list      *argptr,
        unsigned     decl_flags,
        packedvar_t  **pvar)
{
    if (decl_flags & DECL_PACKED) {
        *pvar = va_arg(*argptr, packedvar_t*);
        return (*pvar)->pv_flags;
    } else {
        return va_arg(*argptr, unsigned);
    }
}

/* These functions either return a field from the already-loaded packed
 * variable pvar, or they use the next vararg as the field.
 */
#ifdef NOTDEF
static unsigned
VARTYPE(va_list     *argptr,
        unsigned    decl_flags,
        packedvar_t *pvar)
{
    return decl_flags & DECL_PACKED ? pvar->pv_flags
                                    : va_arg(*argptr, unsigned);
}
#endif

static char*
VARNAME(va_list     *argptr,
        unsigned    decl_flags,
        packedvar_t *pvar)
{
    return decl_flags & DECL_PACKED ? pvar->pv_name
                                    : va_arg(*argptr, char*);
}

static char*
VARTYPENAME(va_list     *argptr,
            unsigned    decl_flags,
            packedvar_t *pvar)
{
    return decl_flags & DECL_PACKED ? pvar->pv_typename
                                    : va_arg(*argptr, char*);
}

static unsigned
VARCOND(va_list     *argptr,
        unsigned    decl_flags,
        packedvar_t *pvar)
{
    return decl_flags & DECL_PACKED ? pvar->pv_cond
                                    : va_arg(*argptr, unsigned);
}

static pSym*
VARSYM(va_list     *argptr,
       unsigned    decl_flags,
       packedvar_t *pvar)
{
    return decl_flags & DECL_PACKED ? pvar->pv_sym
                                    : va_arg(*argptr, pSym*);
}

static struct user_type*
VARUSER(va_list     *argptr,
        unsigned    decl_flags,
        packedvar_t *pvar)
{
    return decl_flags & DECL_PACKED ? pvar->pv_usertype
                                    : va_arg(*argptr, struct user_type*);
}

/*VARARGS*/
void 
declare_vars(FILE *F, unsigned decl_flags, ...)
{
    va_list argptr;
    unsigned first_vtype;
    unsigned num_suffix = -1;
    packedvar_t *pvar = NULL;

    va_start(argptr, decl_flags);

    if (Lang == &ADSIM_language)
        Lang = &ADSIM_language_decl;  /* special mode for commented out decls */

    if (decl_flags & DECL_NUMSUFFIX)
        num_suffix = va_arg(argptr, int);

    first_vtype = GETPVAR(&argptr, decl_flags, &pvar);
    if (Lang->flags & LANG_DECL2) {
        /* languages with the "name, name: [array%(subs%) of] type" model */
        do_declare_vars2(F, first_vtype, pvar, &argptr, decl_flags, num_suffix);
    } else {        /* must be type 1 */
        /* languages with the "type name[%(subs%)], name[%(subs%)]" model */
        do_declare_vars1(F, first_vtype, pvar, &argptr, decl_flags, num_suffix);
    }

    if (Lang == &ADSIM_language_decl)
        Lang = &ADSIM_language;  /* back to normal */
    if (decl_flags & DECL_PACKED)
        free_packedvars();
    va_end(argptr);
}

/* declare_proc(F, decl_flags, [func_type,] proc_name, {type_declaration} ...)
   FILE *F;
   unsigned decl_flags, func_type;
   char *proc_name;

Declares a procedure heading: name, formal parameters, and parameter type
declarations.  Type declarations are the same as declare_vars, except that
VT_PAUSE may not be used.  decl_flags may specify:
        DECL_FORWARD        A procedure declaration before the actual body.  This
                        should only be used with languages that support (and
                        need) the forward declaration.
        DECL_FWDDEFN        A procedure definition that was forward-declared.
        DECL_FUNCTION        This is really a function.  The type follows the
                        flags.  For now, nothing fancy supported -- just
                        VT_INTEGER and VT_REAL.
The proc_name is output preceded by the external name prefix.
*/

/*VARARGS*/
void 
declare_proc(FILE *F, unsigned decl_flags, ...)
{
    va_list     argptr;
    unsigned    vtype, ftype;
    char        *pname;
    packedvar_t *pvar = NULL;

    va_start(argptr, decl_flags);
    if (Lang == &ADSIM_language)
        Lang = &ADSIM_language_decl;  /* special mode for commented out decls */

    if (decl_flags & DECL_FUNCTION) {
        ftype = va_arg(argptr, unsigned);
        if (Lang->flags & LANG_C_FAMILY) {
            /* C wants the type output first */
            if ((ftype & VT_BASETYPE) == VT_INTEGER)
                efprintf(F, Lang->int_decl);
            else if ((ftype & VT_BASETYPE) == VT_REAL)
                efprintf(F, "%t");
            else {
                fprintf(stderr, "declare_proc: bad function type %#x\n", ftype);
                abort();
            }
        }
    }
    pname = va_arg(argptr, char *);
    efprintf(F, "%s%@A%s%>", 
      decl_flags & DECL_FUNCTION ? Lang->func_decl : Lang->proc_decl, 
      pname);

    if (decl_flags & DECL_FWDDEFN && Lang == &Pascal_language) {
        /* no argument list when defining */
        efprintf(F, "%<%s", Lang->proc_decl_end);
        goto allDone;
    }

    vtype = GETPVAR(&argptr, decl_flags, &pvar);
    if (vtype) {
        efprintf(F, "(");
        if (Lang->flags & LANG_DECL_IN_ARGLIST) {
            /* declare arguments within the () */
            if (Lang->flags & LANG_DECL2)
                do_declare_vars2(F, vtype, pvar, &argptr, decl_flags|DECL_PROC, 0);
            else
                do_declare_vars1(F, vtype, pvar, &argptr, decl_flags|DECL_PROC, 0);
            efprintf(F, ")\n");
        } else {
            /* list arguments first, then declare them */
            int first_name = 1;
            char *name, skip = 0;

            while (vtype) {
                if ((vtype & VT_BASETYPE) > VT_PAUSE) {
                    fprintf(stderr, "declare_proc: bad type %#x\n", vtype);
                    abort();
                }
                if (vtype & VT_COND && !VARCOND(&argptr, decl_flags, pvar) ||
                  skip && (vtype & VT_BASETYPE) == VT_DUP) {
                    (void) skip_decl(vtype, &argptr, pvar, decl_flags);
                    if ((vtype & VT_BASETYPE) != VT_DUP)
                        skip = 1;
                } else {
                    skip = 0;
                    if (name = skip_decl(vtype, &argptr, pvar, decl_flags)) {
                        if (!first_name)
                            efprintf(F, ",");
                        else
                            first_name = 0;
                        efprintf(F, name);
                    }
                }
                vtype = GETPVAR(&argptr, decl_flags, &pvar);
            }
            va_end(argptr);
            efprintf(F, ")\n");

            va_start(argptr, decl_flags);

            if (decl_flags & DECL_FUNCTION)
                (void) va_arg(argptr, unsigned);        /* ftype */
            (void) va_arg(argptr, char *);        /* pname */
            vtype = GETPVAR(&argptr, decl_flags, &pvar);
            do_declare_vars1(F, vtype, pvar, &argptr, decl_flags|DECL_PROC, 0);
        }
    } else {
        /* no arguments */
        efprintf(F, "%s", Lang->proc_noargs_decl);
        if (!(Lang->flags & LANG_DECL2))
            efprintf(F, "\n"); 
    }

    if (decl_flags & DECL_FUNCTION) {
        if (Lang->flags & LANG_DECL2)
            efprintf(F, ": ");
        if (!(Lang->flags & LANG_C_FAMILY)) {
            if ((ftype & VT_BASETYPE) == VT_INTEGER)
                efprintf(F, Lang->int_decl);
            else if ((ftype & VT_BASETYPE) == VT_REAL)
                efprintf(F, "%t");
            else {
                fprintf(stderr, "declare_proc: bad function type %#x\n", ftype);
                abort();
            }
        }
        if (Lang == &FORTRAN_language || Lang == &ADSIM_language)
            efprintf(F, "%@A%s%;\n",pname);
    }
    if (decl_flags & DECL_FORWARD)
        efprintf(F, "%<%s", Lang->proc_decl_fwd);
    else
        efprintf(F, "%<%s", Lang->proc_decl_end);

  allDone:
    if (Lang == &ADSIM_language_decl)
        Lang = &ADSIM_language;  /* back to normal */
    if (decl_flags & DECL_PACKED)
        free_packedvars();
    va_end(argptr);
}

/* declare_type(F, decl_flags, {user_type, type_declaration} ... , NULL)
or declare_type(F, decl_flags|DECL_NODSYM, {user_type} ..., NULL)
FILE *F;
unsigned decl_flags;
struct user_type *user_type;

unsigned type;
char *name;
unsigned dimension;

Declares user-defined types required by type 2 languages when declaring arrays
as formal parameters.  Type declarations (preceded by any needed "type section"
keyword(s)) are output for type 2 languages.  For type 1 languages,
user-defined types will be expanded when variables are declared.  The following
type declarations may be specified.  A NULL user_type terminates the list.

DECL_NODSYM         this flag says that we are not declaring new types, rather
                  we want to reference existing ones
DECL_NOPRINT        with this flag, we define the user type but produce no
                  output

VT_INTEGER, name
VT_REAL, name
VT_PROCNAME, name
VT_INTEGER|VT_ARRAY, name {,dimension} ... , 0        int array, VT_BYREF implied
VT_ARRAY, name {,dimension} ... , 0                real array, VT_BYREF implied
VT_INTEGER|VT_SARRAY, name {,dimension} ... , NULL  int array, VT_BYREF implied
VT_SARRAY, name {,dimension} ... , NULL                real array, VT_BYREF implied

Flags (one or neither):
VT_VECTOR        a x3 vector or a {dims} array of vectors (VT_BYREF implied)
VT_MATRIX        a 3x3 matrix or a {dims} array of matrices (VT_BYREF implied)
VT_COND                conditionally declare this type.  An integer parameter
                immediately follows the type.  If 0, the type is not
                declared.
VT_REAL is the default base type.
*/

/*VARARGS*/
void 
declare_type(FILE *F, unsigned decl_flags, ...)
{
    va_list  argptr;
    uintptr_t *dimp;
    int      prt;
    char     **sdimp;
    register struct user_type *user_type;

    va_start(argptr, decl_flags);

    prt = !(decl_flags&DECL_NOPRINT);

    if (decl_flags & DECL_PACKED) {
        fprintf(stderr, "declare_type: packing not supported\n");
        abort();
    }

    if (prt && Lang == &Pascal_language)
        efprintf(F, "type\n%>");
    while (user_type = va_arg(argptr, struct user_type *)) {
        if (!(decl_flags&DECL_NODSYM)) {
            user_type->type = va_arg(argptr, unsigned);
            if (user_type->type & VT_COND && !va_arg(argptr, unsigned)) {
                (void) skip_decl(user_type->type, &argptr, PVNULL, 0);
                continue;
            }
            user_type->name = va_arg(argptr, char *);
            get_dims(user_type->type, &user_type->dimu, &argptr, PVNULL, 0);
            user_type->type |= VT_ISUSER;
        } 
        if ((user_type->type & VT_BASETYPE) > VT_PAUSE
             || !(user_type->type & VT_ISUSER)) 
        {
            fprintf(stderr, "declare_type: bad type %#x\n", 
                user_type->type);
            abort();
        }
        if (prt && Lang->flags & LANG_DECL2) {
            efprintf(F, Lang == &Ada_language ? "type %s is " : "%s = ",
              user_type->name);
            if (user_type->type & (VT_ARRAY|VT_SARRAY|VT_VECTOR|VT_MATRIX)) {
                efprintf(F, "array%(");
                if (user_type->type & VT_SARRAY) {
                    for (sdimp = user_type->dimu.ut_sdim; *sdimp;) {
                        efprintf(F, "%@d..%s", 0, *sdimp++);
                        if (*sdimp)
                            efprintf(F, "%,");
                    }
                } else {
                    for (dimp = user_type->dimu.ut_dim; *dimp;) {
                        efprintf(F, "%@d..%@d", 0, *dimp++ - 1);
                        if (*dimp)
                            efprintf(F, "%,");
                    }
                }
                efprintf(F, "%) of ");
            }
            if ((user_type->type & VT_BASETYPE) == VT_INTEGER)
                efprintf(F, "%s%;\n", Lang->int_decl);
            else if ((user_type->type & VT_BASETYPE) == VT_PROCNAME)
                efprintf(F, "%s%;\n", Lang->procparm_decl);
            else
                efprintf(F, "%t%;\n");
        }
    }
    if (prt && Lang == &Pascal_language)
        efprintf(F, "%<");
    if (prt && Lang->flags & LANG_DECL2)
        efprintf(F, "\n");
}


/* Languages with the "type name[%(subs%)], name[%(subs%)]" model.
 * If we're declaring in a formal parameter list
 * then we will provide a separate declaration
 * for each parameter, use "," instead of ";" between the parameters,
 * and suppress the final separator.
 */
static void 
do_declare_vars1(FILE            *F,
                 unsigned        vtype,
                 packedvar_t     *pvar,
                 va_list         *argptr,
                 unsigned        decl_flags,
                 unsigned        num_suffix)
{
    static unsigned last_vtype;
    static int skip, first_name;
    static char *vname;
    static dim_t dimu;
    uintptr_t *dimp;
    char **sdimp;
    struct user_type *user_type;
    char tmpvname[50],tmpvname2[50];
    int        prt = !(decl_flags&DECL_NOPRINT);
    int formal = (Lang->flags & LANG_DECL_IN_ARGLIST) && (decl_flags & DECL_PROC);

    for (;; vtype = GETPVAR(&*argptr, decl_flags, &pvar)) {
        if ((vtype & VT_BASETYPE) > VT_PAUSE) {
            fprintf(stderr, "do_declare_vars1: bad type %#x\n", vtype);
            abort();
        }

        if (vtype == VT_PAUSE)
            break;        /* exit in current state */

        if (    ((vtype & VT_COND) && !VARCOND(&*argptr, decl_flags, pvar))
             || (skip && ((vtype & VT_BASETYPE) == VT_DUP)))
        {
            (void) skip_decl(vtype, &*argptr, pvar, decl_flags);
            if ((vtype & VT_BASETYPE) != VT_DUP)
                skip = 1;
            continue;
        }

        skip = 0;
        if ((vtype & VT_BASETYPE) == VT_USER) {
            /* just expand the pesky user type */
            user_type = VARUSER(&*argptr, decl_flags, pvar);
            vtype = (vtype & (VT_BYREF|VT_DSYM)) | user_type->type;
            if (vtype & VT_SARRAY)
                memcpy((char *)dimu.ut_sdim, (char *)user_type->dimu.ut_sdim, 
                      sizeof(dimu.ut_sdim));
            else 
                memcpy((char *)dimu.ut_dim, (char *)user_type->dimu.ut_dim, 
                      sizeof(dimu.ut_dim));
        }

        if (!(vtype & VT_BASETYPE) && (vtype & ~VT_BASETYPE))
            vtype |= VT_REAL;

        /* 
         * If we're changing types, or if we're in a formal parameter
         * list, we have to terminate the current declaration.
         */
        if (((vtype & VT_BASETYPE) != VT_DUP) || formal) {
            if (   !vtype 
                || ((last_vtype ^ vtype) & VT_BASETYPE) 
                || formal)
            {
                /* no more vars, or different base type, 
                   or in formal param list  */
                if (prt && last_vtype) {
                    /* Suppress the declaration termination for the final
                       declaration if we're planning to initialize this 
                       variable in a language where the initialization
                       is attached to the declaration, or if we're in
                       a formal parameter list. */
                    if (vtype /*more to come*/
                        || (!formal
                            && (   !(decl_flags & DECL_INIT)
                                || (Lang->flags & LANG_SEPARATE_INIT))))
                    {
                        if (formal)
                            efprintf(F, ",\n");
                        else
                            efprintf(F, "%;\n");
                    }
                }
                if ((vtype & VT_BASETYPE) != VT_DUP)
                    last_vtype = vtype & ~(VT_BYREF|VT_DSYM);
                if (!vtype)
                    break;
                if (prt) {
                    if (decl_flags & DECL_EXTERN && (Lang->flags & LANG_C_FAMILY))
                        efprintf(F, "extern ");
                    if (decl_flags & DECL_STATIC && (Lang->flags & LANG_C_FAMILY))
                        efprintf(F, "static ");
                    if ((last_vtype & VT_BASETYPE) == VT_TYPENAME) {
                        efprintf(F, VARTYPENAME(&*argptr, decl_flags, pvar));
                        efprintf(F, " ");
                    } else if ((last_vtype & VT_BASETYPE) == VT_INTEGER)
                        efprintf(F, Lang->int_decl);
                    else if ((last_vtype & VT_BASETYPE) == VT_PROCNAME)
                        efprintf(F, Lang->procparm_decl);
                    else
                        efprintf(F, "%t");
                }
                first_name = 1;
            } else {
                if ((vtype & VT_BASETYPE) != VT_DUP)
                    last_vtype = vtype & ~(VT_BYREF|VT_DSYM);
                if (!vtype)
                    break;
            }
        }

        vname = VARNAME(&*argptr, decl_flags, pvar);
        if ((decl_flags & DECL_NUMSUFFIX) && vname) {
            esprintf(tmpvname, "%@s%@d", vname, num_suffix);
            vname = tmpvname;
        }

        get_dims(vtype, &dimu, &*argptr, pvar, decl_flags);

        if ((vtype & VT_BASETYPE) == VT_DUP)
            vtype = last_vtype | (vtype & (VT_BYREF|VT_DSYM));

        if (vtype & VT_DSYM)
            declare_sym(vtype, vname, &dimu, VARSYM(&*argptr, decl_flags, pvar), decl_flags);

        if ((decl_flags & DECL_STRUCT) && (Lang->flags & LANG_C_FAMILY) && vname) {
            /* We have to tweak the field names in this struct so that
               they are not the same as the variable names we would like
               to reference them by. */
            esprintf(tmpvname2, "%@s_", vname); /* can't use tmpvname again! */
            vname = tmpvname2;
        } else if ((decl_flags & (DECL_GLOBAL|DECL_EXTERN)) && vname) {
            esprintf(tmpvname2, "%@A%@s", vname);
            vname = tmpvname2;
        }

        if (vname) {
            if (first_name)
                first_name = 0;
            else if (prt)
                efprintf(F, ",");
            if (prt) {
                if (vtype & (VT_ARRAY|VT_SARRAY|VT_VECTOR|VT_MATRIX)) {
                    efprintf(F, "%s%(", vname);
                    if (vtype & VT_SARRAY) {
                        if ((decl_flags&DECL_PROC) &&
                            (Lang->flags & LANG_VARDIM)) 
                        {
                            for (sdimp = dimu.ut_sdim; *sdimp ;) {
                                if ((Lang->flags & LANG_C_FAMILY) 
                                    || Lang->subs_offset == 1)
                                    efprintf(F, "%s", *sdimp++);
                                else
                                    efprintf(F, "%@d:%s", 0, *sdimp++);
                                if (*sdimp)
                                    efprintf(F, "%,");
                            }
                        } else 
                            efprintf(F, "%s", Lang->unknown_len);
                    } else {
                        for (dimp = dimu.ut_dim; *dimp;) {
                            if (Lang->flags & LANG_C_FAMILY)
                                efprintf(F, "%@d", *dimp++);
                            else if (Lang->subs_offset == 1)
                                efprintf(F, "%d", *dimp++);
                            else
                                efprintf(F, "%@d:%@d", 0, *dimp++ - 1);
                            if (*dimp)
                                efprintf(F, "%,");
                        }
                    }
                    efprintf(F, "%)");
                } else {
                    if ((Lang->flags & LANG_C_FAMILY) &&
                           (vtype & VT_BASETYPE) == VT_PROCNAME)
                        efprintf(F, "(*%s)()", vname);
                    else
                        efprintf(F, "%s%s", vtype & VT_BYREF ? Lang->deref : "",
                          vname);
                }
            }
        }
    } /* forever */
}


/* languages with the "name, name: [array%(subs%) of] type" model */
static void 
do_declare_vars2(FILE        *F,
                 unsigned    next_vtype,
                 packedvar_t *pvar,
                 va_list     *argptr,
                 unsigned    decl_flags,
                 unsigned    num_suffix)
{
    static unsigned vtype;
    static int skip, first_name;
    static char *vname, *typename;
    static dim_t dimu;
    uintptr_t *dimp;
    char **sdimp;
    static struct user_type *user_type;
    char tmpvname[50], tmpvname2[50];
    int        prt = !(decl_flags&DECL_NOPRINT);

    for (;; next_vtype = GETPVAR(&*argptr, decl_flags, &pvar)) {
        if ((vtype & VT_BASETYPE) > VT_PAUSE) {
            fprintf(stderr, "do_declare_vars2: bad type %#x\n", vtype);
            abort();
        }

        if (next_vtype == VT_PAUSE)
            break;        /* exit in current state */

        if (next_vtype & VT_COND && !VARCOND(&*argptr, decl_flags, pvar) ||
          skip && (next_vtype & VT_BASETYPE) == VT_DUP) {
            (void) skip_decl(next_vtype, &*argptr, pvar, decl_flags);
            if ((next_vtype & VT_BASETYPE) != VT_DUP)
                skip = 1;
            continue;
        }

        skip = 0;
        if ((next_vtype & VT_BASETYPE) == VT_DUP &&
          vtype & (VT_ARRAY|VT_SARRAY|VT_VECTOR|VT_MATRIX))
            next_vtype |= VT_BYREF;

        if ((next_vtype & VT_BASETYPE) != VT_DUP ||
          (vtype & VT_BYREF) != (next_vtype & VT_BYREF)) 
        {
            if (prt && vtype) {        /* finish up the last type */
                efprintf(F, ": ");
                if (   (decl_flags & DECL_PROC) 
                       && (vtype & VT_BYREF) 
                       && (Lang == &Ada_language))
                    efprintf(F, "in out ");

                if (vtype & VT_ISUSER)
                    efprintf(F, user_type->name);
                else {
                    if (vtype & (VT_ARRAY|VT_SARRAY|VT_VECTOR|VT_MATRIX)) {
                        efprintf(F, "array%(");
                        if (vtype & VT_SARRAY) {
                            if ((decl_flags&DECL_PROC) &&
                                (Lang->flags & LANG_VARDIM)) 
                            {
                                for (sdimp = dimu.ut_sdim; *sdimp;) {
                                    efprintf(F, "%@d..%s", 0, *sdimp++);
                                    if (*sdimp)
                                        efprintf(F, "%,");
                                }
                            } else
                                efprintf(F, "%s", Lang->unknown_len);
                        } else {
                            for (dimp = dimu.ut_dim; *dimp;) {
                                efprintf(F, "%@d..%@d", 0, *dimp++ - 1);
                                if (*dimp)
                                    efprintf(F, "%,");
                            }
                        }
                        efprintf(F, "%) of ");
                    }
                    if ((vtype & VT_BASETYPE) == VT_TYPENAME)
                        efprintf(F, typename);
                    else if ((vtype & VT_BASETYPE) == VT_INTEGER)
                        efprintf(F, Lang->int_decl);
                    else if ((vtype & VT_BASETYPE) == VT_PROCNAME)
                        efprintf(F, Lang->procparm_decl);
                    else
                        efprintf(F, "%t");
                }

                /* Suppress the declaration termination for the final
                   declaration if we're declaring a procedure or if
                   we're planning to initialize this variable in a
                   language where the initialization is attached to
                   the declaration. */
                if (next_vtype || 
                    !(decl_flags & DECL_PROC)
                      && (!(decl_flags & DECL_INIT)
                          || (Lang->flags & LANG_SEPARATE_INIT)))
                    efprintf(F, "%;\n");
            }
            if ((next_vtype & VT_BASETYPE) == VT_DUP)
                vtype = next_vtype & VT_BYREF | vtype & ~VT_BYREF;
            else
                vtype = next_vtype;
            if (!vtype)
                break;
            if ((vtype & VT_BASETYPE) == VT_USER) {
                user_type = VARUSER(&*argptr, decl_flags, pvar);
                vtype = vtype & (VT_BYREF|VT_DSYM) | user_type->type;
                if (vtype & VT_SARRAY)
                    memcpy((char *)dimu.ut_sdim,(char *)user_type->dimu.ut_sdim,
                          sizeof(dimu.ut_sdim));
                else 
                    memcpy((char *)dimu.ut_dim, (char *)user_type->dimu.ut_dim, 
                          sizeof(dimu.ut_dim));
            }
            if ((vtype & VT_BASETYPE) == VT_TYPENAME)
                typename = VARTYPENAME(&*argptr, decl_flags, pvar);
            if (vtype & (VT_ARRAY|VT_SARRAY|VT_VECTOR|VT_MATRIX))
                vtype |= VT_BYREF;
            if (prt && decl_flags & DECL_PROC && vtype & VT_BYREF &&
              Lang == &Pascal_language)
                efprintf(F, "var ");
            first_name = 1;
        }
        vname = VARNAME(&*argptr, decl_flags, pvar);
        if (decl_flags & DECL_NUMSUFFIX && vname) {
            esprintf(tmpvname, "%@s%@d", vname, num_suffix);
            vname = tmpvname;
        }
        if (decl_flags & (DECL_GLOBAL|DECL_EXTERN)) {
            esprintf(tmpvname2, "%@A%@s", vname);
            vname = tmpvname2;
        }
        get_dims(next_vtype, &dimu, &*argptr, pvar, decl_flags);
        if (next_vtype & VT_DSYM)
            declare_sym(vtype, vname, &dimu, VARSYM(&*argptr, decl_flags, pvar), decl_flags);
        if (vname) {
            if (first_name)
                first_name = 0;
            else if (prt)
                efprintf(F, ",");
            if (prt)
                efprintf(F, vname);
        }
    } /* forever */
}


/* If the passed-in variable type is ARRAY, VECTOR, or MATRIX and not
 * a user type, obtain the dimensions and stuff them into dimu.
 * Dimensions for user types are pulled out of the type declaration
 * elsewhere.
 *
 * It is assumed that the first vararg associated with this variable
 * has already been consumed, and that argptr starts with the next vararg.
 * If this is a packed variable, a pointer to the rest of the information
 * is passed in pvar.  Otherwise, the remaining information comes from
 * successive varargs, which are consumed here (and argptr is updated).
 */
static void 
get_dims(unsigned    vtype,
         dim_t       *dimu,
         va_list     *argptr,
         packedvar_t *pvar,
         unsigned    decl_flags)
{
    register uintptr_t *dimp = dimu->ut_dim;
    register char **sdimp = dimu->ut_sdim;

    if (vtype & VT_ISUSER)
        return;

    if (decl_flags & DECL_PACKED) {
        if (vtype & VT_ARRAY) {
            uintptr_t *pvdimp = pvar->pv_dimu.ut_dim;
            while (*dimp++ = *pvdimp++);
            dimp--;
        } else if (vtype & VT_SARRAY) {
            char **pvsdimp = pvar->pv_dimu.ut_sdim;
            while (*sdimp++ = *pvsdimp++);
            sdimp--;
        }
    } else {
        if (vtype & VT_ARRAY) {
            while (*dimp++ = va_arg(*argptr, unsigned));
            dimp--;
        } else if (vtype & VT_SARRAY) {
            while (*sdimp++ = va_arg(*argptr, char *));
            sdimp--;
        }
    }

    if (vtype & (VT_VECTOR|VT_MATRIX)) {
        if (vtype & VT_SARRAY) {
            *sdimp++ = "3";
            if (vtype & VT_MATRIX)
                *sdimp++ = "3";
            *sdimp = 0;
        } else {
            *dimp++ = 3;
            if (vtype & VT_MATRIX)
                *dimp++ = 3;
            *dimp = 0;
        }
    }
}

/* Note: this doesn't work for VT_SARRAY types. */
static void 
declare_sym(
            unsigned vtype,
            char *vname,
            dim_t *dimu,
            register  pSym *S,
            unsigned decl_flags)
{
    uintptr_t d0,d1;
    if (!vname || decl_flags & DECL_NODSYM)
        return;
    if ((vtype & VT_BASETYPE) && (vtype & VT_BASETYPE) != VT_REAL)
        fatal("declare_sym: type must be real");
    if (vtype & VT_SARRAY)
        fatal("declare_sym: no VT_SARRAY symbols allowed");
    *S = newsym(cVariableSym);
    (*S)->SymbolKind = cVariableSym;
    strcpy((*S)->PrintName, vname);
    (*S)->SymValue = NULL;
    (*S)->RememberedVal = NULL;
    (*S)->AssignCountS = 0;

    if (vtype & VT_ARRAY) {
        register uintptr_t *dimp = dimu->ut_dim;
        NodeValueType_t basetype;

        while (*dimp)
            dimp++;
        if (vtype & VT_VECTOR) {
            basetype = cVectorVal;
            dimp--;
        } else if (vtype & VT_MATRIX) {
            basetype = cMatrixVal;
            dimp -= 2;
        } else
            basetype = cScalarVal;
        if ((int)(dimp - dimu->ut_dim) == 1) {
            d0 = dimu->ut_dim[0];
            if (d0 < 1 || d0 > cMaxDim) {
                fprintf(stderr, "declare_sym: bad 1d dimension (%d)\n", (int)d0);
                abort();
            }
            ARRAY1d_TYPE(basetype, (Index_t)d0, &(*S)->SymValueType);
            return;
        } else if ((int)(dimp - dimu->ut_dim) == 2) {
            d0 = dimu->ut_dim[0];
            d1 = dimu->ut_dim[1];
            if (d0 < 1 || d0 > cMaxDim || d1 < 1 || d1 > cMaxDim) {
                fprintf(stderr, "declare_sym: bad 2d dimension (RxC=%dx%d)\n",
                  (int)d0, (int)d1);
                abort();
            }
            ARRAY2d_TYPE(basetype, (Index_t)d0, (Index_t)d1, 
                         &(*S)->SymValueType);
            return;
        } else if ((int)(dimp - dimu->ut_dim) > 2)
            fatal("declare_sym: too many dimensions");
        /* if 0 dims fall through */
    }
    if (vtype & VT_VECTOR)
        VECTOR_TYPE(cScalarVal, &(*S)->SymValueType);
    else if (vtype & VT_MATRIX)
        MATRIX_TYPE(cScalarVal, &(*S)->SymValueType);
    else
        SCALAR_TYPE(&(*S)->SymValueType);
}

/* Skip a variable declaration in a varargs list and return the name of
 * the skipped variable.  We assume that the first vararg for this variable
 * has been consumed and that its type is passed in vtype.  If it is
 * a conditional declaration, we expect the "condition" to have been
 * previously consumed also.  If this is a packed variable,
 * the pointer to the packed information is passed in pvar
 * and no further varargs need be consumed.
 *
 * On output, argptr will have been updated to point to the first vararg
 * of the next variable.
 */
static char *
skip_decl(unsigned    vtype,
          va_list     *argptr,
          packedvar_t *pvar,
          unsigned    decl_flags)
{
    char *name;

    if (!vtype || vtype == VT_PAUSE)
        return NULL;

    if (decl_flags & DECL_PACKED)
        return pvar->pv_name;

    if ((vtype & VT_BASETYPE) == VT_USER)
        (void) va_arg(*argptr, struct user_type *);
    if ((vtype & VT_BASETYPE) == VT_TYPENAME)
        (void) va_arg(*argptr, char *);

    name = va_arg(*argptr, char *);

    if (vtype & VT_ARRAY)
        while (va_arg(*argptr, unsigned));        /* past dimensions */
    else if (vtype & VT_SARRAY)
        while (va_arg(*argptr, char *));        /* past dimensions */
    if (vtype & VT_DSYM)
        (void) va_arg(*argptr, pSym *);

    return name;
}
