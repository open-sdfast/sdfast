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

#include "../libs/libsprot.h"

// Sherm 20260117
// #include "common/src/include/cpp_optional.h"
#define CC_C
// Sherm

extern CC_C int main(int argc, char* argv[]);
extern CC_C int openaux(FILE** dynbF, char* dynname, int auxno);
extern CC_C void auxnoname(int auxno, char* origname, int sz, char* auxname);
extern CC_C void sd_check_for_interrupt(void );
extern CC_C void kane_mm_decls(FILE* F);
extern CC_C int COMPUTE_kane_mm(FILE* mainF, char* rouvar, int maxaux, char* dynname, int* nxtaux);
extern CC_C void ordern_mm_decls(FILE* F);
extern CC_C int COMPUTE_ordern_mm(FILE* mainF, char* rouvar, int maxaux, char* dynname, int* nxtaux);
extern CC_C void SDLDU(FILE* F, int ZeroPossible, expr M11, expr M12, expr M22, expr* L11xx, expr* L21xx, expr* L22xx, expr* D1xx, expr* D2xx);
extern CC_C void  COMPUTE_lptemps(FILE* F);
extern CC_C void  COMPUTE_lvtemps(FILE* F);
extern CC_C void  COMPUTE_lperr(expr perrx);
extern CC_C void  COMPUTE_lverr(expr verrx);
extern CC_C void  COMPUTE_latemps(FILE* F);
extern CC_C void  COMPUTE_laerr(expr aerrx);
extern CC_C void  COMPUTE_presperr(FILE* F);
extern CC_C void  COMPUTE_presverr(FILE* F, expr lux);
extern CC_C void  COMPUTE_presaerr(FILE* F, expr ludotx);
extern CC_C void  PRINT_SDPERR(FILE* F);
extern CC_C void  PRINT_SDVERR(FILE* F, expr lux);
extern CC_C void  PRINT_SDAERR(FILE* F, expr ludotx);
extern CC_C expr BALL_COS(Index_t k, Index_t bnum);
extern CC_C expr PIN_COS(Index_t d, expr Lambda);
extern CC_C void DECOMPOSE_123(FILE* F, expr dcx, sym th1, sym th2, sym th3, expr* e1, expr* e2, expr* e3);
extern CC_C void DECOMPOSE_UJOINT(FILE* F, expr dcx, sym quot, sym angle, sym th1, sym th2, sym th3, sym sinth2, expr* e1, expr* e2, expr* e3);
extern CC_C void DECOMPOSE_GIMBAL(FILE* F, int lj, sym t, sym c, sym s, sym th1, sym th2, sym th3, sym tmpv, sym irefx, sym pin2x, expr* e1, expr* e2, expr* e3);
extern CC_C void GIMBAL_DOT(FILE* F, int lj, sym pin2x, sym tmp1, sym tmp2, sym tmp3, sym wab, sym tmpv2, sym tmpv3, expr* e1d, expr* e2d, expr* e3d);
extern CC_C void GIMBAL_DOTDOT(FILE* F, int lj, sym pin2x, sym tmp1, sym tmp2, sym tmpv1, sym tmpv2, sym p2d, sym p2xd, sym tmpv5, expr* e1dd, expr* e2dd, expr* e3dd);
extern CC_C void DECOMPOSE_QUAT(FILE* F, expr dcx, sym tmp, sym tmp1, sym tmp2, sym tmp3, sym tmp4, expr* e1, expr* e2, expr* e3, expr* e4);
extern CC_C void PRINT_SDDC2ANG(FILE* F);
extern CC_C void PRINT_SDDC2QUAT(FILE* F);
extern CC_C void PRINT_SDANG2DC(FILE* F);
extern CC_C void PRINT_SDQUAT2DC(FILE* F);
extern CC_C void PRINT_VECSUBS(FILE* F);
extern CC_C void declare_sdginput_vars(FILE* F, int decl_flags);
extern CC_C void do_defines(FILE* F, char* sname, char* vnames[], int cond, char* condvnames[]);
extern CC_C void do_common(FILE* F, int more, char* sname, char* vnames[], int cond, char* condvnames[]);
extern CC_C void declare_sdgtopo_vars(FILE* F, int decl_flags);
extern CC_C void declare_sdgtemp_vars(FILE* F, int decl_flags);
extern CC_C void declare_sdgstate_vars(FILE* F, int decl_flags);
extern CC_C void declare_sdglhs_vars(FILE* F, int decl_flags);
extern CC_C void declare_sdgrhs_vars(FILE* F, int decl_flags);
extern CC_C void declare_sdgerror_vars(FILE* F, int decl_flags);
extern CC_C void declare_sdldu_proc(FILE* F, int decl_flags);
extern CC_C void declare_sdbsolv_proc(FILE* F, int decl_flags);
extern CC_C void declare_sdsolvc_proc(FILE* F, int decl_flags);
extern CC_C void COMPUTE_JOINT_CONSTS(FILE* F);
extern CC_C void COMPUTE_Wkk(FILE* F);
extern CC_C void COMPUTE_Vkk(FILE* F);
extern CC_C void COMPUTE_rkWkk(FILE* F);
extern CC_C void COMPUTE_dik(FILE* F);
extern CC_C void COMPUTE_Cik(FILE* F);
extern CC_C void COMPUTE_Cib(FILE* F);
extern CC_C void COMPUTE_gk(FILE* F);
extern CC_C void COMPUTE_Fstar(FILE* F);
extern CC_C void COMPUTE_Tstar(FILE* F);
extern CC_C void COMPUTE_tau(FILE* F);
extern CC_C void COMPUTE_ltau(FILE* F);
extern CC_C void generic_kane_fs(FILE* F, int printall, expr taux, expr fstx, expr tstx, sym fs, expr fsx);
extern CC_C void generic_ordern_fs(FILE* F, int printall, expr taux, expr fstx, expr tstx, sym fs, expr fsx);
extern CC_C void COMPUTE_fs0(FILE* F);
extern CC_C void COMPUTE_fsmult(FILE* F, sym fk, sym tk, sym tau, sym fsmult, expr fs_expr);
extern CC_C void COMPUTE_reaction(FILE* F);
extern CC_C void COMPUTE_ltauforces(FILE* F, expr taux, sym tmpv1, sym tmpv2, sym tmpv3, sym fi, sym fo, sym ti, sym to, expr fx, expr tx);
extern CC_C void PRINT_SDEQUIVHT(FILE* F);
extern CC_C void PRINT_SDFULLTRQ(FILE* F, opstats_t* opcnt);
extern CC_C void COMPUTE_weld_reaction(FILE* F, int b, sym frc, sym trq, expr* fexpr, expr* texpr);
extern CC_C int PROGRAM_EXPIRED(void );
extern CC_C int OPEN_KEY_FILE(void );
extern CC_C int rbod_is_gnd(int bnum);
extern CC_C void COMPUTE_cnk(FILE* F);
extern CC_C void COMPUTE_rnk(FILE* F);
extern CC_C void COMPUTE_wk(FILE* F);
extern CC_C void COMPUTE_vnk(FILE* F);
extern CC_C void COMPUTE_onk(FILE* F);
extern CC_C void COMPUTE_ank(FILE* F);
extern CC_C void COMPUTE_com(FILE* F);
extern CC_C void COMPUTE_cnb(FILE* F);
extern CC_C void COMPUTE_Cio(FILE* F);
extern CC_C void COMPUTE_rnb(FILE* F);
extern CC_C void COMPUTE_wb(FILE* F);
extern CC_C void COMPUTE_wbtemps(FILE* F);
extern CC_C void COMPUTE_vnb(FILE* F);
extern CC_C void COMPUTE_onb(FILE* F);
extern CC_C void COMPUTE_dyad(FILE* F);
extern CC_C void COMPUTE_anb(FILE* F);

extern CC_C void COMPUTE_generic_Otk(FILE* F, int udots, sym udotsym, sym Otksym);
extern CC_C void COMPUTE_generic_Atk(FILE* F, int udots, sym udotsym, sym Otksym, sym AiOiWisym, sym Atksym);
extern CC_C void COMPUTE_Otk(FILE* F);
extern CC_C void COMPUTE_Atk(FILE* F);
extern CC_C void PRINT_SUBR_STAMP(FILE* F);
extern CC_C void PRINT_STUB(FILE* F);
extern CC_C void ZERO_OPCNTS(opstats_t* opcnt);
extern CC_C void START_COMPSUB(opstats_t* opcnt);
extern CC_C void END_COMPSUB(FILE* F, opstats_t* opcnt, long ladd, long lmul, long ldiv, long lasg);
extern CC_C void FLUSH_VEC_GEN(FILE* F, int cl_flag, int remember, sym var, int which, expr var_expr, expr value);
extern CC_C void FLUSH_MAT_GEN(FILE* F, int cl_flag, int remember, sym var, int which, expr var_expr, expr value);
extern CC_C void FLUSH_VEC(FILE* F, sym var, int which, expr var_expr, expr value);
extern CC_C void FLUSH_VEC_NONCONST(FILE* F, sym var, int which, expr var_expr, expr value);
extern CC_C void FLUSH_VEC_ALL(FILE* F, sym var, int which, expr var_expr, expr value);
extern CC_C void FLUSH_VEC_ALL_SAVE(FILE* F, sym var, int which, expr var_expr, expr value);
extern CC_C void FLUSH_MAT(FILE* F, sym var, int which, expr var_expr, expr value);
extern CC_C void FLUSH_MAT_NONCONST(FILE* F, sym var, int which, expr var_expr, expr value);
extern CC_C void FLUSH_MAT_ALL(FILE* F, sym var, int which, expr var_expr, expr value);
extern CC_C void FLUSH_MAT_ALL_SAVE(FILE* F, sym var, int which, expr var_expr, expr value);
extern CC_C void TWOINDX(FILE* F, char* pre, char* vname, char* indx, int known, char* rdim, char* cdim, char* r, char* c, char* post);
extern CC_C void ONEINDX(FILE* F, char* pre, char* vname, char* indx, char* r, char* post);
extern CC_C void SETERR(FILE* F, int routine, int errnum);
extern CC_C void CHECK_STATE(FILE* F, int st1, int st2, int routine, int errnum);
extern CC_C void DECL_CHKBNUM(FILE* F);
extern CC_C void DECL_CHKJNUM(FILE* F);
extern CC_C void DECL_CHKJAXIS(FILE* F);
extern CC_C void DECL_CHKJPIN(FILE* F);
extern CC_C void DECL_CHKUCNUM(FILE* F);
extern CC_C void CHECK_BNUM(FILE* F, char* bnumname, int routine);
extern CC_C void CHECK_JNUM(FILE* F, char* jnumname, int routine);
extern CC_C void CHECK_JAXIS(FILE* F, char* jnumname, char* axnumname, int routine);
extern CC_C void CHECK_JPIN(FILE* F, char* jnumname, char* pinnoname, int routine);
extern CC_C void CHECK_UCNUM(FILE* F, char* ucnumname, int routine);
extern CC_C void PRINTERR(int errnum, int jointno, char* what, char* name);
extern CC_C void PRINT_SDPRERRMSG(FILE* F);
extern CC_C void PRINT_SDSERIALNO(FILE* F);
extern CC_C void PRINT_SDGENTIME(FILE* F);
extern CC_C void PRINT_SDPSEUDO(FILE* F, expr lqx, expr lux);
extern CC_C void PRINT_SDPSDERIV(FILE* F, expr lqdotx, expr ludotx);
extern CC_C void COMPUTE_Cibob(FILE* F);
extern CC_C void COMPUTE_Woiob(FILE* F);
extern CC_C void COMPUTE_Ooiob(FILE* F);
extern CC_C void COMPUTE_euler(FILE* F, sym tmp1, sym tmp2, sym tmp3, sym tmpv1, sym tmpv2, sym tmpv3, sym e1, sym e2, sym e3, sym e4, sym pin2x);
extern CC_C void COMPUTE_euldot(FILE* F, sym pin2x, sym tmp1, sym tmp2, sym tmp3, sym tmpv1, sym tmpv2, sym tmpv3);
extern CC_C void COMPUTE_eulacc(FILE* F, sym pin2x, sym tmp1, sym tmp2, sym tmpv1, sym tmpv2, sym tmpv3, sym tmpv4, sym tmpv5);
extern CC_C void COMPUTE_lq(expr lqx);
extern CC_C void COMPUTE_lu(expr lux);
extern CC_C void COMPUTE_lqdot(expr lqdotx);
extern CC_C void COMPUTE_ludot(expr ludotx);
extern CC_C void COMPUTE_qdot(FILE* F);
extern CC_C void COMPUTE_S_AND_C(FILE* F);
extern CC_C void PRINT_ROADMAP(FILE* F);
extern CC_C void PRINT_SYSTEM_DOC(FILE* F);
extern CC_C void PRINT_JTAXIS_DOC(FILE* F);
extern CC_C void PRINT_SDST2ANG(FILE* F);
extern CC_C void PRINT_SDPOSFUNC(FILE* F);
extern CC_C void PRINT_SDANAL(FILE* F);
extern CC_C void PRINT_SDPOS(FILE* F);
extern CC_C void PRINT_SDVEL(FILE* F);
extern CC_C void PRINT_SDORIENT(FILE* F);
extern CC_C void PRINT_SDANGVEL(FILE* F);
extern CC_C void PRINT_SDTRANS(FILE* F);
extern CC_C void PRINT_SDACC(FILE* F);
extern CC_C void PRINT_SDANGACC(FILE* F);
extern CC_C int PRINT_SDDOWW(FILE* mainF, int maxaux, char* dynname, int* nxtaux, opstats_t* opcnt, int* nonred);
extern CC_C void PRINT_SDUDOT0(FILE* F, opstats_t* opcnt);
extern CC_C void PRINT_SDUDOTM(FILE* F, opstats_t* opcnt);
extern CC_C void PRINT_SDDERIV(FILE* F, opstats_t* opcnt, int nindepc);
extern CC_C void PRINT_SDMULT(FILE* F);

extern CC_C void PRINT_SDLHS(FILE* F, opstats_t* opcnt);
extern CC_C void PRINT_SDDOVPK(FILE* F, opstats_t* opcnt);
extern CC_C void PRINT_SDDOLTAU(FILE* F, opstats_t* opcnt);
extern CC_C void PRINT_SDDOINER(FILE* F, opstats_t* opcnt);
extern CC_C void PRINT_SDDOFS0(FILE* F, opstats_t* opcnt);
extern CC_C int PRINT_SDDOMM(FILE* F, int maxaux, char* dynname, int* nxtaux, opstats_t* opcnt);
extern CC_C void domm_head(FILE* F, int auxno);
extern CC_C void domm_tail(FILE* F);
extern CC_C void PRINT_SDDOMMLDU(FILE* F, opstats_t* opcnt);
extern CC_C void PRINT_MAT(FILE* F);
extern CC_C void PRINT_SDREL2CART(FILE* F);
extern CC_C void PRINT_SDRHS(FILE* F, expr ludotx, opstats_t* opcnt);
extern CC_C void PRINT_SDFS0(FILE* F, opstats_t* opcnt);
extern CC_C void PRINT_SDFSMULT(FILE* F, opstats_t* opcnt);
extern CC_C void PRINT_SDFSFULL(FILE* F, opstats_t* opcnt);
extern CC_C void PRINT_SDFSGENMULT(FILE* F);
extern CC_C void PRINT_SDFSGENFULL(FILE* F);
extern CC_C void PRINT_SDREAC(FILE* F, opstats_t* opcnt);

extern CC_C void PRINT_ERRSTUFF(FILE* F);
extern CC_C void PRINT_SDERROR(FILE* F);
extern CC_C void PRINT_SDPRINTERR(FILE* F);
extern CC_C void PRINT_SDCLEARERR(FILE* F);
extern CC_C void PRINT_SDSETERR(FILE* F);
extern CC_C void PRINT_CHECKSTUFF(FILE* F);
extern CC_C void PRINT_FORCE_MOTION(FILE* F);
extern CC_C void PRINT_SDPRESACC(FILE* F);
extern CC_C void PRINT_SDPRESVEL(FILE* F);
extern CC_C void PRINT_SDPRESPOS(FILE* F);
extern CC_C void PRINT_SDGETHT(FILE* F);
extern CC_C void PRINT_SDHINGET(FILE* F);
extern CC_C void PRINT_SDPOINTF(FILE* F);
extern CC_C void PRINT_SDBODYT(FILE* F);
extern CC_C void PRINT_SDINDX(FILE* F);
extern CC_C void PRINT_SET_ROUTINES(FILE* F);
extern CC_C void PRINT_SDGRAV(FILE* F);
extern CC_C void PRINT_SDMASS(FILE* F);
extern CC_C void PRINT_SDINER(FILE* F);
extern CC_C void PRINT_SDBTJ(FILE* F);
extern CC_C void PRINT_SDITJ(FILE* F);
extern CC_C void PRINT_SDPIN(FILE* F);
extern CC_C void PRINT_SDPRES(FILE* F);
extern CC_C void PRINT_SDCONSCHG(FILE* F);
extern CC_C void PRINT_SDSTAB(FILE* F);
extern CC_C void PRINT_GET_ROUTINES(FILE* F, int nonred);
extern CC_C void PRINT_SDGETGRAV(FILE* F);
extern CC_C void PRINT_SDGETMASS(FILE* F);
extern CC_C void PRINT_SDGETINER(FILE* F);
extern CC_C void PRINT_SDGETBTJ(FILE* F);
extern CC_C void PRINT_SDGETITJ(FILE* F);
extern CC_C void PRINT_SDGETPIN(FILE* F);
extern CC_C void PRINT_SDGETPRES(FILE* F);
extern CC_C void PRINT_SDGETSTAB(FILE* F);
extern CC_C void PRINT_SDINFO(FILE* F, int nonred);
extern CC_C void PRINT_SDJNT(FILE* F);
extern CC_C void PRINT_SDCONS(FILE* F);

extern CC_C void PRINT_SDBTJITJ(FILE* F, int routine, char* which, sym vsym, char* vsymq, sym lvsym, char* lvsymq);
extern CC_C void PRINT_SDGETBTJITJ(FILE* F, int routine, char* which, sym vsym, sym lvsym);
extern CC_C void PRINT_SETPRESVAL(FILE* F, int routine, char* pname, char* vname, char* lvname);

extern CC_C void DECLARE_GLOBALS(FILE* F, int init);
extern CC_C void init_sdgtopo(FILE* F, int decl_flags);
extern CC_C void init_sdginput(FILE* F, int decl_flags);
extern CC_C void DECLARE_LIB_GLOBALS(FILE* F);
extern CC_C void init_sdgerror(FILE* F, int decl_flags);

extern CC_C int PRINT_SDINIT(FILE* F);
extern CC_C void REGION_INITIAL(FILE* F);

extern CC_C void PRINT_SDINTEG(FILE* F);

extern CC_C void COMPUTE_mfrc(FILE* F, expr multx, mfrcsym_t* mfrcsym, expr fk_expr, expr tk_expr, expr tau_expr, int* lasg, int* nxtlab);
extern CC_C void COMPUTE_pmfrc(FILE* F, expr multx, expr mltaux, mfrcsym_t* syms, expr fk_expr, expr tk_expr, expr tau_expr);
extern CC_C void COMPUTE_lmfrc(FILE* F, expr multx, mfrcsym_t* syms, expr fk_expr, expr tk_expr);
extern CC_C void PRINT_SDMFRC(FILE* F, opstats_t* opcnt);

extern CC_C void PRINT_SDMOM(FILE* F, opstats_t* opcnt);
extern CC_C void make_mat(expr r, expr mat);
extern CC_C void PRINT_SDSYS(FILE* F, opstats_t* opcnt);

extern CC_C void PRINT_SDRESID(FILE* F, opstats_t* opcnt);
extern CC_C void PRINT_SDROOT(FILE* F);

extern CC_C void COMPUTE_LDUCOST(int w, opstats_t* opcntldu, opstats_t* opcntbsl, opstats_t* opcntbsd);
extern CC_C void COMPUTE_QRCOST(int r, int c, int k, opstats_t* opcntqr, opstats_t* opcntqrbslv);
extern CC_C void PRINT_SDLDUSLV(FILE* F);
extern CC_C void PRINT_SDQRSLV(FILE* F);
extern CC_C void PRINT_SDLSSLV(FILE* F);

extern CC_C void PRINT_SDSTATE(FILE* F, expr lqx, expr lux, expr lqdotx, opstats_t* opcnt);
extern CC_C void PRINT_SDU2QDOT(FILE* F);
extern CC_C void PRINT_SDQDOT(FILE* F);
extern CC_C void PRINT_SDPSSTATE(FILE* F);
extern CC_C void COMPUTE_rpp(FILE* F);
extern CC_C void COMPUTE_rpri(FILE* F);
extern CC_C void COMPUTE_rpk(FILE* F);
extern CC_C void COMPUTE_rik(FILE* F);
extern CC_C void COMPUTE_rik2(FILE* F);
extern CC_C void COMPUTE_rikt(FILE* F);
extern CC_C void COMPUTE_Wik(FILE* F);
extern CC_C void COMPUTE_Vik(FILE* F);
extern CC_C void COMPUTE_Wirk(FILE* F);
extern CC_C void COMPUTE_Wkrpk(FILE* F);
extern CC_C void COMPUTE_VikWkr(FILE* F);
extern CC_C void COMPUTE_WkIkWk(FILE* F);
extern CC_C void COMPUTE_WkIkbWk(FILE* F);
extern CC_C void COMPUTE_Wpk(FILE* F);
extern CC_C void COMPUTE_Vpk(FILE* F);
extern CC_C void COMPUTE_ping(FILE* F);
extern CC_C void COMPUTE_hngpt(FILE* F);
