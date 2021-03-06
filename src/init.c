#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>
#include "triangulate.h"
#include "Zlibs.h"
#include "adssub.h"

// Generated by using tools::package_native_routine_registration_skeleton(dir)

extern int triangulate(int *npoly, int *tabpt, int *nptTot,double *vertX, double *vertY, int *ntri,
                       double *X1, double *Y1,double *X2, double *Y2,double *X3, double *Y3);
  
static const R_CMethodDef CEntries[] = {
  {"corr_disq",              (DL_FUNC) &corr_disq,              11},
  {"corr_disq_ic",           (DL_FUNC) &corr_disq_ic,           19},
  {"corr_rect",              (DL_FUNC) &corr_rect,              12},
  {"corr_rect_ic",           (DL_FUNC) &corr_rect_ic,           20},
  {"corr_tr_disq",           (DL_FUNC) &corr_tr_disq,           18},
  {"corr_tr_rect",           (DL_FUNC) &corr_tr_rect,           19},
  {"corr_tr_rect_ic",        (DL_FUNC) &corr_tr_rect_ic,        27},
  {"density_disq",           (DL_FUNC) &density_disq,           12},
  {"density_rect",           (DL_FUNC) &density_rect,           13},
  {"density_tr_disq",        (DL_FUNC) &density_tr_disq,        19},
  {"density_tr_rect",        (DL_FUNC) &density_tr_rect,        20},
  {"intertype_disq",         (DL_FUNC) &intertype_disq,         13},
  {"intertype_disq_ic",      (DL_FUNC) &intertype_disq_ic,      29},
  {"intertype_rect",         (DL_FUNC) &intertype_rect,         14},
  {"intertype_rect_ic",      (DL_FUNC) &intertype_rect_ic,      30},
  {"intertype_tr_disq",      (DL_FUNC) &intertype_tr_disq,      20},
  {"intertype_tr_disq_ic",   (DL_FUNC) &intertype_tr_disq_ic,   36},
  {"intertype_tr_rect",      (DL_FUNC) &intertype_tr_rect,      21},
  {"intertype_tr_rect_ic",   (DL_FUNC) &intertype_tr_rect_ic,   37},
  {"intertypelocal_disq",    (DL_FUNC) &intertypelocal_disq,    13},
  {"intertypelocal_rect",    (DL_FUNC) &intertypelocal_rect,    14},
  {"intertypelocal_tr_disq", (DL_FUNC) &intertypelocal_tr_disq, 20},
  {"intertypelocal_tr_rect", (DL_FUNC) &intertypelocal_tr_rect, 21},
  {"mimetic_disq",           (DL_FUNC) &mimetic_disq,           19},
  {"mimetic_rect",           (DL_FUNC) &mimetic_rect,           20},
  {"mimetic_tr_disq",        (DL_FUNC) &mimetic_tr_disq,        26},
  {"mimetic_tr_rect",        (DL_FUNC) &mimetic_tr_rect,        27},
  {"pnpoly",                 (DL_FUNC) &pnpoly,                 11},
  {"rao_disq",               (DL_FUNC) &rao_disq,               19},
  {"rao_disq_ic",            (DL_FUNC) &rao_disq_ic,            27},
  {"rao_rect",               (DL_FUNC) &rao_rect,               20},
  {"rao_rect_ic",            (DL_FUNC) &rao_rect_ic,            28},
  {"rao_tr_disq",            (DL_FUNC) &rao_tr_disq,            26},
  {"rao_tr_disq_ic",         (DL_FUNC) &rao_tr_disq_ic,         34},
  {"rao_tr_rect",            (DL_FUNC) &rao_tr_rect,            27},
  {"rao_tr_rect_ic",         (DL_FUNC) &rao_tr_rect_ic,         35},
  {"ripley_disq",            (DL_FUNC) &ripley_disq,            10},
  {"ripley_disq_ic",         (DL_FUNC) &ripley_disq_ic,         22},
  {"ripley_rect",            (DL_FUNC) &ripley_rect,            11},
  {"ripley_rect_ic",         (DL_FUNC) &ripley_rect_ic,         23},
  {"ripley_tr_disq",         (DL_FUNC) &ripley_tr_disq,         17},
  {"ripley_tr_disq_ic",      (DL_FUNC) &ripley_tr_disq_ic,      29},
  {"ripley_tr_rect",         (DL_FUNC) &ripley_tr_rect,         18},
  {"ripley_tr_rect_ic",      (DL_FUNC) &ripley_tr_rect_ic,      30},
  {"ripleylocal_disq",       (DL_FUNC) &ripleylocal_disq,       10},
  {"ripleylocal_rect",       (DL_FUNC) &ripleylocal_rect,       11},
  {"ripleylocal_tr_disq",    (DL_FUNC) &ripleylocal_tr_disq,    17},
  {"ripleylocal_tr_rect",    (DL_FUNC) &ripleylocal_tr_rect,    18},
  {"shen",                   (DL_FUNC) &shen,                   13},
  {"shen_ic",                (DL_FUNC) &shen_ic,                21},
  {"shimatani_disq",         (DL_FUNC) &shimatani_disq,         14},
  {"shimatani_disq_ic",      (DL_FUNC) &shimatani_disq_ic,      23},
  {"shimatani_rect",         (DL_FUNC) &shimatani_rect,         15},
  {"shimatani_rect_ic",      (DL_FUNC) &shimatani_rect_ic,      24},
  {"shimatani_tr_disq",      (DL_FUNC) &shimatani_tr_disq,      21},
  {"shimatani_tr_disq_ic",   (DL_FUNC) &shimatani_tr_disq_ic,   30},
  {"shimatani_tr_rect",      (DL_FUNC) &shimatani_tr_rect,      22},
  {"shimatani_tr_rect_ic",   (DL_FUNC) &shimatani_tr_rect_ic,   31},
  {"triangulate",            (DL_FUNC) &triangulate,            12},
  {NULL, NULL, 0}
};

void R_init_ads(DllInfo *dll)
{
  R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
