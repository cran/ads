#include <time.h>
#include <stdlib.h>
#include <limits.h>
#include <R_ext/PrtUtil.h>

double bacos(double);
void complete_tab(int,double**,double**,int*,int*,int*,double*,double*); 
void decalCirc(int,double*,double*,double*,double*,double); 
void decalCirc2(int,double*,double*,int,double*,double*,double*,double*,double); 
void decalCircTri(int,double*,double*,double*,double*,double, int,double*,double*,double*,double*,double*,double*);
void decalCircTri2(int,double*,double*,int,double*,double*,double*,double*,double,int,double*,double*,double*,double*,double*,double*); 
void decalRect(int,double*,double*,double*,double*,double*,double*); 
void decalRect2(int,double*,double*,int,double*,double*,double*,double*,double*,double*); 
void decalRectTri(int,double*,double*,double*,double*,double*,double*, int,double*,double*,double*,double*,double*,double*);
void decalRectTri2(int,double*,double*,int,double*,double*,double*,double*,double*,double*, int,double*,double*,double*,double*,double*,double*);
void decalSample(int,double*,double*,double,double); 
void decalVal(double*,int,double); 
void freeinttab(int**); 
void freeintvec(int*); 
void freetab(double**); 
void freevec(double*); 
double Pi(); 
void progress(int,int*,int); 
void taballoc(double***,int,int); 
double** taballoca(int,int*); 
void tabintalloc(int***,int,int); 
void vecalloc(double**,int);
void vecintalloc(int**,int);
