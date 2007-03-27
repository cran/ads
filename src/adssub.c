#include "adssub.h"
#include <math.h>



double Pi(){
	return 2*acos(0);
}


void progress(int i,int *l, int max) {
	int nb=20;
	int p=i*(nb+1)/max;
	int j;

	if(p>*l){
		for(j=*l;j<p;j++) {
			if(j==nb) {
				Rprintf("ok\n");
			}
			else {
				Rprintf(".");
			}
		}
		*l=p;
	}
}



/**************************/
double alea ()
{
    double w;
    w = ((double) rand())/ (double)RAND_MAX;
    return (w);
}




/***********************************************************************/
void freeintvec (int *vec)
/*--------------------------------------------------
* liberation de memoire pour un vecteur
--------------------------------------------------*/
{

    free((char *) vec);

}
/***********************************************************************/
void freetab (double **tab)
/*--------------------------------------------------
* Allocation de memoire dynamique pour un tableau (l1, c1)
--------------------------------------------------*/
{
    int     i, n;

    n = *(*(tab));
    for (i=0;i<=n;i++) {
            free((char *) *(tab+i) );
    }
    free((char *) tab);
}
/***********************************************************************/
void freevec (double *vec)
/*--------------------------------------------------
* liberation de memoire pour un vecteur
--------------------------------------------------*/
{
    free((char *) vec);
}

/***********************************************************************/
void taballoc (double ***tab, int l1, int c1)
/*--------------------------------------------------
* Allocation de memoire dynamique pour un tableau (l1, c1)
--------------------------------------------------*/
{
    int i, j;

    if ( (*tab = (double **) calloc(l1+1, sizeof(double *))) != 0) {
        for (i=0;i<=l1;i++) {
            if ( (*(*tab+i)=(double *) calloc(c1+1, sizeof(double))) == 0 ) {
                return;
                for (j=0;j<i;j++) {
                    free(*(*tab+j));
                }
            }
        }
    }

    **(*tab) = l1;
    **(*tab+1) = c1;
}

void tabintalloc (int ***tab, int l1, int c1)
/*--------------------------------------------------
* Allocation de memoire dynamique pour un tableau
* d'entiers (l1, c1)
--------------------------------------------------*/
{
    int     i, j;

    *tab = (int **) calloc(l1+1, sizeof(int *));

    if ( *tab != NULL) {
        for (i=0;i<=l1;i++) {

            *(*tab+i)=(int *) calloc(c1+1, sizeof(int));
            if ( *(*tab+i) == NULL ) {
                for (j=0;j<i;j++) {
                    free(*(*tab+j));
                }
                return;
            }
        }
    } else return;
    **(*tab) = l1;
    **(*tab+1) = c1;
    for (i=1;i<=l1;i++) {
        for (j=1;j<=c1;j++) {
            (*tab)[i][j] = 0;
        }
    }
}

void freeinttab (int **tab)
/*--------------------------------------------------
* Allocation de memoire dynamique pour un tableau
--------------------------------------------------*/
{
    int     i, n;

    n = *(*(tab));

    for (i=0;i<=n;i++) {
            free((char *) *(tab+i) );
    }

    free((char *) tab);
}


/***********************************************************************/
void vecalloc (double **vec, int n)
/*--------------------------------------------------
* Allocation de memoire pour un vecteur de longueur n
--------------------------------------------------*/
{
    if ( (*vec = (double *) calloc(n+1, sizeof(double))) != 0) {
        **vec = n;
        return;
    } else {
        return;
    }
}
/***********************************************************************/
void vecintalloc (int **vec, int n)
/*--------------------------------------------------
* Allocation de memoire pour un vecteur d'entiers de longueur n
--------------------------------------------------*/
{
    if ( (*vec = (int *) calloc(n+1, sizeof(int))) != NULL) {
        **vec = n;
        return;
    } else {
        return;
    }
}

/*pour les triangles à exclure*/
double bacos(double a) {
	double b;

	if (a>=1)
		b=0;
	else if (a<=-1)
		b=Pi();
	else
		b=acos(a);

	return b;
}


//Décale les valeurs de v de la valeur val
void decalVal(double *v, int n, double val) {
	int i;

	for(i=0;i<n;i++) {
		v[i]=v[i]+val;
	}
}

//Decale les points et la fenetre rectangulaire
void decalRect(int point_nb,double *x, double *y,double *xmin, double *xmax, double *ymin, double *ymax) {
	if(*xmin<0) {
		decalVal(x,point_nb,-*xmin);
		*xmax=*xmax-*xmin;
		*xmin=0;
	}
	if(*ymin<0) {
		decalVal(y,point_nb,-*ymin);
		*ymax=*ymax-*ymin;
		*ymin=0;
	}
}

//Decale les points et la fenetre circulaire
void decalCirc(int point_nb,double *x, double *y,double *x0, double *y0, double r0) {
	int xmin=*x0-r0;
	int ymin=*y0-r0;

	if(xmin<0) {
		decalVal(x,point_nb,-xmin);
		*x0=*x0-xmin;
	}
	if(ymin<0) {
		decalVal(y,point_nb,-ymin);
		*y0=*y0-ymin;
	}
}

//Decale les points et la fenetre rectangulaire + triangles
void decalRectTri(int point_nb,double *x, double *y,double *xmin, double *xmax, double *ymin, double *ymax,
int tri_nb,double *ax, double *ay, double *bx, double *by, double *cx, double *cy) {
	if(*xmin<0) {
		decalVal(x,point_nb,-*xmin);
		decalVal(ax,tri_nb,-*xmin);
		decalVal(bx,tri_nb,-*xmin);
		decalVal(cx,tri_nb,-*xmin);
		*xmax=*xmax-*xmin;
		*xmin=0;
	}
	if(*ymin<0) {
		decalVal(y,point_nb,-*ymin);
		decalVal(ay,tri_nb,-*ymin);
		decalVal(by,tri_nb,-*ymin);
		decalVal(cy,tri_nb,-*ymin);
		*ymax=*ymax-*ymin;
		*ymin=0;
	}
}

//Decale les points et la fenetre circulaire + triangles
void decalCircTri(int point_nb,double *x, double *y,double *x0, double *y0, double r0,
int tri_nb,double *ax, double *ay, double *bx, double *by, double *cx, double *cy) {
	int xmin=*x0-r0;
	int ymin=*y0-r0;

	if(xmin<0) {
		decalVal(x,point_nb,-xmin);
		decalVal(ax,tri_nb,-xmin);
		decalVal(bx,tri_nb,-xmin);
		decalVal(cx,tri_nb,-xmin);
		*x0=*x0-xmin;
	}
	if(ymin<0) {
		decalVal(y,point_nb,-ymin);
		decalVal(ay,tri_nb,-ymin);
		decalVal(by,tri_nb,-ymin);
		decalVal(cy,tri_nb,-ymin);
		*y0=*y0-ymin;
	}
}


//Decale les points et la fenetre rectangulaire (semis bivarié)
void decalRect2(int point_nb1,double *x1, double *y1,int point_nb2,double *x2, double *y2,
double *xmin, double *xmax, double *ymin, double *ymax) {
	if(*xmin<0) {
		decalVal(x1,point_nb1,-*xmin);
		decalVal(x2,point_nb2,-*xmin);
		*xmax=*xmax-*xmin;
		*xmin=0;
	}
	if(*ymin<0) {
		decalVal(y1,point_nb1,-*ymin);
		decalVal(y2,point_nb2,-*ymin);
		*ymax=*ymax-*ymin;
		*ymin=0;
	}
}

//Decale les points et la fenetre circulaire (semis bivarié)
void decalCirc2(int point_nb1,double *x1, double *y1,int point_nb2,double *x2, double *y2,
double *x0, double *y0, double r0) {
	int xmin=*x0-r0;
	int ymin=*y0-r0;

	if(xmin<0) {
		decalVal(x1,point_nb1,-xmin);
		decalVal(x2,point_nb2,-xmin);
		*x0=*x0-xmin;
	}
	if(ymin<0) {
		decalVal(y1,point_nb1,-ymin);
		decalVal(y2,point_nb2,-ymin);
		*y0=*y0-ymin;
	}
}

//Decale les points et la fenetre rectangulaire + triangles (semis bivarié)
void decalRectTri2(int point_nb1,double *x1, double *y1,int point_nb2,double *x2, double *y2,
double *xmin, double *xmax, double *ymin, double *ymax,
int tri_nb,double *ax, double *ay, double *bx, double *by, double *cx, double *cy) {
	if(*xmin<0) {
		decalVal(x1,point_nb1,-*xmin);
		decalVal(x2,point_nb2,-*xmin);
		decalVal(ax,tri_nb,-*xmin);
		decalVal(bx,tri_nb,-*xmin);
		decalVal(cx,tri_nb,-*xmin);
		*xmax=*xmax-*xmin;
		*xmin=0;
	}
	if(*ymin<0) {
		decalVal(y1,point_nb1,-*ymin);
		decalVal(y2,point_nb2,-*ymin);
		decalVal(ay,tri_nb,-*ymin);
		decalVal(by,tri_nb,-*ymin);
		decalVal(cy,tri_nb,-*ymin);
		*ymax=*ymax-*ymin;
		*ymin=0;
	}
}

//Decale les points et la fenetre circulaire + triangles (semis bivarié)
void decalCircTri2(int point_nb1,double *x1, double *y1,int point_nb2,double *x2, double *y2,
double *x0, double *y0, double r0,
int tri_nb,double *ax, double *ay, double *bx, double *by, double *cx, double *cy) {
	int xmin=*x0-r0;
	int ymin=*y0-r0;

	if(xmin<0) {
		decalVal(x1,point_nb1,-xmin);
		decalVal(x2,point_nb2,-xmin);
		decalVal(ax,tri_nb,-xmin);
		decalVal(bx,tri_nb,-xmin);
		decalVal(cx,tri_nb,-xmin);
		*x0=*x0-xmin;
	}
	if(ymin<0) {
		decalVal(y1,point_nb1,-ymin);
		decalVal(y2,point_nb2,-ymin);
		decalVal(ay,tri_nb,-ymin);
		decalVal(by,tri_nb,-ymin);
		decalVal(cy,tri_nb,-ymin);
		*y0=*y0-ymin;
	}
}


//Decale les points d'echantillonnages (pour density)
void decalSample(int sample_nb,double *x, double *y, double xmin, double ymin) {
	if(xmin<0) {
		decalVal(x,sample_nb,-xmin);
	}
	if(ymin<0) {
		decalVal(y,sample_nb,-ymin);
	}
}

