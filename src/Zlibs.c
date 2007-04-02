#include "adssub.h"
#include "Zlibs.h"
#include <math.h>
#include <R.h>


// a exterieur ; b et c interieur
double un_point( double ax, double ay, double bx, double by, double cx, double cy, double x, double y, double d)
{	double alpha, beta, gamma, delta, ttt, ang;
	double ex,ey,fx,fy;

	// premier point d'intersection

	alpha=(bx-ax)*(bx-ax)+(by-ay)*(by-ay);
	beta=(2*(ax-x)*(bx-ax)+2*(ay-y)*(by-ay));
	gamma=((ax-x)*(ax-x)+(ay-y)*(ay-y)-d*d);
	delta=beta*beta-4*alpha*gamma;
	if (delta<=0)
		Rprintf("erreur1\n");
	ttt=(-beta-sqrt(delta))/(2*alpha);
	if ((ttt<=0)||(ttt>=1))
		Rprintf("erreur2\n");
	ex=ax+ttt*(bx-ax);
	ey=ay+ttt*(by-ay);

	// deuxieme point d'intersection

	alpha=(cx-ax)*(cx-ax)+(cy-ay)*(cy-ay);
	beta=(2*(ax-x)*(cx-ax)+2*(ay-y)*(cy-ay));
	delta=beta*beta-4*alpha*gamma;
	if (delta<=0)
		Rprintf("erreur3\n");
	ttt=(-beta-sqrt(delta))/(2*alpha);
	if ((ttt<=0)||(ttt>=1))
		Rprintf("erreur4\n");
	fx=ax+ttt*(cx-ax);
	fy=ay+ttt*(cy-ay);

	// calcul de l'angle
	ang=bacos(((ex-x)*(fx-x)+(ey-y)*(fy-y))/(d*d));
	return ang;
}

// a interieur , b et c exterieur
double deux_point(double ax, double ay, double bx, double by, double cx, double cy,double x, double y, double d)
{	double alpha, beta, gamma, delta, ttt, ang;
	double ex,ey,fx,fy,gx,gy,hx,hy;
	int cas;

	// premier point d'intersection
	alpha=((bx-ax)*(bx-ax)+(by-ay)*(by-ay));
	beta=(2*(ax-x)*(bx-ax)+2*(ay-y)*(by-ay));
	gamma=((ax-x)*(ax-x)+(ay-y)*(ay-y)-d*d);
	delta=beta*beta-4*alpha*gamma;
	if (delta<=0)
	Rprintf("erreur6\n");
	ttt=(-beta+sqrt(delta))/(2*alpha);
	if ((ttt<=0)||(ttt>=1))
		Rprintf("erreur7\n");
	ex=ax+ttt*(bx-ax);
	ey=ay+ttt*(by-ay);

	// deuxieme point d'intersection
	alpha=((cx-ax)*(cx-ax)+(cy-ay)*(cy-ay));
	beta=(2*(ax-x)*(cx-ax)+2*(ay-y)*(cy-ay));
	delta=beta*beta-4*alpha*gamma;
	if (delta<=0)
		Rprintf("erreur8\n");
	ttt=(-beta+sqrt(delta))/(2*alpha);
	if ((ttt<=0)||(ttt>=1))
		Rprintf("erreur9\n");
	fx=ax+ttt*(cx-ax);
	fy=ay+ttt*(cy-ay);

	// y a t il deux autres intersections?
	cas=0;
	alpha=((cx-bx)*(cx-bx)+(cy-by)*(cy-by));
	beta=(2*(bx-x)*(cx-bx)+2*(by-y)*(cy-by));
	gamma=((bx-x)*(bx-x)+(by-y)*(by-y)-d*d);
	delta=beta*beta-4*alpha*gamma;
	if (delta>0)
	{	ttt=(-beta-sqrt(delta))/(2*alpha);
		if ((ttt>=0)&&(ttt<=1))
		{	gx=bx+ttt*(cx-bx);
			gy=by+ttt*(cy-by);
			ttt=(-beta+sqrt(delta))/(2*alpha);
			if ((ttt>=0)&&(ttt<=1))
			{	cas=1;
				hx=bx+ttt*(cx-bx);
				hy=by+ttt*(cy-by);
			}
			else
				Rprintf("erreur9bis\n");
		}
	}

	// calcul de l'angle
	if (cas==0)
		ang=bacos(((ex-x)*(fx-x)+(ey-y)*(fy-y))/(d*d));
	else
	{	ang=bacos(((ex-x)*(gx-x)+(ey-y)*(gy-y))/(d*d));
		ang+=bacos(((fx-x)*(hx-x)+(fy-y)*(hy-y))/(d*d));
	}

	return ang;
}

// a exterieur, b interieur, c sur le bord
double ununun_point(double ax, double ay, double bx, double by, double cx, double cy, double x, double y, double d)
{	double alpha, beta, gamma, delta, ttt, ang;
	double ex,ey,fx,fy;

	// premier point d'intersection sur ab
	alpha=(bx-ax)*(bx-ax)+(by-ay)*(by-ay);
	beta=(2*(ax-x)*(bx-ax)+2*(ay-y)*(by-ay));
	gamma=((ax-x)*(ax-x)+(ay-y)*(ay-y)-d*d);
	delta=beta*beta-4*alpha*gamma;
	if (delta<=0)
		Rprintf("erreur1b\n");
	ttt=(-beta-sqrt(delta))/(2*alpha);
	if ((ttt<=0)||(ttt>=1))
		Rprintf("erreur2b\n");
	ex=ax+ttt*(bx-ax);
	ey=ay+ttt*(by-ay);

	// deuxieme point d'intersection ac
	alpha=(cx-ax)*(cx-ax)+(cy-ay)*(cy-ay);
	beta=(2*(ax-x)*(cx-ax)+2*(ay-y)*(cy-ay));
	delta=beta*beta-4*alpha*gamma;
	ttt=1;
	if (delta>0)
	{	ttt=(-beta-sqrt(delta))/(2*alpha);
		if ((ttt<=0)||(ttt>1))
			ttt=1;
		if (ttt<=0)
			Rprintf("e3b\n");
	}
	fx=ax+ttt*(cx-ax);
	fy=ay+ttt*(cy-ay);

	// calcul de l'angle
	ang=bacos(((ex-x)*(fx-x)+(ey-y)*(fy-y))/(d*d));
	return ang;
}

// a,b et c exterieurs
double trois_point(double ax, double ay, double bx, double by, double cx, double cy, double x, double y, double d)
{	double alpha, beta, gamma, delta, te,tf,tg,th,ti,tj, ang;
	double ex=0,ey=0,fx=0,fy=0,gx=0,gy=0,hx=0,hy=0,ix=0,iy=0,jx=0,jy=0;

	// premier segment ab
	alpha=(bx-ax)*(bx-ax)+(by-ay)*(by-ay);
	beta=2*(ax-x)*(bx-ax)+2*(ay-y)*(by-ay);
	gamma=(ax-x)*(ax-x)+(ay-y)*(ay-y)-d*d;
	delta=beta*beta-4*alpha*gamma;
	if (delta<0)
	{	te=-1;
		tf=-1;
	}
	else
	{	te=(-beta-sqrt(delta))/(2*alpha);
		tf=(-beta+sqrt(delta))/(2*alpha);
		if ((te<0)||(te>=1)||(tf==0))
		{	te=-1;
			tf=-1;
		}
		else
		{	ex=ax+te*(bx-ax);
			ey=ay+te*(by-ay);
			fx=ax+tf*(bx-ax);
			fy=ay+tf*(by-ay);
			if ((tf<=0)||(tf>1))
				Rprintf("pb te %f tf %f\n",te,tf);
		}
	}

	// deuxieme segment bc
	alpha=(cx-bx)*(cx-bx)+(cy-by)*(cy-by);
	beta=2*(bx-x)*(cx-bx)+2*(by-y)*(cy-by);
	gamma=(bx-x)*(bx-x)+(by-y)*(by-y)-d*d;
	delta=beta*beta-4*alpha*gamma;
	if (delta<0)
	{	tg=-1;
		th=-1;
	}
	else
	{	tg=(-beta-sqrt(delta))/(2*alpha);
		th=(-beta+sqrt(delta))/(2*alpha);
		if ((tg<0)||(tg>=1)||(th==0))
		{	tg=-1;
			th=-1;
		}
		else
		{	gx=bx+tg*(cx-bx);
			gy=by+tg*(cy-by);
			hx=bx+th*(cx-bx);
			hy=by+th*(cy-by);
			if ((th<=0)||(th>1))
				Rprintf("pb tg %f th %f\n",tg,th);
		}
	}

	// troisieme segment ca
	alpha=(ax-cx)*(ax-cx)+(ay-cy)*(ay-cy);
	beta=2*(cx-x)*(ax-cx)+2*(cy-y)*(ay-cy);
	gamma=(cx-x)*(cx-x)+(cy-y)*(cy-y)-d*d;
	delta=beta*beta-4*alpha*gamma;
	if (delta<0)
	{	ti=-1;
		tj=-1;
	}
	else
	{	ti=(-beta-sqrt(delta))/(2*alpha);
		tj=(-beta+sqrt(delta))/(2*alpha);
		if ((ti<0)||(ti>=1)||(tj==0))
		{	ti=-1;
			tj=-1;
		}
		else
		{	ix=cx+ti*(ax-cx);
			iy=cy+ti*(ay-cy);
			jx=cx+tj*(ax-cx);
			jy=cy+tj*(ay-cy);
			if ((tj<=0)||(tj>1))
				Rprintf("pb ti %f tj %f\n",ti,tj);
		}
	}

	// quelle configuration ?
	if (te<0)
	{	if (tg<0)
		{	if (ti<0)
				// pas d'intersection... ouf!
				ang=0;
			else
				// un seul cote (ca) coupe le cercle en i,j
				ang=bacos(((ix-x)*(jx-x)+(iy-y)*(jy-y))/(d*d));
		}
		else
		{	if (ti<0)
				// un seul cote (bc) coupe le cercle en g,h
				ang=bacos(((gx-x)*(hx-x)+(gy-y)*(hy-y))/(d*d));
			else
			{	// deux cotes (bc et ca) coupent le cercle en g,h,i,j
				ang=bacos(((gx-x)*(jx-x)+(gy-y)*(jy-y))/(d*d));
				ang+=bacos(((hx-x)*(ix-x)+(hy-y)*(iy-y))/(d*d));
			}
		}
	}
	else
	{	if (tg<0)
		{	if (ti<0)
				// un seul cote (ab) coupe le cercle en e,f
				ang=bacos(((ex-x)*(fx-x)+(ey-y)*(fy-y))/(d*d));
			else
			{	// deux cotes (ab et ca) coupent le cercle en e,f,i,j
				ang=bacos(((ex-x)*(jx-x)+(ey-y)*(jy-y))/(d*d));
				ang+=bacos(((fx-x)*(ix-x)+(fy-y)*(iy-y))/(d*d));
			}
		}
		else
		{	if (ti<0)
			{	// deux cotes (ab et bc) coupent le cercle en e,f,g,h
				ang=bacos(((ex-x)*(hx-x)+(ey-y)*(hy-y))/(d*d));
				ang+=bacos(((fx-x)*(gx-x)+(fy-y)*(gy-y))/(d*d));
			}
			else
			{	// les trois cotes coupent le cercle
				ang=bacos(((ex-x)*(jx-x)+(ey-y)*(jy-y))/(d*d));
				ang+=bacos(((hx-x)*(ix-x)+(hy-y)*(iy-y))/(d*d));
				ang+=bacos(((fx-x)*(gx-x)+(fy-y)*(gy-y))/(d*d));
			}
		}
	}

	//if ((ang<0)||(ang>Pi()))
	if ((ang<0)||(ang>3.141593))
		Rprintf("erreur12 : ang=%11.10f, %d %d %d %d %d %d\n",ang,te,tf,tg,th,ti,tj);

	return ang;
}

// a est le point sur le bord , b et c exterieur
double deuxun_point(double ax, double ay, double bx, double by, double cx, double cy,double x, double y, double d)
{	double alpha, beta, gamma, delta, te,tf,tg,th, ang;
	double ex,ey,fx,fy,gx,gy,hx,hy;
	int cas;

	// premier point d'intersection
	alpha=((bx-ax)*(bx-ax)+(by-ay)*(by-ay));
	beta=(2*(ax-x)*(bx-ax)+2*(ay-y)*(by-ay));
	gamma=((ax-x)*(ax-x)+(ay-y)*(ay-y)-d*d);
	delta=beta*beta-4*alpha*gamma;
	te=0;
	if (delta>0)
	{	te=(-beta+sqrt(delta))/(2*alpha);
		if ((te<0)||(te>=1))
			te=0;
		if (te>=1)
			Rprintf("e15\n");
	}
	ex=ax+te*(bx-ax);
	ey=ay+te*(by-ay);

	// deuxieme point d'intersection
	alpha=((cx-ax)*(cx-ax)+(cy-ay)*(cy-ay));
	beta=(2*(ax-x)*(cx-ax)+2*(ay-y)*(cy-ay));
	delta=beta*beta-4*alpha*gamma;
	tf=0;
	if (delta>0)
	{	tf=(-beta+sqrt(delta))/(2*alpha);
		if ((tf<0)||(tf>=1))
			tf=0;
		if (tf>=1)
			Rprintf("e15\n");
	}
	fx=ax+tf*(cx-ax);
	fy=ay+tf*(cy-ay);

	// y a t il deux autres intersections?
	cas=0;
	alpha=((cx-bx)*(cx-bx)+(cy-by)*(cy-by));
	beta=(2*(bx-x)*(cx-bx)+2*(by-y)*(cy-by));
	gamma=((bx-x)*(bx-x)+(by-y)*(by-y)-d*d);
	delta=beta*beta-4*alpha*gamma;
	if (delta>0)
	{	tg=(-beta-sqrt(delta))/(2*alpha);
		if ((tg>=0)&&(tg<=1))
		{	gx=bx+tg*(cx-bx);
			gy=by+tg*(cy-by);
			th=(-beta+sqrt(delta))/(2*alpha);
			if ((th>=0)&&(th<=1))
			{	cas=1;
				hx=bx+th*(cx-bx);
				hy=by+th*(cy-by);
			}
 			else
				Rprintf("erreur9ter\n");
		}
	}

	// calcul de l'angle
	if (cas==0)
	{	if ((te==0)&&(tf==0))
			ang=0;
		else
			ang=bacos(((ex-x)*(fx-x)+(ey-y)*(fy-y))/(d*d));
	}
	else
	{	ang=bacos(((ex-x)*(gx-x)+(ey-y)*(gy-y))/(d*d));
	ang+=bacos(((fx-x)*(hx-x)+(fy-y)*(hy-y))/(d*d));
	}
	return ang;
}

// a exterieur, b et c sur le bord
double deuxbord_point(double ax, double ay, double bx, double by, double cx, double cy, double x, double y, double d)
{	double alpha, beta, gamma, delta, te,tf, ang;
	double ex,ey,fx,fy;

	// premier point d'intersection sur ab
	alpha=(bx-ax)*(bx-ax)+(by-ay)*(by-ay);
	beta=(2*(ax-x)*(bx-ax)+2*(ay-y)*(by-ay));
	gamma=((ax-x)*(ax-x)+(ay-y)*(ay-y)-d*d);
	delta=beta*beta-4*alpha*gamma;
	te=1;
	if (delta>0)
	{	te=(-beta-sqrt(delta))/(2*alpha);
	if ((te<=0)||(te>=1))
		te=1;
	if (te<=0)
		Rprintf("e1t\n");
	}
	ex=ax+te*(bx-ax);
	ey=ay+te*(by-ay);

	// deuxieme point d'intersection ac
	alpha=(cx-ax)*(cx-ax)+(cy-ay)*(cy-ay);
	beta=(2*(ax-x)*(cx-ax)+2*(ay-y)*(cy-ay));
	delta=beta*beta-4*alpha*gamma;
	tf=1;
	if (delta>0)
	{	tf=(-beta-sqrt(delta))/(2*alpha);
	if ((tf<=0)||(tf>=1))
		tf=1;
	if (tf<=0)
		Rprintf("e4t\n");
	}
	fx=ax+tf*(cx-ax);
	fy=ay+tf*(cy-ay);

	// calcul de l'angle
	ang=bacos(((ex-x)*(fx-x)+(ey-y)*(fy-y))/(d*d));
	return ang;
}



//retourne 1 si le point x,y est du meme cote de la droite (ab) que c (seg=0) ou sur la droite (seg=1)
int in_droite(double x,double y,double ax,double ay,double bx,double by,double cx,double cy,int seg)
{	double vabx,vaby,vacx,vacy,vamx,vamy,pv1,pv2;

	vabx=bx-ax;
	vaby=by-ay;
	vacx=cx-ax;
	vacy=cy-ay;
	vamx=x-ax;
	vamy=y-ay;
	pv1=vabx*vacy-vaby*vacx;
	pv2=vabx*vamy-vaby*vamx;

	if(seg==0)
	{	if (((pv1>0)&&(pv2>0))||((pv1<0)&&(pv2<0))) //pour overlap
			return 1;
		else
			return 0;
	}
	if(seg==1)
	{	if (((pv1>0)&&(pv2>=0))||((pv1<0)&&(pv2<=0))) //pour points
			return 1;
		else
			return 0;
	}
	return -1;
}

//retourne 1 si (x,y) est dans le triangle abc (seg=0) ou sur ses bords (seg=1)
int in_triangle(double x,double y,double ax,double ay,double bx,double by,double cx,double cy,int seg)
{	int res;

	res=0;
	if (in_droite(x,y,ax,ay,bx,by,cx,cy,seg)==1)
		if (in_droite(x,y,bx,by,cx,cy,ax,ay,seg)==1)
			if (in_droite(x,y,cx,cy,ax,ay,bx,by,seg)==1)
				res=1;
	return res;
}


//Range les resultats pour l'ic
void ic(int i,int i0,double **gic,double **kic,double *gic1,double *kic1,int nbInt) {
	int j,cro;
	double mer;

	//On stocke les 2i0+1 premieres valeurs en les triant au fur et a mesure
	if (i<=2*i0+1) {

		for(j=1;j<=nbInt;j++) {
			gic[j][i]=gic1[j-1];
			kic[j][i]=kic1[j-1];
		}

		//De la deuxieme a la 2i0+1 eme valeur : on trie la nouvelle valeur en direct
		if (i>1) {

			//Tri bulle de g vers le bas
			for(j=1;j<=nbInt;j++) {
				if (gic[j][i-1]>gic[j][i]) {
					mer=gic[j][i];
					cro=i-1;
					while ((cro>0)&&(gic[j][cro]>mer)){
						gic[j][cro+1]=gic[j][cro];
						cro=cro-1;
					}
					gic[j][cro+1]=mer;
				}
			}// fin for tri bulle g

			//Tri bulle de k vers le bas
			for(j=1;j<=nbInt;j++) {
				if (kic[j][i-1]>kic[j][i]) {
					mer=kic[j][i];
					cro=i-1;
					while ((cro>0)&&(kic[j][cro]>mer)){
						kic[j][cro+1]=kic[j][cro];
						cro=cro-1;
					}
					kic[j][cro+1]=mer;
				}
			}// fin for tri bulle k

		}// fin if (i>1)
	}// fin if (i<=2*i0+1)
	else {
	//On a deja rempli et trié le tableau des 2i0+1 valeurs, on met la nouvelle valeur en i0
		for(j=1;j<=nbInt;j++) {
				gic[j][i0+1]=gic1[j-1];
				kic[j][i0+1]=kic1[j-1];
		}

		//On trie les nouvelles valeurs de k et g
		for(j=1;j<=nbInt;j++) {

			// si g doit descendre
			if (gic[j][i0+1]<gic[j][i0]) {
				mer=gic[j][i0+1];
				cro=i0;
				while ((cro>0)&&(gic[j][cro]>mer))
				{	gic[j][cro+1]=gic[j][cro];
					cro=cro-1;
				}
				gic[j][cro+1]=mer;
			}// fin if g descendre
			// si g doit monter
			else {
				if (gic[j][i0+1]>gic[j][i0+2]) {
					mer=gic[j][i0+1];
					cro=i0+2;
					while ((cro<2*i0+2)&&(gic[j][cro]<mer))
					{	gic[j][cro-1]=gic[j][cro];
						cro=cro+1;
					}
					gic[j][cro-1]=mer;
				}
			} //Fin if g monter


			// si k doit descendre
			if (kic[j][i0+1]<kic[j][i0]) {
				mer=kic[j][i0+1];
				cro=i0;
				while ((cro>0)&&(kic[j][cro]>mer))
				{	kic[j][cro+1]=kic[j][cro];
					cro=cro-1;
				}
				kic[j][cro+1]=mer;
			}// fin if k descendre
			// si k doit monter
			else {
				if (kic[j][i0+1]>kic[j][i0+2]) {
					mer=kic[j][i0+1];
					cro=i0+2;
					while ((cro<2*i0+2)&&(kic[j][cro]<mer))
					{	kic[j][cro-1]=kic[j][cro];
						cro=cro+1;
					}
					kic[j][cro-1]=mer;
				}
			} //Fin if k monter

		} //Fin for tt trie nouvelle valeur

	} //Fin if cas sup 2k+i, trier
}


/******************************************************************************/
/* Cette routine donne le perimetre/ddd du cercle centre en (xxx,yyy) et      */
/* de rayon ddd, qui est a l'interieur de la zone rectangulaire xmi xma ymiyma*/
/* Elle traite les cas 1 bord, 2 bords d'angle (2), 2 bords opposes, 3 bords  */
/* Ce resultat correspond a la correction des effets de bord pour Ripley      */
/******************************************************************************/

double perim_in_rect(double xxx, double yyy, double ddd, double xmi, double xma, double ymi, double yma)
{	double d1,d2,d3,d4;

   if ((xxx>=xmi+ddd)&&(yyy>=ymi+ddd)&&(xxx<=xma-ddd)&&(yyy<=yma-ddd))
   {	//Rprintf("*");
		return 2*Pi();
   }
   else
   {	d1=(xxx-xmi)/ddd;
      d2=(yyy-ymi)/ddd;
      d3=(xma-xxx)/ddd;
      d4=(yma-yyy)/ddd;
      if (d1>=1)
      {	if (d2>=1)
         {	if (d3>=1)
         	{  if (d4>=1)      		/* cercle dans le rectangle */
            	{  return 2*Pi();
               }
               else                /* bord seul en d4 */
               {	//Rprintf(":a(%f-%f-%f-%f-%f-%f-%f)",xxx,yyy,ddd,xmi,xma,ymi,yma);
               	return (2*(Pi()-acos(d4)));
               }
				}
            else
            {  if (d4>=1)				/* bord seul en d3 */
               {  //Rprintf(":b(%f)",d3);
               	return (2*(Pi()-acos(d3)));
               }
               else    					/* 2 bords d3 et d4 */
					{	if (d3*d3+d4*d4<1)
               	{	//Rprintf(":c(%f-%f)",d3,d4);
                  	return (1.5*Pi()-acos(d3)-acos(d4));
                  }
                  else
                  {  //Rprintf(":d(%f-%f)",d3,d4);
                  	return (2*(Pi()-acos(d3)-acos(d4)));
                  }
               }
            }
         }
         else
      	{  if (d3>=1)
            {	if (d4>=1)			/* bord seul en d2 */
            	{	//Rprintf(":e(%f)",d2,d4);
               	return (2*(Pi()-acos(d2)));
               }
               else       			/* 2 bords d2 et d4 */
            	{	//Rprintf(":f(%f-%f)",d2,d4);
               	return (2*(Pi()-acos(d2)-acos(d4)));
               }
            }
            else
            {  if (d4>=1)			/* 2 bords d2 et d3 */
            	{	if (d2*d2+d3*d3<1)
               	{	//Rprintf(":g(%f-%f)",d2,d3);
                  	return	((1.5*Pi()-acos(d2)-acos(d3)));
                  }
               	else
               	{  //Rprintf(":h(%f-%f)",d2,d3);
               		return (2*(Pi()-acos(d2)-acos(d3)));
               	}
            	}
            	else	/* 3 bords d2,d3,d4 */
            	{  if (d2*d2+d3*d3<1)
               	{	if (d3*d3+d4*d4<1)
               		{	//Rprintf(":i(%f-%f)",d2,d4);
                  		return((Pi()-acos(d2)-acos(d4)));
                  	}
                  	else
                  	{	//Rprintf(":j(%f-%f-%f)",d2,d3,d4);
                  		return((1.5*Pi()-acos(d2)-acos(d3)-2*acos(d4)));
                  	}
              		}
               	else
               	{	if (d3*d3+d4*d4<1)
               		{	//Rprintf(":k(%f-%f-%f)",d2,d3,d4);
                  		return((1.5*Pi()-2*acos(d2)-acos(d3)-acos(d4)));
                  	}
                  	else
                  	{	//Rprintf(":l(%f-%f-%f)",d2,d3,d4);
                  		return(2*(Pi()-acos(d2)-acos(d3)-acos(d4)));
                  	}
               	}
            	}
         	}
      	}
   	}
   	else
   	{	if (d2>=1)
      	{	if (d3>=1)
         	{	if (d4>=1)					/* bord seul en d1 */
            	{	return (2*(Pi()-acos(d1)));
            	}
            	else							/* 2 bords d1 et d4 */
            	{  if (d1*d1+d4*d4<1)
            		{	return ((1.5*Pi()-acos(d1)-acos(d4)));
               	}
               	else
               	{	return (2*(Pi()-acos(d1)-acos(d4)));
               	}
            	}
         	}
         	else
         	{  if (d4>=1)					/* 2 bords d1 et d3 */
            	{	return (2*(Pi()-acos(d1)-acos(d3)));
            	}
            	else							/* 3 bords d1,d3,d4 */
            	{	if (d3*d3+d4*d4<1)
            		{	if (d4*d4+d1*d1<1)
               		{	return ((Pi()-acos(d3)-acos(d1)));
                  	}
                  	else
                  	{	return ((1.5*Pi()-acos(d3)-acos(d4)-2*acos(d1)));
                  	}
               	}
               	else
               	{  if (d4*d4+d1*d1<1)
               		{	return ((1.5*Pi()-2*acos(d3)-acos(d4)-acos(d1)));
                  	}
                  	else
                  	{	return (2*(Pi()-acos(d3)-acos(d4)-acos(d1)));
                  	}
               	}
            	}
         	}
      	}
      	else
      	{	if (d3>=1)
      		{	if (d4>=1)    				/* 2 bords d1 et d2 */
      			{	if (d1*d1+d2*d2<1)
            		{	return ((1.5*Pi()-acos(d1)-acos(d2)));
               	}
               	else
               	{	return (2*(Pi()-acos(d1)-acos(d2)));
               	}
         		}
            	else							/* 3 bords d1,d2,d4 */
            	{	if (d4*d4+d1*d1<1)
            		{	if (d1*d1+d2*d2<1)
               		{	return ((Pi()-acos(d4)-acos(d2)));
                  	}
                  	else
                  	{	return ((1.5*Pi()-acos(d4)-acos(d1)-2*acos(d2)));
                  	}
            		}
               	else
               	{	if (d1*d1+d2*d2<1)
               		{	return ((1.5*Pi()-2*acos(d4)-acos(d1)-acos(d2)));
                  	}
                 		else
                  	{	return (2*(Pi()-acos(d4)-acos(d1)-acos(d2)));
                  	}
               	}
            	}
       		}
         	else
         	{	if (d4>=1)					/* 3 bords d1,d2,d3 */
            	{	if (d1*d1+d2*d2<1)
            		{	if (d2*d2+d3*d3<1)
               		{	return ((Pi()-acos(d1)-acos(d3)));
                  	}
                  	else
                  	{	return ((1.5*Pi()-acos(d1)-acos(d2)-2*acos(d3)));
                  	}
               	}
               	else
               	{	if (d2*d2+d3*d3<1)
               		{	return ((1.5*Pi()-2*acos(d1)-acos(d2)-acos(d3)));
                  	}
                  	else
                  	{	return (2*(Pi()-acos(d1)-acos(d2)-acos(d3)));
                  	}
               	}
            	}
            	else							/* 4 bords : je ne peux pas faire */
            	{	Rprintf("erreur : le nombre d'intervalles est trop grand\n");
               	return -1;
            	}
        		}
      	}
   	}
	}
}

//pour une zone circulaire définie par x0, y0, r0
double perim_in_disq(double xxx, double yyy, double ddd,
	double x0, double y0,double r0)
{	double d1;

	d1=sqrt((xxx-x0)*(xxx-x0)+(yyy-y0)*(yyy-y0));
	if (d1+ddd<=r0)
		return 2*Pi();
	else
		return 2*(Pi()-acos((r0*r0-d1*d1-ddd*ddd)/(2*d1*ddd)));
}

// renvoie la somme des angles du perim a l'interieur des triangles
double perim_triangle(double x,double y, double d, int triangle_nb, double *ax,double *ay, double *bx, double *by, double *cx, double *cy)
{	double angle, epsilon;
	double doa,dob,doc;
	int h,i;

	epsilon=0.0001;
	angle=0;

	for(h=0;h<triangle_nb;h++)
	{	doa=sqrt((x-ax[h])*(x-ax[h])+(y-ay[h])*(y-ay[h]));
		dob=sqrt((x-bx[h])*(x-bx[h])+(y-by[h])*(y-by[h]));
		doc=sqrt((x-cx[h])*(x-cx[h])+(y-cy[h])*(y-cy[h]));

		if (doa-d<-epsilon)
		{	if (dob-d<-epsilon)
			{	if (doc-d<-epsilon)
					i=1;	// le triangle est dans le cercle, TVB
				else if (doc-d>epsilon)
					angle+=un_point(cx[h],cy[h],ax[h],ay[h],bx[h],by[h],x,y,d);
				else
					i=1;	// le triangle est dans le cercle, TVB
			}
			else if (dob-d>epsilon)
			{	if (doc-d<-epsilon)
					angle+=un_point(bx[h],by[h],ax[h],ay[h],cx[h],cy[h],x,y,d);
				else if (doc-d>epsilon)
					angle+=deux_point(ax[h],ay[h],bx[h],by[h],cx[h],cy[h],x,y,d);
				else
					angle+=ununun_point(bx[h],by[h],ax[h],ay[h],cx[h],cy[h],x,y,d);
			}
			else // b sur le bord
			{	if (doc-d<-epsilon)
					i=1;	// le triangle est dans le cercle, TVB
				else if (doc-d>epsilon)
					angle+=ununun_point(cx[h],cy[h],ax[h],ay[h],bx[h],by[h],x,y,d);
				else
					i=1; 	// le triangle est dans le cercle, TVB
			}
		}
		else if (doa-d>epsilon)
		{	if (dob-d<-epsilon)
			{	if (doc-d<-epsilon)
					angle+=un_point(ax[h],ay[h],bx[h],by[h],cx[h],cy[h],x,y,d);
				else if (doc-d>epsilon)
					angle+=deux_point(bx[h],by[h],ax[h],ay[h],cx[h],cy[h],x,y,d);
				else
					angle+=ununun_point(ax[h],ay[h],bx[h],by[h],cx[h],cy[h],x,y,d);
			}
			else if (dob-d>epsilon)
			{	if (doc-d<-epsilon)
					angle+=deux_point(cx[h],cy[h],ax[h],ay[h],bx[h],by[h],x,y,d);
				else if (doc-d>epsilon)
					angle+=trois_point(ax[h],ay[h],bx[h],by[h],cx[h],cy[h],x,y,d);
				else
					angle+=deuxun_point(cx[h],cy[h],ax[h],ay[h],bx[h],by[h],x,y,d);
			}
			else	// b sur le bord
			{	if (doc-d<-epsilon)
					angle+=ununun_point(ax[h],ay[h],cx[h],cy[h],bx[h],by[h],x,y,d);
				else if (doc-d>epsilon)
					angle+=deuxun_point(bx[h],by[h],ax[h],ay[h],cx[h],cy[h],x,y,d);
				else
					angle+=deuxbord_point(ax[h],ay[h],bx[h],by[h],cx[h],cy[h],x,y,d);
			}
		}
		else	// a sur le bord
		{	if (dob-d<-epsilon)
			{	if (doc-d<-epsilon)
					i=1;	// le triangle est dans le cercle, TVB
				else if (doc-d>epsilon)
					angle+=ununun_point(cx[h],cy[h],bx[h],by[h],ax[h],ay[h],x,y,d);
				else
					i=1;	// le triangle est	dans le cercle, TVB
			}
			else if (dob-d>epsilon)
			{	if (doc-d<-epsilon)
					angle+=ununun_point(bx[h],by[h],cx[h],cy[h],ax[h],ay[h],x,y,d);
				else if (doc-d>epsilon)
					angle+=deuxun_point(ax[h],ay[h],bx[h],by[h],cx[h],cy[h],x,y,d);
				else
					angle+=deuxbord_point(bx[h],by[h],ax[h],ay[h],cx[h],cy[h],x,y,d);
			}
			else	// b sur le bord
			{	if (doc-d<-epsilon)
					i=1;	// le triangle est dans le cercle, TVB
				else if (doc-d>epsilon)
					angle+=deuxbord_point(cx[h],cy[h],ax[h],ay[h],bx[h],by[h],x,y,d);
				else
					i=1;	// le triangle est dans le cercle, TVB
			}
		}
	}

	return angle;
}






/******************************************************************************/
/* Calcule la fonction de Ripley K(r) pour un semis (x,y) en parametres       */
/* dans une zone de forme rectangulaire de bornes xmi xma ymi yma             */
/* Les corrections des effets de bords sont fait par la methode de Ripley,    */
/* i.e. l'inverse de la proportion d'arc de cercle inclu dans la fenetre.     */
/* Les calculs sont faits pour les t2 premiers intervalles de largeur dt.     */
/* La routine calcule g, densite des couples de points;  et la fonction K     */
/* Les resultats sont stockes dans des tableaux g et k donnes en parametres   */
/******************************************************************************/

int ripley_rect(int *point_nb,double *x,double *y, double *xmi,double *xma,double *ymi,double *yma,
int *t2,double *dt,double *g,double *k)
{	int i,j,tt;
	double d,cin;

	//Decalage pour n'avoir que des valeurs positives
	decalRect(*point_nb,x,y,xmi,xma,ymi,yma);


	// On rangera dans g le nombre de couples de points par distance tt
	for(tt=0;tt<*t2;tt=tt+1){
		g[tt]=0;
	}


    //On regarde les couples (i,j) et (j,i) : donc pour i>j seulement
	for(i=1;i<*point_nb;i=i+1)
	{	for(j=0;j<i;j=j+1)
		{	d=sqrt((x[i]-x[j])*(x[i]-x[j])+(y[i]-y[j])*(y[i]-y[j]));
			if (d<*t2*(*dt)){
				// dans quelle classe de distance est ce couple ?
				tt=d/(*dt);

				///// pour [i,j] :
				// correction des effets de bord
				cin=perim_in_rect(x[i],y[i],d,*xmi,*xma,*ymi,*yma);
				if (cin<0) {
					Rprintf("cin<0 sur i AVANT\n");
					return -1;
				}
				g[tt]+=2*Pi()/cin;

				///// pour [j,i] :
				// correction des effets de bord
				cin=perim_in_rect(x[j],y[j],d,*xmi,*xma,*ymi,*yma);
				if (cin<0) {
					Rprintf("cin<0 sur j AVANT\n");
					return -1;
				}
				g[tt]+=2*Pi()/cin;
			}
		}
   }


	// on moyenne -> densite
	for(tt=0;tt<*t2;tt=tt+1) {
		g[tt]=g[tt]/(*point_nb);
	}

	// on integre
	k[0]=g[0];
  	for(tt=1;tt<*t2;tt=tt+1) {
  		k[tt]=k[tt-1]+g[tt];
	}


   return 0;
}

//fonction de Ripley pour une zone circulaire
int ripley_disq(int *point_nb, double *x, double *y, double *x0, double *y0, double *r0,
int *t2, double *dt, double *g, double *k)
{	int tt,i,j;
	double d,cin;

	//Decalage pour n'avoir que des valeurs positives
	decalCirc(*point_nb,x,y,x0,y0,*r0);

	for(tt=0;tt<*t2;tt=tt+1) {
		g[tt]=0;
	}
	for(i=1;i<*point_nb;i=i+1) { // On calcule le nombre de couples de points par distance g
		for(j=0;j<i;j=j+1)
		{	d=sqrt((x[i]-x[j])*(x[i]-x[j])+(y[i]-y[j])*(y[i]-y[j]));
			if (d<*t2*(*dt))
			{	tt=d/(*dt);

				///// pour [i,j] :
				// correction des effets de bord
				cin=perim_in_disq(x[i],y[i],d,*x0,*y0,*r0);
				if (cin<0) {
					Rprintf("cin<0 sur i AVANT\n");
					return -1;
				}
				g[tt]+=2*Pi()/cin;

				///// pour [j,i] :
				// correction des effets de bord
				cin=perim_in_disq(x[j],y[j],d,*x0,*y0,*r0);
				if (cin<0) {
					Rprintf("cin<0 sur j AVANT\n");
					return -1;
				}
				g[tt]+=2*Pi()/cin;
			}
		}
	}

	// on moyenne -> densite
	for(tt=0;tt<*t2;tt=tt+1) {
		g[tt]=g[tt]/(*point_nb);
	}

	// on integre
	k[0]=g[0];
	for(tt=1;tt<*t2;tt=tt+1) {
		k[tt]=k[tt-1]+g[tt];
	}


	return 0;
}


//Ripley triangles dans rectangle
int ripley_tr_rect(int *point_nb, double *x, double *y, double *xmi, double *xma, double *ymi, double *yma,
int *triangle_nb, double *ax, double *ay, double *bx, double *by, double *cx, double *cy,
int *t2, double *dt, double *g, double *k)
{	int i,j,tt;
	double d,cin;

	//Decalage pour n'avoir que des valeurs positives
	decalRectTri(*point_nb,x,y,xmi,xma,ymi,yma,*triangle_nb,ax,ay,bx,by,cx,cy);

	// On calcule le nombre de couples de points par distance g
	for(tt=0;tt<*t2;tt=tt+1) {
		g[tt]=0;
	}
	for(i=1;i<*point_nb;i=i+1)
		for(j=0;j<i;j=j+1)
		{	d=sqrt((x[i]-x[j])*(x[i]-x[j])+(y[i]-y[j])*(y[i]-y[j]));
			if (d<*t2*(*dt))
			{	tt=d/(*dt);

				///// pour [i,j] :
				// correction des effets de bord
				cin=perim_in_rect(x[i],y[i],d,*xmi,*xma,*ymi,*yma);
				if (cin<0) {
					Rprintf("cin<0 sur i AVANT\n");
					return -1;
				}
				cin=cin-perim_triangle(x[i],y[i],d,*triangle_nb,ax,ay,bx,by,cx,cy);
				if (cin<0)	{
					Rprintf("Overlapping triangles\n");
					return -1;
				}
				g[tt]+=2*Pi()/cin;

				///// pour [j,i] :
				// correction des effets de bord
				cin=perim_in_rect(x[j],y[j],d,*xmi,*xma,*ymi,*yma);
				if (cin<0) {
					Rprintf("cin<0 sur j AVANT\n");
					return -1;
				}
				cin=cin-perim_triangle(x[j],y[j],d,*triangle_nb,ax,ay,bx,by,cx,cy);
				if (cin<0)	{
					Rprintf("Overlapping triangles\n");
					return -1;
				}
				g[tt]+=2*Pi()/cin;
			}
		}

	// on moyenne -> densite
	for(tt=0;tt<*t2;tt=tt+1) {
		g[tt]=g[tt]/(*point_nb);
	}

	// on integre
	k[0]=g[0];
	for(tt=1;tt<*t2;tt=tt+1) {
		k[tt]=k[tt-1]+g[tt];
	}

	return 0;
}


//Ripley triangle dans disque
int ripley_tr_disq(int *point_nb,double *x,double *y,double *x0,double *y0,double *r0,int *triangle_nb,
double *ax,double *ay,double *bx,double *by,double *cx,double *cy,
int *t2,double *dt,double *g,double *k)
{	int i,j,tt;
	double d,cin;

	//Decalage pour n'avoir que des valeurs positives
	decalCircTri(*point_nb,x,y,x0,y0,*r0,*triangle_nb,ax,ay,bx,by,cx,cy);

	// On calcule le nombre de couples de points par distance g
	for(tt=0;tt<*t2;tt=tt+1) {
		g[tt]=0;
	}
	for(i=1;i<*point_nb;i=i+1)
		for(j=0;j<i;j=j+1)
		{	d=sqrt((x[i]-x[j])*(x[i]-x[j])+(y[i]-y[j])*(y[i]-y[j]));
			if (d<*t2*(*dt))
			{	tt=d/(*dt);

				///// pour [i,j] :
				// correction des effets de bord
				cin=perim_in_disq(x[i],y[i],d,*x0,*y0,*r0);
				if (cin<0) {
					Rprintf("cin<0 sur i AVANT\n");
					return -1;
				}
				cin=cin-perim_triangle(x[i],y[i],d,*triangle_nb,ax,ay,bx,by,cx,cy);
				if (cin<0)	{
					Rprintf("Overlapping triangles\n");
					return -1;
				}
				g[tt]+=2*Pi()/cin;

				///// pour [j,i] :
				// correction des effets de bord
				cin=perim_in_disq(x[j],y[j],d,*x0,*y0,*r0);
				if (cin<0) {
					Rprintf("cin<0 sur j AVANT\n");
					return -1;
				}
				cin=cin-perim_triangle(x[j],y[j],d,*triangle_nb,ax,ay,bx,by,cx,cy);
				if (cin<0)	{
					Rprintf("Overlapping triangles\n");
					return -1;
				}
				g[tt]+=2*Pi()/cin;
			}
		}

	// on moyenne -> densite
	for(tt=0;tt<*t2;tt=tt+1) {
		g[tt]=g[tt]/(*point_nb);
	}

	// on integre
	k[0]=g[0];
	for(tt=1;tt<*t2;tt=tt+1) {
		k[tt]=k[tt-1]+g[tt];
	}

	return 0;
}


//fonction de Ripley avec intervalle de confiance pour une zone rectangulaire
int ripley_rect_ic(int *point_nb,double *x,double *y, double *xmi,double *xma,double *ymi,double *yma,double *densite,
int *t2,double *dt,int *nbSimu, double *prec, double *lev,double *g,double *k,
double *gic1,double *gic2, double *kic1,double *kic2, double *gval, double *kval, double *lval, double *nval) {
	int i,j,i0,i1,i2;
	double **gic,**kic;
	double *gg,*kk,*ll,*nn;
	int erreur=0;

	erreur=ripley_rect(point_nb,x,y,xmi,xma,ymi,yma,t2,dt,g,k);
	if (erreur!=0) {
		return -1;
	}

	///Définition de i0 : indice où sera stocké l'estimation des bornes de l'IC
	i0=*lev/2*(*nbSimu+1);
	if (i0<1) i0=1;

	///Initialisation des tableaux dans lesquels on va stocker les valeurs extremes lors de MC
	taballoc(&gic,*t2+1,2*i0+10+1);
	taballoc(&kic,*t2+1,2*i0+10+1);


	//Normalisation de g et k et calcul de l et n pour le calcul des p-values
	vecalloc(&gg,*t2);
	vecalloc(&kk,*t2);
	vecalloc(&ll,*t2);
	vecalloc(&nn,*t2);
	for(i=0;i<*t2;i++) {
		gg[i]=g[i]/(*densite*(Pi()*(i+1)*(i+1)*(*dt)*(*dt)-Pi()*i*i*(*dt)*(*dt)));
		nn[i]=k[i]/(Pi()*(i+1)*(i+1)*(*dt)*(*dt));
		kk[i]=k[i]/(*densite);
		ll[i]=sqrt(kk[i]/Pi())-(i+1)*(*dt);

		gval[i]=1;
		kval[i]=1;
		nval[i]=1;
		lval[i]=1;
	}


	int lp=0;

	// boucle principale de MC
	Rprintf("Monte Carlo simulation\n");
	for(i=1;i<=*nbSimu;i++) {
		s_alea_rect(*point_nb,x,y,*xmi,*xma,*ymi,*yma,*prec);
		erreur=ripley_rect(point_nb,x,y,xmi,xma,ymi,yma,t2,dt,gic1,kic1);
		// si il y a une erreur on recommence une simulation
		if (erreur!=0) {
			i=i-1;
			Rprintf("ERREUR Ripley\n");
		}
		else {
			//comptage du nombre de |¶obs|<=|¶simu| pour test local
			double gictmp,kictmp,lictmp,nictmp;
			for(j=0;j<*t2;j++) {
				gictmp=gic1[j]/(*densite*(Pi()*(j+1)*(j+1)*(*dt)*(*dt)-Pi()*j*j*(*dt)*(*dt)));
				nictmp=kic1[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt));
				kictmp=kic1[j]/(*densite);
				lictmp=sqrt(kictmp/Pi())-(j+1)*(*dt);

				if ((float)fabs(gg[j]-1)<=(float)fabs(gictmp-1)) {gval[j]+=1;}
				if ((float)fabs(nn[j]-*densite)<=(float)fabs(nictmp-*densite)) {nval[j]+=1;}
				if ((float)fabs(kk[j]-Pi()*(j+1)*(j+1)*(*dt)*(*dt))<=(float)fabs(kictmp-Pi()*(j+1)*(j+1)*(*dt)*(*dt))) {kval[j]+=1;}
				if ((float)fabs(ll[j])<=(float)fabs(lictmp)) {lval[j]+=1;}
			}

			////Traitement des résultats
			ic(i,i0,gic,kic,gic1,kic1,*t2);
		}
		R_FlushConsole();
 		progress(i,&lp,*nbSimu);
	}

	i1=i0+2;
	i2=i0;

	//Copies des valeurs dans les tableaux résultats
	for(i=0;i<*t2;i++) {
		gic1[i]=gic[i+1][i1];
		gic2[i]=gic[i+1][i2];
		kic1[i]=kic[i+1][i1];
		kic2[i]=kic[i+1][i2];
	}


	freetab(gic);
	freetab(kic);

	freevec(gg);
	freevec(kk);
	freevec(ll);
	freevec(nn);


	return 0;
}


//fonction de Ripley avec intervalle de confiance pour une zone circulaire
int ripley_disq_ic(int *point_nb,double *x,double *y, double *x0,double *y0,double *r0,double *densite,
int *t2,double *dt,int *nbSimu, double *prec, double *lev,double *g,double *k,
double *gic1,double *gic2, double *kic1,double *kic2, double *gval, double *kval, double *lval, double *nval) {
	int i,j,i0,i1,i2;
	double **gic,**kic;
	double *gg,*kk,*ll,*nn;
	int erreur=0;

	erreur=ripley_disq(point_nb,x,y,x0,y0,r0,t2,dt,g,k);
	if (erreur!=0) {
		return -1;
	}

	///Définition de i0 : indice où sera stocké l'estimation des bornes de l'IC
	i0=*lev/2*(*nbSimu+1);
	if (i0<1) i0=1;

	///Initialisation des tableaux dans lesquels on va stocker les valeurs extremes lors de MC
	taballoc(&gic,*t2+1,2*i0+10+1);
	taballoc(&kic,*t2+1,2*i0+10+1);


	//Normalisation de g et k et calcul de l et n pour le calcul des p-values
	vecalloc(&gg,*t2);
	vecalloc(&kk,*t2);
	vecalloc(&ll,*t2);
	vecalloc(&nn,*t2);
	for(i=0;i<*t2;i++) {
		gg[i]=g[i]/(*densite*(Pi()*(i+1)*(i+1)*(*dt)*(*dt)-Pi()*i*i*(*dt)*(*dt)));
		nn[i]=k[i]/(Pi()*(i+1)*(i+1)*(*dt)*(*dt));
		kk[i]=k[i]/(*densite);
		ll[i]=sqrt(kk[i]/Pi())-(i+1)*(*dt);

		gval[i]=1;
		kval[i]=1;
		nval[i]=1;
		lval[i]=1;
	}


	int lp=0;

	// boucle principale de MC
	Rprintf("Monte Carlo simulation\n");
	for(i=1;i<=*nbSimu;i++) {
		s_alea_disq(*point_nb,x,y,*x0,*y0,*r0,*prec);
		erreur=ripley_disq(point_nb,x,y,x0,y0,r0,t2,dt,gic1,kic1);
		// si il y a une erreur on recommence une simulation
		if (erreur!=0) {
			i=i-1;
			Rprintf("ERREUR Ripley\n");
		}
		else {
			//comptage du nombre de |¶obs|<=|¶simu| pour test local
			double gictmp,kictmp,lictmp,nictmp;
			for(j=0;j<*t2;j++) {
				gictmp=gic1[j]/(*densite*(Pi()*(j+1)*(j+1)*(*dt)*(*dt)-Pi()*j*j*(*dt)*(*dt)));
				nictmp=kic1[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt));
				kictmp=kic1[j]/(*densite);
				lictmp=sqrt(kictmp/Pi())-(j+1)*(*dt);

				if ((float)fabs(gg[j]-1)<=(float)fabs(gictmp-1)) {gval[j]+=1;}
				if ((float)fabs(nn[j]-*densite)<=(float)fabs(nictmp-*densite)) {nval[j]+=1;}
				if ((float)fabs(kk[j]-Pi()*(j+1)*(j+1)*(*dt)*(*dt))<=(float)fabs(kictmp-Pi()*(j+1)*(j+1)*(*dt)*(*dt))) {kval[j]+=1;}
				if ((float)fabs(ll[j])<=(float)fabs(lictmp)) {lval[j]+=1;}
			}

			////Traitement des résultats
			ic(i,i0,gic,kic,gic1,kic1,*t2);
		}
		R_FlushConsole();
 		progress(i,&lp,*nbSimu);
	}

	i1=i0+2;
	i2=i0;

	//Copies des valeurs dans les tableaux résultats
	for(i=0;i<*t2;i++) {
		gic1[i]=gic[i+1][i1];
		gic2[i]=gic[i+1][i2];
		kic1[i]=kic[i+1][i1];
		kic2[i]=kic[i+1][i2];
	}


	freetab(gic);
	freetab(kic);

	freevec(gg);
	freevec(kk);
	freevec(ll);
	freevec(nn);


	return 0;
}


//fonction de Ripley avec intervalle de confiance pour une zone rectangulaire + triangles
int ripley_tr_rect_ic(int *point_nb,double *x,double *y, double *xmi,double *xma,double *ymi,double *yma,double *densite,
int *triangle_nb, double *ax, double *ay, double *bx, double *by, double *cx, double *cy,
int *t2,double *dt,int *nbSimu, double *prec, double *lev,double *g,double *k,
double *gic1,double *gic2, double *kic1,double *kic2, double *gval, double *kval, double *lval, double *nval) {
	int i,j,i0,i1,i2;
	double **gic,**kic;
	double *gg,*kk,*ll,*nn;
	int erreur=0;

	erreur=ripley_tr_rect(point_nb,x,y,xmi,xma,ymi,yma,triangle_nb,ax,ay,bx,by,cx,cy,t2,dt,g,k);
	if (erreur!=0) {
		return -1;
	}

	///Définition de i0 : indice où sera stocké l'estimation des bornes de l'IC
	i0=*lev/2*(*nbSimu+1);
	if (i0<1) i0=1;

	///Initialisation des tableaux dans lesquels on va stocker les valeurs extremes lors de MC
	taballoc(&gic,*t2+1,2*i0+10+1);
	taballoc(&kic,*t2+1,2*i0+10+1);


	//Normalisation de g et k et calcul de l et n pour le calcul des p-values
	vecalloc(&gg,*t2);
	vecalloc(&kk,*t2);
	vecalloc(&ll,*t2);
	vecalloc(&nn,*t2);
	for(i=0;i<*t2;i++) {
		gg[i]=g[i]/(*densite*(Pi()*(i+1)*(i+1)*(*dt)*(*dt)-Pi()*i*i*(*dt)*(*dt)));
		nn[i]=k[i]/(Pi()*(i+1)*(i+1)*(*dt)*(*dt));
		kk[i]=k[i]/(*densite);
		ll[i]=sqrt(kk[i]/Pi())-(i+1)*(*dt);

		gval[i]=1;
		kval[i]=1;
		nval[i]=1;
		lval[i]=1;
	}


	int lp=0;

	// boucle principale de MC
	Rprintf("Monte Carlo simulation\n");
	for(i=1;i<=*nbSimu;i++) {

		s_alea_tr_rect(*point_nb,x,y,*xmi,*xma,*ymi,*yma,*triangle_nb,ax,ay,bx,by,cx,cy,*prec);
		erreur=ripley_tr_rect(point_nb,x,y,xmi,xma,ymi,yma,triangle_nb,ax,ay,bx,by,cx,cy,t2,dt,gic1,kic1);

		// si il y a une erreur on recommence une simulation
		if (erreur!=0) {
			i=i-1;
			Rprintf("ERREUR Ripley\n");
		}
		else {
			//comptage du nombre de |¶obs|<=|¶simu| pour test local
			double gictmp,kictmp,lictmp,nictmp;
			for(j=0;j<*t2;j++) {
				gictmp=gic1[j]/(*densite*(Pi()*(j+1)*(j+1)*(*dt)*(*dt)-Pi()*j*j*(*dt)*(*dt)));
				nictmp=kic1[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt));
				kictmp=kic1[j]/(*densite);
				lictmp=sqrt(kictmp/Pi())-(j+1)*(*dt);

				if ((float)fabs(gg[j]-1)<=(float)fabs(gictmp-1)) {gval[j]+=1;}
				if ((float)fabs(nn[j]-*densite)<=(float)fabs(nictmp-*densite)) {nval[j]+=1;}
				if ((float)fabs(kk[j]-Pi()*(j+1)*(j+1)*(*dt)*(*dt))<=(float)fabs(kictmp-Pi()*(j+1)*(j+1)*(*dt)*(*dt))) {kval[j]+=1;}
				if ((float)fabs(ll[j])<=(float)fabs(lictmp)) {lval[j]+=1;}
			}

			////Traitement des résultats
			ic(i,i0,gic,kic,gic1,kic1,*t2);
		}
		R_FlushConsole();
 		progress(i,&lp,*nbSimu);
	}

	i1=i0+2;
	i2=i0;

	//Copies des valeurs dans les tableaux résultats
	for(i=0;i<*t2;i++) {
		gic1[i]=gic[i+1][i1];
		gic2[i]=gic[i+1][i2];
		kic1[i]=kic[i+1][i1];
		kic2[i]=kic[i+1][i2];
	}


	freetab(gic);
	freetab(kic);

	freevec(gg);
	freevec(kk);
	freevec(ll);
	freevec(nn);


	return 0;
}


//fonction de Ripley avec intervalle de confiance pour une zone circulaire + triangles
int ripley_tr_disq_ic(int *point_nb,double *x,double *y, double *x0,double *y0,double *r0,double *densite,
int *triangle_nb, double *ax, double *ay, double *bx, double *by, double *cx, double *cy,
int *t2,double *dt,int *nbSimu, double *prec, double *lev,double *g,double *k,
double *gic1,double *gic2, double *kic1,double *kic2, double *gval, double *kval, double *lval, double *nval) {
	int i,j,i0,i1,i2;
	double **gic,**kic;
	double *gg,*kk,*ll,*nn;
	int erreur=0;

	erreur=ripley_tr_disq(point_nb,x,y,x0,y0,r0,triangle_nb,ax,ay,bx,by,cx,cy,t2,dt,g,k);
	if (erreur!=0) {
		return -1;
	}

	///Définition de i0 : indice où sera stocké l'estimation des bornes de l'IC
	i0=*lev/2*(*nbSimu+1);
	if (i0<1) i0=1;

	///Initialisation des tableaux dans lesquels on va stocker les valeurs extremes lors de MC
	taballoc(&gic,*t2+1,2*i0+10+1);
	taballoc(&kic,*t2+1,2*i0+10+1);


	//Normalisation de g et k et calcul de l et n pour le calcul des p-values
	vecalloc(&gg,*t2);
	vecalloc(&kk,*t2);
	vecalloc(&ll,*t2);
	vecalloc(&nn,*t2);
	for(i=0;i<*t2;i++) {
		gg[i]=g[i]/(*densite*(Pi()*(i+1)*(i+1)*(*dt)*(*dt)-Pi()*i*i*(*dt)*(*dt)));
		nn[i]=k[i]/(Pi()*(i+1)*(i+1)*(*dt)*(*dt));
		kk[i]=k[i]/(*densite);
		ll[i]=sqrt(kk[i]/Pi())-(i+1)*(*dt);

		gval[i]=1;
		kval[i]=1;
		nval[i]=1;
		lval[i]=1;
	}


	int lp=0;

	// boucle principale de MC
	Rprintf("Monte Carlo simulation\n");
	for(i=1;i<=*nbSimu;i++) {

		s_alea_tr_disq(*point_nb,x,y,*x0,*y0,*r0,*triangle_nb,ax,ay,bx,by,cx,cy,*prec);
		erreur=ripley_tr_disq(point_nb,x,y,x0,y0,r0,triangle_nb,ax,ay,bx,by,cx,cy,t2,dt,gic1,kic1);

		// si il y a une erreur on recommence une simulation
		if (erreur!=0) {
			i=i-1;
			Rprintf("ERREUR Ripley\n");
		}
		else {
			//comptage du nombre de |¶obs|<=|¶simu| pour test local
			double gictmp,kictmp,lictmp,nictmp;
			for(j=0;j<*t2;j++) {
				gictmp=gic1[j]/(*densite*(Pi()*(j+1)*(j+1)*(*dt)*(*dt)-Pi()*j*j*(*dt)*(*dt)));
				nictmp=kic1[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt));
				kictmp=kic1[j]/(*densite);
				lictmp=sqrt(kictmp/Pi())-(j+1)*(*dt);

				if ((float)fabs(gg[j]-1)<=(float)fabs(gictmp-1)) {gval[j]+=1;}
				if ((float)fabs(nn[j]-*densite)<=(float)fabs(nictmp-*densite)) {nval[j]+=1;}
				if ((float)fabs(kk[j]-Pi()*(j+1)*(j+1)*(*dt)*(*dt))<=(float)fabs(kictmp-Pi()*(j+1)*(j+1)*(*dt)*(*dt))) {kval[j]+=1;}
				if ((float)fabs(ll[j])<=(float)fabs(lictmp)) {lval[j]+=1;}
			}

			////Traitement des résultats
			ic(i,i0,gic,kic,gic1,kic1,*t2);
		}
		R_FlushConsole();
 		progress(i,&lp,*nbSimu);
	}

	i1=i0+2;
	i2=i0;

	//Copies des valeurs dans les tableaux résultats
	for(i=0;i<*t2;i++) {
		gic1[i]=gic[i+1][i1];
		gic2[i]=gic[i+1][i2];
		kic1[i]=kic[i+1][i1];
		kic2[i]=kic[i+1][i2];
	}


	freetab(gic);
	freetab(kic);

	freevec(gg);
	freevec(kk);
	freevec(ll);
	freevec(nn);


	return 0;
}






//fonction de Ripley locale pour une zone rectangulaire
int ripleylocal_rect(int *point_nb,double *x,double *y,double *xmi,double *xma,double *ymi,double *yma,
int *t2,double *dt,double *gi,double *ki)
{	int tt,i,j;
	double d,cin;
	double **g, **k;

	//Decalage pour n'avoir que des valeurs positives
	decalRect(*point_nb,x,y,xmi,xma,ymi,yma);


	taballoc(&g,*point_nb,*t2);
	taballoc(&k,*point_nb,*t2);

	for(i=0;i<*point_nb;i++)
		for(tt=0;tt<*t2;tt++)
			g[i][tt]=0;

	for(i=1;i<*point_nb;i++) 	/* On calcule le nombre de couples de points par distance g */
			for(j=0;j<i;j++)
			{	d=sqrt((x[i]-x[j])*(x[i]-x[j])+(y[i]-y[j])*(y[i]-y[j]));
				if (d<*t2*(*dt)) {
					tt=d/(*dt);

					//// pour [i,j] :
					// correction des effets de bord
					cin=perim_in_rect(x[i],y[i],d,*xmi,*xma,*ymi,*yma);
					if (cin<0)	{
						Rprintf("cin<0 sur i AVANT\n");
						return -1;
					}
					g[i][tt]+=2*Pi()/cin;

					/// pour [j,i] :
					// correction des effets de bord
					cin=perim_in_rect(x[j],y[j],d,*xmi,*xma,*ymi,*yma);
					if (cin<0) {
						Rprintf("cin<0 sur j AVANT\n");
						return -1;
					}
					g[j][tt]+=2*Pi()/cin;
				}
			}


	for(i=0;i<*point_nb;i++)
	{	k[i][0]=g[i][0];
		for(tt=1;tt<*t2;tt++)
			k[i][tt]=k[i][tt-1]+g[i][tt];	/* on integre */
	}


	//Copies des valeurs dans les tableaux resultat
	for(i=0;i<*point_nb;i=i+1) {
		for(tt=0;tt<*t2;tt=tt+1) {
			gi[i*(*t2)+tt]=g[i][tt];
			ki[i*(*t2)+tt]=k[i][tt];
		}
	}



	freetab(g);
	freetab(k);

	return 0;
}


//fonction de Ripley locale pour une zone circulaire
int ripleylocal_disq(int *point_nb,double *x,double *y,double *x0,double *y0,double *r0,
int *t2,double *dt,double *gi,double *ki)
{	int tt,i,j;
	double d,cin;
	double **g, **k;

	//Decalage pour n'avoir que des valeurs positives
	decalCirc(*point_nb,x,y,x0,y0,*r0);


	taballoc(&g,*point_nb,*t2);
	taballoc(&k,*point_nb,*t2);

	for(i=0;i<*point_nb;i++)
		for(tt=0;tt<*t2;tt=tt+1)
			g[i][tt]=0;
	for(i=1;i<*point_nb;i++) 	/* On calcule le nombre de couples de points par distance g */
			for(j=0;j<i;j++)
			{	d=sqrt((x[i]-x[j])*(x[i]-x[j])+(y[i]-y[j])*(y[i]-y[j]));
				if (d<*t2*(*dt))
				{	tt=d/(*dt);

					//// pour [i,j] :
					// correction des effets de bord
					cin=perim_in_disq(x[i],y[i],d,*x0,*y0,*r0);
					if (cin<0)	{
						Rprintf("cin<0 sur i AVANT\n");
						return -1;
					}
					g[i][tt]+=2*Pi()/cin;

					/// pour [j,i] :
					// correction des effets de bord
					cin=perim_in_disq(x[j],y[j],d,*x0,*y0,*r0);
					if (cin<0) {
						Rprintf("cin<0 sur j AVANT\n");
						return -1;
					}
					g[j][tt]+=2*Pi()/cin;
				}
			}

	for(i=0;i<*point_nb;i++)
	{	k[i][0]=g[i][0];
		for(tt=1;tt<*t2;tt++)
			k[i][tt]+=k[i][tt-1]+g[i][tt];	/* on integre */
	}

	//Copies des valeurs dans les tableaux resultat
	for(i=0;i<*point_nb;i=i+1) {
		for(tt=0;tt<*t2;tt=tt+1) {
			gi[i*(*t2)+tt]=g[i][tt];
			ki[i*(*t2)+tt]=k[i][tt];
		}
	}


	freetab(g);
	freetab(k);

	return 0;
}


//fonction de Ripley locale triangles dans rectangle
int ripleylocal_tr_rect(int *point_nb,double *x,double *y,double *xmi,double *xma,double *ymi,double *yma,
int *triangle_nb,double *ax,double *ay,double *bx,double *by,double *cx,double *cy,
int *t2,double *dt,double *gi,double *ki)
{	int tt,i,j;
	double d,cin;
	double **g, **k;

	//Decalage pour n'avoir que des valeurs positives
	decalRectTri(*point_nb,x,y,xmi,xma,ymi,yma,*triangle_nb,ax,ay,bx,by,cx,cy);


	taballoc(&g,*point_nb,*t2);
	taballoc(&k,*point_nb,*t2);

	for(i=0;i<*point_nb;i++)
		for(tt=0;tt<*t2;tt++)
			g[i][tt]=0;

	for(i=1;i<*point_nb;i++) 	/* On calcule le nombre de couples de points par distance g */
			for(j=0;j<i;j++)
			{	d=sqrt((x[i]-x[j])*(x[i]-x[j])+(y[i]-y[j])*(y[i]-y[j]));
				if (d<*t2*(*dt))
				{	tt=d/(*dt);


					//// pour [i,j] :
					// correction des effets de bord
					cin=perim_in_rect(x[i],y[i],d,*xmi,*xma,*ymi,*yma);
					if (cin<0)	{
						Rprintf("cin<0 sur i AVANT\n");
						return -1;
					}
					cin=cin-perim_triangle(x[i],y[i],d,*triangle_nb,ax,ay,bx,by,cx,cy);
					if (cin<0)	{
						Rprintf("Overlapping triangles\n");
						return -1;
					}
					g[i][tt]+=2*Pi()/cin;

					/// pour [j,i] :
					// correction des effets de bord
					cin=perim_in_rect(x[j],y[j],d,*xmi,*xma,*ymi,*yma);
					if (cin<0) {
						Rprintf("cin<0 sur j AVANT\n");
						return -1;
					}
					cin=cin-perim_triangle(x[j],y[j],d,*triangle_nb,ax,ay,bx,by,cx,cy);
					if (cin<0)	{
						Rprintf("Overlapping triangles\n");
						return -1;
					}
					g[j][tt]+=2*Pi()/cin;
				}
			}

	for(i=0;i<*point_nb;i++)
	{	k[i][0]=g[i][0];
		for(tt=1;tt<*t2;tt++)
			k[i][tt]=k[i][tt-1]+g[i][tt];	/* on integre */
	}

	//Copies des valeurs dans les tableaux resultat
	for(i=0;i<*point_nb;i=i+1) {
		for(tt=0;tt<*t2;tt=tt+1) {
			gi[i*(*t2)+tt]=g[i][tt];
			ki[i*(*t2)+tt]=k[i][tt];
		}
	}


	freetab(g);
	freetab(k);

	return 0;
}


//fonction de Ripley locale triangles dans cercle
int ripleylocal_tr_disq(int *point_nb,double *x,double *y,double *x0,double *y0,double *r0,
int *triangle_nb,double *ax,double *ay,double *bx,double *by,double *cx,double *cy,
int *t2,double *dt,double *gi,double *ki)
{	int tt,i,j;
	double d,cin;
	double **g, **k;

	//Decalage pour n'avoir que des valeurs positives
	decalCircTri(*point_nb,x,y,x0,y0,*r0,*triangle_nb,ax,ay,bx,by,cx,cy);


	taballoc(&g,*point_nb,*t2);
	taballoc(&k,*point_nb,*t2);

	for(i=0;i<*point_nb;i++)
		for(tt=0;tt<*t2;tt=tt+1)
			g[i][tt]=0;
	for(i=1;i<*point_nb;i++) 	/* On calcule le nombre de couples de points par distance g */
			for(j=0;j<i;j++)
			{	d=sqrt((x[i]-x[j])*(x[i]-x[j])+(y[i]-y[j])*(y[i]-y[j]));
				if (d<*t2*(*dt))
				{	tt=d/(*dt);


					//// pour [i,j] :
					// correction des effets de bord
					cin=perim_in_disq(x[i],y[i],d,*x0,*y0,*r0);
					if (cin<0)	{
						Rprintf("cin<0 sur i AVANT\n");
						return -1;
					}
					cin=cin-perim_triangle(x[i],y[i],d,*triangle_nb,ax,ay,bx,by,cx,cy);
					if (cin<0)	{
						Rprintf("Overlapping triangles\n");
						return -1;
					}
					g[i][tt]+=2*Pi()/cin;

					/// pour [j,i] :
					// correction des effets de bord
					cin=perim_in_disq(x[j],y[j],d,*x0,*y0,*r0);
					if (cin<0) {
						Rprintf("cin<0 sur j AVANT\n");
						return -1;
					}
					cin=cin-perim_triangle(x[j],y[j],d,*triangle_nb,ax,ay,bx,by,cx,cy);
					if (cin<0)	{
						Rprintf("Overlapping triangles\n");
						return -1;
					}
					g[j][tt]+=2*Pi()/cin;
				}
			}

	for(i=0;i<*point_nb;i++)
	{	k[i][0]=g[i][0];
		for(tt=1;tt<*t2;tt++)
			k[i][tt]+=k[i][tt-1]+g[i][tt];	/* on integre */
	}

	//Copies des valeurs dans les tableaux resultat
	for(i=0;i<*point_nb;i=i+1) {
		for(tt=0;tt<*t2;tt=tt+1) {
			gi[i*(*t2)+tt]=g[i][tt];
			ki[i*(*t2)+tt]=k[i][tt];
		}
	}


	freetab(g);
	freetab(k);

	return 0;
}




//Densité locale pour une zone rectangulaire
int density_rect(int *point_nb,double *x,double *y,double *xmi,double *xma,double *ymi,
double *yma, int *t2, double *dt, double *xx,double *yy,int *sample_nb,double *count)
{	int tt,i,j;
	double ddd,cin;
	double **s;

	//Decalage pour n'avoir que des valeurs positives
	decalSample(*sample_nb,xx,yy,*xmi,*ymi);
	decalRect(*point_nb,x,y,xmi,xma,ymi,yma);


	taballoc(&s,*sample_nb,*t2);

	for(j=0;j<*sample_nb;j=j+1)
	{	for(tt=0;tt<*t2;tt=tt+1)
			s[j][tt]=0;
		for(i=0;i<*point_nb;i=i+1) 	// On calcule le nombre de voisins dans chaque disque de rayon r
		{	ddd=sqrt((xx[j]-x[i])*(xx[j]-x[i])+(yy[j]-y[i])*(yy[j]-y[i]));
			if (ddd<*t2*(*dt)) {
				tt=ddd/(*dt);

				// correction des effets de bord
				cin=perim_in_rect(xx[j],yy[j],ddd,*xmi,*xma,*ymi,*yma);
				if (cin<0)	{
					Rprintf("cin<0 sur i AVANT\n");
					return -1;
				}
				s[j][tt]+=2*Pi()/cin;
			}
		}
	}
	for(i=0;i<*sample_nb;i=i+1)
		for(tt=1;tt<*t2;tt=tt+1)
			s[i][tt]+=s[i][tt-1];	// on integre


	//Copies des valeurs dans le tableau resultat
	for(i=0;i<*sample_nb;i=i+1)
		for(tt=0;tt<*t2;tt=tt+1)
			count[i*(*t2)+tt]=s[i][tt];


	freetab(s);

	return 0;
}


//Densité locale pour une zone circulaire
int density_disq(int *point_nb,double *x,double *y,double *x0,double *y0,double *r0,
	int *t2,double *dt,double *xx,double *yy,int *sample_nb,double *count)
{	int tt,i,j;
	double ddd,cin;
	double **s;


	//Decalage pour n'avoir que des valeurs positives
	decalSample(*sample_nb,xx,yy,*x0-*r0,*y0-*r0);
	decalCirc(*point_nb,x,y,x0,y0,*r0);


	taballoc(&s,*sample_nb,*t2);

	for(j=0;j<*sample_nb;j=j+1)
	{	for(tt=0;tt<*t2;tt=tt+1)
			s[j][tt]=0;
		for(i=0;i<*point_nb;i=i+1) 	// On calcule le nombre de voisins dans chaque disque de rayon r
		{	ddd=sqrt((xx[j]-x[i])*(xx[j]-x[i])+(yy[j]-y[i])*(yy[j]-y[i]));
			if (ddd<*t2*(*dt))
			{	tt=ddd/(*dt);

				// correction des effets de bord
				cin=perim_in_disq(xx[j],yy[j],ddd,*x0,*y0,*r0);
				if (cin<0)	{
					Rprintf("cin<0 sur i AVANT\n");
					return -1;
				}
				s[j][tt]+=2*Pi()/cin;
			}
		}
	}
	for(i=0;i<*sample_nb;i=i+1)
		for(tt=1;tt<*t2;tt=tt+1)
			s[i][tt]+=s[i][tt-1];	// on integre


	//Copies des valeurs dans le tableau resultat
	for(i=0;i<*sample_nb;i=i+1)
		for(tt=0;tt<*t2;tt=tt+1)
			count[i*(*t2)+tt]=s[i][tt];


	freetab(s);

	return 0;
}


//Densité locale pour triangles dans rectangle
int density_tr_rect(int *point_nb,double *x,double *y,double *xmi,double *xma,
	double *ymi,double *yma,int *triangle_nb,double *ax,double *ay,double *bx,
	double *by,double *cx,double *cy,int *t2,double *dt,double *xx,
	double *yy,int *sample_nb,double *count)
{	int tt,i,j;
	double ddd,cin;
	double **s;


	//Decalage pour n'avoir que des valeurs positives
	decalSample(*sample_nb,xx,yy,*xmi,*ymi);
	decalRectTri(*point_nb,x,y,xmi,xma,ymi,yma,*triangle_nb,ax,ay,bx,by,cx,cy);


	taballoc(&s,*sample_nb,*t2);

	for(j=0;j<*sample_nb;j=j+1)
	{	for(tt=0;tt<*t2;tt=tt+1)
			s[j][tt]=0;
		for(i=0;i<*point_nb;i=i+1) 	// On calcule le nombre de voisins dans chaque disque de rayon r
		{	ddd=sqrt((xx[j]-x[i])*(xx[j]-x[i])+(yy[j]-y[i])*(yy[j]-y[i]));
			if (ddd<*t2*(*dt))
			{	tt=ddd/(*dt);

				//correction des effets de bord
				cin=perim_in_rect(xx[j],yy[j],ddd,*xmi,*xma,*ymi,*yma);
				if (cin<0)	{
					Rprintf("cin<0 sur i AVANT\n");
					return -1;
				}
				cin=cin-perim_triangle(xx[j],yy[j],ddd,*triangle_nb,ax,ay,bx,by,cx,cy);
				if (cin<0)	{
					Rprintf("Overlapping triangles\n");
					return -1;
				}
				s[j][tt]+=2*Pi()/cin;
			}
		}
	}
	for(i=0;i<*sample_nb;i=i+1)
		for(tt=1;tt<*t2;tt=tt+1)
			s[i][tt]+=s[i][tt-1];	// on integre

	//Copies des valeurs dans le tableau resultat
	for(i=0;i<*sample_nb;i=i+1)
		for(tt=0;tt<*t2;tt=tt+1)
			count[i*(*t2)+tt]=s[i][tt];


	freetab(s);

	return 0;
}


//Densité locale pour triangles dans cercle
int density_tr_disq(int *point_nb,double *x,double *y,double *x0,double *y0,double *r0,
	int *triangle_nb,double *ax,double *ay,double *bx,double *by,
	double *cx,double *cy,int *t2,double *dt,double *xx,double *yy,
	int *sample_nb,double *count)
{	int tt,i,j;
	double ddd,cin;
	double **s;


	//Decalage pour n'avoir que des valeurs positives
	decalSample(*sample_nb,xx,yy,*x0-*r0,*y0-*r0);
	decalCircTri(*point_nb,x,y,x0,y0,*r0,*triangle_nb,ax,ay,bx,by,cx,cy);


	taballoc(&s,*sample_nb,*t2);


	for(j=0;j<*sample_nb;j=j+1)
	{	for(tt=0;tt<*t2;tt=tt+1)
			s[j][tt]=0;
		for(i=0;i<*point_nb;i=i+1) 	// On calcule le nombre de voisins dans chaque disque de rayon r
		{	ddd=sqrt((xx[j]-x[i])*(xx[j]-x[i])+(yy[j]-y[i])*(yy[j]-y[i]));
			if (ddd<*t2*(*dt))
			{	tt=ddd/(*dt);

				//correction des effets de bord
				cin=perim_in_disq(xx[j],yy[j],ddd,*x0,*y0,*r0);
				if (cin<0)	{
					Rprintf("cin<0 sur i AVANT\n");
					return -1;
				}
				cin=cin-perim_triangle(xx[j],yy[j],ddd,*triangle_nb,ax,ay,bx,by,cx,cy);
				if (cin<0)	{
					Rprintf("Overlapping triangles\n");
					return -1;
				}
				s[j][tt]+=2*Pi()/cin;
			}
		}
	}
	for(i=0;i<*sample_nb;i=i+1)
		for(tt=1;tt<*t2;tt=tt+1)
			s[i][tt]+=s[i][tt-1];	// on integre

	//Copies des valeurs dans le tableau resultat
	for(i=0;i<*sample_nb;i=i+1)
		for(tt=0;tt<*t2;tt=tt+1)
			count[i*(*t2)+tt]=s[i][tt];


	freetab(s);

	return 0;
}


/******************************************************************************/
/* Calcule la fonction intertype pour les semis (x,y) et (x2,y2) en parametres*/
/* dans une zone de forme rectangulaire de bornes xmi xma ymi yma             */
/* Les corrections des effets de bords sont fait par la methode de Ripley,    */
/* i.e. l'inverse de la proportion d'arc de cercle inclu dans la fenetre.     */
/* Les calculs sont faits pour les t2 premiers intervalles de largeur dt.     */
/* La routine calcule g12, densite des couples de points;  et la fonction K12 */
/* Les resultats sont stockes dans des tableaux g et k donnes en parametres   */
/******************************************************************************/

int intertype_rect(int *point_nb1,double *x1,double *y1,int *point_nb2, double *x2, double *y2,
double *xmi,double *xma,double *ymi,double *yma,int *t2,double *dt,double *g,double *k)
{	int i,j,tt;
	double d,cin;

	//Decalage pour n'avoir que des valeurs positives
	decalRect2(*point_nb1,x1,y1,*point_nb2,x2,y2,xmi,xma,ymi,yma);

	// On rangera dans g le nombre de couples de points par distance tt
   for(tt=0;tt<*t2;tt=tt+1)
	{	g[tt]=0;
   }

	// On regarde tous les couples (i,j)
	for(i=0;i<*point_nb1;i=i+1)
	{	for(j=0;j<*point_nb2;j=j+1)
		{	d=sqrt((x1[i]-x2[j])*(x1[i]-x2[j])+(y1[i]-y2[j])*(y1[i]-y2[j]));
			if (d<*t2*(*dt))
			{	// dans quelle classe de distance est ce couple ?
				tt=d/(*dt);
				// correction des effets de bord
				cin=perim_in_rect(x1[i],y1[i],d,*xmi,*xma,*ymi,*yma);
				if (cin<0) {
					Rprintf("\ncin<0 sur i AVANT");
					return -1;
				}
				g[tt]+=2*Pi()/cin;
			}
		}
   }

   // on moyenne -> densite
   for(tt=0;tt<*t2;tt=tt+1)
	{	g[tt]=g[tt]/(*point_nb1);
   }

	//on integre
   k[0]=g[0];
  	for(tt=1;tt<*t2;tt=tt+1)
	{	k[tt]=k[tt-1]+g[tt];
   }

   return 0;
}


//fonction intertype pour une zone circulaire
int intertype_disq(int *point_nb1, double *x1, double *y1, int *point_nb2, double *x2,
	double *y2, double *x0, double *y0, double *r0,int *t2, double *dt, double *g, double *k)
{	int tt,i,j;
	double d,cin;

	//Decalage pour n'avoir que des valeurs positives
	decalCirc2(*point_nb1,x1,y1,*point_nb2,x2,y2,x0,y0,*r0);

	for(tt=0;tt<*t2;tt=tt+1) {
		g[tt]=0;
	}
	for(i=0;i<*point_nb1;i++) 	// On calcule le nombre de couples de points par distance g
			for(j=0;j<*point_nb2;j++)
			{	d=sqrt((x1[i]-x2[j])*(x1[i]-x2[j])+(y1[i]-y2[j])*(y1[i]-y2[j]));
				if (d<*t2*(*dt))
				{	tt=d/(*dt);

					// correction des effets de bord
					cin=perim_in_disq(x1[i],y1[i],d,*x0,*y0,*r0);
					if (cin<0) {
						Rprintf("\ncin<0 sur i AVANT");
					return -1;
					}
					g[tt]+=2*Pi()/cin;
				}
			}

	// on moyenne -> densite
	for(tt=0;tt<*t2;tt++) {
		g[tt]=g[tt]/(*point_nb1);
	}

	// on integre
	k[0]=g[0];
	for(tt=1;tt<*t2;tt++) {
		k[tt]=k[tt-1]+g[tt];
	}

	return 0;
}

//Intertype triangles dans rectangle
int intertype_tr_rect(int *point_nb1,double *x1,double *y1,int *point_nb2,double *x2,double *y2,
double *xmi,double *xma,double *ymi,double *yma,int *triangle_nb,double *ax,double *ay,double *bx,double *by,
double *cx,double *cy,int *t2,double *dt,double *g,double *k)
{	int i,j,tt;
	double d,cin;

	//Decalage pour n'avoir que des valeurs positives
	decalRectTri2(*point_nb1,x1,y1,*point_nb2,x2,y2,xmi,xma,ymi,yma,*triangle_nb,ax,ay,bx,by,cx,cy);

	// On calcule le nombre de couples de points par distance g
	for(tt=0;tt<*t2;tt=tt+1){
		g[tt]=0;
	}
	for(i=0;i<*point_nb1;i++)
		for(j=0;j<*point_nb2;j++)
		{	d=sqrt((x1[i]-x2[j])*(x1[i]-x2[j])+(y1[i]-y2[j])*(y1[i]-y2[j]));
			if (d<*t2*(*dt))
			{	tt=d/(*dt);
				cin=perim_in_rect(x1[i],y1[i],d,*xmi,*xma,*ymi,*yma);
				if (cin<0) {
					Rprintf("\ncin<0 sur i AVANT");
				return -1;
				}
				cin=cin-perim_triangle(x1[i],y1[i],d,*triangle_nb,ax,ay,bx,by,cx,cy);
				if (cin<0)	{
					Rprintf("Overlapping triangles\n");
					return -1;
				}
				g[tt]+=2*Pi()/cin;
			}
		}

	// on moyenne -> densite
	for(tt=0;tt<*t2;tt++) {
		g[tt]=g[tt]/(*point_nb1);
	}

	// on integre
	k[0]=g[0];
	for(tt=1;tt<*t2;tt++){
		k[tt]=k[tt-1]+g[tt];
	}

	return 0;
}

//Intertype triangles dans cercle
int intertype_tr_disq(int *point_nb1,double *x1,double *y1,int *point_nb2,double *x2,double *y2,
double *x0,double *y0,double *r0,int *triangle_nb,double *ax,double *ay,double *bx,double *by,
double *cx,double *cy,int *t2,double *dt,double *g,double *k)
{	int i,j,tt;
	double d,cin;

	//Decalage pour n'avoir que des valeurs positives
	decalCircTri2(*point_nb1,x1,y1,*point_nb2,x2,y2,x0,y0,*r0,*triangle_nb,ax,ay,bx,by,cx,cy);

	// On calcule le nombre de couples de points par distance g
	for(tt=0;tt<*t2;tt=tt+1){
		g[tt]=0;
	}
	for(i=0;i<*point_nb1;i++)
		for(j=0;j<*point_nb2;j++)
		{	d=sqrt((x1[i]-x2[j])*(x1[i]-x2[j])+(y1[i]-y2[j])*(y1[i]-y2[j]));
			if (d<*t2*(*dt))
			{	tt=d/(*dt);
				cin=perim_in_disq(x1[i],y1[i],d,*x0,*y0,*r0);
				if (cin<0) {
					Rprintf("\ncin<0 sur i AVANT");
				return -1;
				}
				cin=cin-perim_triangle(x1[i],y1[i],d,*triangle_nb,ax,ay,bx,by,cx,cy);
				if (cin<0)	{
					Rprintf("Overlapping triangles\n");
					return -1;
				}
				g[tt]+=2*Pi()/cin;
			}
		}

	// on moyenne -> densite
	for(tt=0;tt<*t2;tt++) {
		g[tt]=g[tt]/(*point_nb1);
	}

	// on integre
	k[0]=g[0];
	for(tt=1;tt<*t2;tt++){
		k[tt]=k[tt-1]+g[tt];
	}

	return 0;
}



//fonction intertype avec intervalle de confiance pour une zone rectangulaire
int intertype_rect_ic(int *point_nb1,double *x1,double *y1,int *point_nb2, double *x2, double *y2,
double *xmi,double *xma,double *ymi,double *yma,double *densite2,
int *t2,double *dt,int *nbSimu,int *h0, double *prec, double *lev,double *densite,double *g,double *k,
double *gic1,double *gic2, double *kic1,double *kic2, double *gval, double *kval, double *lval, double *nval) {
	int i,j,i0,i1,i2,ptot;
	double **gic,**kic;
	double *gg,*kk,*ll,*nn;
	double *gt,*kt,*lt,*nt;
	int erreur=0;
	int *type;
	double *x,*y;
	int point_nb=0;

	erreur=intertype_rect(point_nb1,x1,y1,point_nb2,x2,y2,xmi,xma,ymi,yma,t2,dt,g,k);
	if (erreur!=0) {
		return -1;
	}

	///Définition de i0 : indice où sera stocké l'estimation des bornes de l'IC
	i0=*lev/2*(*nbSimu+1);
	if (i0<1) i0=1;

	///Initialisation des tableaux dans lesquels on va stocker les valeurs extremes lors de MC
	taballoc(&gic,*t2+1,2*i0+10+1);
	taballoc(&kic,*t2+1,2*i0+10+1);

	//Normalisation de g et k et calcul de l et n pour le calcul des p-values
		vecalloc(&gg,*t2);
		vecalloc(&kk,*t2);
		vecalloc(&ll,*t2);
		vecalloc(&nn,*t2);
		for(i=0;i<*t2;i++) {
			gg[i]=g[i]/(*densite2*(Pi()*(i+1)*(i+1)*(*dt)*(*dt)-Pi()*i*i*(*dt)*(*dt)));
			nn[i]=k[i]/(Pi()*(i+1)*(i+1)*(*dt)*(*dt));
			kk[i]=k[i]/(*densite2);
			ll[i]=sqrt(kk[i]/Pi())-(i+1)*(*dt);

			gval[i]=1;
			kval[i]=1;
			nval[i]=1;
			lval[i]=1;
	}

	//Initialisations avant la boucle principale
	vecalloc(&x,*point_nb1+*point_nb2);
	vecalloc(&y,*point_nb1+*point_nb2);
	vecintalloc(&type,*point_nb1+*point_nb2);
	if (*h0==1) { //Option 1 : substitutions : on stocke tous les points
		for(i=0;i<*point_nb1;i++) {
			x[i]=x1[i];
			y[i]=y1[i];
		}
		for(i=0;i<*point_nb2;i++) {
			x[*point_nb1+i]=x2[i];
			y[*point_nb1+i]=y2[i];
		}
		//on lance Ripley sur tous les points + normalization pour le calcul des p-values
		vecalloc(&gt,*t2);
		vecalloc(&kt,*t2);
		vecalloc(&lt,*t2);
		vecalloc(&nt,*t2);
		ptot=*point_nb1+*point_nb2;
		erreur=ripley_rect(&ptot,x,y,xmi,xma,ymi,yma,t2,dt,gt,kt);
		if (erreur!=0) {
			return -1;
		}
		for(j=0;j<*t2;j++) {
			gt[j]=gt[j]/(*densite*(Pi()*(j+1)*(j+1)*(*dt)*(*dt)-Pi()*j*j*(*dt)*(*dt)));
			nt[j]=kt[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt));
			kt[j]=kt[j]/(*densite);
			lt[j]=sqrt(kt[j]/Pi())-(j+1)*(*dt);
		}
	}
	//Sinon option 2 : rien a initialiser
	int lp=0;

	// boucle principale de MC
	Rprintf("Monte Carlo simulation\n");
	for(i=1;i<=*nbSimu;i++) {

		//On simule les hypothèses nulles
		if(*h0==1) {
			erreur=randlabelling(x,y,*point_nb1,x1,y1,*point_nb2,x2,y2,type);
		}
		else {
			erreur=randshifting_rect(&point_nb,x,y,*point_nb1,x1,y1,*xmi,*xma,*ymi,*yma,*prec);
		}

		if (erreur==0) {
			if (*h0==1) {
				// étiquetage aléatoire
				erreur=intertype_rect(point_nb1,x1,y1,point_nb2,x2,y2,xmi,xma,ymi,yma,t2,dt,gic1,kic1);
         	}
         	else {
				// décallage avec rectangle
        	 	erreur=intertype_rect(&point_nb,x,y,point_nb2,x2,y2,xmi,xma,ymi,yma,t2,dt,gic1,kic1);
         	}
      	}
		// si il y a une erreur on recommence une simulation
		if (erreur!=0) {
			i=i-1;
			Rprintf("ERREUR Intertype\n");
		}
		else {
			//comptage du nombre de |¶obs|<=|¶simu| pour test local
			double gictmp,kictmp,lictmp,nictmp;
			for(j=0;j<*t2;j++) {
				gictmp=gic1[j]/(*densite2*(Pi()*(j+1)*(j+1)*(*dt)*(*dt)-Pi()*j*j*(*dt)*(*dt)));
				nictmp=kic1[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt));
				kictmp=kic1[j]/(*densite2);
				lictmp=sqrt(kictmp/Pi())-(j+1)*(*dt);
				if(*h0==1) {
					if ((float)fabs(gg[j]-gt[j])<=(float)fabs(gictmp-gt[j])) {gval[j]+=1;}
					if ((float)fabs(nn[j]-(*densite2*kt[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt))))<=(float)fabs(nictmp-(*densite2*kt[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt))))) {nval[j]+=1;}
					if ((float)fabs(kk[j]-kt[j])<=(float)(kictmp-kt[j])) {kval[j]+=1;}
					if ((float)fabs(ll[j]-lt[j])<=(float)fabs(lictmp-lt[j])) {lval[j]+=1;}
				}
				if(*h0==2) {
					if ((float)fabs(gg[j]-1)<=(float)fabs(gictmp-1)) {gval[j]+=1;}
					if ((float)fabs(nn[j]-*densite2)<=(float)fabs(nictmp-*densite2)) {nval[j]+=1;}
					if ((float)fabs(kk[j]-Pi()*(j+1)*(j+1)*(*dt)*(*dt))<=(float)(kictmp-Pi()*(j+1)*(j+1)*(*dt)*(*dt))) {kval[j]+=1;}
					if ((float)fabs(ll[j])<=(float)fabs(lictmp)) {lval[j]+=1;}
				}
			}

			////Traitement des résultats
			ic(i,i0,gic,kic,gic1,kic1,*t2);
		}
		R_FlushConsole();
 		progress(i,&lp,*nbSimu);
	}

	i1=i0+2;
	i2=i0;

	//Copies des valeurs dans les tableaux résultats
	for(i=0;i<*t2;i++) {
		gic1[i]=gic[i+1][i1];
		gic2[i]=gic[i+1][i2];
		kic1[i]=kic[i+1][i1];
		kic2[i]=kic[i+1][i2];
	}


	freetab(gic);
	freetab(kic);
	freevec(gg);
	freevec(kk);
	freevec(ll);
	freevec(nn);
	if(*h0==1) {
		freevec(gt);
		freevec(kt);
		freevec(lt);
		freevec(nt);
	}
	freevec(x);
	freevec(y);
	freeintvec(type);
	return 0;
}


//fonction intertype avec intervalle de confiance pour une zone circulaire
int intertype_disq_ic(int *point_nb1,double *x1,double *y1,int *point_nb2, double *x2, double *y2,
double *x0,double *y0,double *r0,double *densite2,
int *t2,double *dt,int *nbSimu,int *h0, double *prec, double *lev,double *densite,double *g,double *k,
double *gic1,double *gic2, double *kic1,double *kic2, double *gval, double *kval, double *lval, double *nval) {
	int i,j,i0,i1,i2,ptot;
	double **gic,**kic;
	double *gt,*kt,*lt,*nt;
	double *gg,*kk,*ll,*nn;
	int erreur=0;
	int *type;
	double *x,*y;
	int point_nb=0;

	erreur=intertype_disq(point_nb1,x1,y1,point_nb2,x2,y2,x0,y0,r0,t2,dt,g,k);
	if (erreur!=0) {
		return -1;
	}

	///Définition de i0 : indice où sera stocké l'estimation des bornes de l'IC
	i0=*lev/2*(*nbSimu+1);
	if (i0<1) i0=1;

	///Initialisation des tableaux dans lesquels on va stocker les valeurs extremes lors de MC
	taballoc(&gic,*t2+1,2*i0+10+1);
	taballoc(&kic,*t2+1,2*i0+10+1);

	//Normalisation de g et k et calcul de l et n pour le calcul des p-values
		vecalloc(&gg,*t2);
		vecalloc(&kk,*t2);
		vecalloc(&ll,*t2);
		vecalloc(&nn,*t2);
		for(i=0;i<*t2;i++) {
			gg[i]=g[i]/(*densite2*(Pi()*(i+1)*(i+1)*(*dt)*(*dt)-Pi()*i*i*(*dt)*(*dt)));
			nn[i]=k[i]/(Pi()*(i+1)*(i+1)*(*dt)*(*dt));
			kk[i]=k[i]/(*densite2);
			ll[i]=sqrt(kk[i]/Pi())-(i+1)*(*dt);

			gval[i]=1;
			kval[i]=1;
			nval[i]=1;
			lval[i]=1;
	}

	//Initialisations avant la boucle principale
	vecalloc(&x,*point_nb1+*point_nb2);
	vecalloc(&y,*point_nb1+*point_nb2);
	vecintalloc(&type,*point_nb1+*point_nb2);
	if (*h0==1) { //Option 1 : substitutions : on stocke tous les points
		for(i=0;i<*point_nb1;i++) {
			x[i]=x1[i];
			y[i]=y1[i];
		}
		for(i=0;i<*point_nb2;i++) {
			x[*point_nb1+i]=x2[i];
			y[*point_nb1+i]=y2[i];
		}
		//on lance Ripley sur tous les points + normalization pour le calcul des p-values
		vecalloc(&gt,*t2);
		vecalloc(&kt,*t2);
		vecalloc(&lt,*t2);
		vecalloc(&nt,*t2);
		ptot=*point_nb1+*point_nb2;
		erreur=ripley_disq(&ptot,x,y,x0,y0,r0,t2,dt,gt,kt);
		if (erreur!=0) {
			return -1;
		}
		for(j=0;j<*t2;j++) {
			gt[j]=gt[j]/(*densite*(Pi()*(j+1)*(j+1)*(*dt)*(*dt)-Pi()*j*j*(*dt)*(*dt)));
			nt[j]=kt[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt));
			kt[j]=kt[j]/(*densite);
			lt[j]=sqrt(kt[j]/Pi())-(j+1)*(*dt);
		}
	}
	//Sinon option 2 : rien a initialiser
	int lp=0;

	// boucle principale de MC
	Rprintf("Monte Carlo simulation\n");
	for(i=1;i<=*nbSimu;i++) {

		//On simule les hypothèses nulles
		if(*h0==1) {
			erreur=randlabelling(x,y,*point_nb1,x1,y1,*point_nb2,x2,y2,type);
		}
		else {
			erreur=randshifting_disq(&point_nb,x,y,*point_nb1,x1,y1,*x0,*y0,*r0,*prec);
		}

		if (erreur==0) {
			if (*h0==1) {
				erreur=intertype_disq(point_nb1,x1,y1,point_nb2,x2,y2,x0,y0,r0,t2,dt,gic1,kic1);
         	}
         	else {
        	 	erreur=intertype_disq(&point_nb,x,y,point_nb2,x2,y2,x0,y0,r0,t2,dt,gic1,kic1);
         	}
      	}
		// si il y a une erreur on recommence une simulation
		if (erreur!=0) {
			i=i-1;
			Rprintf("ERREUR Intertype\n");
		}
		else {
			//comptage du nombre de |¶obs|<=|¶simu| pour test local
			double gictmp,kictmp,lictmp,nictmp;
			for(j=0;j<*t2;j++) {
				gictmp=gic1[j]/(*densite2*(Pi()*(j+1)*(j+1)*(*dt)*(*dt)-Pi()*j*j*(*dt)*(*dt)));
				nictmp=kic1[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt));
				kictmp=kic1[j]/(*densite2);
				lictmp=sqrt(kictmp/Pi())-(j+1)*(*dt);
				if(*h0==1) {
					if ((float)fabs(gg[j]-gt[j])<=(float)fabs(gictmp-gt[j])) {gval[j]+=1;}
					if ((float)fabs(nn[j]-(*densite2*kt[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt))))<=(float)fabs(nictmp-(*densite2*kt[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt))))) {nval[j]+=1;}
					if ((float)fabs(kk[j]-kt[j])<=(float)(kictmp-kt[j])) {kval[j]+=1;}
					if ((float)fabs(ll[j]-lt[j])<=(float)fabs(lictmp-lt[j])) {lval[j]+=1;}
				}
				if(*h0==2) {
					if ((float)fabs(gg[j]-1)<=(float)fabs(gictmp-1)) {gval[j]+=1;}
					if ((float)fabs(nn[j]-*densite2)<=(float)fabs(nictmp-*densite2)) {nval[j]+=1;}
					if ((float)fabs(kk[j]-Pi()*(j+1)*(j+1)*(*dt)*(*dt))<=(float)(kictmp-Pi()*(j+1)*(j+1)*(*dt)*(*dt))) {kval[j]+=1;}
					if ((float)fabs(ll[j])<=(float)fabs(lictmp)) {lval[j]+=1;}
				}
			}
			////Traitement des résultats
			ic(i,i0,gic,kic,gic1,kic1,*t2);
		}
		R_FlushConsole();
 		progress(i,&lp,*nbSimu);
	}
	i1=i0+2;
	i2=i0;
	//Copies des valeurs dans les tableaux résultats
	for(i=0;i<*t2;i++) {
		gic1[i]=gic[i+1][i1];
		gic2[i]=gic[i+1][i2];
		kic1[i]=kic[i+1][i1];
		kic2[i]=kic[i+1][i2];
	}
	freetab(gic);
	freetab(kic);
	freevec(gg);
	freevec(kk);
	freevec(ll);
	freevec(nn);
	if(*h0==1) {
		freevec(gt);
		freevec(kt);
		freevec(lt);
		freevec(nt);
	}
	freevec(x);
	freevec(y);
	freeintvec(type);
	return 0;
}


//fonction intertype avec intervalle de confiance pour une zone rectangulaire + triangles
int intertype_tr_rect_ic(int *point_nb1,double *x1,double *y1,int *point_nb2, double *x2, double *y2,
double *xmi,double *xma,double *ymi,double *yma,double *densite2,
int *triangle_nb,double *ax,double *ay,double *bx,double *by,double *cx,double *cy,
int *t2,double *dt,int *nbSimu,int *h0, double *prec, double *lev,double *densite,double *g,double *k,
double *gic1,double *gic2, double *kic1,double *kic2, double *gval, double *kval, double *lval, double *nval) {
	int i,j,i0,i1,i2,ptot;
	double **gic,**kic;
	double *gg,*kk,*ll,*nn;
	double *gt,*kt,*lt,*nt;
	int erreur=0;
	int *type;
	double *x,*y;
	int point_nb=0;

	erreur=intertype_tr_rect(point_nb1,x1,y1,point_nb2,x2,y2,xmi,xma,ymi,yma,triangle_nb,ax,ay,bx,by,cx,cy,t2,dt,g,k);
	if (erreur!=0) {
		return -1;
	}

	///Définition de i0 : indice où sera stocké l'estimation des bornes de l'IC
	i0=*lev/2*(*nbSimu+1);
	if (i0<1) i0=1;

	///Initialisation des tableaux dans lesquels on va stocker les valeurs extremes lors de MC
	taballoc(&gic,*t2+1,2*i0+10+1);
	taballoc(&kic,*t2+1,2*i0+10+1);

	//Normalisation de g et k et calcul de l et n pour le calcul des p-values
		vecalloc(&gg,*t2);
		vecalloc(&kk,*t2);
		vecalloc(&ll,*t2);
		vecalloc(&nn,*t2);
		for(i=0;i<*t2;i++) {
			gg[i]=g[i]/(*densite2*(Pi()*(i+1)*(i+1)*(*dt)*(*dt)-Pi()*i*i*(*dt)*(*dt)));
			nn[i]=k[i]/(Pi()*(i+1)*(i+1)*(*dt)*(*dt));
			kk[i]=k[i]/(*densite2);
			ll[i]=sqrt(kk[i]/Pi())-(i+1)*(*dt);

			gval[i]=1;
			kval[i]=1;
			nval[i]=1;
			lval[i]=1;
	}

	//Initialisations avant la boucle principale
	vecalloc(&x,*point_nb1+*point_nb2);
	vecalloc(&y,*point_nb1+*point_nb2);
	vecintalloc(&type,*point_nb1+*point_nb2);
	if (*h0==1) { //Option 1 : substitutions : on stocke tous les points
		for(i=0;i<*point_nb1;i++) {
			x[i]=x1[i];
			y[i]=y1[i];
		}
		for(i=0;i<*point_nb2;i++) {
			x[*point_nb1+i]=x2[i];
			y[*point_nb1+i]=y2[i];
		}
		//on lance Ripley sur tous les points + normalization pour le calcul des p-values
		vecalloc(&gt,*t2);
		vecalloc(&kt,*t2);
		vecalloc(&lt,*t2);
		vecalloc(&nt,*t2);
		ptot=*point_nb1+*point_nb2;
		erreur=ripley_tr_rect(&ptot,x,y,xmi,xma,ymi,yma,triangle_nb,ax,ay,bx,by,cx,cy,t2,dt,gt,kt);
		if (erreur!=0) {
			return -1;
		}
		for(j=0;j<*t2;j++) {
			gt[j]=gt[j]/(*densite*(Pi()*(j+1)*(j+1)*(*dt)*(*dt)-Pi()*j*j*(*dt)*(*dt)));
			nt[j]=kt[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt));
			kt[j]=kt[j]/(*densite);
			lt[j]=sqrt(kt[j]/Pi())-(j+1)*(*dt);
		}
	}
	//Sinon option 2 : rien a initialiser
	int lp=0;

	// boucle principale de MC
	Rprintf("Monte Carlo simulation\n");
	for(i=1;i<=*nbSimu;i++) {

		//On simule les hypothèses nulles
		if(*h0==1) {
			erreur=randlabelling(x,y,*point_nb1,x1,y1,*point_nb2,x2,y2,type);
		}
		else {
			erreur=randshifting_tr_rect(&point_nb,x,y,*point_nb1,x1,y1,*xmi,*xma,*ymi,*yma,*triangle_nb,ax,ay,bx,by,cx,cy,*prec);
		}

		if (erreur==0) {
			if (*h0==1) {
				// étiquetage aléatoire
				erreur=intertype_tr_rect(point_nb1,x1,y1,point_nb2,x2,y2,xmi,xma,ymi,yma,triangle_nb,ax,ay,bx,by,cx,cy,t2,dt,gic1,kic1);
         	}
         	else {
				// décallage avec rectangle
        	 	erreur=intertype_tr_rect(&point_nb,x,y,point_nb2,x2,y2,xmi,xma,ymi,yma,triangle_nb,ax,ay,bx,by,cx,cy,t2,dt,gic1,kic1);
         	}
      	}
		// si il y a une erreur on recommence une simulation
		if (erreur!=0) {
			i=i-1;
			Rprintf("ERREUR Intertype\n");
		}
		else {
			//comptage du nombre de |¶obs|<=|¶simu| pour test local
			double gictmp,kictmp,lictmp,nictmp;
			for(j=0;j<*t2;j++) {
				gictmp=gic1[j]/(*densite2*(Pi()*(j+1)*(j+1)*(*dt)*(*dt)-Pi()*j*j*(*dt)*(*dt)));
				nictmp=kic1[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt));
				kictmp=kic1[j]/(*densite2);
				lictmp=sqrt(kictmp/Pi())-(j+1)*(*dt);
				if(*h0==1) {
					if ((float)fabs(gg[j]-gt[j])<=(float)fabs(gictmp-gt[j])) {gval[j]+=1;}
					if ((float)fabs(nn[j]-(*densite2*kt[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt))))<=(float)fabs(nictmp-(*densite2*kt[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt))))) {nval[j]+=1;}
					if ((float)fabs(kk[j]-kt[j])<=(float)(kictmp-kt[j])) {kval[j]+=1;}
					if ((float)fabs(ll[j]-lt[j])<=(float)fabs(lictmp-lt[j])) {lval[j]+=1;}
				}
				if(*h0==2) {
					if ((float)fabs(gg[j]-1)<=(float)fabs(gictmp-1)) {gval[j]+=1;}
					if ((float)fabs(nn[j]-*densite2)<=(float)fabs(nictmp-*densite2)) {nval[j]+=1;}
					if ((float)fabs(kk[j]-Pi()*(j+1)*(j+1)*(*dt)*(*dt))<=(float)(kictmp-Pi()*(j+1)*(j+1)*(*dt)*(*dt))) {kval[j]+=1;}
					if ((float)fabs(ll[j])<=(float)fabs(lictmp)) {lval[j]+=1;}
				}
			}

			////Traitement des résultats
			ic(i,i0,gic,kic,gic1,kic1,*t2);
		}
		R_FlushConsole();
 		progress(i,&lp,*nbSimu);
	}

	i1=i0+2;
	i2=i0;

	//Copies des valeurs dans les tableaux résultats
	for(i=0;i<*t2;i++) {
		gic1[i]=gic[i+1][i1];
		gic2[i]=gic[i+1][i2];
		kic1[i]=kic[i+1][i1];
		kic2[i]=kic[i+1][i2];
	}


	freetab(gic);
	freetab(kic);
	freevec(gg);
	freevec(kk);
	freevec(ll);
	freevec(nn);
	if(*h0==1) {
		freevec(gt);
		freevec(kt);
		freevec(lt);
		freevec(nt);
	}
	freevec(x);
	freevec(y);
	freeintvec(type);
	return 0;
}


//fonction intertype avec intervalle de confiance pour une zone circulaire + triangles
int intertype_tr_disq_ic(int *point_nb1,double *x1,double *y1,int *point_nb2, double *x2, double *y2,
double *x0,double *y0,double *r0,double *densite2,
int *triangle_nb,double *ax,double *ay,double *bx,double *by,double *cx,double *cy,
int *t2,double *dt,int *nbSimu,int *h0, double *prec, double *lev,double *densite,double *g,double *k,
double *gic1,double *gic2, double *kic1,double *kic2, double *gval, double *kval, double *lval, double *nval) {
	int i,j,i0,i1,i2,ptot;
	double **gic,**kic;
	double *gg,*kk,*ll,*nn;
	double *gt,*kt,*lt,*nt;
	int erreur=0;
	int *type;
	double *x,*y;
	int point_nb=0;

	erreur=intertype_tr_disq(point_nb1,x1,y1,point_nb2,x2,y2,x0,y0,r0,triangle_nb,ax,ay,bx,by,cx,cy,t2,dt,g,k);
	if (erreur!=0) {
		return -1;
	}

	///Définition de i0 : indice où sera stocké l'estimation des bornes de l'IC
	i0=*lev/2*(*nbSimu+1);
	if (i0<1) i0=1;

	///Initialisation des tableaux dans lesquels on va stocker les valeurs extremes lors de MC
	taballoc(&gic,*t2+1,2*i0+10+1);
	taballoc(&kic,*t2+1,2*i0+10+1);

	//Normalisation de g et k et calcul de l et n pour le calcul des p-values
		vecalloc(&gg,*t2);
		vecalloc(&kk,*t2);
		vecalloc(&ll,*t2);
		vecalloc(&nn,*t2);
		for(i=0;i<*t2;i++) {
			gg[i]=g[i]/(*densite2*(Pi()*(i+1)*(i+1)*(*dt)*(*dt)-Pi()*i*i*(*dt)*(*dt)));
			nn[i]=k[i]/(Pi()*(i+1)*(i+1)*(*dt)*(*dt));
			kk[i]=k[i]/(*densite2);
			ll[i]=sqrt(kk[i]/Pi())-(i+1)*(*dt);

			gval[i]=1;
			kval[i]=1;
			nval[i]=1;
			lval[i]=1;
	}

	//Initialisations avant la boucle principale
	vecalloc(&x,*point_nb1+*point_nb2);
	vecalloc(&y,*point_nb1+*point_nb2);
	vecintalloc(&type,*point_nb1+*point_nb2);
	if (*h0==1) { //Option 1 : substitutions : on stocke tous les points
		for(i=0;i<*point_nb1;i++) {
			x[i]=x1[i];
			y[i]=y1[i];
		}
		for(i=0;i<*point_nb2;i++) {
			x[*point_nb1+i]=x2[i];
			y[*point_nb1+i]=y2[i];
		}
		//on lance Ripley sur tous les points + normalization pour le calcul des p-values
		vecalloc(&gt,*t2);
		vecalloc(&kt,*t2);
		vecalloc(&lt,*t2);
		vecalloc(&nt,*t2);
		ptot=*point_nb1+*point_nb2;
		erreur=ripley_tr_disq(&ptot,x,y,x0,y0,r0,triangle_nb,ax,ay,bx,by,cx,cy,t2,dt,gt,kt);
		if (erreur!=0) {
			return -1;
		}
		for(j=0;j<*t2;j++) {
			gt[j]=gt[j]/(*densite*(Pi()*(j+1)*(j+1)*(*dt)*(*dt)-Pi()*j*j*(*dt)*(*dt)));
			nt[j]=kt[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt));
			kt[j]=kt[j]/(*densite);
			lt[j]=sqrt(kt[j]/Pi())-(j+1)*(*dt);
		}
	}
	//Sinon option 2 : rien a initialiser
	int lp=0;

	// boucle principale de MC
	Rprintf("Monte Carlo simulation\n");
	for(i=1;i<=*nbSimu;i++) {

		//On simule les hypothèses nulles
		if(*h0==1) {
			erreur=randlabelling(x,y,*point_nb1,x1,y1,*point_nb2,x2,y2,type);
		}
		else {
			erreur=randshifting_tr_disq(&point_nb,x,y,*point_nb1,x1,y1,*x0,*y0,*r0,*triangle_nb,ax,ay,bx,by,cx,cy,*prec);
		}

		if (erreur==0) {
			if (*h0==1) {
				// étiquetage aléatoire
				erreur=intertype_tr_disq(point_nb1,x1,y1,point_nb2,x2,y2,x0,y0,r0,triangle_nb,ax,ay,bx,by,cx,cy,t2,dt,gic1,kic1);
         	}
         	else {
				// décallage avec rectangle
        	 	erreur=intertype_tr_disq(&point_nb,x,y,point_nb2,x2,y2,x0,y0,r0,triangle_nb,ax,ay,bx,by,cx,cy,t2,dt,gic1,kic1);
         	}
      	}
		// si il y a une erreur on recommence une simulation
		if (erreur!=0) {
			i=i-1;
			Rprintf("ERREUR Intertype\n");
		}
		else {
			//comptage du nombre de |¶obs|<=|¶simu| pour test local
			double gictmp,kictmp,lictmp,nictmp;
			for(j=0;j<*t2;j++) {
				gictmp=gic1[j]/(*densite2*(Pi()*(j+1)*(j+1)*(*dt)*(*dt)-Pi()*j*j*(*dt)*(*dt)));
				nictmp=kic1[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt));
				kictmp=kic1[j]/(*densite2);
				lictmp=sqrt(kictmp/Pi())-(j+1)*(*dt);
				if(*h0==1) {
					if ((float)fabs(gg[j]-gt[j])<=(float)fabs(gictmp-gt[j])) {gval[j]+=1;}
					if ((float)fabs(nn[j]-(*densite2*kt[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt))))<=(float)fabs(nictmp-(*densite2*kt[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt))))) {nval[j]+=1;}
					if ((float)fabs(kk[j]-kt[j])<=(float)(kictmp-kt[j])) {kval[j]+=1;}
					if ((float)fabs(ll[j]-lt[j])<=(float)fabs(lictmp-lt[j])) {lval[j]+=1;}
				}
				if(*h0==2) {
					if ((float)fabs(gg[j]-1)<=(float)fabs(gictmp-1)) {gval[j]+=1;}
					if ((float)fabs(nn[j]-*densite2)<=(float)fabs(nictmp-*densite2)) {nval[j]+=1;}
					if ((float)fabs(kk[j]-Pi()*(j+1)*(j+1)*(*dt)*(*dt))<=(float)(kictmp-Pi()*(j+1)*(j+1)*(*dt)*(*dt))) {kval[j]+=1;}
					if ((float)fabs(ll[j])<=(float)fabs(lictmp)) {lval[j]+=1;}
				}
			}

			////Traitement des résultats
			ic(i,i0,gic,kic,gic1,kic1,*t2);
		}
		R_FlushConsole();
 		progress(i,&lp,*nbSimu);
	}

	i1=i0+2;
	i2=i0;

	//Copies des valeurs dans les tableaux résultats
	for(i=0;i<*t2;i++) {
		gic1[i]=gic[i+1][i1];
		gic2[i]=gic[i+1][i2];
		kic1[i]=kic[i+1][i1];
		kic2[i]=kic[i+1][i2];
	}


	freetab(gic);
	freetab(kic);
	freevec(gg);
	freevec(kk);
	freevec(ll);
	freevec(nn);
	if(*h0==1) {
		freevec(gt);
		freevec(kt);
		freevec(lt);
		freevec(nt);
	}
	freevec(x);
	freevec(y);
	freeintvec(type);
	return 0;
}







//fonction intertype locale pour une zone rectangulaire
int intertypelocal_rect(int *point_nb1,double *x1,double *y1,int *point_nb2,double *x2,double *y2,double *xmi,double *xma,
	double *ymi,double *yma,int *t2,double *dt,double *gi,double *ki)
{	int tt,i,j;
	double d,cin;
	double **g, **k;

	//Decalage pour n'avoir que des valeurs positives
	decalRect2(*point_nb1,x1,y1,*point_nb2,x2,y2,xmi,xma,ymi,yma);


	taballoc(&g,*point_nb1,*t2);
	taballoc(&k,*point_nb1,*t2);

	for(i=0;i<*point_nb1;i++)
		for(tt=0;tt<*t2;tt++)
			g[i][tt]=0;

	for(i=0;i<*point_nb1;i++) 	/* On calcule le nombre de couples de points par distance g */
			for(j=0;j<*point_nb2;j++)
			{	d=sqrt((x1[i]-x2[j])*(x1[i]-x2[j])+(y1[i]-y2[j])*(y1[i]-y2[j]));
				if (d<*t2*(*dt)) {
					tt=d/(*dt);

					// correction des effets de bord
					cin=perim_in_rect(x1[i],y1[i],d,*xmi,*xma,*ymi,*yma);
					if (cin<0)	{
						Rprintf("cin<0 sur i AVANT\n");
						return -1;
					}
					g[i][tt]+=2*Pi()/cin;
				}
			}

	for(i=0;i<*point_nb1;i++)
	{	k[i][0]=g[i][0];
		for(tt=1;tt<*t2;tt++)
			k[i][tt]=k[i][tt-1]+g[i][tt];	/* on integre */
	}

	//Copies des valeurs dans les tableaux resultat
	for(i=0;i<*point_nb1;i=i+1) {
		for(tt=0;tt<*t2;tt=tt+1) {
			gi[i*(*t2)+tt]=g[i][tt];
			ki[i*(*t2)+tt]=k[i][tt];
		}
	}



	freetab(g);
	freetab(k);

	return 0;
}


//fonction intertype locale pour une zone circulaire
int intertypelocal_disq(int *point_nb1,double *x1,double *y1,int *point_nb2,double *x2,double *y2,double *x0,double *y0,
	double *r0,int *t2,double *dt,double *gi,double *ki)
{	int tt,i,j;
	double d,cin;
	double **g, **k;

	//Decalage pour n'avoir que des valeurs positives
	decalCirc2(*point_nb1,x1,y1,*point_nb2,x2,y2,x0,y0,*r0);


	taballoc(&g,*point_nb1,*t2);
	taballoc(&k,*point_nb1,*t2);

	for(i=0;i<*point_nb1;i++)
		for(tt=0;tt<*t2;tt++)
			g[i][tt]=0;

	for(i=0;i<*point_nb1;i++) 	/* On calcule le nombre de couples de points par distance g */
			for(j=0;j<*point_nb2;j++)
			{	d=sqrt((x1[i]-x2[j])*(x1[i]-x2[j])+(y1[i]-y2[j])*(y1[i]-y2[j]));
				if (d<*t2*(*dt)) {
					tt=d/(*dt);

					// correction des effets de bord
					cin=perim_in_disq(x1[i],y1[i],d,*x0,*y0,*r0);
					if (cin<0)	{
						Rprintf("cin<0 sur i AVANT\n");
						return -1;
					}
					g[i][tt]+=2*Pi()/cin;
				}
			}

	for(i=0;i<*point_nb1;i++)
	{	k[i][0]=g[i][0];
		for(tt=1;tt<*t2;tt++)
			k[i][tt]=k[i][tt-1]+g[i][tt];	/* on integre */
	}

	//Copies des valeurs dans les tableaux resultat
	for(i=0;i<*point_nb1;i=i+1) {
		for(tt=0;tt<*t2;tt=tt+1) {
			gi[i*(*t2)+tt]=g[i][tt];
			ki[i*(*t2)+tt]=k[i][tt];
		}
	}



	freetab(g);
	freetab(k);

	return 0;
}


//fonction intertype locale pour une zone rectangulaire + triangles
int intertypelocal_tr_rect(int *point_nb1,double *x1,double *y1,int *point_nb2,double *x2,double *y2,double *xmi,double *xma,
double *ymi,double *yma,int *triangle_nb,double *ax,double *ay,double *bx,double *by,double *cx,double *cy,
int *t2,double *dt,double *gi,double *ki)
{	int tt,i,j;
	double d,cin;
	double **g, **k;

	//Decalage pour n'avoir que des valeurs positives
	decalRectTri2(*point_nb1,x1,y1,*point_nb2,x2,y2,xmi,xma,ymi,yma,*triangle_nb,ax,ay,bx,by,cx,cy);


	taballoc(&g,*point_nb1,*t2);
	taballoc(&k,*point_nb1,*t2);

	for(i=0;i<*point_nb1;i++)
		for(tt=0;tt<*t2;tt++)
			g[i][tt]=0;

	for(i=0;i<*point_nb1;i++) 	/* On calcule le nombre de couples de points par distance g */
			for(j=0;j<*point_nb2;j++)
			{	d=sqrt((x1[i]-x2[j])*(x1[i]-x2[j])+(y1[i]-y2[j])*(y1[i]-y2[j]));
				if (d<*t2*(*dt)) {
					tt=d/(*dt);

					// correction des effets de bord
					cin=perim_in_rect(x1[i],y1[i],d,*xmi,*xma,*ymi,*yma);
					if (cin<0)	{
						Rprintf("cin<0 sur i AVANT\n");
						return -1;
					}
					cin=cin-perim_triangle(x1[i],y1[i],d,*triangle_nb,ax,ay,bx,by,cx,cy);
					if (cin<0)	{
						Rprintf("Overlapping triangles\n");
						return -1;
					}
					g[i][tt]+=2*Pi()/cin;
				}
			}

	for(i=0;i<*point_nb1;i++)
	{	k[i][0]=g[i][0];
		for(tt=1;tt<*t2;tt++)
			k[i][tt]=k[i][tt-1]+g[i][tt];	/* on integre */
	}

	//Copies des valeurs dans les tableaux resultat
	for(i=0;i<*point_nb1;i=i+1) {
		for(tt=0;tt<*t2;tt=tt+1) {
			gi[i*(*t2)+tt]=g[i][tt];
			ki[i*(*t2)+tt]=k[i][tt];
		}
	}



	freetab(g);
	freetab(k);

	return 0;
}


//fonction intertype locale pour une zone circulaire + triangles
int intertypelocal_tr_disq(int *point_nb1,double *x1,double *y1,int *point_nb2,double *x2,double *y2,double *x0,double *y0,
double *r0,int *triangle_nb,double *ax,double *ay,double *bx,double *by,double *cx,double *cy,
int *t2,double *dt,double *gi,double *ki)
{	int tt,i,j;
	double d,cin;
	double **g, **k;

	//Decalage pour n'avoir que des valeurs positives
	decalCircTri2(*point_nb1,x1,y1,*point_nb2,x2,y2,x0,y0,*r0,*triangle_nb,ax,ay,bx,by,cx,cy);


	taballoc(&g,*point_nb1,*t2);
	taballoc(&k,*point_nb1,*t2);

	for(i=0;i<*point_nb1;i++)
		for(tt=0;tt<*t2;tt++)
			g[i][tt]=0;

	for(i=0;i<*point_nb1;i++) 	/* On calcule le nombre de couples de points par distance g */
			for(j=0;j<*point_nb2;j++)
			{	d=sqrt((x1[i]-x2[j])*(x1[i]-x2[j])+(y1[i]-y2[j])*(y1[i]-y2[j]));
				if (d<*t2*(*dt)) {
					tt=d/(*dt);

					// correction des effets de bord
					cin=perim_in_disq(x1[i],y1[i],d,*x0,*y0,*r0);
					if (cin<0)	{
						Rprintf("cin<0 sur i AVANT\n");
						return -1;
					}
					cin=cin-perim_triangle(x1[i],y1[i],d,*triangle_nb,ax,ay,bx,by,cx,cy);
					if (cin<0)	{
						Rprintf("Overlapping triangles\n");
						return -1;
					}
					g[i][tt]+=2*Pi()/cin;
				}
			}

	for(i=0;i<*point_nb1;i++)
	{	k[i][0]=g[i][0];
		for(tt=1;tt<*t2;tt++)
			k[i][tt]=k[i][tt-1]+g[i][tt];	/* on integre */
	}

	//Copies des valeurs dans les tableaux resultat
	for(i=0;i<*point_nb1;i=i+1) {
		for(tt=0;tt<*t2;tt=tt+1) {
			gi[i*(*t2)+tt]=g[i][tt];
			ki[i*(*t2)+tt]=k[i][tt];
		}
	}



	freetab(g);
	freetab(k);

	return 0;
}










/******************************************************************************/
/* Cette routine cree un semis poissonnien a la precision p pour x et y,      */
/* dans une forme rectangulaire xmi xma ymi yma, a la precision p,            */
/* et le range dans les tableaux dont les pointeurs sont fournis en parametre */
/******************************************************************************/
void s_alea_rect(int point_nb,double x[], double y[],
					double xmi,double xma, double ymi,double yma,double p)
{	int i;
	double xr,yr;
	xr=xma-xmi;
	yr=yma-ymi;
	GetRNGstate();
	for(i=0;i<point_nb;i=i+1)
	{	x[i]=xmi+(unif_rand()*(xr/p))*p;
		y[i]=ymi+(unif_rand()*(yr/p))*p;
	}
	PutRNGstate();
}

//pour un zone circulaire
void s_alea_disq(int point_nb, double *x, double *y, double x0, double y0, double r0, double p)
{	int i;
	double xx, yy, rr;
	rr=2*r0;
	GetRNGstate();
	i=0;
	while (i<point_nb)
	{	xx=x0-r0+(unif_rand()*(rr/p))*p;
		yy=y0-r0+(unif_rand()*(rr/p))*p;
		if ((xx-x0)*(xx-x0)+(yy-y0)*(yy-y0)<r0*r0)
		{	i=i+1;
			x[i]=xx;
			y[i]=yy;
		}
	}
	PutRNGstate();
}

//pour une zone rectangulaire avec exclusion de triangles
void s_alea_tr_rect(int point_nb,double *x, double *y,double xmi,double xma, double ymi,double yma,int triangle_nb,
double *ax,double *ay,double *bx,double *by,double *cx,double *cy,double p)
{	int i,j,erreur;
	double xr,yr;
	xr=xma-xmi;
	yr=yma-ymi;
	GetRNGstate();

	i=0;
   while (i<point_nb)
	{  // on simule le ieme point dans le rectangle
   	x[i]=xmi+(unif_rand()*(xr/p))*p;
	y[i]=ymi+(unif_rand()*(yr/p))*p;

      // si il n'est dans aucun triangle, on passe au suivant, sinon on recommence
      erreur=0;
		j=0;
		while ((j<triangle_nb)&&(erreur==0))
		{	if (in_triangle(x[i],y[i],ax[j],ay[j],bx[j],by[j],cx[j],cy[j],1))
			{
				erreur=1;
			}
			j=j+1;
		}
		if (erreur==0)
		{	i=i+1;
		}
	}
	PutRNGstate();
}

//pour une zone circulaire avec exclusion de triangles
void s_alea_tr_disq(int point_nb,double *x, double *y,double x0,double y0, double r0,int triangle_nb,
double *ax,double *ay,double *bx,double *by,double *cx,double *cy,double p)
{	int i,j,erreur;
	double rr;
	rr=2*r0;
	GetRNGstate();

	i=0;
	while (i<point_nb) {
		erreur=0;
		// on simule le ieme point dans le cercle
		x[i]=x0-r0+(unif_rand()*(rr/p))*p;
		y[i]=y0-r0+(unif_rand()*(rr/p))*p;
		if ((x[i]-x0)*(x[i]-x0)+(y[i]-y0)*(y[i]-y0)>r0*r0) erreur=1;

		// si il n'est dans aucun triangle, on passe au suivant, sinon on recommence
		j=0;
		while ((j<triangle_nb)&&(erreur==0))
		{	if (in_triangle(x[i],y[i],ax[j],ay[j],bx[j],by[j],cx[j],cy[j],1))
			{
				erreur=1;
			}
			j=j+1;
		}
		if (erreur==0)
		{	i=i+1;
		}
	}
	PutRNGstate();
}





//hypotheses nulles pour intertype :

//1 : etiquetage aleatoire
int randlabelling(double *x, double *y, int point_nb1, double *x1, double *y1,int point_nb2, double *x2, double *y2,int *type) {
	int j,jj;
	int erreur=0;

	GetRNGstate();

	for(j=0;j<point_nb1+point_nb2;j++) {
			type[j]=2;
	}
	// on tire point_nb type 1
	j=0;
	while (j<point_nb1) {
		jj=unif_rand()*(point_nb1+point_nb2);
		while (type[jj]!=2) {
			jj=unif_rand()*(point_nb1+point_nb2);
	   }
	   type[jj]=1;
	   x1[j]=x[jj];
	   y1[j]=y[jj];
	   j=j+1;
	}
	PutRNGstate();
	//Il reste point_nb2 type 2
	jj=0;
	for(j=0;j<point_nb1+point_nb2;j++) {
		if (type[j]==2) {
			x2[jj]=x[j];
			y2[jj]=y[j];
			jj=jj+1;
	   }
	}
	if (jj!=point_nb2) {
		Rprintf("erreur substitution\n");
		erreur=1;
	}
	else {
		erreur=0;
	}

	return erreur;
}

//2 : decallage
int randshifting_rect(int *point_nb,double *x, double *y, int point_nb1, double *x1, double *y1,
double xmi, double xma, double ymi, double yma, double prec) {
	int j;
	int dx,dy;

	GetRNGstate();

	//On decalle type 1
	*point_nb=point_nb1;

	//en x d'abord
	dx=unif_rand()*((xma-xmi)/prec)*prec;
	for(j=0;j<*point_nb;j++) {
		x[j]=x1[j]+dx;
		if (x[j]>xma) {
			x[j]=x[j]-(xma-xmi);
	   }
	}
	//en y ensuite
	dy=unif_rand()*((yma-ymi)/prec)*prec;
	for(j=0;j<*point_nb;j++) {
		y[j]=y1[j]+dy;
		if (y[j]>yma) {
			y[j]=y[j]-(yma-ymi);
		}
	}

	PutRNGstate();

	return 0;
}

int randshifting_disq(int *point_nb,double *x, double *y, int point_nb1, double *x1, double *y1,
double x0, double y0, double r0, double prec) {
	int i;

	randshifting_rect(point_nb,x,y,point_nb1,x1,y1,x0-r0,x0+r0,y0-r0,y0+r0,prec);

	//suppression des points hors cercle
	i=0;
	while (i<*point_nb)
	{	if((x[i]-x0)*(x[i]-x0)+(y[i]-y0)*(y[i]-y0)>r0*r0)
		{	x[i]=x[*point_nb];
			y[i]=y[*point_nb];
			i=i-1;
			*point_nb=*point_nb-1;
		}
		i=i+1;
	}

	return 0;
}

int randshifting_tr_rect(int *point_nb,double *x, double *y, int point_nb1, double *x1, double *y1,
double xmi, double xma, double ymi, double yma,int triangle_nb, double *ax, double *ay, double *bx, double *by,
double *cx, double *cy,double prec) {
	int i,j;
	int erreur;

	randshifting_rect(point_nb,x,y,point_nb1,x1,y1,xmi,xma,ymi,yma,prec);

	//suppression des points dans triangles
	i=0;
	erreur=0;
	while (i<*point_nb)
	{	j=0;
		while ((j<triangle_nb)&&(erreur==0))
		{	if (in_triangle(x[i],y[i],ax[j],ay[j],bx[j],by[j],cx[j],cy[j],1)) erreur=1;
			j=j+1;
		}
		if (erreur == 1)
		{	x[i]=x[*point_nb];
			y[i]=y[*point_nb];
			i=i-1;
			*point_nb=*point_nb-1;
		}
		i=i+1;
		erreur=0;
	}

	return 0;
}

int randshifting_tr_disq(int *point_nb,double *x, double *y, int point_nb1, double *x1, double *y1,
double x0, double y0, double r0,int triangle_nb, double *ax, double *ay, double *bx, double *by,
double *cx, double *cy,double prec) {
	int i,j;
	int erreur;

	randshifting_disq(point_nb,x,y,point_nb1,x1,y1,x0,y0,r0,prec);

	//suppression des points dans triangles
	i=0;
	erreur=0;
	while (i<*point_nb)
	{	j=0;
		while ((j<triangle_nb)&&(erreur==0))
		{	if (in_triangle(x[i],y[i],ax[j],ay[j],bx[j],by[j],cx[j],cy[j],1)) erreur=1;
			j=j+1;
		}
		if (erreur == 1)
		{	x[i]=x[*point_nb];
			y[i]=y[*point_nb];
			i=i-1;
			*point_nb=*point_nb-1;
		}
		i=i+1;
		erreur=0;
	}

	return 0;
}

/******************************************************************************/
/* Calcule la fonction de corrŽlation des marques Km(r) pour un semis (x,y)   */
/* affectŽ d'une marque quantitative c(x,y) en parametres					  */
/* dans une zone de forme rectangulaire de bornes xmi xma ymi yma             */
/* Les corrections des effets de bords sont fait par la methode de Ripley,    */
/* i.e. l'inverse de la proportion d'arc de cercle inclu dans la fenetre.     */
/* Les calculs sont faits pour les t2 premiers intervalles de largeur dt.     */
/* La routine calcule g, densite des couples de points;  et la fonction K     */
/* Les resultats sont stockes dans des tableaux g et k donnes en parametres   */
/******************************************************************************/
int corr_rect(int *point_nb,double *x,double *y,double *c, double *xmi,double *xma,double *ymi,double *yma,
int *t2,double *dt,double *gm,double *km)
{	int i,j,tt;
	double d,cin,cmoy,cvar;
	double *g,*k;

	//Decalage pour n'avoir que des valeurs positives
	decalRect(*point_nb,x,y,xmi,xma,ymi,yma);
	
	//On calcule la moyenne des marques
	cmoy=0;
	for(i=0;i<*point_nb;i++)
		cmoy+=c[i];
	cmoy=cmoy/(*point_nb);
	
	//On calcule la variance des marques
	cvar=0;
	for(i=0;i<*point_nb;i++)
		cvar+=(c[i]-cmoy)*(c[i]-cmoy);
	cvar=cvar/(*point_nb);
	
	vecalloc(&g,*t2);
	vecalloc(&k,*t2);
	
	// On rangera dans g le nombre de couples de points par distance tt
	// et dans gm la somme des covariances des marques 
	for(tt=0;tt<*t2;tt=tt+1) {
		g[tt]=0;
		gm[tt]=0;
	}

    //On regarde les couples (i,j) et (j,i) : donc pour i>j seulement
	for(i=1;i<*point_nb;i=i+1)
	{	for(j=0;j<i;j=j+1)
		{	d=sqrt((x[i]-x[j])*(x[i]-x[j])+(y[i]-y[j])*(y[i]-y[j]));
			if (d<*t2*(*dt)){
				// dans quelle classe de distance est ce couple ?
				tt=d/(*dt);

				///// pour [i,j] :
				// correction des effets de bord
				cin=perim_in_rect(x[i],y[i],d,*xmi,*xma,*ymi,*yma);
				if (cin<0) {
					Rprintf("cin<0 sur i AVANT\n");
					return -1;
				}
				g[tt]+=2*Pi()/cin;
				gm[tt]+=(c[i]-cmoy)*(c[j]-cmoy)*2*Pi()/cin;

				///// pour [j,i] :
				// correction des effets de bord
				cin=perim_in_rect(x[j],y[j],d,*xmi,*xma,*ymi,*yma);
				if (cin<0) {
					Rprintf("cin<0 sur j AVANT\n");
					return -1;
				}
				g[tt]+=2*Pi()/cin;
				gm[tt]+=(c[i]-cmoy)*(c[j]-cmoy)*2*Pi()/cin;
			}
		}
   }


	// on integre
	k[0]=g[0];
	km[0]=gm[0];
  	for(tt=1;tt<*t2;tt=tt+1) {
  		k[tt]=k[tt-1]+g[tt];
		km[tt]=km[tt-1]+gm[tt];
	}

	// on normalise
	for(tt=0;tt<*t2;tt=tt+1) {
		gm[tt]=gm[tt]/(g[tt]*cvar);
		km[tt]=km[tt]/(k[tt]*cvar);
	}
	
	freevec(g);
	freevec(k);
   return 0;
}

//function de corrŽlation dans forme circulaire
int corr_disq(int *point_nb,double *x,double *y,double *c, double *x0,double *y0,double *r0,
int *t2,double *dt,double *gm,double *km)
{	int i,j,tt;
	double d,cin,cmoy,cvar;
	double *g,*k;

	//Decalage pour n'avoir que des valeurs positives
	decalCirc(*point_nb,x,y,x0,y0,*r0);
	
	//On calcule la moyenne des marques
	cmoy=0;
	for(i=0;i<*point_nb;i++)
		cmoy+=c[i];
	cmoy=cmoy/(*point_nb);
	
	//On calcule la variance des marques
	cvar=0;
	for(i=0;i<*point_nb;i++)
		cvar+=(c[i]-cmoy)*(c[i]-cmoy);
	cvar=cvar/(*point_nb);
	
	vecalloc(&g,*t2);
	vecalloc(&k,*t2);
	
	// On rangera dans g le nombre de couples de points par distance tt
	// et dans gm la somme des covariances des marques 
	for(tt=0;tt<*t2;tt=tt+1) {
		g[tt]=0;
		gm[tt]=0;
	}

    //On regarde les couples (i,j) et (j,i) : donc pour i>j seulement
	for(i=1;i<*point_nb;i=i+1)
	{	for(j=0;j<i;j=j+1)
		{	d=sqrt((x[i]-x[j])*(x[i]-x[j])+(y[i]-y[j])*(y[i]-y[j]));
			if (d<*t2*(*dt)){
				// dans quelle classe de distance est ce couple ?
				tt=d/(*dt);

				///// pour [i,j] :
				// correction des effets de bord
				cin=perim_in_disq(x[i],y[i],d,*x0,*y0,*r0);
				if (cin<0) {
					Rprintf("cin<0 sur i AVANT\n");
					return -1;
				}
				g[tt]+=2*Pi()/cin;
				gm[tt]+=(c[i]-cmoy)*(c[j]-cmoy)*2*Pi()/cin;

				///// pour [j,i] :
				// correction des effets de bord
				cin=perim_in_disq(x[j],y[j],d,*x0,*y0,*r0);
				if (cin<0) {
					Rprintf("cin<0 sur j AVANT\n");
					return -1;
				}
				g[tt]+=2*Pi()/cin;
				gm[tt]+=(c[i]-cmoy)*(c[j]-cmoy)*2*Pi()/cin;
			}
		}
   }

	// on integre
	k[0]=g[0];
	km[0]=gm[0];
  	for(tt=1;tt<*t2;tt=tt+1) {
  		k[tt]=k[tt-1]+g[tt];
		km[tt]=km[tt-1]+gm[tt];
	}

	// on normalise
	for(tt=0;tt<*t2;tt=tt+1) {
		gm[tt]=gm[tt]/(g[tt]*cvar);
		km[tt]=km[tt]/(k[tt]*cvar);
	}
	
	freevec(g);
	freevec(k);
   return 0;
}

//Kcor triangles dans rectangle
int corr_tr_rect(int *point_nb,double *x,double *y,double *c, double *xmi,double *xma,double *ymi,double *yma,
int *triangle_nb, double *ax, double *ay, double *bx, double *by, double *cx, double *cy,
int *t2,double *dt,double *gm,double *km)
{	int i,j,tt;
	double d,cin,cmoy,cvar;
	double *g,*k;

	//Decalage pour n'avoir que des valeurs positives
	decalRectTri(*point_nb,x,y,xmi,xma,ymi,yma,*triangle_nb,ax,ay,bx,by,cx,cy);
	
	//On calcule la moyenne des marques
	cmoy=0;
	for(i=0;i<*point_nb;i++)
		cmoy+=c[i];
	cmoy=cmoy/(*point_nb);
	
	//On calcule la variance des marques
	cvar=0;
	for(i=0;i<*point_nb;i++)
		cvar+=(c[i]-cmoy)*(c[i]-cmoy);
	cvar=cvar/(*point_nb);
	
	vecalloc(&g,*t2);
	vecalloc(&k,*t2);
	
	// On rangera dans g le nombre de couples de points par distance tt
	// et dans gm la somme des covariances des marques 
	for(tt=0;tt<*t2;tt=tt+1) {
		g[tt]=0;
		gm[tt]=0;
	}

    //On regarde les couples (i,j) et (j,i) : donc pour i>j seulement
	for(i=1;i<*point_nb;i=i+1)
	{	for(j=0;j<i;j=j+1)
		{	d=sqrt((x[i]-x[j])*(x[i]-x[j])+(y[i]-y[j])*(y[i]-y[j]));
			if (d<*t2*(*dt)){
				// dans quelle classe de distance est ce couple ?
				tt=d/(*dt);

				///// pour [i,j] :
				// correction des effets de bord
				cin=perim_in_rect(x[i],y[i],d,*xmi,*xma,*ymi,*yma);
				if (cin<0) {
					Rprintf("cin<0 sur i AVANT\n");
					return -1;
				}
				cin=cin-perim_triangle(x[i],y[i],d,*triangle_nb,ax,ay,bx,by,cx,cy);
				if (cin<0)	{
					Rprintf("Overlapping triangles\n");
					return -1;
				}
				g[tt]+=2*Pi()/cin;
				gm[tt]+=(c[i]-cmoy)*(c[j]-cmoy)*2*Pi()/cin;

				///// pour [j,i] :
				// correction des effets de bord
				cin=perim_in_rect(x[j],y[j],d,*xmi,*xma,*ymi,*yma);
				if (cin<0) {
					Rprintf("cin<0 sur j AVANT\n");
					return -1;
				}
				cin=cin-perim_triangle(x[j],y[j],d,*triangle_nb,ax,ay,bx,by,cx,cy);
				if (cin<0)	{
					Rprintf("Overlapping triangles\n");
					return -1;
				}
				g[tt]+=2*Pi()/cin;
				gm[tt]+=(c[i]-cmoy)*(c[j]-cmoy)*2*Pi()/cin;
			}
		}
   }


	// on integre
	k[0]=g[0];
	km[0]=gm[0];
  	for(tt=1;tt<*t2;tt=tt+1) {
  		k[tt]=k[tt-1]+g[tt];
		km[tt]=km[tt-1]+gm[tt];
	}

	// on normalise
	for(tt=0;tt<*t2;tt=tt+1) {
		gm[tt]=gm[tt]/(g[tt]*cvar);
		km[tt]=km[tt]/(k[tt]*cvar);
	}
	
	freevec(g);
	freevec(k);
   return 0;
}

//kcor triangles dans disque
int corr_tr_disq(int *point_nb,double *x,double *y,double *c, double *x0,double *y0,double *r0,
int *triangle_nb,double *ax,double *ay,double *bx,double *by,double *cx,double *cy,
int *t2,double *dt,double *gm,double *km)
{	int i,j,tt;
	double d,cin,cmoy,cvar;
	double *g,*k;

	//Decalage pour n'avoir que des valeurs positives
	decalCircTri(*point_nb,x,y,x0,y0,*r0,*triangle_nb,ax,ay,bx,by,cx,cy);
	
	//On calcule la moyenne des marques
	cmoy=0;
	for(i=0;i<*point_nb;i++)
		cmoy+=c[i];
	cmoy=cmoy/(*point_nb);
	
	//On calcule la variance des marques
	cvar=0;
	for(i=0;i<*point_nb;i++)
		cvar+=(c[i]-cmoy)*(c[i]-cmoy);
	cvar=cvar/(*point_nb);
	
	vecalloc(&g,*t2);
	vecalloc(&k,*t2);
	
	// On rangera dans g le nombre de couples de points par distance tt
	// et dans gm la somme des covariances des marques 
	for(tt=0;tt<*t2;tt=tt+1) {
		g[tt]=0;
		gm[tt]=0;
	}

    //On regarde les couples (i,j) et (j,i) : donc pour i>j seulement
	for(i=1;i<*point_nb;i=i+1)
	{	for(j=0;j<i;j=j+1)
		{	d=sqrt((x[i]-x[j])*(x[i]-x[j])+(y[i]-y[j])*(y[i]-y[j]));
			if (d<*t2*(*dt)){
				// dans quelle classe de distance est ce couple ?
				tt=d/(*dt);

				///// pour [i,j] :
				// correction des effets de bord
				cin=perim_in_disq(x[i],y[i],d,*x0,*y0,*r0);
				if (cin<0) {
					Rprintf("cin<0 sur i AVANT\n");
					return -1;
				}
				cin=cin-perim_triangle(x[i],y[i],d,*triangle_nb,ax,ay,bx,by,cx,cy);
				if (cin<0)	{
					Rprintf("Overlapping triangles\n");
					return -1;
				}
				g[tt]+=2*Pi()/cin;
				gm[tt]+=(c[i]-cmoy)*(c[j]-cmoy)*2*Pi()/cin;

				///// pour [j,i] :
				// correction des effets de bord
				cin=perim_in_disq(x[j],y[j],d,*x0,*y0,*r0);
				if (cin<0) {
					Rprintf("cin<0 sur j AVANT\n");
					return -1;
				}
				cin=cin-perim_triangle(x[j],y[j],d,*triangle_nb,ax,ay,bx,by,cx,cy);
				if (cin<0)	{
					Rprintf("Overlapping triangles\n");
					return -1;
				}
				g[tt]+=2*Pi()/cin;
				gm[tt]+=(c[i]-cmoy)*(c[j]-cmoy)*2*Pi()/cin;
			}
		}
   }

	// on integre
	k[0]=g[0];
	km[0]=gm[0];
  	for(tt=1;tt<*t2;tt=tt+1) {
  		k[tt]=k[tt-1]+g[tt];
		km[tt]=km[tt-1]+gm[tt];
	}

	// on normalise
	for(tt=0;tt<*t2;tt=tt+1) {
		gm[tt]=gm[tt]/(g[tt]*cvar);
		km[tt]=km[tt]/(k[tt]*cvar);
	}
	
	freevec(g);
	freevec(k);
   return 0;
}

//Kcor dans rectangle + ic
int corr_rect_ic(int *point_nb,double *x,double *y,double *c, double *xmi,double *xma,double *ymi,double *yma,
int *t2,double *dt,int *nbSimu, double *lev,double *gm,double *km,
double *gmic1,double *gmic2, double *kmic1,double *kmic2, double *gmval, double *kmval) {
	int i,j,i0,i1,i2;
	double *c2;
	double **gmic,**kmic;	
	double *ggm,*kkm;
	
	int erreur=0;

	erreur=corr_rect(point_nb,x,y,c,xmi,xma,ymi,yma,t2,dt,gm,km);
	if (erreur!=0) {
		return -1;
	}

	///Définition de i0 : indice où sera stocké l'estimation des bornes de l'IC
	i0=*lev/2*(*nbSimu+1);
	if (i0<1) i0=1;

	///Initialisation des tableaux dans lesquels on va stocker les valeurs extremes lors de MC
	taballoc(&gmic,*t2+1,2*i0+10+1);
	taballoc(&kmic,*t2+1,2*i0+10+1);

	//Normalisation de g et k et calcul de l et n pour le calcul des p-values
		vecalloc(&ggm,*t2);
		vecalloc(&kkm,*t2);
		for(i=0;i<*t2;i++) {
			ggm[i]=gm[i];
			kkm[i]=km[i];

			gmval[i]=1;
			kmval[i]=1;
	}

	int lp=0;
	vecalloc(&c2,*point_nb);
	// boucle principale de MC
	Rprintf("Monte Carlo simulation\n");
	for(i=1;i<=*nbSimu;i++) {
		randmark(*point_nb,c,c2);
		erreur=corr_rect(point_nb,x,y,c2,xmi,xma,ymi,yma,t2,dt,gmic1,kmic1);
		// si il y a une erreur on recommence une simulation
		if (erreur!=0) {
			i=i-1;
			Rprintf("ERREUR mark correlation\n");
		}
		else {
			//comptage du nombre de |¶obs|<=|¶simu| pour test local
			double gmictmp,kmictmp;
			for(j=0;j<*t2;j++) {
				gmictmp=gmic1[j];
				kmictmp=kmic1[j];
				if ((float)fabs(ggm[j]-1)<=(float)fabs(gmictmp-1)) {gmval[j]+=1;}
				if ((float)fabs(kkm[j])<=(float)fabs(kmictmp)) {kmval[j]+=1;}
			}

			////Traitement des résultats
			ic(i,i0,gmic,kmic,gmic1,kmic1,*t2);
		}
		R_FlushConsole();
 		progress(i,&lp,*nbSimu);
	}

	i1=i0+2;
	i2=i0;

	//Copies des valeurs dans les tableaux résultats
	for(i=0;i<*t2;i++) {
		gmic1[i]=gmic[i+1][i1];
		gmic2[i]=gmic[i+1][i2];
		kmic1[i]=kmic[i+1][i1];
		kmic2[i]=kmic[i+1][i2];
	}


	freetab(gmic);
	freetab(kmic);
	freevec(ggm);
	freevec(kkm);
	freevec(c2);
	return 0;
}

//Kcor dans disque + ic
int corr_disq_ic(int *point_nb,double *x,double *y,double *c, double *x0,double *y0,double *r0,
int *t2,double *dt,int *nbSimu, double *lev,double *gm,double *km,
double *gmic1,double *gmic2, double *kmic1,double *kmic2, double *gmval, double *kmval) {
	int i,j,i0,i1,i2;
	double *c2;
	double **gmic,**kmic;	
	double *ggm,*kkm;
	
	int erreur=0;
	
	erreur=corr_disq(point_nb,x,y,c,x0,y0,r0,t2,dt,gm,km);
	if (erreur!=0) {
		return -1;
	}

	///Définition de i0 : indice où sera stocké l'estimation des bornes de l'IC
	i0=*lev/2*(*nbSimu+1);
	if (i0<1) i0=1;

	///Initialisation des tableaux dans lesquels on va stocker les valeurs extremes lors de MC
	taballoc(&gmic,*t2+1,2*i0+10+1);
	taballoc(&kmic,*t2+1,2*i0+10+1);

	//Normalisation de g et k et calcul de l et n pour le calcul des p-values
		vecalloc(&ggm,*t2);
		vecalloc(&kkm,*t2);
		for(i=0;i<*t2;i++) {
			ggm[i]=gm[i];
			kkm[i]=km[i];

			gmval[i]=1;
			kmval[i]=1;
	}

	int lp=0;
	vecalloc(&c2,*point_nb);
	// boucle principale de MC
	Rprintf("Monte Carlo simulation\n");
	for(i=1;i<=*nbSimu;i++) {
		randmark(*point_nb,c,c2);
		erreur=corr_disq(point_nb,x,y,c2,x0,y0,r0,t2,dt,gmic1,kmic1);
			// si il y a une erreur on recommence une simulation
		if (erreur!=0) {
			i=i-1;
			Rprintf("ERREUR mark correlation\n");
		}
		else {
			//comptage du nombre de |¶obs|<=|¶simu| pour test local
			double gmictmp,kmictmp;
			for(j=0;j<*t2;j++) {
				gmictmp=gmic1[j];
				kmictmp=kmic1[j];
				if ((float)fabs(ggm[j]-1)<=(float)fabs(gmictmp-1)) {gmval[j]+=1;}
				if ((float)fabs(kkm[j])<=(float)fabs(kmictmp)) {kmval[j]+=1;}
			}

			////Traitement des résultats
			ic(i,i0,gmic,kmic,gmic1,kmic1,*t2);
		}
		R_FlushConsole();
 		progress(i,&lp,*nbSimu);
	}

	i1=i0+2;
	i2=i0;

	//Copies des valeurs dans les tableaux résultats
	for(i=0;i<*t2;i++) {
		gmic1[i]=gmic[i+1][i1];
		gmic2[i]=gmic[i+1][i2];
		kmic1[i]=kmic[i+1][i1];
		kmic2[i]=kmic[i+1][i2];
	}


	freetab(gmic);
	freetab(kmic);
	freevec(ggm);
	freevec(kkm);
	freevec(c2);
	return 0;
}


//Kcor triangles dans rectangle + ic
int corr_tr_rect_ic(int *point_nb,double *x,double *y,double *c, double *xmi,double *xma,double *ymi,double *yma,
int *triangle_nb, double *ax, double *ay, double *bx, double *by, double *cx, double *cy,
int *t2,double *dt,int *nbSimu, double *lev,double *gm,double *km,
double *gmic1,double *gmic2, double *kmic1,double *kmic2, double *gmval, double *kmval) {
	int i,j,i0,i1,i2;
	double *c2;
	double **gmic,**kmic;	
	double *ggm,*kkm;
	
	int erreur=0;

	erreur=corr_tr_rect(point_nb,x,y,c,xmi,xma,ymi,yma,triangle_nb,ax,ay,bx,by,cx,cy,t2,dt,gm,km);
	if (erreur!=0) {
		return -1;
	}

	///Définition de i0 : indice où sera stocké l'estimation des bornes de l'IC
	i0=*lev/2*(*nbSimu+1);
	if (i0<1) i0=1;

	///Initialisation des tableaux dans lesquels on va stocker les valeurs extremes lors de MC
	taballoc(&gmic,*t2+1,2*i0+10+1);
	taballoc(&kmic,*t2+1,2*i0+10+1);

	//Normalisation de g et k et calcul de l et n pour le calcul des p-values
		vecalloc(&ggm,*t2);
		vecalloc(&kkm,*t2);
		for(i=0;i<*t2;i++) {
			ggm[i]=gm[i];
			kkm[i]=km[i];

			gmval[i]=1;
			kmval[i]=1;
	}

	int lp=0;
	vecalloc(&c2,*point_nb);
	// boucle principale de MC
	Rprintf("Monte Carlo simulation\n");
	for(i=1;i<=*nbSimu;i++) {
		randmark(*point_nb,c,c2);
		erreur=corr_tr_rect(point_nb,x,y,c2,xmi,xma,ymi,yma,triangle_nb,ax,ay,bx,by,cx,cy,t2,dt,gmic1,kmic1);
		// si il y a une erreur on recommence une simulation
		if (erreur!=0) {
			i=i-1;
			Rprintf("ERREUR Intertype\n");
		}
		else {
			//comptage du nombre de |¶obs|<=|¶simu| pour test local
			double gmictmp,kmictmp;
			for(j=0;j<*t2;j++) {
				gmictmp=gmic1[j];
				kmictmp=kmic1[j];
				if ((float)fabs(ggm[j]-1)<=(float)fabs(gmictmp-1)) {gmval[j]+=1;}
				if ((float)fabs(kkm[j])<=(float)fabs(kmictmp)) {kmval[j]+=1;}
			}

			////Traitement des résultats
			ic(i,i0,gmic,kmic,gmic1,kmic1,*t2);
		}
		R_FlushConsole();
 		progress(i,&lp,*nbSimu);
	}

	i1=i0+2;
	i2=i0;

	//Copies des valeurs dans les tableaux résultats
	for(i=0;i<*t2;i++) {
		gmic1[i]=gmic[i+1][i1];
		gmic2[i]=gmic[i+1][i2];
		kmic1[i]=kmic[i+1][i1];
		kmic2[i]=kmic[i+1][i2];
	}


	freetab(gmic);
	freetab(kmic);
	freevec(ggm);
	freevec(kkm);
	freevec(c2);
	return 0;
}

//Kcor triangles dans disque + ic
int corr_tr_disq_ic(int *point_nb,double *x,double *y,double *c, double *x0,double *y0,double *r0,
int *triangle_nb, double *ax, double *ay, double *bx, double *by, double *cx, double *cy,
int *t2,double *dt,int *nbSimu, double *lev,double *gm,double *km,
double *gmic1,double *gmic2, double *kmic1,double *kmic2, double *gmval, double *kmval) {
	int i,j,i0,i1,i2;
	double *c2;
	double **gmic,**kmic;	
	double *ggm,*kkm;
	
	int erreur=0;

	erreur=corr_tr_disq(point_nb,x,y,c,x0,y0,r0,triangle_nb,ax,ay,bx,by,cx,cy,t2,dt,gm,km);
	if (erreur!=0) {
		return -1;
	}

	///Définition de i0 : indice où sera stocké l'estimation des bornes de l'IC
	i0=*lev/2*(*nbSimu+1);
	if (i0<1) i0=1;

	///Initialisation des tableaux dans lesquels on va stocker les valeurs extremes lors de MC
	taballoc(&gmic,*t2+1,2*i0+10+1);
	taballoc(&kmic,*t2+1,2*i0+10+1);

	//Calcul de gm et km pour le calcul des p-values
		vecalloc(&ggm,*t2);
		vecalloc(&kkm,*t2);
		for(i=0;i<*t2;i++) {
			ggm[i]=gm[i];
			kkm[i]=km[i];

			gmval[i]=1;
			kmval[i]=1;
	}

	int lp=0;
	vecalloc(&c2,*point_nb);
	// boucle principale de MC
	Rprintf("Monte Carlo simulation\n");
	for(i=1;i<=*nbSimu;i++) {
		randmark(*point_nb,c,c2);
		erreur=corr_tr_disq(point_nb,x,y,c2,x0,y0,r0,triangle_nb,ax,ay,bx,by,cx,cy,t2,dt,gmic1,kmic1);
		// si il y a une erreur on recommence une simulation
		if (erreur!=0) {
			i=i-1;
			Rprintf("ERREUR Intertype\n");
		}
		else {
			//comptage du nombre de |¶obs|<=|¶simu| pour test local
			double gmictmp,kmictmp;
			for(j=0;j<*t2;j++) {
				gmictmp=gmic1[j];
				kmictmp=kmic1[j];
				if ((float)fabs(ggm[j]-1)<=(float)fabs(gmictmp-1)) {gmval[j]+=1;}
				if ((float)fabs(kkm[j])<=(float)fabs(kmictmp)) {kmval[j]+=1;}
			}

			////Traitement des résultats
			ic(i,i0,gmic,kmic,gmic1,kmic1,*t2);
		}
		R_FlushConsole();
 		progress(i,&lp,*nbSimu);
	}

	i1=i0+2;
	i2=i0;

	//Copies des valeurs dans les tableaux résultats
	for(i=0;i<*t2;i++) {
		gmic1[i]=gmic[i+1][i1];
		gmic2[i]=gmic[i+1][i2];
		kmic1[i]=kmic[i+1][i1];
		kmic2[i]=kmic[i+1][i2];
	}


	freetab(gmic);
	freetab(kmic);
	freevec(ggm);
	freevec(kkm);
	freevec(c2);
	return 0;
}

//mark permutations
void randmark(int point_nb,double *c,double *c2)
{   int j,jj;

	for(j=0;j<point_nb;j++)
		c2[j]=-1;
	j=0;
	GetRNGstate();
	while (j<point_nb) {
		jj=unif_rand()*(point_nb);
		while (c2[jj]>-1) {	
			jj=unif_rand()*(point_nb);
		}
		c2[jj]=c[j];
		j=j+1;
	}
	PutRNGstate();
}
