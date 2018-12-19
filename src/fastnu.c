/* The fast trigonometric real approximation described in
 * "Spectral Analysis Methods of Nonuniformly Sampled Time Series"
 * by Adolf Mathias et al.; <dolfi@zkm.de> 2003
 * Compile e.g. with
 * gcc -O3 -funroll-loops -Wall -Wno-parentheses -lm fastnu.c -o fastnu
 * sudo R CMD INSTALL /home/dolf/result/stat_soft_03/nuspectral
 * R CMD check /home/dolf/result/stat_soft_03/nuspectral
 *
   gcc -I/usr/local/lib/R/include -I/usr/local/include -D__NO_MATH_INLINES -mieee-fp -Wall -fPIC  -g -c fastnu.c -o fastnu.o; gcc -shared -L/usr/local/lib -o nuspectral.so fastnu.o; sudo R CMD INSTALL /home/dolf/result/stat_soft_03/nuspectral
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <complex.h>

#define MAXCOLUMN 8

/* We use the ISO C99 complex facility as implemented by GCC, but only in two places */

typedef _Complex double Complex;
typedef double Real;

#define RE(x_)                  (__real__ x_)
#define IM(x_)                  (__imag__ x_)
#define CSET(x_, r_, i_)        (RE(x_) = (r_), IM(x_) = (i_))
#define PHISET(x_, p_)          CSET(x_, cos(tmp=p_), sin(tmp))
#define SCALEPHISET(x_, f_, p_) CSET(x_, f_ cos(tmp=p_), f_ sin(tmp))

typedef struct Data
{   double x[MAXCOLUMN]; 
    struct Data *next;
} Data;

typedef struct
{   Real x, t;
} XTElem;

/* Added static keyword to compile properly MJG 03/14/18 */
static inline double sqr(double x) 
{   return x*x;   }

/* PNUM has to match the definition of EXP_IOT_SERIES! */
#define PNUM 12
#define SETXT(p_, op_, x_, t_) (p_)->x op_ x_; (p_++)->t op_ t_;
#define SETT(p_, op_, x_, t_)  *p_++ op_ t_;
#define SETX(p_, op_, x_, t_) *p_++ op_ x_;
/* h is a complex aux. variable; it is used for assignment times I everywhere */
#define SETIX(p_, op_, x_, t_) h = x_; RE(*(p_)) op_ -IM(h); IM(*(p_)) op_ RE(h); p_++;

        /* Macro that sums up the power series terms into the power series
	 * element record pointed to by p_.
	 * By using = and += for o_, initial setting and accumulation can be selected.
	 * t_ is the expression specifying the abscissa value. set_ can be either
	 * SETXT to set the x and t fields of an XTElem record, or SETT/SETX to set
	 * the elements of a Real array representing alternately real and imaginary
	 * values.
         */
#define EXP_IOT_SERIES(p_, el_, t_, op_, setr_, seti_)		\
{       Real t = t_, tt; p_ = el_;    setr_(p_, op_, x, 1)	\
        tt  = -t;            seti_(p_, op_, x*tt, tt)		\
        tt *= t*(1.0/2.0);   setr_(p_, op_, x*tt, tt)		\
        tt *= t*(-1.0/3.0);  seti_(p_, op_, x*tt, tt)		\
        tt *= t*(1.0/4.0);   setr_(p_, op_, x*tt, tt)		\
        tt *= t*(-1.0/5.0);  seti_(p_, op_, x*tt, tt)		\
        tt *= t*(1.0/6.0);   setr_(p_, op_, x*tt, tt)		\
        tt *= t*(-1.0/7.0);  seti_(p_, op_, x*tt, tt)		\
        tt *= t*(1.0/8.0);   setr_(p_, op_, x*tt, tt)		\
        tt *= t*(-1.0/9.0);  seti_(p_, op_, x*tt, tt)		\
        tt *= t*(1.0/10.0);  setr_(p_, op_, x*tt, tt)		\
        tt *= t*(-1.0/11.0); seti_(p_, op_, x*tt, tt)		\
}

/* same as the above, but without alternating signs */
#define EXPIOT_SERIES(p_, el_, t_, op_, setr_, seti_)		\
{       Real t = t_, tt; p_ = el_;    setr_(p_, op_, x, 1)	\
			     seti_(p_, op_, x*t,  t )		\
        tt  = t*t*(1.0/2.0); setr_(p_, op_, x*tt, tt)		\
        tt *= t*(1.0/3.0);   seti_(p_, op_, x*tt, tt)		\
        tt *= t*(1.0/4.0);   setr_(p_, op_, x*tt, tt)		\
        tt *= t*(1.0/5.0);   seti_(p_, op_, x*tt, tt)		\
        tt *= t*(1.0/6.0);   setr_(p_, op_, x*tt, tt)		\
        tt *= t*(1.0/7.0);   seti_(p_, op_, x*tt, tt)		\
        tt *= t*(1.0/8.0);   setr_(p_, op_, x*tt, tt)		\
        tt *= t*(1.0/9.0);   seti_(p_, op_, x*tt, tt)		\
        tt *= t*(1.0/10.0);  setr_(p_, op_, x*tt, tt)		\
        tt *= t*(1.0/11.0);  seti_(p_, op_, x*tt, tt)		\
}

#ifdef _STANDALONE_
#   define SRCARG   Data *in, int ct, int cx, int n, double length
#   define SRCVAR   Data *dp;
#   define SRCT     dp->x[ct]
#   define SRCX     dp->x[cx]
#   define SRCFIRST dp = in
#   define SRCAVAIL (dp)
#   define SRCNEXT  dp=dp->next
#else
#   define SRCARG   Real *tptr, Real *xptr, int n, double *lengthptr
#   define SRCVAR   int k; Real length = *lengthptr;
#   define SRCT     tptr[k]
#   define SRCX     xptr[k]
#   define SRCFIRST k = 0
#   define SRCAVAIL (k<n)
#   define SRCNEXT  k++
#endif

/* Fast nonuniform real trigonometric approximation for logarithmic spectral range.
 * The sections are labelled correspondingly to the paper.
 * Parameters:
 * length  : abscissa extent of input; we don't assume the data to be ordered in t!
 * ct	   : Index of abscissa component within the Data record
 * cx	   : Index of ordinate component within the Data record
 * n	   : Number of input data records

 * rp	   : Result array, enough storage for ncoeff*noctave complex numbers is required
 * omegamax: The highest frequency to be computed
 * ncoeff  : Number of spectral coefficients per octave to be computed
 * ncoeff  : Number of octaves to be computed
 */
#ifdef _STANDALONE_
void fastnureal(Data *in, int ct, int cx, int n, double length, int ncoeff, int noctave, Real omegamax, Complex *rp)
{   Data *dp;
#else
void fastnureal(Real *tptr, Real *xptr, int *nptr, double *lengthptr, int *ncoeffptr, int *noctaveptr, Real *omegamaxptr, Complex *rp)
{
    int  k, n = *nptr, ncoeff = *ncoeffptr, noctave = *noctaveptr;
    Real length = *lengthptr, omegamax = *omegamaxptr;
#endif

    struct SumVec			/* Precomputation record */
    {   struct SumVec *next;		/* A singly linked list */
	XTElem elems[PNUM];		/* the summed power series elements */
        int cnt;			/* number of samples for which the power series elements were added */
    }
             *shead, *stail, *sp, *sq;  /* power series element lists */
    Complex  zeta, iota, zz, ii,        /* Accumulators for spectral coefficients */
             e, emul,
             e2, e2mul;		        /* summation factor exp(-i o tau_h) */
    Real     dtelems[PNUM],		/* power series elements of exp(-i dtau)  */
	     *dtp, *r,			/* Pointers into dtelems */
             x,				/* abscissa and ordinate value, p-th power of t */
             tau, tau0, te,		/* Precomputation range centers and range end */
             tau0d,	                /* tau_h of first summand range at level d */
             dtau = (0.5*M_PI)/omegamax,/* initial precomputation interval radius */
             dtaud,			/* precomputation interval radius at d'th merging step */
             n_1 = 1.0/n,		/* reciprocal of sample count */
             ooct, o, o2, omul,	        /* omega/mu for octave's top omega and per band, mult. factor  */
             omegaoct, omega,		/* Max. frequency of octave and current frequency */
             on_1, o2n_1,		/* n_1*(omega/mu)^p, n_1*(2*omega/mu)^p */
             mu = (0.5*M_PI)/length,  	/* Frequency shift: a quarter period of exp(i mu t) on length */
	     tmp;
    XTElem   eoelems[PNUM],             /* Alternately addition and subtraction of power series elements */
					/* of adjacent precomputation ranges */
	     oeelems[PNUM],		/* and vice versa */
	     *eop, *oep,		/* Pointers into eoelems and oeelems */
	     *ep,			/* Pointer into the current choice of eoelems and oeelems */
	     *p, *q, *pe;	        /* Pointers into power series elements of adjacent precomputation ranges */
    int      i, j;			/* Coefficient and octave counter */

    /* Subdivision and Precomputation */
    SRCFIRST;
    tau = SRCT+dtau; te = tau+dtau;
    tau0 = tau;
    shead = stail = sp = alloca(sizeof(*sp)); sp->next = 0;
    for(te = SRCT+2*dtau; ; )
    {   x = SRCX;
        EXP_IOT_SERIES(p, sp->elems, mu*(SRCT-tau), =, SETXT, SETXT); sp->cnt = 1;
	for(SRCNEXT; SRCAVAIL && SRCT<te; SRCNEXT)
        {   x = SRCX; 
            EXP_IOT_SERIES(p, sp->elems, mu*(SRCT-tau), +=, SETXT, SETXT); sp->cnt++;
        }
        if(!SRCAVAIL) break;
        tau = te+dtau; te = tau+dtau;
        sp = alloca(sizeof(*sp)); stail->next = sp; stail = sp; sp->next = 0; sp->cnt = 0;
    }

    ooct = omegamax/mu;
    omul = exp(-M_LN2/ncoeff);
    omegaoct = omegamax;
    tau0d = tau0;
    dtaud = dtau;
    /*** Loop over Octaves ***/
    for(j = noctave, tau0d = tau0; ; ooct *= 0.5, omegaoct *= 0.5, tau0d += dtaud, dtaud *= 2)
    {   /*** Results per frequency ***/
        for(i = ncoeff, o = ooct, omega = omegaoct; i--; o *= omul, omega *= omul)
        {   PHISET(e, -omega*tau0d); e2 = e*e;
            PHISET(emul, -2*omega*dtaud); e2mul = emul*emul;
            for(zeta = iota = 0, sp = shead; sp; sp = sp->next, e *= emul, e2 *= e2mul)
                if(sp->cnt)
                {   for(zz = ii = 0, p = sp->elems, pe = p+PNUM, on_1 = o2n_1 = n_1, o2 = 2*o; p <pe; )
                    {   RE(zz) += p->x * on_1; RE(ii) += p++->t * o2n_1; on_1 *= o; o2n_1 *= o2;
                        IM(zz) += p->x * on_1; IM(ii) += p++->t * o2n_1; on_1 *= o; o2n_1 *= o2;
                    }
                    zeta += e * zz; iota += e2 * ii;
                }
            *rp++ = 2/(1-sqr(RE(iota))-sqr(IM(iota)))*(conj(zeta)-conj(iota)*zeta);
        }
	if(--j<=0) break;		    /* avoid unnecessary merging at the end */
        /* Merging of the s_h;
         * 4 different possibilities, depending on which of the merged ranges actually contain data.
         * The computation is described in the paper; The result of a merger is stored in the
	 * left precomputation record (sp). Before one power series element is stored, the 
	 * sum and difference of the original values *p and *q are stored in eoelems and oeelems,
	 * respectively. The result is then stored in *p.
         */
	EXP_IOT_SERIES(dtp, dtelems, mu*dtaud, =, SETT, SETT);
        for(sp = shead; sp; sp = sp->next)
        {   if(!(sq = sp->next) || !sq->cnt)
            {   if(sp->cnt)
		{   for(p = sp->elems, eop = eoelems, oep = oeelems, dtp = dtelems, pe = p+PNUM; ; )
                    {   eop->x = p->x; eop++->t = p->t; oep->x =-p->x;  oep++->t =-p->t;
                        for(ep = eoelems, r = dtp++, p->x = ep->x * *r, p->t = ep->t * *r; ; )
                        {   if(--r<dtelems) break; ++ep; p->x -= ep->x * *r; p->t -= ep->t * *r;
		    	    if(--r<dtelems) break; ++ep; p->x += ep->x * *r; p->t += ep->t * *r;
		        }
                        if(++p>=pe) break;
                        eop->x =-p->x; eop++->t =-p->t; oep->x = p->x;  oep++->t = p->t;
                        for(ep = oeelems, r = dtp++, p->x = ep->x * *r, p->t = ep->t * *r; r > dtelems; )
                        {   ++ep; --r; p->x += ep->x * *r; p->t += ep->t * *r;   }
                        if(++p>=pe) break;
                    }
		}
		if(!sq) break;
	    }
            else
                if(sp->cnt)
                    for(p = sp->elems, q = sq->elems, eop = eoelems, oep = oeelems, dtp = dtelems, pe = p+PNUM; ; )
                    {   eop->x = p->x+q->x; eop++->t = p->t+q->t; oep->x = q->x-p->x; oep++->t = q->t-p->t;
                        for(ep = eoelems, r = dtp++, p->x = ep->x * *r, p->t = ep->t * *r; ; )
                        {   if(--r<dtelems) break; ++ep; p->x -= ep->x * *r; p->t -= ep->t * *r;
			    if(--r<dtelems) break; ++ep; p->x += ep->x * *r; p->t += ep->t * *r;
			}
                        if(++p<pe) q++; else break;
			eop->x = q->x-p->x; eop++->t = q->t-p->t; oep->x = p->x+q->x; oep++->t = p->t+q->t;
                        for(ep = oeelems, r = dtp++, p->x = ep->x * *r, p->t = ep->t * *r; r > dtelems; )
                        {   ++ep; --r; p->x += ep->x * *r; p->t += ep->t * *r;   }
                        if(++p<pe) q++; else break;
                    }
                else
                    for(q = sq->elems, eop = eoelems, dtp = dtelems, pe = q+PNUM; ; )
                    {   eop->x = q->x; eop++->t = q->t;
                        for(ep = eoelems, r = dtp++, q->x = ep->x * *r, q->t = ep->t * *r; ; )
                        {   if(--r<dtelems) break; ++ep; q->x += ep->x * *r; q->t += ep->t * *r;
			    if(--r<dtelems) break; ++ep; q->x -= ep->x * *r; q->t -= ep->t * *r;
			}
			if(++q>=pe) break;
                        eop->x = q->x; eop++->t = q->t;
                        for(ep = eoelems, r = dtp++, q->x = ep->x * *r, q->t = ep->t * *r; r > dtelems; )
                        {   ++ep; --r; q->x += ep->x * *r; q->t += ep->t * *r;   }
			if(++q>=pe) break;
                    }
	    sp->cnt += sq->cnt; sp->next = sq->next; /* free(sq) if malloc'ed */
        }
    }
}

#ifdef _STANDALONE_
void fastnucomplex()
{
#else
void fastnucomplex(Real *tptr, Complex *xptr, int *nptr, double *lengthptr, int *ncoeffptr, int *noctaveptr, Real *omegamaxptr, Complex *rp)
{
    int  k, n = *nptr, ncoeff = *ncoeffptr, noctave = *noctaveptr;
    Real length = *lengthptr, omegamax = *omegamaxptr;
#endif

    struct SumVec			/* Precomputation record */
    {   struct SumVec *next;		/* A singly linked list */
	Complex elems[PNUM];		/* the summed power series elements */
        int cnt;			/* number of samples for which the power series elements were added */
    }
             *shead, *stail, *sp, *sq;  /* power series element lists */
    Real     dtelems[PNUM],		/* power series elements of exp(-i dtau)  */
	     *dte, *r,			/* Pointers into dtelems */
             x,				/* abscissa and ordinate value, p-th power of t */
             tau, tau0, te,		/* Precomputation range centers and range end */
             tau0d,	                /* tau_h of first summand range at level d */
             dtau = (0.5*M_PI)/omegamax,/* initial precomputation interval radius */
             dtaud,			/* precomputation interval radius at d'th merging step */
             n_1 = 1.0/n,		/* reciprocal of sample count */
             ooct, o, omul,	        /* omega/mu for octave's top omega and per band, mult. factor  */
             omegaoct, omega,		/* Max. frequency of octave and current frequency */
             on_1,			/* n_1*(omega/mu)^p, n_1*(2*omega/mu)^p */
             mu = (0.5*M_PI)/length,  	/* Frequency shift: a quarter period of exp(i mu t) on length */
	     tmp;
    Complex  zeta, zz,			/* Accumulators for spectral coefficients */
             e, emul,		        /* summation factor exp(-i o tau_h) */
	     h, eoelems[PNUM], oeelems[PNUM],
	     *eop, *oep,
	     *ep, *op,			/* Pointer into the current choice of eoelems and oeelems */
	     *p, *q, *pe;
    int      i, j;			/* Coefficient and octave counter */

    /* Subdivision and Precomputation */
    SRCFIRST;
    tau = SRCT+dtau; te = tau+dtau;
    tau0 = tau;
    shead = stail = sp = alloca(sizeof(*sp)); sp->next = 0;
    for(te = SRCT+2*dtau; ; )
    {   x = SRCX;
        EXP_IOT_SERIES(p, sp->elems, mu*(SRCT-tau), =, SETX, SETIX); sp->cnt = 1;
	for(SRCNEXT; SRCAVAIL && SRCT<te; SRCNEXT)
        {   x = SRCX; 
            EXP_IOT_SERIES(p, sp->elems, mu*(SRCT-tau), +=, SETX, SETIX); sp->cnt++;
        }
        if(!SRCAVAIL) break;
        tau = te+dtau; te = tau+dtau;
        sp = alloca(sizeof(*sp)); stail->next = sp; stail = sp; sp->next = 0; sp->cnt = 0;
    }

    ooct = omegamax/mu;
    omul = exp(-M_LN2/ncoeff);
    omegaoct = omegamax;
    tau0d = tau0;
    dtaud = dtau;
    /*** Loop over Octaves ***/
    for(j = noctave; ; ooct *= 0.5, omegaoct *= 0.5, tau0d += dtaud, dtaud *= 2)
    {   /*** Results per frequency ***/
        for(i = ncoeff, o = ooct, omega = omegaoct; i--; o *= omul, omega *= omul)
        {   PHISET(e, -omega*tau0d);
            PHISET(emul, -2*omega*dtaud);
            for(zeta = 0, sp = shead; sp; sp = sp->next, e *= emul)
                if(sp->cnt)
                {   for(zz = 0, p = sp->elems, pe = p+PNUM, on_1 = n_1; p < pe; )
                    {   zz += *p++ * on_1; on_1 *= o;   }
                    zeta += e * zz;
                }
	    *rp++ = zeta;
        }
	if(--j<=0) break;		    /* avoid unnecessary merging at the end */
        /* Merging of the s_h;
         * 4 different possibilities, depending on which of the merged ranges actually contain data.
         * The computation is described in the paper; The result of a merger is stored in the
	 * left precomputation record (sp). Before one power series element is stored, the 
	 * sum and difference of the original values *p and *q are stored in eoelems and oeelems,
	 * respectively. The result is then stored in *p.
         */
	EXPIOT_SERIES(r, dtelems, mu*dtaud, =, SETT, SETT);
        for(sp = shead; sp; sp = sp->next)
        {   if(!(sq = sp->next) || !sq->cnt)
            {   if(sp->cnt)
		    for(p = sp->elems, eop = eoelems, dte = dtelems+1, pe = p+PNUM; p < pe; p++, dte++)
                    {   ep = eop; *eop++ = *p;
			for(r = dtelems, *p = *ep * *r; ; )
			{   if(++r>=dte) break; --ep; h   =  *ep * *r; RE(*p) -= IM(h); IM(*p) += RE(h);
			    if(++r>=dte) break; --ep; *p -=  *ep * *r;
			    if(++r>=dte) break; --ep; h   = -*ep * *r; RE(*p) -= IM(h); IM(*p) += RE(h);
			    if(++r>=dte) break; --ep; *p +=  *ep * *r;
			}
		    }
		if(!sq) break;		    /* reached the last precomputation range */
	    }
            else
                if(sp->cnt)
                    for(p = sp->elems, q = sq->elems, eop = eoelems, oep = oeelems, dte = dtelems+1, pe = p+PNUM;
			p < pe; p++, q++, dte++)
                    {   ep = eop; *eop++ = *p+*q; *oep++ = *p-*q; op = oep;
			for(r = dtelems, *p = *ep * *r; ; )
			{   if(++r>=dte) break; op -= 2; h   =  *op * *r; RE(*p) -= IM(h); IM(*p) += RE(h);
			    if(++r>=dte) break; ep -= 2; *p -=  *ep * *r;
			    if(++r>=dte) break; op -= 2; h   = -*op * *r; RE(*p) -= IM(h); IM(*p) += RE(h);
			    if(++r>=dte) break; ep -= 2; *p +=  *ep * *r;
			}
		    }
		else
		for(q = sq->elems, eop = eoelems, oep = oeelems, dte = dtelems+1, pe = q+PNUM; q<pe; q++, dte++)
                {   ep = eop; *eop++ = *q;
		    for(r = dtelems, *q = *ep * *r; ; )
		    {   if(++r>=dte) break; --ep; h   =  *ep * *r; RE(*q) -= IM(h); IM(*q) += RE(h);
			if(++r>=dte) break; --ep; *p -=  *ep * *r;
			if(++r>=dte) break; --ep; h   = -*ep * *r; RE(*q) -= IM(h); IM(*q) += RE(h);
			if(++r>=dte) break; --ep; *q +=  *ep * *r;
		    }
		}
                    
	    sp->cnt += sq->cnt; sp->next = sq->next; /* free(sq) if malloc'ed */
        }
    }
}

void nucomplex(Real *tptr, Complex *xptr, int *nptr, int *ncoeffptr, int *noctaveptr, Real *omegamaxptr, Complex *rp)
{   int     k, n = *nptr, ncoeff = *ncoeffptr, noctave = *noctaveptr;
    Real    omegamax = *omegamaxptr, o, omul = exp(-M_LN2/ncoeff), *tp, *te, n_1 = 1.0/n, ot;
    Complex *xp, zeta, e_ot;

    for(o = omegamax, k = ncoeff*noctave; k--; o *= omul)
    {   for(zeta = 0, tp = tptr, te = tp+n, xp = xptr; tp < te; )
	{   ot = o * *tp++; RE(e_ot) = cos(ot); IM(e_ot) = -sin(ot);
	    zeta += e_ot* *xp++;
	}
	*rp++ = zeta*n_1;
    }
}


#ifdef _STANDALONE_
void nurealwavelet()
{
#else
void nurealwavelet(Real *tptr, Real *xptr, int *nptr, double *lengthptr, int *ncoeffptr, int *noctaveptr,
		       double *tminptr, double *tmaxptr, int *tsubdivptr, Real *sigmaptr, Real *omegamaxptr, Complex *result)
{
#endif
    int     j, k, /* n = *nptr, */ ncoeff = *ncoeffptr, noctave = *noctaveptr, tsubdiv = *tsubdivptr, cnt;
    Real    omegamax = *omegamaxptr,
	    tmin = *tminptr, tmax = *tmaxptr, deltat = (tmax-tmin)/(*tsubdivptr-1), sigma = *sigmaptr,
	    o, omul = exp(-M_LN2/ncoeff), omul_1 = 1.0/omul, ot,
	    winrad = M_PI/(sigma*omegamax),			     /* abscissa dist. from Hanning window center to its borders */
	    *xp, *tp, *xq, *tq, dt, t, iota0, sot, w;
    Complex *rp = result, iota, zeta;

    for(o = omegamax, k = ncoeff*noctave; k--; o *= omul, winrad *= omul_1)
	for(tp = tptr, xp = xptr, t = tmin, j = tsubdiv; j--; t += deltat)
	{   for( ;(dt = t-*(tq = tp+1)) < winrad; tp++, xp++);
	    for(tq = tp, xq = xq, cnt = 0, zeta = iota0 = iota = 0; (sot = sigma*(ot = o*(*tq-t))) < M_PI; cnt++)
	    {   w = 0.5*(1+cos(sot)); iota0+=w;
		RE(zeta) += w*cos(ot); IM(zeta) += w*sin(ot);
		ot *= 2;
		RE(iota) += w*cos(ot); IM(zeta) += w*sin(ot);
	    }
	    if(cnt>0)
		*rp++ = 2/(sqr(cnt+iota0)-sqr(RE(iota))-sqr(IM(iota)))*(conj(zeta)*iota0+zeta*conj(iota));
	    else
		*rp++ = 0;
	    *rp++ = 0;
	}
}

#ifdef _STANDALONE_
void fastnurealwavelet()
{
#else
void fastnurealwavelet(Real *tptr, Real *xptr, int *nptr, double *lengthptr, int *ncoeffptr, int *noctaveptr,
		       double *tminptr, double *tmaxptr, int *tsubdivptr, Real *sigmaptr, Real *omegamaxptr, Complex *result)
{
    int  k, n = *nptr, ncoeff = *ncoeffptr, noctave = *noctaveptr;
    Real length = *lengthptr, omegamax = *omegamaxptr,
	 tmin = *tminptr, tmax = *tmaxptr, deltat = (tmax-tmin)/(*tsubdivptr-1), sigma = *sigmaptr;
#endif
    struct SumVec			     /* Precomputation record */
    {   struct SumVec *next;		     /* A singly linked list */
	XTElem elems[PNUM];		     /* the summed power series elements */
        int cnt;			     /* number of samples for which the power series elements were added */
        double tau;
    }
             *shead, *stail, *sp, *sq;	     /* power series element lists */
    Complex  zeta, iota, iota0,	             /* Accumulators for spectral coefficients */
             zzp, zzm, zz, ii, ii0, iip, iim,/* ii0 is real */
             e, e0, eplus, eminus,	     /* summation factor exp(-i o tau_h) */
	     e2, e2plus, e2minus,
	     emul, e0mul, eplusmul, eminusmul, 
	     e2mul, e2plusmul, e2minusmul,
	     *rp = result;
    Real     dtelems[PNUM],		     /* power series elements of exp(-i dtau)  */
	     *dtp, *r,			     /* Pointers into dtelems */
             t, x,			     /* abscissa and ordinate value */
             tau, tau0, te,		     /* Precomputation range centers and range end */
             tau0d,	                     /* tau_h of first summand range at level d */
             dtau = (0.5*M_PI)/omegamax,     /* initial precomputation interval radius */
             dtaud,			     /* precomputation interval radius at d'th merging step */
             ooct, o, o2,
	     oplus, o2plus, ominus, o2minus, osigma,
	     omul, omul_1,                   /* omega/mu for octave's top omega and per band, mult. factor and reciprocal */
             omegaoct, omega,		     /* Max. frequency of octave and current frequency */
             on_1, oplusn_1, ominusn_1,	     /* n_1*(omega/mu)^p, n_1*(omegaplus/mu)^p, n_1*(omegaminus/mu)^p */
	     o2n_1, o2minusn_1, o2plusn_1, osign_1,  /* n_1*(2*omega/mu)^p, n_1*(2*omegaplus/mu)^p, n_1*(2*omegaminus/mu)^p, n_1*(omega*sigma/mu)^n */
             mu = (0.5*M_PI)/length,  	     /* Frequency shift: a quarter period of exp(i mu t) on length */
	     winrad,			     /* abscissa dist. from Hanning window center to its borders */
	     tmp;
    XTElem   eoelems[PNUM],                  /* Alternately addition and subtraction of power series elements */
					     /* of adjacent precomputation ranges */
	     oeelems[PNUM],		     /* and vice versa */
	     *eop, *oep,		     /* Pointers into eoelems and oeelems */
	     *ep,			     /* Pointer into the current choice of eoelems and oeelems */
	     *p, *q, *pe;	             /* Pointers into power series elements of adjacent precomputation ranges */
    int      i, j, ti, cnt;		     /* Coefficient, octave, time and sample counter */

    /* Subdivision and Precomputation */
    SRCFIRST;
    tau = SRCT+dtau; te = tau+dtau;
    tau0 = tau;
    shead = stail = sp = alloca(sizeof(*sp)); sp->next = 0; sp->tau = tau;
    for(te = SRCT+2*dtau; ; )
    {   x = SRCX;
        EXP_IOT_SERIES(p, sp->elems, mu*(SRCT-tau), =, SETXT, SETXT); sp->cnt = 1;
	for(SRCNEXT; SRCAVAIL && SRCT<te; SRCNEXT)
        {   x = SRCX; 
            EXP_IOT_SERIES(p, sp->elems, mu*(SRCT-tau), +=, SETXT, SETXT); sp->cnt++;
        }
        if(!SRCAVAIL) break;
        tau = te+dtau; te = tau+dtau;
        sp = alloca(sizeof(*sp)); stail->next = sp; stail = sp; sp->next = 0;
	sp->cnt = 0; sp->tau = tau;
    }

    ooct = omegamax/mu;
    omul = exp(-M_LN2/ncoeff); omul_1 = 1.0/omul;
    omegaoct = omegamax;
    tau0d = tau0;
    dtaud = dtau;
    /*** Loop over Octaves ***/
    for(j = noctave, tau0d = tau0, winrad = M_PI/(sigma*omegaoct); ; ooct *= 0.5, omegaoct *= 0.5, tau0d += dtaud, dtaud *= 2)
    {   /*** Results per frequency ***/
        for(i = ncoeff, o = ooct, omega = omegaoct; i--; o *= omul, omega *= omul, winrad *= omul_1)
        {   oplus  = o+sigma; ominus  = o-sigma;
	    o2plus = oplus+o; o2minus = ominus+o;
	    osigma = o*sigma;
	    /*** Results per time point ***/
	    for(t = tmin, ti = *tsubdivptr, sp = shead; ti--; t += deltat)
            {   zeta = iota = iota0 = 0;
		for( ; sp && t-sp->tau > winrad; sp = sp->next);
		if(!sp)
		{   *rp++ = 0; continue;   }
		tau0d = sp->tau;
		PHISET(e,       -omega*(tau0d-t)); e2 = e*e;
		PHISET(e0,      -omega*sigma*(tau0d-t));
		SCALEPHISET(eplus,   0.5*, -omega*(tau0d-t+sigma));
		SCALEPHISET(eminus,  0.5*, -omega*(tau0d-t-sigma));
		SCALEPHISET(e2plus,  0.5*, -omega*(2*(tau0d-t)+sigma));
		SCALEPHISET(e2minus, 0.5*, -omega*(2*(tau0d-t)-sigma));
		PHISET(emul,       -2*omega*dtaud); e2mul = emul*emul;
		PHISET(e0mul,      -2*omega*sigma*dtaud);
		PHISET(eplusmul,   -2*omega*(dtaud+sigma));
		PHISET(eminusmul,  -2*omega*(dtaud-sigma));
		PHISET(e2plusmul,  -2*omega*(2*dtaud+sigma));
		PHISET(e2minusmul, -2*omega*(2*dtaud-sigma));
		
		for(cnt = 0, sq = sp; sq && sq->tau-t < winrad; 
		    sq = sq->next, e *= emul, eplus *= eplusmul, eminus *= eminusmul, e2 *= e2mul, e2plus *= e2plusmul, e2minus *= e2minusmul)
		    if(sq->cnt)
		    {   /* Power series elements */
			on_1 = oplusn_1 = ominusn_1 = o2n_1 = o2plusn_1 = o2minusn_1 = osign_1 = 1; //n_1;
			zz = zzp = zzm = ii = ii0 = iip = iim = 0;
			for(p = sq->elems, pe = p+PNUM, o2 = 2*o; p <pe; )
			{   RE(zz)  += p->x * on_1;   RE(zzp) += p->x * oplusn_1; RE(zzm) += p->x * ominusn_1;
			    RE(ii0) += p->t * osign_1; RE(ii)  += p->x * o2n_1;
			    RE(iip) += p->x * o2plusn_1; RE(iim) += p++->x * o2minusn_1;
			    on_1  *= o;  oplusn_1  *= oplus;  ominusn_1  *= ominus;
			    osign_1 *= osigma; o2n_1 *= o2; o2plusn_1 *= o2plus; o2minusn_1 *= o2minus;
			    
			    IM(zz)  += p->x * on_1;   IM(zzp) += p->x * oplusn_1; IM(zzm) += p->x * ominusn_1;
			    IM(ii0) += p->t * osign_1; IM(ii)  += p->x * o2n_1;
			    IM(iip) += p->x * o2plusn_1;
			    IM(iim) += p++->x * o2minusn_1;
			    on_1  *= o;  oplusn_1  *= oplus;  ominusn_1  *= ominus;
			    osign_1 *= osigma; o2n_1 *= o2; o2plusn_1 *= o2plus; o2minusn_1 *= o2minus;
			}
			zeta  += e*zz+eplus*zzp+eminus*zzm;
			iota  += e2*ii+e2plus*iip+e2minus*iim;
			iota0 += e0*ii0;
			cnt += sq->cnt;
		    }
		if(cnt>0)
		    *rp++ = 2/(sqr(cnt+RE(iota0))-sqr(RE(iota))-sqr(IM(iota)))*(conj(zeta)*iota0+zeta*conj(iota));
		else
		    *rp++ = 0;
	    }
        }
	if(--j<=0) break;		    /* avoid unnecessary merging at the end */
        /* Merging of the s_h;
         * 4 different possibilities, depending on which of the merged ranges actually contain data.
         * The computation is described in the paper; The result of a merger is stored in the
	 * left precomputation record (sp). Before one power series element is stored, the 
	 * sum and difference of the original values *p and *q are stored in eoelems and oeelems,
	 * respectively. The result is then stored in *p.
         */
	EXP_IOT_SERIES(dtp, dtelems, mu*dtaud, =, SETT, SETT);
        for(sp = shead; sp; sp = sp->next)
        {   sp->tau += dtaud; /* ??? */
	    if(!(sq = sp->next) || !sq->cnt)
            {   if(sp->cnt)
		{   for(p = sp->elems, eop = eoelems, oep = oeelems, dtp = dtelems, pe = p+PNUM; ; )
                    {   eop->x = p->x; eop++->t = p->t; oep->x =-p->x;  oep++->t =-p->t;
                        for(ep = eoelems, r = dtp++, p->x = ep->x * *r, p->t = ep->t * *r; ; )
                        {   ++ep; --r; p->x -= ep->x * *r; p->t -= ep->t * *r; if(r<=dtelems) break;
		    	    ++ep; --r; p->x += ep->x * *r; p->t += ep->t * *r; if(r<=dtelems) break;
		        }
                        if(++p>=pe) break;
                        eop->x =-p->x; eop++->t =-p->t; oep->x = p->x;  oep++->t = p->t;
                        for(ep = oeelems, r = dtp++, p->x = ep->x * *r, p->t = ep->t * *r; r > dtelems; )
                        {   ++ep; --r; p->x += ep->x * *r; p->t += ep->t * *r;   }
                        if(++p>=pe) break;
                    }
		}
		if(!sq) break;
	    }
            else
                if(sp->cnt)
                    for(p = sp->elems, q = sq->elems, eop = eoelems, oep = oeelems, dtp = dtelems, pe = p+PNUM; ; )
                    {   eop->x = p->x+q->x; eop++->t = p->t+q->t; oep->x = q->x-p->x; oep++->t = q->t-p->t;
                        for(ep = eoelems, r = dtp++, p->x = ep->x * *r, p->t = ep->t * *r; ; )
                        {   ++ep; --r; p->x -= ep->x * *r; p->t -= ep->t * *r; if(r<=dtelems) break;
			    ++ep; --r; p->x += ep->x * *r; p->t += ep->t * *r; if(r<=dtelems) break;
			}
                        if(++p<pe) q++; else break;
			eop->x = q->x-p->x; eop++->t = q->t-p->t; oep->x = p->x+q->x; oep++->t = p->t+q->t;
                        for(ep = oeelems, r = dtp++, p->x = ep->x * *r, p->t = ep->t * *r; r > dtelems; )
                        {   ++ep; --r; p->x += ep->x * *r; p->t += ep->t * *r;   }
                        if(++p<pe) q++; else break;
                    }
                else
                    for(q = sq->elems, eop = eoelems, dtp = dtelems, pe = q+PNUM; ; )
                    {   eop->x = q->x; eop++->t = q->t;
                        for(ep = eoelems, r = dtp++, q->x = ep->x * *r, q->t = ep->t * *r; ; )
                        {   ++ep; --r; q->x += ep->x * *r; q->t += ep->t * *r; if(r<=dtelems) break;
			    ++ep; --r; q->x -= ep->x * *r; q->t -= ep->t * *r; if(r<=dtelems) break;
			}
			if(++q>=pe) break;
                        eop->x = q->x; eop++->t = q->t;
                        for(ep = eoelems, r = dtp++, q->x = ep->x * *r, q->t = ep->t * *r; r > dtelems; )
                        {   ++ep; --r; q->x += ep->x * *r; q->t += ep->t * *r;   }
			if(++q>=pe) break;
                    }
	    sp->cnt += sq->cnt; sp->next = sq->next; /* free(sq) if malloc'ed */
        }
    }
}

#ifdef _STANDALONE_
void nureal(Data *in, int ct, int cx, int n, int ncoeff, int noctave, Real omegamax, Complex *rp)
{   Data *dp;
#else
void nureal(Real *tptr, Real *xptr, int *nptr, int *ncoeffptr, int *noctaveptr, Real *omegamaxptr, Complex *rp)
{   int  k, n = *nptr, ncoeff = *ncoeffptr, noctave = *noctaveptr;
    Real o = *omegamaxptr;
#endif
    Real    od = exp(-M_LN2/ncoeff), ot, n_1 = 1.0/(double)n;
    Complex zeta, iota;

    for(ncoeff *= noctave; ncoeff--; o *= od)
    {   for(SRCFIRST, zeta = iota = 0; SRCAVAIL; SRCNEXT)
        {   ot = o*SRCT;
            RE(zeta) += cos(ot)*SRCX; IM(zeta) -= sin(ot)*SRCX;
            ot *= 2;
            RE(iota) += cos(ot); IM(iota) -= sin(ot); 
        }
        zeta *= n_1; iota *= n_1;
        *rp++ = 2/(1-sqr(RE(iota))-sqr(IM(iota)))*(conj(zeta)-conj(iota)*zeta);
    }
}

#ifdef _STANDALONE_

static Data *free_data = 0;
static Real min[MAXCOLUMN], max[MAXCOLUMN];

/* Create a preallocated list of items of a type as indicated by the type of p_;
 * That type has to be large enough to accomodate a pointer.
 * Use f_ as a free list and n_ as the preallocation number when the
 * free list is 0, i.e. empty.
 */
#define ALLOCLINK(p_,n_,f_,c_)							\
{ if(!f_)									\
  { int _s = sizeof(*(p_)); void *_q, *_r, *_rn;				\
    if(!(p_=malloc(n_*_s))) { fprintf(stderr, "couldn't malloc\n"); exit(1); }	\
    _q = (p_+(n_)-1); f_ = p_;							\
    for(_r = p_; (_rn = _r+_s) <= _q; _r = *(void**)_r = _rn);			\
    *(void**)_q = 0; c_;							\
  }										\
  p_=f_;*(void**)&f_=*(void**)(f_);						\
}

/* Just for completeness; dispose of a preallocated element by concatenating it 
 * in front of the freelist.
 */
#define FREELINK(p_,f_) (*(void**)(p_)=f_,f_=(void*)p_)

/* Create one Data record */
static Data *new_data(Data **head, Data **tail)
{   Data *s;
    int         i;

    ALLOCLINK(s, 256, free_data,);
    s->next = 0;
    if(*tail) (*tail)->next = s; else *head = s;
    *tail = s;

    for(i = 0; i<MAXCOLUMN; s->x[i++] = 0);
    return s;
}

/* Helper for loadvalues */
static inline void setvalue(Data *s, int i, double v)
{   if(i>=MAXCOLUMN) return;
    s->x[i] = v;
    if(max[i]<v) max[i] = v;
    if(min[i]>v) min[i] = v;
}

/* Generate a linked list of the tabular data in the given filename
 * should be able to deal with a variety of CSV-like formats; Adapt the 
 * separator chars and MAXCOLUMN if necessary.
 * If n != 0, the number of records is written into *n. 
 */
Data *loadvalues(char *name, int *n)
{   FILE   *fp;
    double v;
    Data   *s = 0, *head = 0, *tail = 0;
    int    ch, ncol = 0, garbage = 0, cnt, i;

    for(i = 0; i < MAXCOLUMN; min[i] = HUGE, max[i] = -HUGE, i++);
    cnt = 0;
    if(fp = fopen(name, "r"))
    {   if(fscanf(fp, " %lf", &v)>0) /* File starts without header */
        {   s = new_data(&head, &tail); cnt++; ncol = 0;
            setvalue( s, ncol++, v);
        }
        for( ; ; )
        {   if(fscanf(fp, "%*[\r\n;] %lf", &v)>0)
            {   s = new_data(&head, &tail); cnt++; ncol = 0;
                setvalue( s, ncol++, v);
            }
            else if(fscanf(fp, "%*[ \t,]%lf", &v)>0)
            {   if(!s)
                {   s = new_data(&head, &tail); cnt++; ncol = 0;   }
                setvalue( s, ncol++, v);
            }
            else if((ch = getc(fp))==EOF)
                break;
            else
            {        
            } // fputc(ch, stderr); fflush(stderr); garbage = 1;   }
        }
        fclose(fp);
        if(garbage) fputc('\n', stderr);
    }
    if(n) *n = cnt;
    return head;
}

/* Define this to do timing analysis; If undefined, the comparison
 * between results of nureal and fastnureal is activated.
 */
/* #define REPEAT 500 */

#define OMAX 2*M_PI/5000
#define NOCT 9
#define NVOI 24

int main(int argc, char *argv[])
{   Data *d;
    Complex r1[NOCT*NVOI], r2[NOCT*NVOI];
    int n;
    char *file = argc==2?argv[1]:"/home/dolf/banquet/399429a0.deutnat.txt";

    if(!(d = loadvalues(file, &n)))
    {   fprintf(stderr, "couldn't load %s\n", file); exit(1);   }

#ifdef REPEAT
    int cnt;
    if(strstr(argv[0], "fast"))
    	for(cnt = REPEAT; cnt--; fastnureal(d, 1, 2, n, max[1]-min[1], OMAX, NVOI, NOCT, r2));
    else
	for(cnt = REPEAT; cnt--; nureal(d, 1, 2, n, OMAX, NVOI, NOCT, r1));
#else
    nureal(d, 1, 2, n, OMAX, NVOI, NOCT, r1);
    fastnureal(d, 1, 2, n, max[1]-min[1], OMAX, NVOI, NOCT, r2);

    Complex *rp, *rq, *re, h;
    for(rp = r1, rq = r2, re = (void*)r1+sizeof(r1); rp < re; rp++, rq++)
    {   h = *rq / *rp;
        printf("%f%+fI\t%f%+fI\t%.16f%+.16fI\n", __real__ *rp, __imag__ *rp, __real__ *rq, __imag__ *rq, __real__ h, __imag__ h);
    }
#endif
    exit(0);
}

#endif
