/*
** Prototypes of all the survival functions
**  Including this in each routine helps prevent mismatched argument errors
*/
SEXP agfit4(SEXP surv2,      SEXP covar2,    SEXP strata2,
            SEXP weights2,   SEXP offset2,   SEXP ibeta2,
            SEXP sort12,     SEXP sort22,    SEXP method2,
            SEXP maxiter2,   SEXP  eps2,     SEXP tolerance2);


void agfit5a(int *nusedx,     int *nvarx,     double *yy, 
	      double *covar2,   double *offset2, double *weights2, 
	      int   *strata,    int   *sort,    double *means,   
              double *beta,     double *u,       double *loglik, 
	      int *methodx,    int *ptype2,    int *pdiag2,
	      int *nfrail,     int *frail2,
              void *fexpr1,     void *fexpr2,    void *rho) ;

void agfit5b( int *maxiter,   int *nusedx,    int *nvarx, 
	       int *strata,    double *beta,    double *u,
	       double *imat2,   double *jmat2,   double *loglik, 
	       int *flag,      double *eps,     double *tolerch, 
	       int *methodx,   int *nfrail,     double *fbeta, 
	       double *fdiag,
               void *fexpr1,    void *fexpr2,     void *rho);

void agfit5c(int *nusedx,   int *nvar,    int *strata,
	      int *methodx,  double *expect) ;
 
void agexact(int *maxiter,  int *nusedx,   int *nvarx,   double *start, 
	     double *stop,   int *event,    double *covar2,double *offset, 
	     int   *strata, double *means,  double *beta,  double *u, 
	     double *imat2,  double loglik[2], int *flag,  double *work, 
	     int   *work2,  double *eps,    double *tol_chol, double *sctest);

void agmart(int   *n,     int   *method,  double *start,   double *stop, 
	    int   *event, double *score,   double *wt,      int   *strata, 
	    double *resid);

void agmart2(int   *n,     int   *method,  double *start,   double *stop, 
	     int   *event,  int   *nstrat,  int *strata,    int *sort1,
	     int   *sort2,  double *score,   double *wt,      
	     double *resid,  double *haz);
void agscore(int   *nx,       int   *nvarx,      double *y,
	     double *covar2,   int   *strata,     double *score,
	     double *weights,  int   *method,     double *resid2, double *a);

void agsurv3(int   *sn,    int   *snvar,    int   *sncurve, 
	     int   *snpt,  int   *sse,      double *score, 
	     double *sy,    int   *grpx,     double *r,        double *coef, 
	     double *var,   double *xmean,    int   *scn, 
	     double *cy,    double *cx,       double *ssurv,
	     double *varh,  double *sused,    int   *smethod);

void agsurv4(int   *ndeath,   double *risk,    double *wt,
             int   *sn,        double *denom,   double *km);

void agsurv5(int *n2,     int *nvar2,  int *dd, double *x1,  
             double *x2,   double *xsum, double *xsum2, 
             double *sum1, double *sum2, double *xbar) ;

void chinv2  (double **matrix, int n);
int cholesky2(double **matrix, int n, double toler);
void chsolve2(double **matrix, int n, double *y);
void chinv3(double **matrix , int n, int m, double *fdiag);
int cholesky3(double **matrix, int n, int m, double *diag, double toler);
void chsolve3(double **matrix, int n, int m, double *diag, double *y);

SEXP concordance1(SEXP y, SEXP wt2,  SEXP indx2, SEXP ntree2);
 
SEXP concordance2(SEXP y,     SEXP wt2,  SEXP indx2, SEXP ntree2,
                  SEXP sortstop, SEXP sortstart) ;

SEXP coxcount1(SEXP y2, SEXP strat2) ;
SEXP coxcount2(SEXP y2, SEXP isort1, SEXP isort2, SEXP strat2) ;

void coxdetail(int   *nusedx,   int   *nvarx,    int   *ndeadx, 
	       double *y,        double *covar2,   int   *strata,  
	       double *score,    double *weights,  double *means2, 
	       double *u2,       double *var,      int   *rmat,
	       double *nrisk2,   double *work);
 
SEXP coxexact(SEXP maxiter2,  SEXP y2, 
              SEXP covar2,    SEXP offset2, SEXP strata2,
              SEXP ibeta,     SEXP eps2,    SEXP toler2) ;
 
void coxfit5_a(int *nusedx,     int *nvarx,     double *yy, 
 	       double *covar2,   double *offset2, double *weights2, 
	       int   *strata,    int   *sort,    double *means,   
               double *beta,     double *u,       double *loglik, 
	       int *methodx,    int *ptype2,    int *pdiag2,
	       int *nfrail,     int *frail2,
               void *fexpr1,     void *fexpr2,    void *rho) ;

void coxfit5_b( int *maxiter,   int *nusedx,    int *nvarx, 
	        int *strata,    double *beta,    double *u,
	        double *imat2,   double *jmat2,   double *loglik, 
	        int *flag,      double *eps,     double *tolerch, 
	        int *methodx,   int *nfrail,     double *fbeta, 
	        double *fdiag,
                void *fexpr1,    void *fexpr2,     void *rho);

void coxfit5_c(int *nusedx,   int *nvar,    int *strata,
	       int *methodx,  double *expect) ;

SEXP coxfit6(SEXP maxiter2,  SEXP time2,   SEXP status2, 
	     SEXP covar2,    SEXP offset2, SEXP weights2,
	     SEXP strata2,   SEXP method2, SEXP eps2, 
	     SEXP toler2,    SEXP ibeta,    SEXP doscale2) ;

void coxmart(int   *sn,     int   *method,    double *time, 
	     int   *status, int   * strata,   double *score, 
	     double *wt,     double *expect);

void coxmart2(int   *sn,     double *time, 
	     int   *status, int   * strata,   double *score, 
	     double *wt,     double *resid);

void coxph_wtest(int *nvar2, int *ntest, double *var, double *b,
                 double *scratch, double *tolerch);

void coxscho(int   *nusedx,    int   *nvarx,    double *y, 
	     double *covar2,    double *score,    int   *strata,  
	     int   *method2,   double *work);

void coxscore(int   *nx,      int   *nvarx,    double *y, 
	      double *covar2,  int   *strata,   double *score, 
	      double *weights, int   *method,   double *resid2,
	      double *scratch);

double coxsafe(double x);
double **dmatrix(double *array, int ncol, int nrow);

void init_doloop(int min, int max);
int doloop      (int nloops, int *index);

void pyears1(int   *sn,      int   *sny,      int   *sdoevent, 
	     double *sy,      double *wt,       
	     int   *sedim,   int   *efac, 
	     int   *edims,   double *secut,    double *expect, 
	     double *sedata,  int   *sodim,    int   *ofac, 
	     int   *odims,   double *socut,    int   *smethod, 
	     double *sodata,  double *pyears,   double *pn, 
	     double *pcount,  double *pexpect,  double *offtable);

void pyears2(int   *sn,      int   *sny,   int   *sdoevent, 
	     double *sy,      double *wt,    
	     int   *sodim,   int   *ofac, 
	     int   *odims,   double *socut, double *sodata,
	     double *pyears,  double *pn,    double *pcount, 
	     double *offtable);

SEXP pyears3b(SEXP   death2,    SEXP   efac2,   SEXP edims2,
	      SEXP   ecut2,     SEXP   expect2, SEXP grpx2,
	      SEXP   x2, 	SEXP   y2,      SEXP times2,
	      SEXP   ngrp2);

double pystep(int nc,        int  *index,  int  *index2,   double *wt, 
	      double *data,  int *fac,    int *dims,     double **cuts, 
	      double step,   int  edge);

void survdiff2(int   *nn,     int   *nngroup,    int   *nstrat, 
	       double *rho,    double *time,       int   *status, 
	       int   *group,  int   *strata,	   double *obs, 
	       double *exp,    double *var,        double *risk, 
	       double *kaplan);

void survfit4(int *n,	int *dd,  double *x1,  double *x2) ;

 SEXP survfitci(SEXP ftime2,  SEXP sort12,  SEXP sort22, SEXP ntime2,
                    SEXP status2, SEXP cstate2, SEXP wt2,  SEXP id2,
                    SEXP p2,  SEXP sefit2) ;
  
SEXP survreg6(SEXP maxiter2,   SEXP nvarx,  SEXP y,
	      SEXP ny2,        SEXP covar2, SEXP wtx,
	      SEXP offset2,    SEXP beta2,  SEXP nstratx,
	      SEXP stratax,    SEXP epsx,   SEXP tolx,       
	      SEXP dist,       SEXP expr,   SEXP rho);

SEXP survreg7(SEXP maxiter2,   SEXP nvarx,  SEXP y,
	      SEXP ny2,        SEXP covar2, SEXP wtx,
	      SEXP offset2,    SEXP beta2,  SEXP nstratx,
	      SEXP stratax,    SEXP epsx,   SEXP tolx,
	      SEXP dist,       SEXP dexpr,  SEXP rho,
	      SEXP ptype2,     SEXP pdiag2, SEXP nfrail2,
	      SEXP fgrp2,      SEXP pexpr1, SEXP pexpr2) ;

double survregc1(int n,          int nvar,     int nstrat,      int whichcase,
		 double *beta,   int dist,     int *strat,     double *offset,
		 double *time1,  double *time2, double *status, double *wt,
		 double **covar, double **imat, double **JJ,    double *u, 
		 SEXP expr,      SEXP rho,      double *dummy,  int nf,
		 int *frail,    double *fdiag, double *jdiag );

double survregc2(int n,          int nvar,     int nstrat,      int whichcase,
		 double *beta,   int dist,     int *strat,     double *offset,
		 double *time1,  double *time2, double *status, double *wt,
		 double **covar, double **imat, double **JJ,    double *u, 
		 SEXP expr,      SEXP rho,      double *dummy,  int nf,
		 int *frail,    double *fdiag, double *jdiag );

void survpenal(int whichcase, int nfrail,    int  nvar2,    double **hmat, 
	       double **JJ,   double *hdiag, double *jdiag,
	       double *u,     double *beta,  double *loglik,
	       int ptype,     int pdiag,     SEXP pexpr1,   double *cptr1, 
	       SEXP pexpr2,   double *cptr2, SEXP rho);

