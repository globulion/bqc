#include "libio.h"

typedef boost::shared_ptr<Options> OptionsPtr;
typedef boost::shared_ptr<BasiSet> BasisSetPtr;
typedef boost::shared_ptr<System> SystemPtr;

void print_dvector(double* V, int m) {
    int i;
    for (i=0; i<m; i++) {
         fprintf(stdout, "%14.6f", V[i]);
    }
    fprintf(stdout, "\n");
};

void print_dmatrix(double* A, int m, int n) {
    int i, j, ij;
    for (i=0; i<m; i++) {
       for (j=0; j<n; j++) {
           ij = i*n + j ;
           fprintf(stdout, "%14.6f", A[ij]);
       }
       fprintf(stdout, "\n");
    }
};

boost::shared_ptr<System> read_bqc_input(const char* input) {
   signed short task;          
   int         interp, nfrag;
   int         read_eri;
   double      crit, damp;
   BasisSetPtr B;
   SystemPtr   S;
   OptionsPtr  opt;

/*!
 * Reading a BQC input file creates the BQC System object
 * or objects. System object is the kernel of BQC function
 * because all the molecules and actions that are to be performed 
 * on them are defined in it. 
 *
 * System Object contains molecules, basis functions and all the
 * BQC options.
 * Reading an input file creates first the BasisSet object. 
 * BasisSet object is composed of the listing of all PGTO's
 * in the system. Therefore it describes all the expansion center
 * along with the Gaussian functions. The contraction of basis set
 * is handled by continuous list of PGTO's and aligning them
 * into basis functions. When the BasisSet object is formed
 * BQC normalizes the primitives and computes the orthogonalizing
 * matrix.
 * 
 * The next object is Options instance which contains the structures
 * of strings with associated string, integer or double values.
 *
 * Once the BasisSet and Options objects are generated, BQC sets the
 * System instance. It provides also the number of electrons in the system
 * and the total charge. It also specifies the distribution of molecular
 * fragments along with their charges and multiplicities. 
 */
   Eigen::VectorXi  charge       = Eigen::VectorXi(nfrag);
   Eigen::VectorXi  multiplicity = Eigen::VectorXi(nfrag);
   Eigen::VectorXi  n_unpaired   = Eigen::VectorXi(nfrag);
   Eigen::VectorXi  nstate1      = Eigen::VectorXi(nfrag);
   Eigen::VectorXi  nstate2      = Eigen::VectorXi(nfrag);
   Eigen::VectorXi  nelec        = Eigen::VectorXi(nfrag);
   Eigen::VectorXi  ncmx         = Eigen::VectorXi(nfrag);
   Eigen::VectorXi  nbfns        = Eigen::VectorXi(nfrag);
   Eigen::VectorXi  ngmx         = Eigen::VectorXi(nfrag);
   Eigen::VectorXi  noffsn       = Eigen::VectorXi(nfrag);
   Eigen::VecotrXi  noffsb       = Eigen::VectorXi(nfrag);
   Eigen::VectorXi  noffsg       = Eigen::VectorXi(nfrag);
   Eigen::VectorXi  ntype                                ;
   Eigen::VectorXi  ncntr                                ;
   Eigen::VectorXi  nfirst                               ;
   Eigen::VectorXi  nlast                                ;

   OptionsPtr opt(new Options());

   // read the input file
   ...
   
   // get total size of input data
   long ngmx_tot = nxmx_tot = nbfns_tot = 0;
   for (int i = 0; i< nfrag; i++) {
        ngmx_tot += ngmx(i);
        ncmx_tot += ncmx(i);
        nbfns_tot+= nbfns(i);
        n_unpaired(i) = multiplicity(i) - 1;
   }

   // set Options
  *opt.add("Task", task);
  *opt.add("SCF damp", damp);
  *opt.add("SCF interpol", interp);
  *opt.add("Read ERI", read_eri);

   // create BasisSet object
   BasisSetPtr B (new BasisSet(nfrag,
                      ncmx, nbfns, ngmx, 
                      noffsn, noffsb, noffsg,
                      ntype, ncntr, nfirst, nlast) );

   // normalize the primitives
  *B.normalize_pgto();

   // then the BQC System is born
   SystemPtr S (new System(nfrag, 
                           nelec, charge, multiplicity, n_unpaired,
                           B, opt));

   // calculate nuclear repulsion energy
  *S.eval_nuclear_repulsion_energy()

   return S;
};
