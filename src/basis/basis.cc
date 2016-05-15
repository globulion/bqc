#include "libbasis.h"

Options::Options() { 
    add(string("Task ="          )  , 90       ); 
    add(string("SCF damp ="      )  , 0.3000   );
    add(string("SCF interpol ="  )  , 30       );
    add(string("Read ERI?"       )  , false    );
}; 


boost::shared_ptr<System> read_bqc_input(const char* input) {
   int         task;          
   int         scf_interp, nfrag;
   int         read_eri;
   double      crit_eri, scf_damp;
   FragmentPtrVector Fragments;
   FILE*       bqc_input;


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
   int charge, multiplicity, nstate1, nstate2, nelec, ncmx, nbfns, ngmx;

   // read the input file
   bqc_input = fopen( input, "r");

   fscanf(bqc_input, "%d", &task);
   fscanf(bqc_input, "%lf %lf %d", &crit_eri, &scf_damp, &scf_interp);
   fscanf(bqc_input, "%d", &read_eri);
   fscanf(bqc_input, "%d", &nfrag);

   for (int i=0; i<nfrag; i++) {
        fscanf(bqc_input, "%d", &charge);       
        fscanf(bqc_input, "%d", &multiplicity);
        fscanf(bqc_input, "%d", &nstate1);
        fscanf(bqc_input, "%d", &nstate2);
        fscanf(bqc_input, "%d", &nelec);
        fscanf(bqc_input, "%d", &ncmx);
        fscanf(bqc_input, "%d", &nbfns);
        fscanf(bqc_input, "%d", &ngmx);

        VectorNlist  ntype    = VectorNlist(nbfns);  
        VectorNlist  ncntr    = VectorNlist(nbfns);
        VectorNlist  nfirst   = VectorNlist(nbfns);
        VectorNlist  nlast    = VectorNlist(nbfns);
        MatrixVlist  vlist    = MatrixVlist(ncmx,4);
        MatrixEta    eta      = MatrixEta  (ngmx,5);

        for (int i=0; i<ncmx ; i++) fscanf(bqc_input, "%lf %lf %lf %lf"   , &vlist(i,0), &vlist(i,1), &vlist(i,2), &vlist(i,3));    
        for (int i=0; i<ngmx ; i++) fscanf(bqc_input, "%lf %lf %lf %lf %lf", &eta(i,0), &eta(i,1), &eta(i,2), &eta(i,3), &eta(i,4));
        for (int i=0; i<nbfns; i++) fscanf(bqc_input, "%d", &ntype(i));
        for (int i=0; i<nbfns; i++) fscanf(bqc_input, "%d", &ncntr(i));
        for (int i=0; i<nbfns; i++) fscanf(bqc_input, "%d", &nfirst(i));
        for (int i=0; i<nbfns; i++) fscanf(bqc_input, "%d", &nlast(i));

        // create BasisSet object                                    
        BasisSetPtr B (new BasisSet(ncmx, nbfns, ngmx, 
                                    ntype, ncntr, nfirst, nlast,
                                    vlist, eta) );
 
        B->normalize_pgto();

        // create Fragment object
        FragmentPtr F (new Fragment(nelec, charge, multiplicity, B,
                                    nstate1, nstate2) );

        // 
        Fragments.push_back(F);

   }
   fclose(bqc_input);

   // set Options
   OptionsPtr Opts(new Options());

   Opts->add(string("Task ="         ), task);
   Opts->add(string("SCF damp ="     ), scf_damp);
   Opts->add(string("SCF interpol =" ), scf_interp);
   Opts->add(string("Read ERI?"      ), static_cast<bool>(read_eri));



   // then the BQC System is born
   SystemPtr S (new System(Fragments, Opts));

   // calculate nuclear repulsion energy
   S->eval_nuclear_repulsion_energy();

   return S;
};
/*
BasisSet::BasisSet (int integer) {

     Eigen::MatrixXd _S = Eigen::MatrixXd(integer,integer);
     for (int i = 0; i < integer; i++) {
          _S(i,i) = static_cast<double>(i);
     }   
     _nbfns = integer;
}

Eigen::MatrixXd BasisSet::get_S () {

      for (int i = 0; i < this->_nbfns; i++) {
           fprintf(stdout, "%15.5f\n", _S(i,i));
      }
      return _S;
}
*/

/*! Print BasisSet object.
 *
 *
 */
void BasisSet::print(std::ostream& out) const {
     out << " ::-> BasisSet" << std::endl;
};

std::ostream &operator<<(std::ostream &out, const BasisSet &bfs)
{
     bfs.print(out);
     return out;
};

