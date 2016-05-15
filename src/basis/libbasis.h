/*! \file 
 *
 *
 *
 */
#include <boost/shared_ptr.hpp>
#include <boost/variant.hpp>
#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <vector>
#include <map>
#include <Eigen/Dense>

#ifndef __BQC_C_LIBBASIS__
#define __BQC_C_LIBBASIS__

using std::string;

class Data;
class Options;
class BasisSet;
class Fragment;
class System;

typedef std::map<string, Data> map_s_D;
typedef std::vector<boost::shared_ptr<Fragment> > FragmentPtrVector;
typedef boost::shared_ptr<Options> OptionsPtr;
typedef boost::shared_ptr<BasisSet> BasisSetPtr;
typedef boost::shared_ptr<Fragment> FragmentPtr;
typedef boost::shared_ptr<System> SystemPtr;
typedef boost::variant<bool, int, double, string> data_type;
typedef Eigen::Matrix<int   , Eigen::Dynamic, 1> VectorNlist;
typedef Eigen::Matrix<double, Eigen::Dynamic, 5> MatrixEta;
typedef Eigen::Matrix<double, Eigen::Dynamic, 4> MatrixVlist;

/*! Represents one element of data.
 *
 *  It is introduced here to handle options
 *  in a unified way.
 */
class Data {
   protected:
     //bool _is_bool, _is_string, _is_int, _is_double;
     bool _is_none;
     // data
     data_type _data;
   public:
     Data() : _is_none(true) {};
     /*
     Data() {_is_bool    =false; 
             _is_string  =false;
             _is_int     =false;
             _is_double  =false;
             _is_none    =true;};
     */
     /*
     Data(bool   d) {_is_bool=true  ; _is_none=false; _data=d;};
     Data(int    d) {_is_int=true   ; _is_none=false; _data=d;};
     Data(double d) {_is_double=true; _is_none=false; _data=d;};
     Data(string d) {_is_string=true; _is_none=false; _data=d;};
     */
     Data(data_type d) {_data=d;_is_none=false;};

     bool is_none(void) {return _is_none;};

     data_type get(void) { 
        if (_is_none) return string("NONE");
        return _data;
     };

};

/*! Container for TODO specifiers for BQC program.
 *
 *  Contains the options names and their values. 
 *  Can be suplemented by option pairs (std::string, data_type)
 *  where data_type can be std::string, int, double or bool.
 */
class Options {
   protected:
      map_s_D _opts_s_D;

   public:
      // initialize default options
      Options();

      // add/change option values
      void add   (string s)              {_opts_s_D[s] = Data(true);};
      void add   (string s, data_type d) {_opts_s_D[s] = Data(d);};

      data_type operator[](string key) {return _opts_s_D[key].get();};
};



/*! Define the CGTO basis set for a set of points in space.
 *
 *  Each basis function is a primitive gaussian-type orbital (PGTO)
 *  and the whole set forms the contracted GTO set (CGTO).
 *  BasisSet object can be constructed by providing the list
 *  of basis function centers along with their complete specification.
 *  The following information are required for each PGTO:
 *
 *  * exponents 
 *  * contraction coefficients
 *  * number of atomic site it belongs to
 *  * atomic coordinates
 *
 *  It is also necessary to specify the contraction lenghts by
 *  providing the first and last indices of PGTO per basis set
 *  function, as well as the type of GTO (angular momentum quantum number).
 */  
class BasisSet {
  
    public:
       BasisSet() {};
       BasisSet (int ncmx, int nbfns, int ngmx, 
                 const VectorNlist& ntype, const VectorNlist& ncntr, const VectorNlist& nfirst, const VectorNlist& nlast,
                 const MatrixVlist& vlist, const MatrixEta& eta) {
                    _ncmx = ncmx; _nbfns = nbfns; _ngmx = ngmx;                       
                    _ntype = ntype; _ncntr = ncntr; _nfirst = nfirst; _nlast = nlast;
                    _vlist = vlist; _eta = eta;
                 };

       Eigen::MatrixXd get_S();

       void normalize_pgto(void) {};
       void print(std::ostream& out = std::cout) const;

    protected: 
       /// number of PGTO's centers
       int _ncmx;
       /// number of BFS
       int _nbfns;
       /// number of PGTO's
       int _ngmx;
       /// PGTO cartesian coordinates, coefficients and exponents
       MatrixEta _eta;
       /// Atomic coordinates and numbers
       MatrixVlist _vlist;
       /// list of first PGTO indices per BSF
       VectorNlist _nfirst;
       /// list of last  PGTO indices per BSF
       VectorNlist _nlast;
       /// list of BSF types
       VectorNlist _ntype;
       /// list of atomic centers each BSF belongs to
       VectorNlist _ncntr;


       /// overlap matrix in AO basis
       Eigen::MatrixXd _S;

};



/*! Defines a single fragment of the system.
 *
 *  The Fragment object is an integral part of System on which QM method can act.
 */
class Fragment {
    protected:
       int _nelec, _charge, _multiplicity, _n_unpaired;
       BasisSetPtr _bfs;
       int _nstate1, _nstate2;

    public:
       Fragment(int nelec, int charge, int multiplicity, 
                BasisSetPtr B, int nstate1, int nstate2) : 
                 _nelec(nelec), _charge(charge), _multiplicity(multiplicity), 
                 _bfs(B), _nstate1(nstate1), _nstate2(nstate2) {
           _n_unpaired = _multiplicity - 1;     
        };
};


/*! The entire molecular system.
 *
 *  Contains all fragments that are treated by BQC methods.
 */
class System {
    protected:
       FragmentPtrVector _frags;
       OptionsPtr _opts;

    public:
       System (FragmentPtrVector frags, OptionsPtr opts) : _frags(frags), _opts(opts) {};

       void eval_nuclear_repulsion_energy(void) {};
};


/*! \fn Read BQC input file.
 *
 *  Input is an ASCI file containing basis set and all molecules
 *  in the system. The returned object is a shared poiter to
 *  System instance that describe all the system and task to be 
 *  performed on it.
 */
SystemPtr read_bqc_input(const char* input);


#endif
