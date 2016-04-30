#include <boost/shared_ptr.hpp>
#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <Eigen/Dense>

/*! \typedef Dynamic allocation of matrices and arrays
 *
 *
 */
typedef Eigen::Matrix<long, Eigen::Dynamic, 1> VectorNlist;
typedef Eigen::Matrix<double, Eigen::Dynamic, 5> MatrixEta;
typedef Eigen::Matrix<double, Eigen::Dynamic, 4> MatrixVlist;

#ifndef __BQC_C_BASISSET__
#define __BQC_C_BASISSET__
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
       BasisSet(int ); 

       Eigen::MatrixXd get_S();

    protected: 
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
       /// number of BFS
       int _nbfns;
       /// number of PGTO's
       int _ngmx;
       /// number of PGTO's centers
       int _ncmx;

       /// overlap matrix in AO basis
       Eigen::MatrixXd _S;

};
#endif
