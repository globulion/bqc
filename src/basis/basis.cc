#include "libbasis.h"

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
