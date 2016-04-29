#include <boost/shared_ptr.hpp>
#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include "libbasis.h"

BasisSet::BasisSet (int integer) {

     S_ = boost::shared_ptr<double> ( new double[integer] );
     for (int i = 0; i < integer; i++) {
          *(S_.get()+i) = static_cast<double>(i);
     }   
     nbfns_ = integer;
}

boost::shared_ptr<double> BasisSet::get_S () {

      for (int i = 0; i < this->nbfns_; i++) {
           fprintf(stdout, "%15.5f\n", *(S_.get()+i));
      }
      return S_;
}
