#include <boost/shared_ptr.hpp>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "../basis/libbasis.h"

/*! Printing libraries. They contain useful and simple routines
 *   of displaying vectors, matrices and other basic data structures
 */
void print_dvector(double* V, int m);
void print_dmatrix(double* A, int m, int n);

/*! Read input file. Input file is in ASCI format.
 */
boost::shared_ptr<BasisSet> read_bqc_input(const char* input);

