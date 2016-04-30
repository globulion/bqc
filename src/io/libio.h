#include <boost/shared_ptr.hpp>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "../basis/libbasis.h"

typedef boost::shared_ptr<System> SystemPtr;

/*! Printing libraries. They contain useful and simple routines
 *   of displaying vectors, matrices and other basic data structures
 */
void print_dvector(double* V, int m);
void print_dmatrix(double* A, int m, int n);

/*! Read BQC input file.
 *
 *  Input is an ASCI file containing basis set and all molecules
 *  in the system. The returned object is a shared poiter to
 *  System instance that describe all the system and task to be 
 *  performed on it.
 */
SystemPtr read_bqc_input(const char* input);
