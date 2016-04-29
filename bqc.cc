#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#include "BqcConfig.h"
#include "src/basis/libbasis.h"
#include "src/math/libmath.h"
#include "src/io/libio.h"
#include "src/ints/libints.h"
#include "src/mol/libmol.h"
#define OK 0
 
int main (int argc, char *argv[])
{
  // Print the information
  if (argc < 2)
    {
    fprintf(stdout,"%s Version %s\n", argv[0], BQC_VERSION);
    fprintf(stdout,"Usage: %s [not settled yet]\n",argv[0]);
    return 1;
    }
 
  int iptr = 0;
  long int i, j, k, l; short int mu;
  double val;

  while (getint("None", &i, &j, &k, &l, &mu, &val, &iptr) == OK ) {
        fprintf(stdout, "Integral: %4ld %4ld %4ld %4ld %14.6f\n", i,j,k,l,val);
  }

  fprintf(stdout,"Success!\n");

  Molecule Mol("Happy Molecule!");
  std::cout << Mol << std::endl;
  std::stringstream buffer;
  buffer << Mol << "Ojojo~!" << std::endl;
  fprintf(stdout, "%s\n", buffer.str().c_str());

  Atom A("Na", 11);
  AtomList Al;
  Al.append(A);
  Al.append(Atom("H", 1));
  Al.print();

  return 0;
}
