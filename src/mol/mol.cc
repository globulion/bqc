#include <Eigen/Dense>
#include "libmol.h"

Molecule::~Molecule() {
   //delete   this-> name;
   //delete[] this-> atoms;
   //delete   this-> charge;
   //delete   this-> multiplicity;
   //delete   this-> nelec;
};

void Molecule::write(const char* out, const char* format) const {
     return;
};

std::ostream &operator<<(std::ostream &os, const Molecule &Mol)
{
      os << "Molecule: " << Mol.name << std::endl;
      return os;
}

