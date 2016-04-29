#include <string>
#include <list>
#include <ostream>
#include <Eigen/Dense>

typedef Eigen::Matrix<double, Eigen::Dynamic, 4> CoordX4;

/*! Define an atom entity.
 * 
 *  Contains the description of an atomic object.
 *  The Atom instance stores atomic symbol and atomic number (as an int
 *  and double for computational purposes).
 */
class Atom {
    public:
       Atom() {};
       // Set the symbol and atomic numbers only
       Atom(const std::string symbol, const int atno);       
       Atom(const std::string symbol, const double atno_d);

       const std::string symbol() const {return _symbol;};
       const int atno() const {return _atno;};
       const double atno_d() const {return _atno_d;}; 
       void print() const;
    protected:
       // Atomic symbol
       std::string _symbol;
       // Atomic number
       int _atno;
       // Double precision atomic number
       double _atno_d;
    
};

/*! Handle atomic lists.
 * 
 *  AtomList objects are lists of Atom instances.
 *  The list string can be redirected to a buffer.
 */
class AtomList {
     public:
       AtomList() {}

       void append(Atom a);
       Atom get_atom(unsigned int i) const;
       void print() const;
     protected:
       std::list<Atom> _atom_list;
};


/*! Define molecular entity.
 *
 *  Describes the molecule in terms of atoms, from which it is constructed.
 *  It can carry a BasisSet instance that also provides information about 
 *  the basis functions that describe the Molecule instance.
 * 
 *  Features:
 *    * Molecule object can be redirected to a buffer string.
 *    * add more (rotation, translation)
 */
class Molecule {
   public:
      Molecule() {};
      Molecule(const char* name) {this->name = name;};
      //Molecule(const char* name, const AtomSet &Atoms, const CoordX4 &M);
     ~Molecule();
  
      // name
      const char* name;

      // write XYZ file
      void write(const char* out, const char* format="xyz") const;
      
   protected:
};

/// Print the Molecule object.
std::ostream &operator<<(std::ostream &os, const Molecule &Mol);
