#include <iostream>
#include <string>
#include <list>
#include "libmol.h"

// Atom
Atom::Atom(const std::string symbol, const int atno) {
    _symbol = symbol; 
    _atno   = atno;
    _atno_d = static_cast<double>(atno);
};

Atom::Atom(const std::string symbol, const double atno_d) {
    _symbol = symbol;
    _atno   = static_cast<int>(atno_d);
    _atno_d = atno_d;
};

void Atom::print() const {
   std::cout << " Atom: " << _symbol << std::endl;
};

// AtomList
Atom AtomList::get_atom(unsigned int i) const {
    std::list<Atom>::const_iterator it = _atom_list.begin();
    for (unsigned int ii = 0; ii< i; ii++) it++;
    return *it;
};

void AtomList::append(Atom a) {
   _atom_list.insert(_atom_list.end()++, a);
};

void AtomList::print() const {
   for (std::list<Atom>::const_iterator it = _atom_list.begin(); 
        it != _atom_list.end(); it++) 
                                      (*it).print();
};

