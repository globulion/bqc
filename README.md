Project objectives
==================

The main goal of BQC is to implement novel approaches for Quantum Chemistry, with a strong emphasis on the
*fragment-based* approaches. This would enable one to study very large systems up to hundreds of atoms and use
the high-level methodology at the same time.

Installation and usage
----------------------

Installation can be done by [Cmake] which generates `Makefile` automatically for you. BQC uses [Eigen3] for manipulations
on arrays, vectors and matrices. The configuration of compiling BQC is in `CMakeLists.txt` file. Generally
the default options are OK but you might want to modify them if necessary.
After you checked the contents of `CMakeLists.txt` file, you need to specify your `include` directory that contains all
[Eigen3] header files. To do this set the following environment variable, run [Cmake] and install BQC:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.sh}
export EIGEN3_INCLUDE_DIR=<path to Eigen3>
cmake .
make
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This should work fine if you have properly installed [Eigen3] header files. No other dependencies are necessary at
present.

[cmake]:  https://cmake.org/ "Cmake package"
[eigen3]: http://eigen.tuxfamily.org/dox/ "High-performance numerical libraries for manipulations on arrays, vectors and matrices"
