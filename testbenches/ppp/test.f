#include "eigen.f"
#define OUTPUT_UNIT 10
      program TestOfEigen
      double precision H(16), U(16)
      data H/
     &       3.3,  2.0, -0.1, -7.5,
     &       2.0,  5.5, -9.5,-17.2,
     &      -0.1, -9.5, -2.6,  8.9,
     &      -7.5,-17.2,  8.9,  9.0/
C
C.....calculate the eigenpairs of H
      call eigen(H, U, 4)
C
C.....print the eigenvalues
      write(OUTPUT_UNIT, 200) (H(4*(i-1)+i), i=1,4)
 200  format(" The eigenvalues are: ", (4f10.5))
      STOP
      end
