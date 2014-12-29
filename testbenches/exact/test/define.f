#define OK 0
#define JIII(a) 1.0 +a
#define USER(k,l) i=k*l ; u=1-i ; f=1 
#define AAA write(*,*) "HAPPY", JIII(3)
#define OUTPUT_UNIT 10
#define long_macro_name(x,y) \
x*y
#include "sub.f"
      PROGRAM HAPPY
      integer i,j,m,u, sign
      double precision V(9)
      data V / 1.333, -0.222, 4.4, 2.3, 7777.8475895459583057, 
     &  -0.32546334, -0.387328278, 44.2386824762462746329862, 4.345/
      WRITE(*,*) OK
      CALL TEST
      i = 1 ; j = 4 ; m = 44
      USER(1,2)
      WRITE(*,*) i,u
      do i=1,9
         write(*,*) V(i)
      enddo
      WRITE(OUTPUT_UNIT, 200)
      sign = 12
      write(*,*) sign*4, JIII(4), long_macro_name(5,4)
      AAA
      
 200  format("Happy")
      END
