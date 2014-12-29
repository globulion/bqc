#define USE(i,j,k,l,val)\
irs=m*(j-1)+i;itu=m*(l-1)+k;\
iru=m*(l-1)+i;its=m*(j-1)+k;\
G(irs)=G(irs)+2.0d00*R(iru)*val;\
G(iru)=G(iru)-R(its)*val
      subroutine USER(i,j,k,l,val,G,R) 
      implicit double precision (a-h,o-z)
      double precision G(1), R(1) 
      irs=m*(j-1)+i;itu=m*(l-1)+k
      iru=m*(l-1)+i;its=m*(j-1)+k
      G(irs)=G(irs)+2.0d00*R(iru)*val
      G(iru)=G(iru)-R(its)*val
      return
      end

