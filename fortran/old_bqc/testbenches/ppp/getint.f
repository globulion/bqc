#define NUMBER 228
#define OK 0
#define END_OF_FILE 1
#define C_DATA double precision xy(20,2);common/xydata/xy,m
#define RCC 1.414d00
      integer function getint(file, i, j, k, l, mu, val, pointer)
      integer file, i, j, k, l, mu, pointer
      double precision val
      integer ppptwo
      save id, jd, kd, ld
C
      if (pointer.EQ.0) then
          id = 0
          kd = 0
      end if
      pointer = pointer + 1
      kd = kd + 1
      if (kd.GT.id) then
          kd = 1
          id = id + 1
      end if
C.....ZDO approximation for integrals
      jd = id ; ld = kd
      getint = ppptwo(id, kd, val)
C
      i = id
      j = jd
      k = kd
      l = ld
C
      return              
      end

      integer function ppptwo(i, j, val)
      integer i, j
      double precision val
      double precision r, two
C     C_DATA
      double precision xy(20,2)
      common /xydata/xy,m
C
      data two/2.0d00/
C
      if (i.GT.m) then
          ppptwo = END_OF_FILE
          return
      end if
C
      if (i.EQ.j) then
          val = 11.4d00
          ppptwo = OK
          return
      end if
C
      r = dsqrt((xy(i,1)-xy(j,1))**2 + (xy(i,2)-xy(j,2))**2)
C
      if (dabs(r-two*RCC).LT.0.1d00) then
          val = 10.528d00 - 2.625d00*r + 0.2157d00*r*r
      else 
          val = (1.0d00 + 2.0d00/r)**(-0.5d00)
          val = 7.2d00*(1.0d00 + val)/r
      end if
C
      ppptwo = OK
C
      return
      end

      double precision function pppone(xy, m, i, j)
      double precision xy(20,2)
      integer i, j, m, junk
      double precision zero, r, val, gamma, half
      integer ppptwo
      data zero/0.0d00/, half/0.5d00/
C
      val = zero
      if (i.EQ.j) then
          val = -11.16d00
          do 10 k = 1, m
             if (i.EQ.k) go to 10
             junk = ppptwo(i,k,gamma)
             val = val - gamma
 10       continue
      else
          r = dsqrt((xy(i,1)-xy(j,1))**2 + (xy(i,2) - xy(j,2))**2)
          if (dabs(r-RCC).LT.0.1d00) then
              val = -2.395d00
          end if
      end if
      pppone = val
      return
      end
