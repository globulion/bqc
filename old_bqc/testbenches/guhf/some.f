
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle some.web"
C  RUN TIME:     "Friday, December 5, 2014 at 0:07."
C  WEB FILE:     "some.web"
C  CHANGE FILE:  (none)
Cline 49 "some.web"
      subroutine beigen(H,U,n,init,nblock,iblock)
        
C* 2: * 
Cline 91 "some.web"
C* :2 * 
Cline 52 "some.web"
        
        
        implicit double precision(a-h,o-z)
        double precision H(*),U(*)
        integer n,init,nblock,iblock(*)
        
        
        
        data zero,eps,one,two,four/0.0d0,1.0d-20,1.0d0,2.0d0,4.0d0/
        
        
C* 3: * 
Cline 102 "some.web"
        CONTINUE
C ---  "if" ---
        IF(init.EQ.0)THEN
          CONTINUE
C ---  "do" ---
          DO 90000 i=1,n
            ii=n*(i-1)+i
            CONTINUE
C ---  "do" ---
            DO 90002 j=1,n
              ij=n*(j-1)+i
              U(ij)=zero
90002       CONTINUE
            U(ii)=one
90000     CONTINUE
C* :3 * 
Cline 58 "some.web"
        ENDIF
        nmax=0
        
        CONTINUE
C ---  "do" ---
        DO 90004 nsym=1,nblock
          
          
          nmin=nmax+1
          nmax=nmax+iblock(nsym)
          
          
          
          CONTINUE
C ---  "repeat" ---
90006     CONTINUE
            
C* 4: * 
Cline 120 "some.web"
            hmax=zero
            
            
            CONTINUE
C ---  "do" ---
            DO 90009 i=nmin+1,nmax
              
              jtop=i-1
              
              CONTINUE
C ---  "do" ---
              DO 90011 j=nmin,jtop
                
C* 5: * 
Cline 150 "some.web"
                ii=n*(i-1)+i
                jj=n*(j-1)+j
                ij=n*(j-1)+i
                ji=n*(i-1)+j
                hii=H(ii)
                hjj=H(jj)
                hij=H(ij)
                hsq=hij*hij
                CONTINUE
C ---  "if" ---
                IF(hsq.GT.hmax)THEN
                  hmax=hsq
                ENDIF
                CONTINUE
C ---  "if" ---
                IF(hsq.LT.eps)THEN
                  CONTINUE
C ---  "next" ---
                  GOTO 90011
                ENDIF
                del=hii-hjj
                sign=one
                CONTINUE
C ---  "if" ---
                IF(del.LT.zero)THEN
                  sign=-one
                  del=-del
                ENDIF
                denom=del+dsqrt(del*del+four*hsq)
                
                
                tan=two*sign*hij/denom
                c=one/dsqrt(one+tan*tan)
                s=c*tan
C* :5 * 
Cline 134 "some.web"
                
C* 6: * 
Cline 166 "some.web"
                CONTINUE
C ---  "do" ---
                DO 90013 k=1,n
                  kj=n*(j-1)+k
                  ki=n*(i-1)+k
                  jk=n*(k-1)+j
                  ik=n*(k-1)+i
                  temp=c*U(kj)-s*U(ki)
                  U(ki)=s*U(kj)+c*U(ki)
                  U(kj)=temp
                  CONTINUE
C ---  "if" ---
                  IF((i.EQ.k).OR.(j.EQ.k))THEN
                    CONTINUE
C ---  "next" ---
                    GOTO 90013
                  ENDIF
                  temp=c*H(kj)-s*H(ki)
                  H(ki)=s*H(kj)+c*H(ki)
                  H(kj)=temp
                  H(ik)=H(ki)
                  H(jk)=H(kj)
90013           CONTINUE
                
                
C* :6 * 
Cline 136 "some.web"
                
C* 7: * 
Cline 183 "some.web"
                H(ii)=c*c*hii+s*s*hjj+two*c*s*hij
                H(jj)=c*c*hjj+s*s*hii-two*c*s*hij
                H(ij)=zero
                H(ji)=zero
C* :7 * 
Cline 138 "some.web"
                
90011         CONTINUE
              
90009       CONTINUE
            
C* :4 * 
Cline 77 "some.web"
C ---  "until(hmax<eps)" ---
          IF(.NOT.(hmax.LT.eps))GOTO 90006
          
90004   CONTINUE
        
C* 8: * 
Cline 198 "some.web"
        nmax=0
        CONTINUE
C ---  "do" ---
        DO 90015 nsym=1,nblock
          nmin=nmax+1
          nmax=nmax+iblock(nsym)
          call epsort(H,U,n,nmin,nmax)
90015   CONTINUE
        
        
C* :8 * 
Cline 84 "some.web"
        RETURN
        
      END
      
C* :1 * 
C* 9: * 
Cline 209 "some.web"
      subroutine epsort(H,U,n,n1,n2)
        
        
        implicit double precision(a-h,o-z)
        double precision H(*),U(*)
        integer n,n1,n2
        
        double precision temp
        integer iq,jq,ii,jj,k,ilr,imr
        
        iq=(n1-2)*n
        CONTINUE
C ---  "do" ---
        DO 90017 i=n1,n2
          iq=iq+n
          ii=(i-1)*n+i
          jq=n*(i-2)
          CONTINUE
C ---  "do" ---
          DO 90019 j=i,n2
            jq=jq+n
            jj=(j-1)*n+j
            CONTINUE
C ---  "if" ---
            IF(H(ii).LT.H(jj))THEN
              CONTINUE
C ---  "next" ---
              GOTO 90019
            ENDIF
            temp=H(ii)
            H(ii)=H(jj)
            H(jj)=temp
            CONTINUE
C ---  "do" ---
            DO 90021 k=1,n
              ilr=iq+k
              imr=jq+k
              temp=U(ilr)
              U(ilr)=U(imr)
              U(imr)=temp
90021       CONTINUE
90019     CONTINUE
90017   CONTINUE
        RETURN
        
      END
      
C* :9 * 
      
