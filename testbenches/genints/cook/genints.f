
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -TD genints.web"
C  RUN TIME:     "Friday, December 5, 2014 at 1:50."
C  WEB FILE:     "genints.web"
C  CHANGE FILE:  (none)
#line 11 "genints.web"
      subroutine genint(ngmx,nbfns,eta,ntype,ncntr,nfirst,nlast,vlist,nc
     &mx,noc,S,H,nfile,ifPSE)
        
        
        double precision S(*),H(*)
        integer pointer,last,ifPSE
        double precision eta(ngmx,5),vlist(ncmx,4)
        integer ntype(*),nfirst(*),nlast(*),ncntr(*),nfile
        
        double precision val,crit,ovltot,kintot
        double precision generi,genoei
        integer i,j,k,l,ltop,ij,ji,mu
        integer nr(20,3)
        data nr/0,1,0,0,2,0,0,1,1,0,3,0,0,2,2,1,0,1,0,1,0,0,1,0,0,2,0,1,
     &0,1,0,3,0,1,0,2,2,0,1,1,0,0,0,1,0,0,2,0,1,1,0,0,3,0,1,0,1,2,2,1/
        data crit/1.0d-08/
        
        mu=0
        
        
        
        
        CONTINUE
C ---  "do" ---
        DO 90000 i=1,nbfns
          CONTINUE
C ---  "do" ---
          DO 90002 j=1,i
            ij=(j-1)*nbfns+i
            ji=(i-1)*nbfns+j
            CONTINUE
C ---  "if" ---
            IF(ifPSE.EQ.1)THEN
              H(ij)=genoei(i,j,eta,ngmx,nfirst,nlast,ntype,nr,20,vlist,n
     &oc,ncmx,ovltot,kintot)
            ENDIF
            H(ji)=H(ij)
            S(ij)=ovltot
            S(ji)=ovltot
90002     CONTINUE
90000   CONTINUE
        
        
        rewind nfile
        pointer=0
        last=1
        
        CONTINUE
C ---  "do" ---
        DO 90004 i=1,nbfns
          
          CONTINUE
C ---  "do" ---
          DO 90006 j=1,i
            
            CONTINUE
C ---  "do" ---
            DO 90008 k=1,i
              
              ltop=k
              CONTINUE
C ---  "if" ---
              IF(i.EQ.k)THEN
                ltop=j
              ENDIF
              CONTINUE
C ---  "do" ---
              DO 90010 l=1,ltop
                
                CONTINUE
C ---  "if" ---
                IF(l.EQ.nbfns)THEN
                  last=0
                ENDIF
                val=generi(i,j,k,l,0,eta,ngmx,nfirst,nlast,ntype,nr,20)
                CONTINUE
C ---  "if" ---
                IF(dabs(val).LT.crit)THEN
                  CONTINUE
C ---  "next" ---
                  GOTO 90010
                ENDIF
                call putint(nfile,i,j,k,l,mu,val,pointer,last)
                
90010         CONTINUE
              
90008       CONTINUE
            
90006     CONTINUE
          
90004   CONTINUE
        
        RETURN
      END
      
      
      
      
C* :1 * 
      
