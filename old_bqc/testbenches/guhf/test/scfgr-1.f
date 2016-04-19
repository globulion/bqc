
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle scfgr-1.web"
C  RUN TIME:     "Thursday, December 4, 2014 at 15:37."
C  WEB FILE:     "scfgr-1.web"
C  CHANGE FILE:  (none)
Cline 21 "scfgr-1.web"
      subroutine scfGR(R,G,n,nfile,ntype)
        
        
        
        
        implicit double precision(a-h,o-z)
        double precision R(*),G(*)
        integer n,nfile,ntype
        
        
        
        integer pointer,spin,skip
        integer getint
        data zero,one,two/0.0d0,1.0d0,2.0d0/
        
        rewind nfile
        pointer=0
        
C* 2: * 
Cline 65 "scfgr-1.web"
        CONTINUE
C ---  "if" ---
        IF(ntype.EQ.145)THEN
          
          n2=n
          em=two
          en=one
          
        ENDIF
        CONTINUE
C ---  "if" ---
        IF(ntype.EQ.2345)THEN
          n2=2*n
          em=one
          en=one
          
C* :2 * 
Cline 38 "scfgr-1.web"
        ENDIF
        CONTINUE
C ---  "while(getint(nfile,is,js,ks,ls,mu,va,pointer)!=1)" ---
90000   IF(getint(nfile,is,js,ks,ls,mu,va,pointer).NE.1)THEN
          
          ijs=is*(is-1)/2+js
          kls=ks*(ks-1)/2+ls
          
C* 3: * 
Cline 100 "scfgr-1.web"
          CONTINUE
C ---  "do" ---
          DO 90002 spin=1,4
            
            
            CONTINUE
C ---  "if" ---
            IF((spin.GT.1).AND.(ntype.EQ.145))THEN
              CONTINUE
C ---  "break" ---
              GOTO 90003
            ENDIF
            skip=NO
            
            
            CONTINUE
C ---  "switch(spin)" ---
            GOTO(90005,90006,90007,90008),(spin)-(0)
            GOTO 90004
C ---  "case 1:" ---
90005       CONTINUE
              i=is
              j=js
              k=ks
              l=ls
              CONTINUE
C ---  "break" ---
              GOTO 90004
              
C ---  "case 2:" ---
90006       CONTINUE
              i=is+n
              j=js+n
              k=ks+n
              l=ls+n
              CONTINUE
C ---  "break" ---
              GOTO 90004
              
C ---  "case 3:" ---
90007       CONTINUE
              i=is+n
              j=js+n
              k=ks
              l=ls
              CONTINUE
C ---  "break" ---
              GOTO 90004
              
C ---  "case 4:" ---
90008       CONTINUE
              
              
              CONTINUE
C ---  "if" ---
              IF(ijs.EQ.kls)THEN
                skip=38
              ENDIF
              i=is
              j=js
              k=ks+n
              l=ls+n
              call order(i,j,k,l)
              CONTINUE
C ---  "break" ---
              GOTO 90004
              
              
90004       CONTINUE
            
            CONTINUE
C ---  "if" ---
            IF(skip.EQ.38)THEN
              CONTINUE
C ---  "next" ---
              GOTO 90002
            ENDIF
            emm=em
            enn=en
            
            
            
            CONTINUE
C ---  "if" ---
            IF(spin.GE.3)THEN
              enn=zero
            ENDIF
            call GofR(R,G,n2,emm,enn,i,j,k,l,va)
            
90002     CONTINUE
90003     CONTINUE
          
C* :3 * 
Cline 48 "scfgr-1.web"
          
          GOTO 90000
        ENDIF
        
C* 4: * 
Cline 153 "scfgr-1.web"
        CONTINUE
C ---  "do" ---
        DO 90009 i=1,n2
          
          CONTINUE
C ---  "do" ---
          DO 90011 j=1,i
            
            ij=n2*(j-1)+i
            ji=n2*(i-1)+j
            CONTINUE
C ---  "if" ---
            IF(i.EQ.j)THEN
              CONTINUE
C ---  "next" ---
              GOTO 90011
            ENDIF
            G(ji)=G(ij)
            
90011     CONTINUE
90009   CONTINUE
        
        
        
C* :4 * 
Cline 52 "scfgr-1.web"
        
        RETURN
        
      END
      
C* :1 * 
      
