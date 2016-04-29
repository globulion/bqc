
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle fmch.web"
C  RUN TIME:     "Friday, December 5, 2014 at 1:41."
C  WEB FILE:     "fmch.web"
C  CHANGE FILE:  (none)
#line 112 "fmch.web"
      
      double precision function fmch(nu,x,y)
        
        
        
        implicit double precision(a-h,o-z)
        double precision x,y
        integer nu
        
        
        
        
        
C* 2: * 
#line 158 "fmch.web"
        double precision ten,half,one,zero,rootpi4,xd,crit
        double precision term,partialsum
        integer m,i,numberofterms,maxone,maxtwo
        data zero,half,one,rootpi4,ten/0.0,0.5,1.0,0.88622692,10.0/
        
        
        data crit/1.0e-08/
        
        
        
        
        data maxone/50/,maxtwo/200/
        
C* :2 * 
#line 125 "fmch.web"
        
        
        m=nu
        a=dble(float(m))
        
        
        CONTINUE
C ---  "if" ---
        IF(x.LE.ten)THEN
          
          
C* 3: * 
#line 188 "fmch.web"
          a=a+half
          term=one/a
          
          
          partialsum=term
          CONTINUE
C ---  "for(i=2; i<=maxone; i=i+1)" ---
          i=2
90000     IF(i.LE.maxone)THEN
            a=a+one
            term=term*x/a
            partialsum=partialsum+term
            CONTINUE
C ---  "if" ---
            IF(term/partialsum.LT.crit)THEN
              CONTINUE
C ---  "break" ---
              GOTO 90002
            ENDIF
C --- Reinitialization of "for(i=2; i<=maxone; i=i+1)" ---
            i=i+1
            GOTO 90000
          ENDIF
90002     CONTINUE
          
          
          CONTINUE
C ---  "if" ---
          IF(i.EQ.maxone)THEN
            write(6,200)
200          format(' i > 50 in fmch')
            STOP
          ENDIF
          CONTINUE
C ---  "return (half*partialsum*y)" ---
          fmch=(half*partialsum*y)
          RETURN
          
C* :3 * 
#line 140 "fmch.web"
          
        ELSE
          
          
C* 4: * 
#line 227 "fmch.web"
          
          b=a+half
          a=a-half
          
          xd=one/x
          approx=rootpi4*dsqrt(xd)*xd**m
          CONTINUE
C ---  "if" ---
          IF(m.GT.0)THEN
            CONTINUE
C ---  "for(i=1; i<=m; i=i+1)" ---
            i=1
90003       IF(i.LE.m)THEN
              b=b-one
              approx=approx*b
C --- Reinitialization of "for(i=1; i<=m; i=i+1)" ---
              i=i+1
              GOTO 90003
            ENDIF
          ENDIF
          fimult=half*y*xd
          partialsum=zero
          
          
          CONTINUE
C ---  "if" ---
          IF(fimult.EQ.zero)THEN
            CONTINUE
C ---  "return (approx)" ---
            fmch=(approx)
            RETURN
          ENDIF
          fiprop=fimult/approx
          term=one
          partialsum=term
          numberofterms=maxtwo
          CONTINUE
C ---  "for(i=2; i<=numberofterms; i=i+1)" ---
          i=2
90006     IF(i.LE.numberofterms)THEN
            term=term*a*xd
            partialsum=partialsum+term
            
            
            CONTINUE
C ---  "if" ---
            IF(dabs(term*fiprop/partialsum).LE.crit)THEN
              CONTINUE
C ---  "return (approx-fimult*partialsum)" ---
              fmch=(approx-fimult*partialsum)
              RETURN
            ENDIF
            a=a-one
C --- Reinitialization of "for(i=2; i<=numberofterms; i=i+1)" ---
            i=i+1
            GOTO 90006
          ENDIF
          
          
          
          write(6,201)
201        format('  numberofterms reached in fmch')
          STOP
        ENDIF
        
C* :4 * 
#line 150 "fmch.web"
        
      END
      
C* :1 * 
      
