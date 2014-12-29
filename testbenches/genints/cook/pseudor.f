
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -TD pseudor.web"
C  RUN TIME:     "Friday, December 5, 2014 at 1:47."
C  WEB FILE:     "pseudor.web"
C  CHANGE FILE:  (none)
      C* 1: * 
#line 19 "pseudor.web"
      
      double precision function genpse(i,j,eta,ngmx,nfirst,nlast,ntype,n
     &r,ntmx,vlist,noc,ncmx,ovltot,kintot)
        
        
        implicit double precision(a-h,o-z)
        integer i,j,ngmx,ncmx,noc,ntmx
        integer nfirst(*),nlast(*),ntype(*),nr(ntmx,3)
        double precision ovltot,kintot
        double precision eta(ngmx,5),vlist(ncmx,4)
        
        
        
        double precision Airu(10),Ajsv(10),Aktw(10)
        double precision p(3),sf(10,3),tf(20),ca(3),ba(3)
        double precision fact(20),g(50)
        double precision kin,tnai,totnai,tpse,totpse
        integer bigZ
        
        data zero,one,two,half,quart/0.0d00,1.0d00,2.0d00,0.5d00,0.25d00
     &/
        data pi/3.141592653589d00/
        
C* 2: * 
#line 155 "pseudor.web"
        data fact/1.0,1.0,2.0,6.0,24.0,120.0,720.0,5040.0,40320.0,362880
     &.0,3628800.0,39916800.0,479001600.0,6227020800.0,6*0.0/
        
        
C* :2 * 
#line 40 "pseudor.web"
C* 3: * 
#line 165 "pseudor.web"
        ityp=ntype(i)
        jtyp=ntype(j)
        l1=nr(ityp,1)
        m1=nr(ityp,2)
        n1=nr(ityp,3)
        l2=nr(jtyp,1)
        m2=nr(jtyp,2)
        n2=nr(jtyp,3)
        imax=l1+l2+1
        jmax=m1+m2+1
        kmax=n1+n2+1
        maxall=imax
        CONTINUE
C ---  "if" ---
        IF(maxall.LT.jmax)THEN
          maxall=jmax
        ENDIF
        CONTINUE
C ---  "if" ---
        IF(maxall.LT.kmax)THEN
          maxall=kmax
        ENDIF
        CONTINUE
C ---  "if" ---
        IF(maxall.LT.2)THEN
          maxall=2
        ENDIF
        iss=nfirst(i)
        il=nlast(i)
        jss=nfirst(j)
        jl=nlast(j)
        
C* :3 * 
#line 41 "pseudor.web"
        
        
        rAB=(eta(iss,1)-eta(jss,1))**2+(eta(iss,2)-eta(jss,2))**2+(eta(i
     &ss,3)-eta(jss,3))**2
        
        genoei=zero
        totpse=zero
        totnai=zero
        kintot=zero
        ovltot=zero
        
        CONTINUE
C ---  "for(irun=iss; irun<=il; irun=irun+1)" ---
        irun=iss
90000   IF(irun.LE.il)THEN
          
          CONTINUE
C ---  "for(jrun=jss; jrun<=jl; jrun=jrun+1)" ---
          jrun=jss
90003     IF(jrun.LE.jl)THEN
            
C* 4: * 
#line 181 "pseudor.web"
            aexp=eta(irun,4)
            anorm=eta(irun,5)
            bexp=eta(jrun,4)
            bnorm=eta(jrun,5)
            t1=aexp+bexp
            deleft=one/t1
            p(1)=(aexp*eta(irun,1)+bexp*eta(jrun,1))*deleft
            p(2)=(aexp*eta(irun,2)+bexp*eta(jrun,2))*deleft
            p(3)=(aexp*eta(irun,3)+bexp*eta(jrun,3))*deleft
            pax=p(1)-eta(irun,1)
            pay=p(2)-eta(irun,2)
            paz=p(3)-eta(irun,3)
            pbx=p(1)-eta(jrun,1)
            pby=p(2)-eta(jrun,2)
            pbz=p(3)-eta(jrun,3)
            prefa=exp(-aexp*bexp*rAB/t1)*pi*anorm*bnorm/t1
            
            
C* :4 * 
#line 60 "pseudor.web"
            
C* 6: * 
#line 201 "pseudor.web"
            prefa=two*prefa
            expab=exp(-aexp*bexp*rAB/t1)
            s00=(pi/t1)**1.5*expab
            dum=one
            tf(1)=one
            del=half/t1
            CONTINUE
C ---  "for(n=2; n<=maxall; n=n+1)" ---
            n=2
90006       IF(n.LE.maxall)THEN
              tf(n)=tf(n-1)*dum*del
              dum=dum+two
C --- Reinitialization of "for(n=2; n<=maxall; n=n+1)" ---
              n=n+1
              GOTO 90006
            ENDIF
            
            ox0=ovrlap(l1,l2,pax,pbx,tf)
            oy0=ovrlap(m1,m2,pay,pby,tf)
            oz0=ovrlap(n1,n2,paz,pbz,tf)
            ox2=ovrlap(l1,l2+2,pax,pbx,tf)
            oxm2=ovrlap(l1,l2-2,pax,pbx,tf)
            oy2=ovrlap(m1,m2+2,pay,pby,tf)
            oym2=ovrlap(m1,m2-2,pay,pby,tf)
            oz2=ovrlap(n1,n2+2,paz,pbz,tf)
            ozm2=ovrlap(n1,n2-2,paz,pbz,tf)
            ov0=ox0*oy0*oz0
            ovl=ov0*s00
            ov1=ox2*oy0*oz0
            ov4=oxm2*oy0*oz0
            ov2=ox0*oy2*oz0
            ov5=ox0*oym2*oz0
            ov3=ox0*oy0*oz2
            ov6=ox0*oy0*ozm2
            
            
            
C* :6 * 
#line 62 "pseudor.web"
            
            ovltot=ovltot+anorm*bnorm*ovl
            
C* 7: * 
#line 232 "pseudor.web"
            xl=dfloat(l2*(l2-1))
            xm=dfloat(m2*(m2-1))
            xn=dfloat(n2*(n2-1))
            xj=dfloat(2*(l2+m2+n2)+3)
            kin=s00*(bexp*(xj*ov0-two*bexp*(ov1+ov2+ov3))-half*(xl*ov4+x
     &m*ov5+xn*ov6))
            
C* :7 * 
#line 66 "pseudor.web"
            
            kintot=kintot+anorm*bnorm*kin
            
            tpse=zero
            tnai=zero
            
C* 8: * 
#line 241 "pseudor.web"
            m=imax+jmax+kmax-2
            CONTINUE
C ---  "for(n=1; n<=imax; n=n+1)" ---
            n=1
90009       IF(n.LE.imax)THEN
              sf(n,1)=fj(l1,l2,n-1,pax,pbx)
C --- Reinitialization of "for(n=1; n<=imax; n=n+1)" ---
              n=n+1
              GOTO 90009
            ENDIF
            CONTINUE
C ---  "for(n=1; n<=jmax; n=n+1)" ---
            n=1
90012       IF(n.LE.jmax)THEN
              sf(n,2)=fj(m1,m2,n-1,pay,pby)
C --- Reinitialization of "for(n=1; n<=jmax; n=n+1)" ---
              n=n+1
              GOTO 90012
            ENDIF
            CONTINUE
C ---  "for(n=1; n<=kmax; n=n+1)" ---
            n=1
90015       IF(n.LE.kmax)THEN
              sf(n,3)=fj(n1,n2,n-1,paz,pbz)
C --- Reinitialization of "for(n=1; n<=kmax; n=n+1)" ---
              n=n+1
              GOTO 90015
            ENDIF
            
            
C* :8 * 
#line 73 "pseudor.web"
            
            
            CONTINUE
C ---  "for(n=1; n<=noc; n=n+1)" ---
            n=1
90018       IF(n.LE.noc)THEN
              
              bigZ=vlist(n,4)+0.001d00
              CONTINUE
C ---  "do" ---
              DO 90021 kpse=1,3
                
                ca(kpse)=eta(irun,kpse)-vlist(n,kpse)
                ba(kpse)=eta(jrun,kpse)-vlist(n,kpse)
                
90021         CONTINUE
              
              pse=Vps(l1,m1,n1,aexp,ca,l2,m2,n2,bexp,ba,bigZ)
              
              
              tpse=tpse+pse
              
              pn=zero
              
              
C* 11: * 
#line 271 "pseudor.web"
              cpx=p(1)-vlist(n,1)
              cpy=p(2)-vlist(n,2)
              cpz=p(3)-vlist(n,3)
              pcsq=cpx*cpx+cpy*cpy+cpz*cpz
              
C* :11 * 
#line 101 "pseudor.web"
              
              t=t1*pcsq
              
              call auxg(m,t,g)
              
C* 10: * 
#line 256 "pseudor.web"
              epsi=quart/t1
              CONTINUE
C ---  "for(ii=1; ii<=10; ii=ii+1)" ---
              ii=1
90023         IF(ii.LE.10)THEN
                Airu(ii)=zero
                Ajsv(ii)=zero
                Aktw(ii)=zero
C --- Reinitialization of "for(ii=1; ii<=10; ii=ii+1)" ---
                ii=ii+1
                GOTO 90023
              ENDIF
              
              call aform(imax,sf,fact,cpx,epsi,Airu,1)
              call aform(jmax,sf,fact,cpy,epsi,Ajsv,2)
              call aform(kmax,sf,fact,cpz,epsi,Aktw,3)
              
              
              
C* :10 * 
#line 107 "pseudor.web"
              
              
              CONTINUE
C ---  "for(ii=1; ii<=imax; ii=ii+1)" ---
              ii=1
90026         IF(ii.LE.imax)THEN
                
                CONTINUE
C ---  "for(jj=1; jj<=jmax; jj=jj+1)" ---
                jj=1
90029           IF(jj.LE.jmax)THEN
                  
                  CONTINUE
C ---  "for(kk=1; kk<=kmax; kk=kk+1)" ---
                  kk=1
90032             IF(kk.LE.kmax)THEN
                    
                    nu=ii+jj+kk-2
                    pn=pn+Airu(ii)*Ajsv(jj)*Aktw(kk)*g(nu)
                    
C --- Reinitialization of "for(kk=1; kk<=kmax; kk=kk+1)" ---
                    kk=kk+1
                    GOTO 90032
                  ENDIF
                  
C --- Reinitialization of "for(jj=1; jj<=jmax; jj=jj+1)" ---
                  jj=jj+1
                  GOTO 90029
                ENDIF
                
C --- Reinitialization of "for(ii=1; ii<=imax; ii=ii+1)" ---
                ii=ii+1
                GOTO 90026
              ENDIF
              
              tnai=tnai-pn*float(bigZ)
              
C --- Reinitialization of "for(n=1; n<=noc; n=n+1)" ---
              n=n+1
              GOTO 90018
            ENDIF
            
            totnai=totnai+prefa*tnai
            totpse=totpse+anorm*bnorm*tpse
            
C --- Reinitialization of "for(jrun=jss; jrun<=jl; jrun=jrun+1)" ---
            jrun=jrun+1
            GOTO 90003
          ENDIF
          
C --- Reinitialization of "for(irun=iss; irun<=il; irun=irun+1)" ---
          irun=irun+1
          GOTO 90000
        ENDIF
        
        genpse=totnai+totpse+kintot
        
        
        
        RETURN
        
      END
      
      
C* :1 * 
C* 12: * 
#line 283 "pseudor.web"
      
      double precision function Vps(l1,m1,n1,alpha,ca,l2,m2,n2,beta,ba,b
     &igZ)
        
        
        implicit double precision(a-h,o-z)
        integer l1,l2,m1,m2,n1,n2,bigZ
        double precision alpha,beta,ca(*),ba(*)
        integer nupse(NU_DIM)
        integer nc(3),nb(3)
        integer kpsemx,lpsemx
        integer idparm,inparm
        integer doff,noff
        double precision dpse(DPSE_DIM),dzu(NU_DIM)
        double precision pse
        double precision fourpi
        double precision psepot
        
        data fourpi/12.56637061d00/,zero/0.0d00/
        
        
C* 13: * 
#line 390 "pseudor.web"
        data(dpse(i),i=1,LDIM1)/3.48672d00,0.49988d00,0.0d00,0.0d00,0.0d
     &00,-0.77469d00,0.99509d00,-0.02612d00,0.0d00,0.0d00,0.0d00,-0.2701
     &0d00,1.04649d00,-0.07501d00,0.0d00,0.0d00,0.0d00,-0.36886d00,1.077
     &85d00,-0.17140d00,0.0d00,0.0d00,0.0d00,-0.40843d00,1.09851d00,-0.3
     &3854d00,0.0d00,0.0d00,0.0d00,-0.43676d00,1.6477d00,45.0783d00,0.0d
     &00,0.0d00,0.0d00,-7.7907d00,1.12060d00,-0.98560d00,0.0d00,0.0d00,0
     &.0d00,-0.44625d00,1.12861d00,-1.55047d00,0.0d00,0.0d00,0.0d00,-0.4
     &6631d00/
        data(dpse(i),i=
C "! Identifier not allowed as binary operand:  left = "LDIM1" (548), right = "$
     &$ASCII" (1).  (Undefined WEB macro?).  (Output l. 376 in pseudor.f; near i
     &nput l. 390.)  "
        LDIM1+1,
C "! Identifier not allowed as binary operand:  left = "LDIM1" (548), right = "L
     &DIM2" (551).  (Undefined WEB macro?).  (Output l. 380 in pseudor.f; near i
     &nput l. 390.)  "
        LDIM1+LDIM2)/1.74854d00,-0.01388d00,5*0.0d00,1.46565d00,0.15319d
     &00,5*0.0d00,-2.83231d00,2.00073d00,-0.03023d00,5*0.0d00,1.40926d00
     &,-0.00871d00,5*0.0d00,-92.89133d00,2.16121d00,-0.06021d00,5*0.0d00
     &,1.40609d00,-0.02270d00,5*0.0d00,-0.93152d00,2.30683d00,-0.10463d0
     &0,5*0.0d00,1.61465d00,-0.04644d00,5*0.0d00,-0.18945d00,2.42266d00,
     &-0.16784d00,5*0.0d00,1.72241d00,-0.08401d00,5*0.0d00,-0.40202d00,2
     &.51686d00,-0.25672d00,5*0.0d00,1.79573d00,-0.13150d00,5*0.0d00,-0.
     &74309d00,2.60459d00,-0.37281d00,5*0.0d00,1.85329d00,-0.20197d00,5*
     &0.0d00,-0.98109d00,2.66818d00,-0.52659d00,5*0.0d00,1.90592d00,-0.2
     &9464d00,5*0.0d00,-1.35035d00/
        data(dpse(i),i=
C "! Identifier not allowed as binary operand:  left = "LDIM1" (548), right = "L
     &DIM2" (551).  (Undefined WEB macro?).  (Output l. 394 in pseudor.f; near i
     &nput l. 390.)  "
C "! Identifier not allowed as binary operand:  left = "LDIM1" (548), right = "L
     &DIM2" (551).  (Undefined WEB macro?).  (Output l. 398 in pseudor.f; near i
     &nput l. 390.)  "
        LDIM1+LDIM2+1,DPSE_DIM)/135*0.0d00,5.57207d00,-0.11685d00,0.0d00
     &,0.0d00,0.0d00,0.0d00,0.0d00,5.49578d00,0.23560d00,0.0d00,0.0d00,0
     &.0d00,0.0d00,0.0d00,-6.10669d00,5.75316d00,-0.13096d00,0.0d00,0.0d
     &00,0.0d00,0.0d00,0.0d00,5.98399d00,0.55456d00,0.0d00,0.0d00,0.0d00
     &,0.0d00,0.0d00,-6.12527d00,15*0.0d00/
        data dzu/1.04883d00,1.04883d00,1.40580d00,0.25784d00,0.25784d00,
     &0.96124d00,0.40255d00,0.40255d00,1.91861d00,0.57656d00,0.57656d00,
     &3.18301d00,0.77911d00,0.77911d00,4.76414d00,10.37387d00,10.37387d0
     &0,25.320084,1.26132d00,1.26132d00,8.17605d00,1.53611d00,1.53611d00
     &,10.74498d00,2*0.25753d00,2*0.50138d00,1.01908d00,2*0.30666d00,2*0
     &.27481d00,6.35656d00,2*34298d00,2*0.28766d00,0.85476d00,0.39512d00
     &,0.39512d00,0.40442d00,0.40442d00,0.25050d00,0.45424d00,0.45424d00
     &,0.49582d00,0.49582d00,0.56256d00,0.51644d00,0.51644d00,0.59819d00
     &,0.59819d00,1.13649d00,0.59299d00,0.59299d00,0.69783d00,0.69783d00
     &,2.00000d00,0.66212d00,0.66212d00,0.80903d00,0.80903d00,3.50000d00
     &,45*0.0d00,0.63800d00,0.638000d00,0.70978d00,0.70978d00,2.44040d00
     &,0.71270d00,0.71270d00,0.89496d00,0.89498d00,2.67294d00,5*0.0d00/
        data nupse/0,4,1,0,4,1,0,4,1,0,4,1,0,4,1,1,2,2,0,4,1,0,4,1,0,4,0
     &,4,1,0,4,0,4,1,0,4,0,4,1,0,4,0,4,1,0,4,0,4,1,0,4,0,4,1,0,4,0,4,1,0
     &,4,0,4,1,0,4,0,4,1,0,4,0,4,1,0,4,0,4,1,0,4,0,4,1,0,4,0,4,1,0,4,0,4
     &,1,0,4,0,4,1,0,4,0,4,1,0,4,0,4,1,0,4,0,4,1,0,4,0,4,1,0,4,0,4,1/
        
        
        
        
        
        
        
        
C* 13: * 
#line 489 "pseudor.web"
        
C* :13 * 
#line 306 "pseudor.web"
        
        nc(1)=l1
        nc(2)=m1
        nc(3)=n1
        nb(1)=l2
        nb(2)=m2
        nb(3)=n2
        doff=1
        
        noff=1
        
        
        kpsemx=FIRST_ROW_KMAX
        lpsemx=FIRST_ROW_LMAX
        CONTINUE
C ---  "if" ---
        IF((NEON.LT.bigZ).AND.(bigZ.LE.ARGON))THEN
          
          
          doff=NUMBER_OF_FIRST_ROW*FIRST_ROW_KMAX*(FIRST_ROW_LMAX+1)+1
          noff=NUMBER_OF_FIRST_ROW*FIRST_ROW_KMAX+1
          
          
          kpsemx=SECOND_ROW_KMAX
          lpsemx=SECOND_ROW_LMAX
          
        ENDIF
        CONTINUE
C ---  "if" ---
        IF((ARGON.LT.bigZ).AND.(bigZ.LE.ZINC))THEN
          
          
          doff=NUMBER_OF_FIRST_ROW*FIRST_ROW_KMAX*(FIRST_ROW_LMAX+1)+NUM
     &BER_OF_SECOND_ROW*SECOND_ROW_KMAX*(SECOND_ROW_LMAX+1)+1
          
          noff=NUMBER_OF_FIRST_ROW*FIRST_ROW_KMAX+NUMBER_OF_SECOND_ROW*S
     &ECOND_ROW_KMAX+1
          
          
          kpsemx=THIRD_ROW_KMAX
          lpsemx=THIRD_ROW_LMAX
          
        ENDIF
        pse=zero
        
        CONTINUE
C ---  "if" ---
        IF(bigZ.LE.HELIUM)THEN
          CONTINUE
C ---  "return (pse)" ---
          Vps=(pse)
          RETURN
        ENDIF
        CONTINUE
C ---  "if" ---
        IF((HELIUM.LT.bigZ).AND.(bigZ.LE.NEON))THEN
          bigZ=bigZ-HELIUM
        ENDIF
        CONTINUE
C ---  "if" ---
        IF((NEON.LT.bigZ).AND.(bigZ.LE.ARGON))THEN
          bigZ=bigZ-NEON
        ENDIF
        CONTINUE
C ---  "if" ---
        IF((ARGON.LT.bigZ).AND.(bigZ.LE.ZINC))THEN
          bigZ=bigZ-ARGON
        ENDIF
        CONTINUE
C ---  "if" ---
        IF(bigZ.GT.ZINC)THEN
          write(6,200)
          STOP
        ENDIF
200     format(" No Pseudo potential for this atom")
        idparm=(bigZ-1)*kpsemx*(lpsemx+1)+doff
        inparm=(bigZ-1)*kpsemx+noff
        pse=psepot(nc,alpha,ca,nb,beta,ba,dzu(inparm),dpse(idparm),nupse
     &(inparm),kpsemx,lpsemx)
        pse=pse*fourpi
        CONTINUE
C ---  "return (pse)" ---
        Vps=(pse)
        RETURN
        
      END
      
      
      
      
C* :12 * 
C* 14: * 
#line 504 "pseudor.web"
      
      double precision function psepot(nc,zc,ca,nb,zb,ba,zu,Td,nu,kmax,l
     &max)
        
        
        implicit double precision(a-h,o-z)
        integer nc(*),nb(*),nu(*),kmax,lmax
        double precision zc,zb,ca(*),ba(*),zu(*),Td(*)
        
        double precision r1,r11,r
        integer rinit,rl,rl1
        
C* 15: * 
#line 542 "pseudor.web"
        logical log,logc,logb,lgo,lgl(252),lgl1(233)
        double precision tpic(9,4,6),tpib(9,4,6),tpc(9),tpb(189),ttpp(18
     &9)
        double precision a(2)
        double precision zero,quarter,half,one,two
        common/cocapi/lgo
        common/in1/lc,lc1,lc2
        common/c0/tzcb,td1,td2,tg1,tg2,tg3,a,ts,ncb,jj,imax,log
        equivalence(tpc(1),tpic(1,1,1),ttpp(40)),(tpb(1),tpib(1,1,1),t0)
        data zero/0.0d00/,one/1.0d00/,two/2.0d00/
        data quarter,half/0.25d00,0.5d00/
        
C* :15 * 
#line 515 "pseudor.web"
        
C* 16: * 
#line 558 "pseudor.web"
        lc=lmax+1
        lc1=lc+1
        lc2=lc*lc
        call capi(nc,ncn,ca,tca,tca2,lamx1,tpic,logc)
        CONTINUE
C ---  "if" ---
        IF(.NOT.lgo)THEN
          CONTINUE
C ---  "return (zero)" ---
          psepot=(zero)
          RETURN
        ENDIF
        CONTINUE
C ---  "if" ---
        IF(logc)THEN
          CONTINUE
C ---  "for(lm=1; lm<=lc2; lm=lm+1)" ---
          lm=1
90035     IF(lm.LE.lc2)THEN
            tpb(lm)=tpc(lm)
C --- Reinitialization of "for(lm=1; lm<=lc2; lm=lm+1)" ---
            lm=lm+1
            GOTO 90035
          ENDIF
          call capi(nb,nbn,ba,tba,tba2,lamx1,tpic,logb)
        ELSE
          call capi(nb,nbn,ba,tba,tba2,la1mx1,tpib,logb)
        ENDIF
        CONTINUE
C ---  "if" ---
        IF(.NOT.lgo)THEN
          CONTINUE
C ---  "return (zero)" ---
          psepot=(zero)
          RETURN
C* :16 * 
#line 517 "pseudor.web"
        ENDIF
        tzcb=zc+zb
        ncb=ncn+nbn
        CONTINUE
C ---  "if" ---
        IF(logc.AND.logb)THEN
C* 17: * 
#line 576 "pseudor.web"
          t0=t0*tpc(1)
          CONTINUE
C ---  "if" ---
          IF(lc.GT.1)THEN
            lm2=1
            CONTINUE
C ---  "for(l=2; l<=lc; l=l+1)" ---
            l=2
90038       IF(l.LE.lc)THEN
              lm1=lm2+1
              lm2=l*l
              t2=zero
              CONTINUE
C ---  "for(lm=lm1; lm<=lm2; lm=lm+1)" ---
              lm=lm1
90041         IF(lm.LE.lm2)THEN
                t2=t2+tpc(lm)*tpb(lm)
C --- Reinitialization of "for(lm=lm1; lm<=lm2; lm=lm+1)" ---
                lm=lm+1
                GOTO 90041
              ENDIF
              tpb(l)=t2
C --- Reinitialization of "for(l=2; l<=lc; l=l+1)" ---
              l=l+1
              GOTO 90038
            ENDIF
          ENDIF
          t2=zero
          CONTINUE
C ---  "for(k=1; k<=kmax; k=k+1)" ---
          k=1
90044     IF(k.LE.kmax)THEN
            t1=zero
            CONTINUE
C ---  "for(l=1; l<=lc; l=l+1)" ---
            l=1
90047       IF(l.LE.lc)THEN
              t1=t1+tpb(l)*td((l-1)*kmax+k)
C --- Reinitialization of "for(l=1; l<=lc; l=l+1)" ---
              l=l+1
              GOTO 90047
            ENDIF
            CONTINUE
C ---  "if" ---
            IF(t1.EQ.zero)THEN
              CONTINUE
C ---  "next" ---
              GOTO 90045
            ENDIF
            t2=t2+r11(zu(k),nu(k))*t1
90045       CONTINUE
C --- Reinitialization of "for(k=1; k<=kmax; k=k+1)" ---
            k=k+1
            GOTO 90044
          ENDIF
          psepot=t2
          RETURN
          
C* :17 * 
#line 524 "pseudor.web"
        ENDIF
        log=logc.OR.logb
        td1=two*tca*zc
        td2=two*tba*zb
        t2=zc*zb
        tg1=t2*(tba-tca)**2
        tg2=zb*tba2+zc*tca2
        a(1)=td1+td2
        CONTINUE
C ---  "if" ---
        IF(log)THEN
C* 18: * 
#line 608 "pseudor.web"
          
          
          CONTINUE
C ---  "if" ---
          IF(.NOT.logb)THEN
            n1=ncn
            ncn=nbn
            nbn=n1
          ENDIF
          jj=1
          imax=lamx1+ncn
          nc1=ncn+1
          i=1
          ii=1
          iii=0
          CONTINUE
C ---  "for(la=1; la<=lamx1; la=la+1)" ---
          la=1
90050     IF(la.LE.lamx1)THEN
            l1=max0(1,la-ncn)
            CONTINUE
C ---  "for(ka=1; ka<=nc1; ka=ka+1)" ---
            ka=1
90053       IF(ka.LE.nc1)THEN
              lm2=(l1-1)**2
              lgl(i)=.TRUE.
              irl=lc1-l1
              CONTINUE
C ---  "for(l=l1; l<=lc; l=l+1)" ---
              l=l1
90056         IF(l.LE.lc)THEN
                lm1=lm2+1
                lm2=l*l
                t1=zero
                CONTINUE
C ---  "for(lm=lm1; lm<=lm2; lm=lm+1)" ---
                lm=lm1
90059           IF(lm.LE.lm2)THEN
                  t1=t1+tpb(lm)*tpic(lm,ka,la)
C --- Reinitialization of "for(lm=lm1; lm<=lm2; lm=lm+1)" ---
                  lm=lm+1
                  GOTO 90059
                ENDIF
                lgl1(ii)=t1.EQ.zero
                CONTINUE
C ---  "if" ---
                IF(lgl1(ii))THEN
                  ii=ii+1
                  CONTINUE
C ---  "next" ---
                  GOTO 90057
                ENDIF
                lgl(i)=.FALSE.
                iii=iii+1
                ttpp(iii)=t1
                ii=ii+1
90057           CONTINUE
C --- Reinitialization of "for(l=l1; l<=lc; l=l+1)" ---
                l=l+1
                GOTO 90056
              ENDIF
              CONTINUE
C ---  "if" ---
              IF(lgl(i))THEN
                ii=ii-irl
              ENDIF
              i=i+1
C --- Reinitialization of "for(ka=1; ka<=nc1; ka=ka+1)" ---
              ka=ka+1
              GOTO 90053
            ENDIF
C --- Reinitialization of "for(la=1; la<=lamx1; la=la+1)" ---
            la=la+1
            GOTO 90050
          ENDIF
          CONTINUE
C ---  "if" ---
          IF(iii.EQ.0)THEN
            CONTINUE
C ---  "return (zero)" ---
            psepot=(zero)
            RETURN
          ENDIF
          t0=zero
          CONTINUE
C ---  "for(k=1; k<=kmax; k=k+1)" ---
          k=1
90062     IF(k.LE.kmax)THEN
            nu1=rinit(zu(k),nu(k))+nbn
            i=1
            ii=1
            iii=0
            CONTINUE
C ---  "for(la=1; la<=lamx1; la=la+1)" ---
            la=1
90065       IF(la.LE.lamx1)THEN
              l1=max0(1,la-ncn)
              irl=rl1(la)
              t1=zero
              CONTINUE
C ---  "for(ka=1; ka<=nc1; ka=ka+1)" ---
              ka=1
90068         IF(ka.LE.nc1)THEN
                CONTINUE
C ---  "if" ---
                IF(lgl(i))THEN
                  i=i+1
                  CONTINUE
C ---  "next" ---
                  GOTO 90069
                ENDIF
                t2=zero
                CONTINUE
C ---  "for(l=l1; l<=lc; l=l+1)" ---
                l=l1
90071           IF(l.LE.lc)THEN
                  CONTINUE
C ---  "if" ---
                  IF(lgl1(ii))THEN
                    ii=ii+1
                    CONTINUE
C ---  "next" ---
                    GOTO 90072
                  ENDIF
                  iii=iii+1
                  t2=t2+td((l-1)*kmax+k)*ttpp(iii)
                  ii=ii+1
90072             CONTINUE
C --- Reinitialization of "for(l=l1; l<=lc; l=l+1)" ---
                  l=l+1
                  GOTO 90071
                ENDIF
                CONTINUE
C ---  "if" ---
                IF(t2.EQ.zero)THEN
                  i=i+1
                  CONTINUE
C ---  "next" ---
                  GOTO 90069
                ENDIF
                t1=t1+t2*r1(nu1+ka,la)
                i=i+1
90069           CONTINUE
C --- Reinitialization of "for(ka=1; ka<=nc1; ka=ka+1)" ---
                ka=ka+1
                GOTO 90068
              ENDIF
              t0=t0+t1*irl
C --- Reinitialization of "for(la=1; la<=lamx1; la=la+1)" ---
              la=la+1
              GOTO 90065
            ENDIF
C --- Reinitialization of "for(k=1; k<=kmax; k=k+1)" ---
            k=k+1
            GOTO 90062
          ENDIF
          psepot=t0*half
          CONTINUE
C ---  "return (psepot)" ---
          psepot=(psepot)
          RETURN
          
C* :18 * 
#line 534 "pseudor.web"
C* 19: * 
#line 689 "pseudor.web"
        ENDIF
        tg3=t2*(tba+tca)**2
        t1=td1-td2
        ts=dsign(one,t1)
        a(2)=ts*t1
        imax=2*(ncb+lc)-1
        jj=2
        nmax=ncb+1
        nc1=ncn+1
        i=1
        ii=1
        iii=0
        CONTINUE
C ---  "for(la=1; la<=lamx1; la=la+1)" ---
        la=1
90074   IF(la.LE.lamx1)THEN
          l2=max0(1,la-ncn)
          CONTINUE
C ---  "for(la1=1; la1<=la1mx1; la1=la1+1)" ---
          la1=1
90077     IF(la1.LE.la1mx1)THEN
            l1=max0(l2,la1-nbn)
            irl=lc1-l1
            CONTINUE
C ---  "for(n=1; n<=nmax; n=n+1)" ---
            n=1
90080       IF(n.LE.nmax)THEN
              n1=n+1
              kamin=max0(1,n-nbn)
              kamax=min0(nc1,n)
              lgl(i)=.TRUE.
              lm2=(l1-1)**2
              CONTINUE
C ---  "for(l=l1; l<=lc; l=l+1)" ---
              l=l1
90083         IF(l.LE.lc)THEN
                lm1=lm2+1
                lm2=l*l
                t1=zero
                CONTINUE
C ---  "for(lm=lm1; lm<=lm2; lm=lm+1)" ---
                lm=lm1
90086           IF(lm.LE.lm2)THEN
                  CONTINUE
C ---  "for(ka=kamin; ka<=kamax; ka=ka+1)" ---
                  ka=kamin
90089             IF(ka.LE.kamax)THEN
                    n1ka=n1-ka
                    t1=t1+tpic(lm,ka,la)*tpib(lm,n1ka,la1)
C --- Reinitialization of "for(ka=kamin; ka<=kamax; ka=ka+1)" ---
                    ka=ka+1
                    GOTO 90089
                  ENDIF
C --- Reinitialization of "for(lm=lm1; lm<=lm2; lm=lm+1)" ---
                  lm=lm+1
                  GOTO 90086
                ENDIF
                lgl1(ii)=t1.EQ.zero
                CONTINUE
C ---  "if" ---
                IF(lgl1(ii))THEN
                  ii=ii+1
                  CONTINUE
C ---  "next" ---
                  GOTO 90084
                ENDIF
                lgl(i)=.FALSE.
                iii=iii+1
                ttpp(iii)=t1
                ii=ii+1
90084           CONTINUE
C --- Reinitialization of "for(l=l1; l<=lc; l=l+1)" ---
                l=l+1
                GOTO 90083
              ENDIF
              CONTINUE
C ---  "if" ---
              IF(lgl(i))THEN
                ii=ii-irl
              ENDIF
              i=i+1
C --- Reinitialization of "for(n=1; n<=nmax; n=n+1)" ---
              n=n+1
              GOTO 90080
            ENDIF
C --- Reinitialization of "for(la1=1; la1<=la1mx1; la1=la1+1)" ---
            la1=la1+1
            GOTO 90077
          ENDIF
C --- Reinitialization of "for(la=1; la<=lamx1; la=la+1)" ---
          la=la+1
          GOTO 90074
        ENDIF
        CONTINUE
C ---  "if" ---
        IF(iii.EQ.0)THEN
          CONTINUE
C ---  "return (zero)" ---
          psepot=(zero)
          RETURN
        ENDIF
        CONTINUE
C ---  "for(i=1; i<=iii; i=i+1)" ---
        i=1
90092   IF(i.LE.iii)THEN
          tpb(i)=zero
C --- Reinitialization of "for(i=1; i<=iii; i=i+1)" ---
          i=i+1
          GOTO 90092
        ENDIF
        CONTINUE
C ---  "for(k=1; k<=kmax; k=k+1)" ---
        k=1
90095   IF(k.LE.kmax)THEN
          nu1=rinit(zu(k),nu(k))
          i=1
          ii=1
          iii=0
          CONTINUE
C ---  "for(la=1; la<=lamx1; la=la+1)" ---
          la=1
90098     IF(la.LE.lamx1)THEN
            l2=max0(1,la-ncn)
            CONTINUE
C ---  "for(la1=1; la1<=la1mx1; la1=la1+1)" ---
            la1=1
90101       IF(la1.LE.la1mx1)THEN
              l1=max0(l2,la1-nbn)
              irl=rl(la,la1)
              CONTINUE
C ---  "for(n=1; n<=nmax; n=n+1)" ---
              n=1
90104         IF(n.LE.nmax)THEN
                CONTINUE
C ---  "if" ---
                IF(lgl(i))THEN
                  i=i+1
                  CONTINUE
C ---  "next" ---
                  GOTO 90105
                ENDIF
                t1=r(nu1+n)*irl
                CONTINUE
C ---  "for(l=l1; l<=lc; l=l+1)" ---
                l=l1
90107           IF(l.LE.lc)THEN
                  CONTINUE
C ---  "if" ---
                  IF(lgl1(ii))THEN
                    ii=ii+1
                    CONTINUE
C ---  "next" ---
                    GOTO 90108
                  ENDIF
                  iii=iii+1
                  tpb(iii)=tpb(iii)+t1*td((l-1)*kmax+k)
                  ii=ii+1
90108             CONTINUE
C --- Reinitialization of "for(l=l1; l<=lc; l=l+1)" ---
                  l=l+1
                  GOTO 90107
                ENDIF
                i=i+1
90105           CONTINUE
C --- Reinitialization of "for(n=1; n<=nmax; n=n+1)" ---
                n=n+1
                GOTO 90104
              ENDIF
C --- Reinitialization of "for(la1=1; la1<=la1mx1; la1=la1+1)" ---
              la1=la1+1
              GOTO 90101
            ENDIF
C --- Reinitialization of "for(la=1; la<=lamx1; la=la+1)" ---
            la=la+1
            GOTO 90098
          ENDIF
C --- Reinitialization of "for(k=1; k<=kmax; k=k+1)" ---
          k=k+1
          GOTO 90095
        ENDIF
        t1=zero
        CONTINUE
C ---  "for(i=1; i<=iii; i=i+1)" ---
        i=1
90110   IF(i.LE.iii)THEN
          t1=t1+ttpp(i)*tpb(i)
C --- Reinitialization of "for(i=1; i<=iii; i=i+1)" ---
          i=i+1
          GOTO 90110
        ENDIF
        psepot=t1*quarter
        CONTINUE
C ---  "return (psepot)" ---
        psepot=(psepot)
        RETURN
        
        
C* :19 * 
#line 537 "pseudor.web"
      END
      
C* :14 * 
C* 20: * 
#line 781 "pseudor.web"
      
      double precision function derf(x)
        
        
        implicit double precision(a-h,o-z)
        double precision x
        
        data one/1.0d00/,p/0.3275911d00/
        data a1,a2,a3,a4,a5/0.254829592,-0.284496736d00,1.421413741d00,-
     &1.453152027d00,1.061405429d00/
        xx=dabs(x)
        t=one/(one+p*xx)
        ee=dexp(-xx*xx)
        derf=one-(a1*t+a2*t*t+a3*t*t*t+a4*t*t*t*t+a5*t*t*t*t*t)*ee
        RETURN
      END
      
C* :20 * 
      
