
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle gints.web"
C  RUN TIME:     "Sunday, December 7, 2014 at 0:37."
C  WEB FILE:     "gints.web"
C  CHANGE FILE:  (none)
      C* 3: * 
*line 11 "gints.web"
      
      
      
      integer function getint(file,i,j,k,l,mu,val,pointer)
      integer file,i,j,k,l,mu,pointer
      double precision val
      save
      integer max_pointer,id,iend
      double precision zero
      double precision labels(INT_BLOCK_SIZE),value(INT_BLOCK_SIZE)
      data max_pointer/0/,iend/NOT_LAST_BLOCK/,zero/0.0D00/
      
      if(pointer.EQ.max_pointer)then
      if(iend.EQ.LAST_BLOCK)then
      val=zero
      i=0
      j=0
      k=0
      l=0
      max_pointer=0
      iend=NOT_LAST_BLOCK
      getint=END_OF_FILE
      return
      end if
      read(file)max_pointer,iend,labels,value
      pointer=0
      end if
      pointer=pointer+1
      call unpack(labels(pointer),i,j,k,l,mu,id)
      val=value(pointer)
      getint=OK
      return
      end
C* :3 * 
C* 5: * 
*line 53 "gints.web"
      
      subroutine putint(nfile,i,j,k,l,mu,val,pointer,last)
      implicit double precision(a-h,o-z)
      save
      integer nfile,i,j,k,l,mu,pointer,last
      double precision labels(INT_BLOCK_SIZE),value(INT_BLOCK_SIZE)
      double precision val
      data max_pointer/INT_BLOCK_SIZE/,id/0/
      if(last.EQ.ERR)go to 100
      iend=NOT_LAST_BLOCK
      if(pointer.EQ.max_pointer)then
      write(nfile)pointer,iend,labels,value
      pointer=0
      end if
      pointer=pointer+1
      call pack(labels(pointer),i,j,k,l,mu,id)
      value(pointer)=val
      if(last.EQ.YES)then
      iend=LAST_BLOCK
      last=ERR
      write(nfile)pointer,iend,labels,value
      end if
100   return
      end
C* :5 * 
C* 7: * 
*line 87 "gints.web"
      
      subroutine genint(ngmx,nbfns,eta,ntype,ncntr,nfirst,nlast,vlist,nc
     &mx,noc,S,H,nfile)
      integer ngmx,nbfns,noc,ncmx
      double precision eta(MAX_PRIMITIVES,5),vlist(MAX_CENTRES,4)
      double precision S(ARB),H(ARB)
      integer ntype(ARB),nfirst(ARB),nlast(ARB),ncntr(ARB),nfile
      
      integer i,j,k,l,ltop,ij,ji,mu,m,n,jtyp,js,jf,ii,jj
      double precision generi,genoei
      integer pointer,last
      double precision ovltot,kintot
      double precision val,crit,alpha,t,t1,t2,t3,sum,pitern
      double precision SOO
      double precision gtoC(ngmx)
      double precision dfact(20)
      integer nr(NO_OF_TYPES,3)
      data nr/0,1,0,0,2,0,0,1,1,0,3,0,0,2,2,1,0,1,0,1,0,0,1,0,0,2,0,1,0,
     &1,0,3,0,1,0,2,2,0,1,1,0,0,0,1,0,0,2,0,1,1,0,0,3,0,1,0,1,2,2,1/
      data crit,half,onep5,one,zero/1.0D-08,0.5D+00,1.5D+00,1.0D+00,0.0D
     &+00/
      data dfact/1.0,3.0,15.0,105.0,945.0,10395.0,135135.0,2027025.0,12*
     &0.0/
      mu=0
C* 8: * 
*line 150 "gints.web"
      
      do i=1,ngmx
      gtoC(i)=eta(i,5)
      end do
C* :8 * 
*line 114 "gints.web"
      
C* 9: * 
*line 159 "gints.web"
      
      pitern=5.568327997D+00
      do j=1,nbfns
      jtyp=ntype(j)
      js=nfirst(j)
      jf=nlast(j)
      l=nr(jtyp,1)
      m=nr(jtyp,2)
      n=nr(jtyp,3)
      do i=js,jf
      alpha=eta(i,4)
      SOO=pitern*(half/alpha)**1.5
      t1=dfact(l)/alpha**l
      t2=dfact(m)/alpha**m
      t3=dfact(n)/alpha**n
      eta(i,5)=one/dsqrt(SOO*t1*t2*t3)
      end do
      end do
      
      do j=1,nbfns
      jtyp=ntype(j)
      js=nfirst(j)
      jf=nlast(j)
      l=nr(jtyp,1)
      m=nr(jtyp,2)
      n=nr(jtyp,3)
      sum=zero
      do ii=js,jf
      do jj=js,jf
      t=one/(eta(ii,4)+eta(jj,4))
      SOO=pitern*(t**onep5)*eta(ii,5)*eta(jj,5)
      t=half*t
      t1=dfact(l)/t**l
      t2=dfact(m)/t**m
      t3=dfact(n)/t**n
      sum=sum+gtoC(ii)*gtoC(jj)*SOO*t1*t2*t3
      end do
      end do
      sum=one/sqrt(sum)
      do ii=js,jf
      gtoC(ii)=gtoC(ii)*sum
      end do
      end do
      do ii=1,ngmx
      eta(ii,5)=eta(ii,5)*gtoC(ii)
      end do
C* :9 * 
*line 116 "gints.web"
      
      
      DO i=1,nbfns
      DO j=1,i
      ij=(j-1)*nbfns+i
      ji=(i-1)*nbfns+j
      H(ij)=genoei(i,j,eta,ngmx,nfirst,nlast,ntype,nr,NO_OF_TYPES,vlist,
     &noc,ncmx,ovltot,kintot)
      H(ji)=H(ij)
      S(ij)=ovltot
      S(ji)=ovltot
      END DO
      END DO
      write(*,*)" ONE ELECTRON INTEGRALS COMPUTED"
      
      rewind nfile
      pointer=0
      last=NO
      i=1
      j=1
      k=1
      l=0
      DO 10 WHILE(next_label(i,j,k,l,nbfns).EQ.YES)
      IF(l.EQ.nbfns)last=YES
      val=generi(i,j,k,l,0,eta,ngmx,nfirst,nlast,ntype,nr,NO_OF_TYPES)
      IF(dabs(val).LT.crit)go to 10
      CALL putint(nfile,i,j,k,l,mu,val,pointer,last)
10    CONTINUE
      
      return
      end
C* :7 * 
      
