
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle utils.web"
C  RUN TIME:     "Friday, December 5, 2014 at 1:38."
C  WEB FILE:     "utils.web"
C  CHANGE FILE:  (none)
#line 12 "utils.web"
      subroutine pack(a,i,j,k,l,m,n)
        
        
        character*8 a,b
        integer i,j,k,l,m,n
        
        data b/'        '/
        a=b
        a(1:1)=char(i)
        a(2:2)=char(j)
        a(3:3)=char(k)
        a(4:4)=char(l)
        a(5:5)=char(m)
        a(6:6)=char(n)
        RETURN
      END
      
C* :1 * 
C* 2: * 
#line 28 "utils.web"
      subroutine unpack(a,i,j,k,l,m,n)
        
        
        character*8 a
        integer i,j,k,l,m,n
        
        i=ichar(a(1:1))
        j=ichar(a(2:2))
        k=ichar(a(3:3))
        l=ichar(a(4:4))
        m=ichar(a(5:5))
        n=ichar(a(6:6))
        RETURN
      END
      
C* :2 * 
C* 3: * 
#line 41 "utils.web"
      subroutine putint(nfile,i,j,k,l,mu,val,pointer,last)
        
        
        implicit double precision(a-h,o-z)
        save
        integer nfile,i,j,k,l,mu,pointer,last
        double precision val
        
        double precision value(20)
        character*8 labels(20)
        data maxpointer/20/,id/0/
        CONTINUE
C ---  "if" ---
        IF(last.EQ.-1)THEN
          RETURN
        ENDIF
        iend=0
        CONTINUE
C ---  "if" ---
        IF(pointer.EQ.maxpointer)THEN
          
          write(nfile)pointer,iend,labels,value
          pointer=0
          
        ENDIF
        pointer=pointer+1
        call pack(labels(pointer),i,j,k,l,mu,id)
        value(pointer)=val
        CONTINUE
C ---  "if" ---
        IF(last.EQ.0)THEN
          
          iend=1
          last=-1
          write(nfile)pointer,iend,labels,value
          
        ENDIF
        RETURN
        
      END
      
C* :3 * 
C* 4: * 
#line 81 "utils.web"
      integer function getint(file,i,j,k,l,mu,val,pointer)
        
        
        
        integer file,i,j,k,l,mu,pointer
        double precision val
        
        
        
        save
        
        integer maxpointer,id,iend
        double precision zero
        double precision value(20)
        character*8 labels(20)
        data maxpointer/0/,iend/0/,zero/0.0d00/
        
        
        
        CONTINUE
C ---  "if" ---
        IF(pointer.EQ.maxpointer)THEN
          
          CONTINUE
C ---  "if" ---
          IF(iend.EQ.1)THEN
            
            val=zero
            i=0
            j=0
            k=0
            l=0
            maxpointer=0
            iend=0
            CONTINUE
C ---  "return (-1)" ---
            getint=(-1)
            RETURN
            
          ENDIF
          read(file)maxpointer,iend,labels,value
          
          pointer=0
          
        ENDIF
        pointer=pointer+1
        call unpack(labels(pointer),i,j,k,l,mu,id)
        val=value(pointer)
        CONTINUE
C ---  "return (OK)" ---
        getint=(OK)
        RETURN
        
      END
      
C* :4 * 
      
