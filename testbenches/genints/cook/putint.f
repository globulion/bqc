      subroutine putint(nfile, i, j, k, l, mu, val, pointer, last)
      implicit double precision(a-h,o-z)
      save

      integer nfile, i, j, k, l, mu, pointer, last
      double precision labels(INT_BLOCK_SIZE), value(INT_BLOCK_SIZE)
      double precision val
      data max_pointer/INT_BLOCK_SIZE/, id/0/
C
C     id is now unused
C
      if (last.EQ.ERR) go to 100
      iend = NOT_LAST_BLOCK
      if (pointer.EQ.max_pointer) then
          write(nfile) pointer, iend, labels, value
          pointer = 0
      end if
      pointer = pointer + 1
      call pack(labels(pointer),i,j,k,l,mu,id)
      value(pointer) = val
      if (last.EQ.YES) then
          iend = LAST_BLOCK
          last = ERR
          write(nfile) pointer, iend, labels, value
      end if
C
100   return
      end
