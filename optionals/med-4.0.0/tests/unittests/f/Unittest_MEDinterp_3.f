C*  This file is part of MED.
C*
C*  COPYRIGHT (C) 1999 - 2019  EDF R&D, CEA/DEN
C*  MED is free software: you can redistribute it and/or modify
C*  it under the terms of the GNU Lesser General Public License as published by
C*  the Free Software Foundation, either version 3 of the License, or
C*  (at your option) any later version.
C*
C*  MED is distributed in the hope that it will be useful,
C*  but WITHOUT ANY WARRANTY; without even the implied warranty of
C*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C*  GNU Lesser General Public License for more details.
C*
C*  You should have received a copy of the GNU Lesser General Public License
C*  along with MED.  If not, see <http://www.gnu.org/licenses/>.
C*

C******************************************************************************
C * Tests for interp module
C *
C *****************************************************************************
      program MEDinterp2
C     
      implicit none
      include 'med.hf'
C
C     
      integer cret
      integer*8 fid

      character*64 fname
      parameter (fname = "Unittest_MEDinterp_1.med")
      integer n,ni
      parameter (ni=1)
      integer it
      character *64 name1,name
      parameter (name1="Interpolation family name")
      integer gtype1,gtype
      parameter (gtype1=MED_TRIA3)
      integer cnode1,cnode
      parameter (cnode1=MED_FALSE)
      integer nvar1,maxd1,nmaxc1
      integer nvar,maxd,nmaxc
      parameter (nvar1=2,maxd1=1,nmaxc1=3)
      integer nbf,nbf1
      parameter (nbf1=3)
C 
C
C     file creation
      call mfiope(fid,fname,MED_ACC_RDONLY,cret)
      print *,'Open file',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : file creation'
         call efexit(-1)
      endif 
C
C
C     number of interpolation
      call mipnip(fid,n,cret)
      print *,'Number of interpolation',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : number of interpolation'
         call efexit(-1)
      endif 
      if (n .ne. ni) then
         print *,'ERROR : number of interpolation'
         call efexit(-1)
      endif
C
C
C     read information
      do it=1,n
         call mipipi(fid,it,name,gtype,cnode,
     &               nbf,nvar,maxd,nmaxc,cret)
         print *,'interpolation information',cret
         if (cret .ne. 0 ) then
            print *,'ERROR : interpolation information'
            call efexit(-1)
         endif 
c     
         if (it .eq. 1) then 
            if ( (gtype .ne. gtype1) .or.
     &           (cnode .ne. cnode1) .or.
     &           (nbf .ne. nbf1) .or.
     &           (nvar .ne. nvar1) .or.
     &           (maxd .ne. maxd1) .or.
     &           (nmaxc .ne. nmaxc1) ) then
               print *,'ERROR : interpolation information'
               call efexit(-1)
            endif
         endif
c
      enddo

C
C
C     close file
      call mficlo(fid,cret)
      print *,'Close file',cret
      if (cret .ne. 0 ) then
         print *,'ERROR :  close file'
         call efexit(-1)
      endif  
C
C
C
      end

