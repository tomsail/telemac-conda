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
      character *64 name1
      parameter (name1="Interpolation family name")
      integer gtype1,gtype
      parameter (gtype1=MED_TRIA3)
      integer cnode1,cnode
      parameter (cnode1=MED_FALSE)
      integer nvar1,maxd1,nmaxc1
      integer nvar,maxd,nmaxc
      parameter (nvar1=2,maxd1=1,nmaxc1=3)
      integer ncoef1,ncoef2,ncoef3,ncoef
      parameter (ncoef1=3,ncoef2=1,ncoef3=1)
      integer power1(6),power2(2),power3(2)
      integer power(6)
      data power1 / 0,0, 1,0, 0,1 /
      data power2 / 1,0 /
      data power3 / 0,1 /
      real*8 coef1(3), coef2(1), coef3(1)
      real*8 coef(3)
      data coef1 / 1., -1., -1. /
      data coef2 / 1. /
      data coef3 / 1. /
      integer nbf,nbf1,it,size,size1,size2,size3
      parameter (nbf1=3,size1=3,size2=1,size3=1) 
C 
C
C     file creation
      call mfiope(fid,fname,MED_ACC_RDONLY,cret)
      print *,'Open file',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : open file'
         call efexit(-1)
      endif 
C
C
C     interpolation information
      call mipiin(fid,name1,gtype,cnode,nbf,nvar,
     &            maxd,nmaxc,cret)
      print *,'interpolation information',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : interpolation information'
         call efexit(-1)
      endif 
c
      if ( (gtype .ne. gtype1) .or.
     &     (cnode .ne. cnode1) .or.
     &     (nbf .ne. nbf1) .or.
     &     (nvar .ne. nvar1) .or.
     &     (maxd .ne. maxd1) .or.
     &     (nmaxc .ne. nmaxc1) ) then
         print *,'ERROR : interpolation information'
         call efexit(-1)
      endif
C
C
C     read functions
      do it=1,nbf
         call mipcsz(fid,name1,it,size,cret)
         print *,'memory size',cret
         if (cret .ne. 0 ) then
            print *,'ERROR : memory size'
            call efexit(-1)
         endif 
c
         if (it .eq. 1) then
            if (size .ne. size1) then
               print *,'ERROR : memory size size'
               call efexit(-1)
            endif
         endif
c
         if (it .eq. 2) then
            if (size .ne. size2) then
               print *,'ERROR : allocation size'
               call efexit(-1)
            endif
         endif
c
         if (it .eq. 3) then
            if (size .ne. size3) then
               print *,'ERROR : allocation size'
               call efexit(-1)
            endif
         endif
C
         call mipbfr(fid,name1,it,ncoef,power,coef,cret)
         print *,'read function',cret
         if (cret .ne. 0 ) then
            print *,'ERROR : read function'
            call efexit(-1)
         endif 
c
         if (it .eq. 1) then
            if ( (ncoef .ne. ncoef1) .or.  
     &           (power(1) .ne. power1(1)) .or.
     &           (power(2) .ne. power1(2)) .or.
     &           (power(3) .ne. power1(3)) .or.
     &           (power(4) .ne. power1(4)) .or.
     &           (power(5) .ne. power1(5)) .or.
     &           (power(6) .ne. power1(6)) .or. 
     &           (coef(1) .ne. coef1(1)) .or.
     &           (coef(2) .ne. coef1(2)) .or.
     &           (coef(3) .ne. coef1(3)) ) then
               print *,'ERROR : read function'
               call efexit(-1)
            endif
         endif
c
         if (it .eq. 2) then
            if ( (ncoef .ne. ncoef2) .or.  
     &           (power(1) .ne. power2(1)) .or.
     &           (power(2) .ne. power2(2)) .or.
     &           (coef(1) .ne. coef2(1)) ) then
               print *,'ERROR : read function'
               call efexit(-1)
            endif
         endif
c
         if (it .eq. 3) then
            if ( (ncoef .ne. ncoef3) .or.  
     &           (power(1) .ne. power3(1)) .or.
     &           (power(2) .ne. power3(2)) .or.
     &           (coef(1) .ne. coef3(1)) ) then
               print *,'ERROR : read function'
               call efexit(-1)
            endif
         endif
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

