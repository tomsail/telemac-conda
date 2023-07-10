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
      program MEDinterp1
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
      integer gtype1
      parameter (gtype1=MED_TRIA3)
      integer cnode1
      parameter (cnode1=MED_FALSE)
      integer nvar1,maxd1,nmaxc1
      parameter (nvar1=2,maxd1=1,nmaxc1=3)
      integer ncoef1,ncoef2,ncoef3
      parameter (ncoef1=3,ncoef2=1,ncoef3=1)
      integer power1(6),power2(2),power3(2)
      data power1 / 0,0, 1,0, 0,1 /
      data power2 / 1,0 /
      data power3 / 0,1 /
      real*8 coef1(3), coef2(1), coef3(1)
      data coef1 / 1., -1., -1. /
      data coef2 / 1. /
      data coef3 / 1. /
C 
C
C     file creation
      call mfiope(fid,fname,MED_ACC_CREAT,cret)
      print *,'Open file',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : file creation'
         call efexit(-1)
      endif 
C
C
C     interpolation function family creation
      call mipcre(fid,name1,gtype1,cnode1,nvar1,
     &            maxd1,nmaxc1,cret)
      print *,'interpolation creation',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : interpolation creation'
         call efexit(-1)
      endif 
C
C
C     functions creation
      call mipbfw(fid,name1,1,ncoef1,power1,coef1,cret)
      print *,'function creation',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : function creation'
         call efexit(-1)
      endif 
c
      call mipbfw(fid,name1,2,ncoef2,power2,coef2,cret)
      print *,'function creation',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : function creation'
         call efexit(-1)
      endif 
c
      call mipbfw(fid,name1,3,ncoef3,power3,coef3,cret)
      print *,'function creation',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : function creation'
         call efexit(-1)
      endif 
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

