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
C * Tests for file module
C *
C *****************************************************************************
      program MEDprofile1
C     
      implicit none
      include 'med.hf'
C
C     
      integer cret
      integer*8 fid

      character*64  fname, pname1, pname2
      parameter (fname="Unittest_MEDprofile_1.med")  
      parameter (pname1="Profile name1")
      parameter (pname2="Profile name 2")
      integer psize1,psize2
      parameter (psize1=4, psize2=2)
      integer profile1(4), profile2(2)
      data profile1 /1,2, 3,4/
      data profile2 /5,6/ 
C 
C
C     file creation
      call mfiope(fid,fname,MED_ACC_CREAT,cret)
      print *,cret
      print *,fid
      if (cret .ne. 0 ) then
         print *,'ERROR : file creation'
         call efexit(-1)
      endif  
C
C
C     write a first profile
      call  mpfprw(fid,pname1,psize1,profile1,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'ERROR : profile creation'
         call efexit(-1)
      endif  
C
C
C     write a second profile
      call  mpfprw(fid,pname2,psize2,profile2,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'ERROR : profile creation'
         call efexit(-1)
      endif  
C
C
C     close file
      call mficlo(fid,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'ERROR :  close file'
         call efexit(-1)
      endif        
C
C
C
      end

