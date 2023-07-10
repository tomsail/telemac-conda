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
C * Tests for link module
C *
C *****************************************************************************
      program MEDlink1
C     
      implicit none
      include 'med.hf'
C
C     
      integer cret
      integer*8 fid

      character*64 fname
      parameter (fname = "Unittest_MEDlink_1.med")
      character*64 mname1, mname2,lname1,lname2
      parameter(mname1 = "mesh name")
      parameter(lname1 = "/local/study1/filename.med")
      parameter(mname2 = "second mesh name")
      parameter(lname2 = "/local/study2/filename.med")
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
C     links creation
      call mlnliw(fid,mname1,lname1,cret)
      print *,'Create a link',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : link creation'
         call efexit(-1)
      endif 
c
      call mlnliw(fid,mname2,lname2,cret)
      print *,'Create a link',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : link creation'
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

