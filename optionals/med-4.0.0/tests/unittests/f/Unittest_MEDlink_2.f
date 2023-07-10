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
      program MEDlink2
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
      integer lsize,lsize1,lsize2 
      parameter (lsize1=26, lsize2=26)
      character*64 lname
C 
C
C     open file 
      call mfiope(fid,fname,MED_ACC_RDONLY,cret)
      print *,'Open file',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : open file'
         call efexit(-1)
      endif 
C
C
C     read link size
      call mlnlai(fid, mname1, lsize, cret)
      print *,'read link information',cret,lsize
      if (cret .ne. 0 .or.
     &    lsize .ne. lsize1 ) then
         print *,'ERROR : link information'
         call efexit(-1)
      endif 
c
      call mlnlai(fid, mname2, lsize, cret)
      print *,'read link information',cret,lsize
      if (cret .ne. 0 .or.
     &    lsize .ne. lsize2 ) then
         print *,'ERROR : link information'
         call efexit(-1)
      endif 
C
C
C     read links
      call mlnlir(fid,mname1,lname,cret)
      print *,'read link',cret,lname
      if (cret .ne. 0 ) then
         print *,'ERROR : read link'
         call efexit(-1)
      endif 
c
      call mlnlir(fid,mname2,lname,cret)
      print *,'read link',cret,lname
      if (cret .ne. 0 ) then
         print *,'ERROR : read link'
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

