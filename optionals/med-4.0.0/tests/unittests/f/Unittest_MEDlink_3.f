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
      character*64 mname1, mname2,lname1,lname2,mname
      parameter(mname1 = "mesh name")
      parameter(lname1 = "/local/study1/filename.med")
      parameter(mname2 = "second mesh name")
      parameter(lname2 = "/local/study2/filename.med")
      integer lsize,lsize1,lsize2 
      parameter (lsize1=26, lsize2=26)
      character*64 lname(26)
      integer nlink,n,i
      parameter (nlink=2)
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
C     number of link
      call mlnnln(fid,n,cret)
      print *,'Number of link',cret
      if ((cret .ne. 0) .or.
     &    (n .ne. nlink) ) then
         print *,'ERROR : number of link'
         call efexit(-1)
      endif  
C
C
C     informations
      do i=1,n
         call mlnlni(fid,i,mname,lsize,cret)
         print *,'Link information',cret
         if (cret .ne. 0) then
            print *,'ERROR : link information'
            call efexit(-1)
         endif  
c
         if (i .eq. 1) then
            if ((mname .ne. mname1) .or.
     &          (lsize .ne. lsize1)) then
               print *,'ERROR : link information'
               call efexit(-1)
            endif
         endif
c
         if (i .eq. 2) then
            if ((mname .ne. mname2) .or.
     &          (lsize .ne. lsize2)) then
               print *,'ERROR : link information'
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

