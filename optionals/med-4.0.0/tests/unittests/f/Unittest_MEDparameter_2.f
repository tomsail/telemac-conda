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
C * Tests for parameter module
C *
C *****************************************************************************
      program MEDparameter2
C     
      implicit none
      include 'med.hf'
C
C     
      integer cret
      integer*8 fid

      character*64 fname
      parameter (fname = "Unittest_MEDparameter_1.med")
      character*64 pname1,pname2,pname
      parameter (pname1="first parameter name") 
      parameter (pname2="second parameter name") 
      integer type1,type2,type
      parameter (type1=MED_FLOAT64, type2=MED_INT)
      character*200 desc1,desc2,desc
      parameter (desc1="First parameter description")
      parameter (desc2="Second parameter description")
      character*16 dtunit1,dtunit2,dtunit
      parameter (dtunit1="unit1")
      parameter (dtunit2="unit2")
      integer nstep1,nstep2,nstep
      parameter (nstep1=2,nstep2=2)
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
C     read information
      call mprpin(fid,pname1,type,desc,dtunit,
     &            nstep,cret)
      print *,'read information',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : read information'
         call efexit(-1)
      endif 
c
c      if ( (type .ne. type1) .or.
c     &     (desc .ne. desc1) .or.
c     &     (dtunit .ne. dtunit1) .or.
c     &     (nstep .ne. nstep1) ) then
c         print *,'ERROR : read information'
c         call efexit(-1)
c      endif 
C
C     read information
C
      call mprpin(fid,pname2,type,desc,dtunit,
     &            nstep,cret)
      print *,'read information',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : read information'
        call efexit(-1)
      endif 
c
c      if ((type .ne. type2) .or.
c     &    (desc .ne. desc2) .or.
c     &    (dtunit .ne. dtunit2) .or.
c     &    (nstep .ne. nstep2)) then
c         print *,'ERROR : read information'
c         call efexit(-1)
c      endif 
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
