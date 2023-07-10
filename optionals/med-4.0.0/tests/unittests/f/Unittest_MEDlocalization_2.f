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
C * Tests for localization module
C *
C *****************************************************************************
      program MEDloc2
C     
      implicit none
      include 'med.hf'
C
C     
      integer cret
      integer*8 fid

      character*64  fname,lname1,giname1,isname1
      character*64  giname,isname
      parameter (fname="Unittest_MEDlocalization_1.med")  
      parameter (lname1 = "Localization name")
      parameter (giname1=MED_NO_INTERPOLATION)
      parameter (isname1=MED_NO_MESH_SUPPORT)
      integer gtype1,sdim1,nip1
      integer gtype,sdim,nip
      parameter(gtype1=MED_TRIA3)
      parameter(sdim1=2)
      parameter(nip1=3)
      real*8 ecoo1(6), ipcoo1(6), wght1(3)
      real*8 ecoo(6), ipcoo(6), wght(3)
      data ecoo1   / 0.0, 0.0,  1.0, 0.0,  0.0,1.0 /
      data ipcoo1  / 0.166666, 0.166666,  0.66666, 0.166666,  
     &               0.166666, 0.666666 /
      data wght1   / 0.166666, 0.166666, 0.166666 /
      integer nsmc, nsmc1
      parameter (nsmc1=0)
      integer sgtype,sgtype1
      parameter (sgtype1=MED_UNDEF_GEOTYPE)
C 
C
C     open file
      call mfiope(fid,fname,MED_ACC_RDONLY,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'ERROR : open file'
         call efexit(-1)
      endif  
C
C
C     read information
      call  mlclni(fid, lname1, gtype, sdim, nip,
     &     giname, isname, nsmc, sgtype, cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'ERROR : read information'
         call efexit(-1)
      endif  
      if ((gtype .ne. gtype1) .or.
     &    (sdim .ne. sdim1) .or.
     &    (nip .ne. nip1) .or.
     &    (giname .ne. giname1) .or.
     &    (isname .ne. isname1) .or.
     &    (nsmc .ne. nsmc1) .or.
     &    (sgtype .ne. sgtype1) ) then
         print *,cret
         print *,gtype1,sdim1,nip1,"|",giname1,"|","|",
     &        isname1,"|",nsmc1,sgtype1
         print *,gtype,sdim,nip,"|",giname,"|","|",isname,"|",
     &        nsmc,sgtype
         print *,'ERROR : read information'
         call efexit(-1)
      endif  
C
C
C     read localization
      call mlclor(fid,lname1,MED_FULL_INTERLACE,
     &            ecoo,ipcoo,wght,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'ERROR : read localization'
         call efexit(-1)
      endif  
c
      if ((ecoo(1) .ne. ecoo1(1)) .or.
     &    (ecoo(2) .ne. ecoo1(2)) .or.
     &    (ecoo(3) .ne. ecoo1(3)) .or.
     &    (ecoo(4) .ne. ecoo1(4)) .or.
     &    (ecoo(5) .ne. ecoo1(5)) .or.
     &    (ecoo(6) .ne. ecoo1(6))) then
         print *,'ERROR : read localization'
         call efexit(-1)
      endif  
c
      if ((ipcoo(1) .ne. ipcoo1(1)) .or.
     &    (ipcoo(2) .ne. ipcoo1(2)) .or.
     &    (ipcoo(3) .ne. ipcoo1(3)) .or.
     &    (ipcoo(4) .ne. ipcoo1(4)) .or.
     &    (ipcoo(5) .ne. ipcoo1(5)) .or.
     &    (ipcoo(6) .ne. ipcoo1(6))) then
         print *,'ERROR : read localization'
         call efexit(-1)
      endif  
c
      if ((wght(1) .ne. wght1(1)) .or.
     &    (wght(2) .ne. wght1(2)) .or.
     &    (wght(3) .ne. wght1(3)))  then
         print *,'ERROR : read localization'
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

