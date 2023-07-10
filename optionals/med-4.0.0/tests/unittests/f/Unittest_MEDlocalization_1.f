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
      program MEDloc1
C     
      implicit none
      include 'med.hf'
C
C     
      integer cret
      integer*8 fid

      character*64 fname,lname,giname,isname
      parameter (fname = "Unittest_MEDlocalization_1.med")
      parameter (lname = "Localization name")
      parameter (giname=MED_NO_INTERPOLATION)
      parameter (isname=MED_NO_MESH_SUPPORT)
      integer gtype,sdim,nip
      parameter(gtype=MED_TRIA3)
      parameter(sdim=2)
      parameter(nip=3)
      real*8 ecoo(6), ipcoo(6), wght(3)
      data ecoo   / 0.0, 0.0,  1.0, 0.0,  0.0,1.0 /
      data ipcoo  / 0.166666, 0.166666,  0.66666, 0.166666,  
     &              0.166666, 0.666666 /
      data wght / 0.166666, 0.166666, 0.166666 /
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
C     localization creation
      call mlclow(fid,lname,gtype,sdim,ecoo,MED_FULL_INTERLACE,
     &            nip,ipcoo, wght, giname, isname, cret)
      print *,'localization creation',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : localization creation'
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

