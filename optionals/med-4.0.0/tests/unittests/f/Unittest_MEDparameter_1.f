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
C * Tests for paramter module
C *
C *****************************************************************************
      program MEDparameter1
C     
      implicit none
      include 'med.hf'
C
C     
      integer cret
      integer*8 fid

      character*64 fname
      parameter (fname = "Unittest_MEDparameter_1.med")
      character*64 pname1,pname2
      parameter (pname1="first parameter name") 
      parameter (pname2="second parameter name") 
      integer type1,type2
      parameter (type1=MED_FLOAT64, type2=MED_INT)
      character*200 desc1,desc2
      parameter (desc1="First parameter description")
      parameter (desc2="Second parameter description")
      character*16 dtunit1,dtunit2
      parameter (dtunit1="unit1")
      parameter (dtunit2="unit2")
      real*8 p1v1, p1v2
      parameter (p1v1=1.0,p1v2=2.0)
      integer p1numdt1,p1numdt2,p2numdt1,p2numdt2
      parameter (p1numdt1=MED_NO_DT,p1numdt2=1)
      parameter (p2numdt1=2, p2numdt2=3)
      real*8 dt1, dt2
      parameter (dt1=MED_UNDEF_DT,dt2=5.5)
      integer p2v1,p2v2
      parameter (p2v1=3,p2v2=4)
      integer p1numit1, p1numit2, p2numit1, p2numit2
      parameter (p1numit1=MED_NO_IT, p1numit2=1)
      parameter (p2numit1=2, p2numit2=3)
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
C     first parameter creation
      call mprcre(fid,pname1,type1,desc1,dtunit1,cret)
      print *,'parameter creation',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : parameter creation'
         call efexit(-1)
      endif 
C
C
C     write values
      call mprrvw(fid,pname1,p1numdt1,p1numit1,dt1,p1v1,cret)
      print *,'write value',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : write value'
         call efexit(-1)
      endif 
c
      call mprrvw(fid,pname1,p1numdt2,p1numit2,dt2,p1v2,cret)
      print *,'write value',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : write value'
         call efexit(-1)
      endif 
C
C
C     second parameter creation
      call mprcre(fid,pname2,type2,desc2,dtunit2,cret)
      print *,'parameter creation',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : parameter creation'
         call efexit(-1)
      endif 
C
C
C     write values
      call mprivw(fid,pname2,p2numdt1,p2numit1,dt1,p2v1,cret)
      print *,'write value',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : write value'
         call efexit(-1)
      endif 
c
      call mprivw(fid,pname2,p2numdt2,p2numit2,dt2,p2v2,cret)
      print *,'write value',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : write value'
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

