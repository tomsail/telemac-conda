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
      program MEDparameter3
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
      real*8 p1v1, p1v2,rv
      parameter (p1v1=1.0,p1v2=2.0)
      integer p1numdt1,p1numdt2,p2numdt1,p2numdt2,numdt
      parameter (p1numdt1=MED_NO_DT,p1numdt2=1)
      parameter (p2numdt1=2, p2numdt2=3)
      real*8 dt1, dt2,dt
      parameter (dt1=MED_UNDEF_DT,dt2=5.5)
      integer p2v1,p2v2,iv
      parameter (p2v1=3,p2v2=4)
      integer p1numit1, p1numit2, p2numit1, p2numit2
      integer numit
      parameter (p1numit1=MED_NO_IT, p1numit2=1)
      parameter (p2numit1=2, p2numit2=3)
      integer nstep1,nstep2,nstep,sit
      parameter (nstep1=2,nstep2=2)
      integer np,np1,it
      parameter (np1=2)
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
C     number of parameter
      call mprnpr(fid,np,cret)
      print *,'Number of parameter',cret
      if ((cret .ne. 0) .or.
     &    (np .ne. np1)) then
         print *,'ERROR : number of parameter'
         call efexit(-1)
      endif 
C
C
C     read parameters
      do it=1,np
c
         call mprpri(fid,it,pname,type,desc, 
     &               dtunit,nstep,cret)
         print *,'interpolation information',cret
         if (cret .ne. 0 ) then
            print *,'ERROR : interpolation information'
            call efexit(-1)
         endif 
c     
c         if (it .eq. 1) then
c            if ((pname .ne. pname1) .or.
c     &          (type .ne. type1) .or.
c     &          (desc .ne. desc1) .or.
c     &          (dtunit .ne. dtunit1) .or.
c     &          (nstep .ne. nstep1)) then
c               print *,'ERROR : interpolation information'
c               call efexit(-1)
c            endif
c         endif
c
c         if (it .eq. 2) then
c            if ((pname .ne. pname2) .or.
c     &          (type .ne. type2) .or.
c     &          (desc .ne. desc2) .or.
c     &          (dtunit .ne. dtunit2) .or.
c     &          (nstep .ne. nstep2)) then
c               print *,'ERROR : interpolation information'
c               call efexit(-1)
c            endif
c         endif
c
         do sit=1,nstep
c
            call mprcsi(fid,pname,sit,numdt,numit,
     &                  dt,cret)
            print *,'computation step information',cret
            if (cret .ne. 0 ) then
               print *,'ERROR : computation step information'
               call efexit(-1)
            endif 
c
c            if ((pname .eq. pname1) .and.
c     &          (sit .eq. 1)) then
c               if ((numdt .ne. p1numdt1) .or.
c     &              (numit .ne. p1numit1) .or.
c     &              (dt .ne. dt1)) then
c                  print *,'ERROR : read value'
c                  call efexit(-1)
c               endif
c            endif 
c
c            if ((pname .eq. pname1) .and.
c     &          (sit .eq. 2)) then
c               if ((numdt .ne. p1numdt2) .or.
c     &              (numit .ne. p1numit2) .or.
c     &              (dt .ne. dt2)) then
c                  print *,'ERROR : read value'
c                  call efexit(-1)
c               endif
c            endif 
c
c            if ((pname .eq. pname2) .and.
c     &          (sit .eq. 1)) then
c               if ((numdt .ne. p2numdt1) .or.
c     &             (numit .ne. p2numit1) .or.
c     &             (dt .ne. dt1)) then
c                  print *,'ERROR : read value'
c                  call efexit(-1)
c               endif
c            endif 
c
c            if ((pname .eq. pname2) .and.
c     &          (sit .eq. 2)) then
c              if ((numdt .ne. p2numdt2) .or.
c     &             (numit .ne. p2numit2) .or.
c     &             (dt .ne. dt2)) then
c                  print *,'ERROR : read value'
c                  call efexit(-1)
c               endif
c            endif 
c
c            if (type .eq. MED_INT) then
c               call mprivr(fid,pname,numdt,numit,iv,cret)
c               print *,'read value',cret
c               if (cret .ne. 0 ) then
c                  print *,'ERROR : read value'
c                  call efexit(-1)
c               endif 
c
c               if ((sit .eq. 1) .and.
c     &              (iv .ne. p2v1)) then
c                  print *,'ERROR : read value'
c                  call efexit(-1)
c               endif
c
c               if ((sit .eq. 2) .and.
c     &              (iv .ne. p2v2)) then
c                  print *,'ERROR : read value'
c                  call efexit(-1)
c               endif
c            else
c               call mprrvr(fid,pname,numdt,numit,rv,cret)
c               print *,'read value',cret
c               if (cret .ne. 0 ) then
c                  print *,'ERROR : read value'
c                  call efexit(-1)
c               endif 
c
c               if ((sit .eq. 1) .and.
c     &              (rv .ne. p1v1)) then
c                  print *,'ERROR : read value'
c                  call efexit(-1)
c               endif
c
c               if ((sit .eq. 2) .and.
c     &             (rv .ne. p1v2)) then
c                  print *,'ERROR : read value'
c                  call efexit(-1)
c               endif
c            endif
         enddo
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

