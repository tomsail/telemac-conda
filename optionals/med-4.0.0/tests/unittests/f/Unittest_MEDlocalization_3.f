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
      program MEDloc3
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
      integer it,n,nloc
      parameter (nloc=1)
      parameter (giname1=MED_NO_INTERPOLATION)
      parameter (isname1=MED_NO_MESH_SUPPORT)
      integer gtype1,sdim1,nip1
      integer gtype,sdim,nip
      parameter(gtype1=MED_TRIA3)
      parameter(sdim1=2)
      parameter(nip1=3)
      integer sgtype,sgtype1
      parameter (sgtype1=MED_UNDEF_GEOTYPE)
      integer nsmc, nsmc1
      parameter (nsmc1=0)
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
C     number of localization
      call mlcnlc(fid,n,cret)
      print *,cret
      if ((cret .ne. 0) .or.
     &    (n .ne. nloc) )then
         print *,'ERROR : number of localization'
         call efexit(-1)
      endif  
C
C
C     informations bt iteration
      do it=1,n
         call mlclci(fid,it,lname1,gtype,sdim,nip,
     &               giname,isname,nsmc,sgtype,cret)
         print *,cret
         if (cret .ne. 0 ) then
            print *,'ERROR : read information'
            call efexit(-1)
         endif  
c
         if ((gtype .ne. gtype1) .or.
     &        (sdim .ne. sdim1) .or.
     &        (nip .ne. nip1) .or.
     &        (giname .ne. giname1) .or.
     &        (isname .ne. isname1) .or.
     &        (nsmc .ne. nsmc1) .or.
     &        (sgtype .ne. sgtype1) ) then
            print *,cret
            print *,'ERROR : read information'
            call efexit(-1)
         endif  
      enddo
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

