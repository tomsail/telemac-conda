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
C * Tests for support mesh module
C *
C *****************************************************************************
      program MEDsupportMesh2
C     
      implicit none
      include 'med.hf'
C
C     
      integer cret
      integer*8 fid

      character*64  fname
      parameter (fname = "Unittest_MEDsupportMesh_1.med")
      character*64 smname1
      integer sdim1,mdim1
      parameter (sdim1=2, mdim1=2)
      integer sdim2,mdim2
      parameter (sdim2=3,mdim2=2)
      parameter (smname1 = "supportMesh1")
      character*64 smname2
      parameter (smname2 = "supportMesh2")
      character*200 description1
      parameter (description1="support mesh1 description")
      character*200 description2
      parameter (description2="support mesh2 description")
      character*16 nomcoo2D(2)
      character*16 unicoo2D(2)
      data  nomcoo2D /"x","y"/, unicoo2D /"cm","cm"/
      character*16 nomcoo3D(3)
      character*16 unicoo3D(3)
      data  nomcoo3D /"x","y","z"/, unicoo3D /"cm","cm","cm"/
      integer atype1, atype2
      parameter (atype1=MED_CARTESIAN, atype2=MED_CARTESIAN)
      integer nsmesh, i
      character*64  smname
      character*16 aunit(3), aname(3)
      character*200 description
      integer sdim, mdim, atype
C
C
C     open file in read only access mode
      call mfiope(fid,fname,MED_ACC_RDONLY,cret)
      print *,'Open file in RD_ONLY access mode',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : open file in READ_ONLY access mode'
         call efexit(-1)
      endif  
C
C     Read number of axis by name
C
      call  msmnan(fid,smname1,sdim,cret)
      print *,'Number of axis (by name) : ',sdim
      if (cret .ne. 0 ) then
         print *,'ERROR : read number of axis (by name)'
         call efexit(-1)
      endif  
      if (sdim .ne. sdim1) then
         print *,'ERROR : number of axis (by name)'
         call efexit(-1)         
      endif

      call  msmnan(fid,smname2,sdim,cret)
      print *,'Number of axis (by name) : ',sdim
      if (cret .ne. 0 ) then
         print *,'ERROR : read number of axis (by name)'
         call efexit(-1)
      endif  
      if (sdim .ne. sdim2) then
         print *,'ERROR : number of axis (by name)'
         call efexit(-1)         
      endif
C
C     Read support mesh information by name
C
      call  msmsni(fid,smname1,sdim,mdim, 
     &             description,atype, 
     &             aname, aunit, cret)
      print *,'Support mesh information by name',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : read support mesh information by name'
         call efexit(-1)
      endif  
      if ((sdim .ne. sdim1) .or.
     &    (mdim .ne. mdim1) .or.
     &    (description .ne. description1) .or.
     &    (atype .ne. atype1) .or.
     &    (aunit(1) .ne. unicoo2D(1)) .or.
     &    (aunit(2) .ne. unicoo2D(2)) .or.
     &    (aname(1) .ne. nomcoo2D(1)) .or.
     &    (aname(2) .ne. nomcoo2D(2))
     & ) then
         print *,'ERROR : support mesh information by name'
         call efexit(-1)
      endif  
C
C
C
      call  msmsni(fid,smname2,sdim,mdim, 
     &             description,atype, 
     &             aname, aunit, cret)
      print *,'Support mesh information by name',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : read support mesh information by name'
         call efexit(-1)
      endif  
      if ((sdim .ne. sdim2) .or.
     &    (mdim .ne. mdim2) .or.
     &    (description .ne. description2) .or.
     &    (atype .ne. atype2) .or.
     &    (aunit(1) .ne. unicoo3D(1)) .or.
     &    (aunit(2) .ne. unicoo3D(2)) .or.
     &    (aunit(3) .ne. unicoo3D(3)) .or.
     &    (aname(1) .ne. nomcoo3D(1)) .or.
     &    (aname(2) .ne. nomcoo3D(2)) .or.
     &    (aname(3) .ne. nomcoo3D(3))
     & ) then
         print *,'ERROR : support mesh information by name'
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

