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
      program MEDsupportMesh1
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
C     file creation
      call mfiope(fid,fname,MED_ACC_CREAT,cret)
      print *,'Open file',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : file creation'
         call efexit(-1)
      endif  
C
C
C     support mesh creation : 2D
      call msmcre(fid,smname1,sdim1,mdim1,description1,
     &            MED_CARTESIAN,nomcoo2D,unicoo2D,cret)
      print *,'Support mesh creation : 2D space dimension',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : support mesh creation'
         call efexit(-1)
      endif  
C
C
C    support mesh creation : 3D space diminsion
      call msmcre(fid,smname2,sdim2,mdim2,description2,
     &            MED_CARTESIAN,nomcoo3D,unicoo3D,cret)
      print *,'Support mesh creation : 3D space dimension ',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : support mesh creation'
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

