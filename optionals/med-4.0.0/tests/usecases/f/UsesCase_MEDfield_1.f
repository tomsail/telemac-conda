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
C *
C * Field use case 1 : write a field on mesh vertices and elements
C *
C *****************************************************************************
      program UsesCase_MEDfield_1
C
      implicit none
      include 'med.hf77'
C
C
C
      integer cret
      integer*8 fid

C     component number, node number
      integer ncompo, nnodes
C     triangular elements number, quadrangular elements number
      integer ntria3, nquad4
C     med file name, field name, link file name
      character*64  fname, finame, lfname
C     component name, commponent unit
      character*16 cpname, cpunit
C     mesh name      
      character*64 mname
      character*16 dtunit
      real*8 dt
C     vertices values      
      real*8 verval(15)
      real*8 tria3v(8)
      real*8 quad4v(4)
C
      parameter (fname = "./UsesCase_MEDfield_1.med")
      parameter (lfname= "./UsesCase_MEDmesh_1.med")
      parameter (mname = "2D unstructured mesh")
      parameter (finame = "TEMPERATURE_FIELD")
      parameter (cpname = "TEMPERATURE")
      parameter (cpunit = "C")
      parameter (dtunit = " ")
      parameter (nnodes = 15, ncompo = 1 )
      parameter (ntria3 =  8, nquad4 = 4)
      parameter (dt = 0.0d0)
C
      data verval /   0.,  100., 200.,  300.,  400., 
     &              500.,  600., 700.,  800.,  900,
     &             1000., 1100, 1200., 1300., 1500. /
      data tria3v / 1000., 2000., 3000., 4000., 
     &              5000., 6000., 7000., 8000. /
      data quad4v / 10000., 20000., 30000., 4000. /
C
C
C     file creation
      call mfiope(fid,fname,MED_ACC_CREAT,cret)
      if (cret .ne. 0 ) then
         print *,'ERROR : file creation'
         call efexit(-1)
      endif
C
C
C     create mesh link
      call mlnliw(fid,mname,lfname,cret)
      if (cret .ne. 0 ) then
         print *,'ERROR : create mesh link ...'
         call efexit(-1)
      endif
C
C
C     field creation : temperature field  : 1 component in celsius degree
C                      the mesh is the 2D unstructured mesh of
C                      UsecaseMEDmesh_1.f
      call mfdcre(fid,finame,MED_FLOAT64,ncompo,cpname,cpunit,dtunit,
     &            mname,cret)
      if (cret .ne. 0 ) then
         print *,'ERROR : create field ...'
         call efexit(-1)
      endif
C
C
C     write field values at vertices
      call mfdrvw(fid,finame,MED_NO_DT,MED_NO_IT,dt,MED_NODE,
     &            MED_NONE,MED_FULL_INTERLACE,MED_ALL_CONSTITUENT,
     &            nnodes,verval,cret)
      if (cret .ne. 0 ) then
         print *,'ERROR : write field values on vertices'
         call efexit(-1)
      endif
C
C
C     write values at cell centers : 8 MED_TRIA3 and 4 MED_QUAD4
C     MED_TRIA3
      call mfdrvw(fid,finame,MED_NO_DT,MED_NO_IT,dt,MED_CELL,
     &            MED_TRIA3,MED_FULL_INTERLACE,MED_ALL_CONSTITUENT,
     &            ntria3,tria3v,cret)
      if (cret .ne. 0 ) then
         print *,'ERROR : write field values on MED_TRIA3'
         call efexit(-1)
      endif
C
C
C     MED_QUAD4
      call mfdrvw(fid,finame,MED_NO_DT,MED_NO_IT,dt,MED_CELL,
     &            MED_QUAD4,MED_FULL_INTERLACE,MED_ALL_CONSTITUENT,
     &            nquad4,quad4v,cret)
      if (cret .ne. 0 ) then
         print *,'ERROR : write field values on MED_QUAD4'
         call efexit(-1)
      endif
C
C
C     close file
      call mficlo(fid,cret)
      if (cret .ne. 0 ) then
         print *,'ERROR :  close file'
         call efexit(-1)
      endif
C
      end
C
