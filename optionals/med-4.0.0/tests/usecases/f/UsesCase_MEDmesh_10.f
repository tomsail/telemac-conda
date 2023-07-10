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
C * How to create an unstructured mesh
C * Use case 10 : write a 2D unstructured mesh with 15 nodes, 8 triangular
C *               cells, 4 quadrangular cells, and families
C *
C *****************************************************************************
      program UsesCase_MEDmesh_10
C     
      implicit none
      include 'med.hf77'
C
C     
      integer cret
      integer*8 fid

C     space dim, mesh dim      
      integer sdim, mdim
C     axis name, unit name
      character*16 axname(2), unname(2)
C     mesh name, family name, time step unit, file name
      character*64 mname, fyname, dtunit, finame
C     mesh type, sorting type, grid type
      integer mtype, stype, grtype
C     family number, number of group      
      integer fnum, ngro
C     group name
      character*80 gname
C     coordinates, date
      real*8 coords(30), dt
      integer nnodes, ntria3, nquad4
C     triangular and quadrangular cells connectivity
      integer tricon(24), quacon(16)
C     family numbers
      integer fanbrs(15)
C     comment 1, mesh description
      character*200 cmt1, mdesc
C
      parameter (sdim = 2, mdim = 2)
      parameter (mname = "2D unstructured mesh")
      parameter (fyname = "BOUNDARY_VERTICES")
      parameter (dtunit = " ")
      parameter (dt = 0.0d0)
      parameter (finame = "UsesCase_MEDmesh_10.med")
      parameter (gname = "MESH_BOUNDARY_VERTICES")
      parameter (nnodes = 15, ntria3 = 8, nquad4 = 4)
      parameter (cmt1 ="A 2D unstructured mesh : 15 nodes, 12 cells")
      parameter (mtype=MED_UNSTRUCTURED_MESH, stype=MED_SORT_DTIT )
      parameter (mdesc = "A 2D unstructured mesh")
      parameter (grtype=MED_CARTESIAN_GRID)
C
      data axname  /"x" ,"y" /
      data unname  /"cm","cm"/
      data coords /2.,1.,  7.,1.,  12.,1.,  17.,1.,  22.,1.,
     &             2.,6.,  7.,6.,  12.,6.,  17.,6.,  22.,6.,
     &             2.,11., 7.,11., 12.,11., 17.,11., 22.,11./
      data tricon /1,7,6,   2,7,1,  3,7,2,   8,7,3,   
     &             13,7,8, 12,7,13, 11,7,12, 6,7,11/
      data quacon /3,4,9,8,    4,5,10,9, 
     &             15,14,9,10, 13,8,9,14/
      data fanbrs /1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1/
C 
C
C     file creation
      call mfiope(fid,finame,MED_ACC_CREAT,cret)
      if (cret .ne. 0 ) then
         print *,'ERROR : file creation'
         call efexit(-1)
      endif  
C
C
C     write a comment in the file
      call mficow(fid,cmt1,cret)
      if (cret .ne. 0 ) then
         print *,'ERROR : write file description'
         call efexit(-1)
      endif
C
C
C     mesh creation : a 2D unstructured mesh
      call mmhcre(fid, mname, sdim, mdim, mtype, mdesc, dtunit,
     &            stype, grtype, axname, unname, cret)
      if (cret .ne. 0 ) then
         print *,'ERROR : mesh creation'
         call efexit(-1)
      endif
C
C
C     nodes coordinates in a cartesian axis in full interlace mode
C     (X1,Y1, X2,Y2, X3,Y3, ...) with no iteration and computation step
      call mmhcow(fid,mname,MED_NO_DT,MED_NO_IT,dt,
     &            MED_FULL_INTERLACE,nnodes,coords,cret)
      if (cret .ne. 0 ) then
         print *,'ERROR : write nodes coordinates description'
         call efexit(-1)
      endif
C
C
C     cells connectiviy is defined in nodal mode
      call mmhcyw(fid,mname,MED_NO_DT,MED_NO_IT,dt,MED_CELL,
     &            MED_TRIA3,MED_NODAL,MED_FULL_INTERLACE,
     &            ntria3,tricon,cret)
      if (cret .ne. 0 ) then
         print *,'ERROR : triangular cells connectivity'
         call efexit(-1)
      endif 
      call mmhcyw(fid,mname,MED_NO_DT,MED_NO_IT,dt,MED_CELL,
     &            MED_QUAD4,MED_NODAL,MED_FULL_INTERLACE,
     &            nquad4,quacon,cret)
      if (cret .ne. 0 ) then
         print *,'ERROR : quadrangular cells connectivity'
         call efexit(-1)
      endif
C
C
C     create family 0 : by default, all mesh entities family number is 0
      call mfacre(fid,mname,MED_NO_NAME,0,0,MED_NO_GROUP,cret)
      if (cret .ne. 0 ) then
         print *,'ERROR : create family 0'
         call efexit(-1)
      endif
C
C
C     create a family for boundary vertices : by convention a nodes family number is > 0,
C     and an element family number is < 0
      fnum = 1
      ngro = 1
      call mfacre(fid, mname, fyname, fnum, ngro, gname, cret)
      if (cret .ne. 0 ) then
         print *,'ERROR : create family 0'
         call efexit(-1)
      endif
C
C
C     write family number for nodes 
      call mmhfnw(fid, mname, MED_NO_DT, MED_NO_IT, MED_NODE, MED_NONE,
     &            nnodes, fanbrs, cret)
      if (cret .ne. 0 ) then
         print *,'ERROR : nodes family numbers ...'
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
C
C
      end
C
