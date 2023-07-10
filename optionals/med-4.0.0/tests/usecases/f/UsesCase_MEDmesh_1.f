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
C * Use case 1 : a 2D unstructured mesh with 15 nodes, 
C *              8 triangular cells, 4 quadrangular cells
C *
C *****************************************************************************
      program UsesCase_MEDmesh_1
C
      implicit none
      include 'med.hf77'
C
C
C
      integer cret
      integer*8 fid

      integer sdim, mdim, stype, mtype, atype, nnode
      integer ntria, nquad
      integer fnum, ngro
      character*200 cmt1,mdesc
      character*64  fname
      character*64 mname
      character*16 nomcoo(2)
      character*16 unicoo(2)
      character*16 dtunit
      real*8 dt
      parameter (fname = "UsesCase_MEDmesh_1.med")
      parameter (mdesc = "A 2D unstructured mesh")
      parameter (cmt1 = "A 2D unstructured mesh : 15 nodes, 12 cells")
      parameter (mname = "2D unstructured mesh")
      parameter (sdim = 2, mdim = 2, nnode=15)
      parameter (stype=MED_SORT_DTIT, mtype=MED_UNSTRUCTURED_MESH)
      parameter (atype=MED_CARTESIAN)
      parameter (dt=0.0d0)
      parameter (ntria =  8, nquad = 4)
      parameter (fnum = 0, ngro = 0) 
      data  dtunit /" "/
      data  nomcoo /"x" ,"y" /
      data  unicoo /"cm","cm"/
      real*8 coo(30)
      data  coo /2.,1.,7.,1.,12.,1.,17.,1.,22.,1.,
     &           2.,6.,  7.,6.,  12.,6.,  17.,6.,  22.,6.,
     &           2.,11., 7.,11., 12.,11., 17.,11., 22.,11./
      integer tricon(24)
      data tricon /1,7,6,   2,7,1,  3,7,2,   8,7,3,
     &             13,7,8, 12,7,13, 11,7,12, 6,7,11/
      integer quacon(16)
      data quacon /3,4,9,8,    4,5,10,9,
     &             15,14,9,10, 13,8,9,14 /
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
C     write a comment in the file
      call mficow(fid,cmt1,cret)
      if (cret .ne. 0 ) then
         print *,'ERROR : write file description'
         call efexit(-1)
      endif
C
C
C     mesh creation
      call mmhcre(fid, mname, sdim, mdim, mtype,mdesc,
     &            dtunit, stype, atype, nomcoo, unicoo, cret)
      if (cret .ne. 0 ) then
         print *,'ERROR : mesh creation'
         call efexit(-1)
      endif
C
C
C     write nodes coordinates
      call mmhcow(fid,mname,MED_NO_DT,MED_NO_IT,dt, 
     &            MED_FULL_INTERLACE,nnode,coo,cret)
      if (cret .ne. 0 ) then
         print *,'ERROR : write nodes coordinates description'
         call efexit(-1)
      endif
C
C
C     cells connectiviy is defined in nodal mode with
C     no iteration and computation step
      call mmhcyw(fid,mname,MED_NO_DT,MED_NO_IT,dt,MED_CELL,
     &            MED_TRIA3,MED_NODAL,MED_FULL_INTERLACE,
     &            ntria,tricon,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'ERROR : triangular cells connectivity'
         call efexit(-1)
      endif
C
      call mmhcyw(fid,mname,MED_NO_DT,MED_NO_IT,dt,MED_CELL,
     &            MED_QUAD4,MED_NODAL,MED_FULL_INTERLACE,
     &            nquad,quacon,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'ERROR : quadrangular cells connectivity'
         call efexit(-1)
      endif
C
C
C     create family 0 : by default, all mesh entities family number is 0
      call mfacre(fid,mname,MED_NO_NAME,fnum,ngro,MED_NO_GROUP,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'ERROR : family 0 creation'
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
