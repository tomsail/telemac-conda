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
C* 
C* Use case 13 : a 2D unstructured mesh with 10 nodes and 2 polygons
C* 
C* poly1 : 1,4,7,9,6,3
C* poly2 : 2,5,8,10,7,4
C
C*      9   10
C*
C*   6    7    8
C* 
C*   3    4    5
C*  
C*      1     2
C*
C *****************************************************************************
      program UsesCase_MEDmesh_13
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
C     mesh name, file name
      character*64 mname, finame
      character*64 dtunit
C     coordinates
      real*8 coords(2*10)
      integer nnodes
      integer isize
      integer index(3)
      integer conity(12)
C     comment 1, mesh description
      character*200 cmt1, mdesc
C
      parameter (sdim = 2, mdim = 2)
      parameter (mname = "2D unstructured mesh")
      parameter (dtunit = "")
      parameter (finame = "UsesCase_MEDmesh_13.med")
C     Dix noeuds dont deux communs aux deux polygones */
      parameter (nnodes = 10)
      parameter (isize = 3)
      parameter (cmt1 ="A 2D unstructured mesh : 10 nodes, 2 polygons")
      parameter (mdesc = "A 2D mesh with 2 polygons")
C
      data axname  /"x               ","y               "/
      data unname  /"cm              ","cm              "/
      data coords / 0.5,   0.,  
     &              1.5,   0.,  
     &              0.,    0.5,
     &              1.,    0.5,
     &              2.,    0.5,
     &              0.,    1.,
     &              1.,    1.,
     &              2.,    1.,
     &              0.5,   2.,
     &              1.5,   2. /
      data index / 1, 7, 13 /
      data conity / 1,4,7,9,6,3,
     &              2,5,8,10,7,4 /
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
      call mmhcre(fid, mname, sdim, mdim, MED_UNSTRUCTURED_MESH, mdesc,
     &            dtunit, MED_SORT_DTIT, MED_CARTESIAN, 
     &            axname, unname, cret)
      if (cret .ne. 0 ) then
         print *,'ERROR : mesh creation'
         call efexit(-1)
      endif
C
C
C     nodes coordinates in a cartesian axis in full interlace mode
C     (X1,Y1, X2,Y2, X3,Y3, ...) with no iteration and computation step
      call mmhcow(fid,mname,MED_NO_DT,MED_NO_IT, MED_UNDEF_DT,
     &            MED_FULL_INTERLACE,nnodes,coords,cret)
      if (cret .ne. 0 ) then
         print *,'ERROR : write nodes coordinates description'
         call efexit(-1)
      endif
C
C
C     cells connectiviy is defined in nodal mode
C     2 polygons
      call mmhpgw(fid, mname, MED_NO_DT, MED_NO_IT, MED_UNDEF_DT,
     &            MED_CELL, MED_NODAL, isize, index, conity, cret)
      if (cret .ne. 0 ) then
         print *,'ERROR : polygon connectivity ...'
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
