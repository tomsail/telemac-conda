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
C *  How to create an unstructured mesh 
C *
C *  Use case 9 : 2D unstructured mesh with moving grid transformation 
C *  
C *
C *****************************************************************************
      program UsesCase_MEDmesh_9
C     
      implicit none
      include 'med.hf77'
C
C     
      integer cret
      integer*8 fid

C
      character (MED_NAME_SIZE) mname
      character (MED_NAME_SIZE) fname
      character (MED_COMMENT_SIZE) cmt1,mdesc
      integer sdim, mdim
C     axis name
      character (MED_SNAME_SIZE) axname(2)
C     unit name
      character (MED_SNAME_SIZE)  unname(2)
      real*8 inicoo(30)
      integer nnodes, ntria3, nquad4
C     tria connectivity
      integer triacy(24)
C     quad connectivity
      integer quadcy(16)
C     transformation matrix step 1
      real*8 trama1(7)
C     transformation matrix step 2
      real*8 trama2(7)

      parameter (fname = "UsesCase_MEDmesh_9.med")
      parameter (cmt1 = "A 2D unstructured mesh : 15 nodes, 12 cells")
      parameter (mdesc = "A 2D unstructured mesh")
      parameter (mname="2D unstructured mesh")
      parameter (sdim=2, mdim=2)
      parameter (nnodes=15,ntria3=8,nquad4=4)

      data axname /"x", "y"/
      data unname /"cm", "cm"/
      data inicoo /2.,1., 7.,1., 12.,1., 17.,1., 22.,1.,
     &             2.,6., 7.,6., 12.,6., 17.,6., 22.,6.,
     &             2.,11.,7.,11.,12.,11.,17.,11., 22.,11./
      data triacy /1,7,6,   2,7,1,  3,7,2,   8,7,3,
     &             13,7,8, 12,7,13, 11,7,12, 6,7,11/
      data quadcy /3,4,9,8,    4,5,10,9,
     &             15,14,9,10, 13,8,9,14/ 
C     transformation matrix (step 1) : rotation about the Y-axis : 45 degrees
      data trama1 /0.0, 0.0, 0.0, 0.92388, 0.0, 0.38268, 0.0/
C     transformation matrix (step 2) : rotation about the Y-axis : 90 degrees
      data trama2 /0.0, 0.0, 0.0, 0.707,   0.0, 0.707,   0.0/
C
C     file creation
      call mfiope(fid,fname,MED_ACC_CREAT,cret)
      if (cret .ne. 0 ) then
         print *,"ERROR : file creation"
         call efexit(-1)
      endif
C
C     write a comment in the file
      call mficow(fid,cmt1,cret)
      if (cret .ne. 0 ) then
         print *,"ERROR : write file description"
         call efexit(-1)
      endif
C
C     mesh creation : a 2D unstructured mesh
      call mmhcre(fid, mname, sdim, mdim, MED_UNSTRUCTURED_MESH, mdesc,
     &           "", MED_SORT_DTIT, MED_CARTESIAN, axname, unname, cret)
      if (cret .ne. 0 ) then
         print *,"ERROR : mesh creation"
         call efexit(-1)
      endif
C
C
C     initial nodes coordinates in a cartesian axis in full interlace mode 
C     (X1,Y1, X2,Y2, X3,Y3, ...) with no iteration and computation step
      call mmhcpw(fid, mname, MED_NO_DT, MED_NO_IT, 0.0D0,
     &            MED_COMPACT_STMODE, MED_NO_PROFILE,
     &            MED_FULL_INTERLACE, MED_ALL_CONSTITUENT,
     &            nnodes, inicoo, cret)
      if (cret .ne. 0 ) then
         print *,"ERROR : nodes coordinates"
         call efexit(-1)
      endif
C
C
C     cells connectivity is defined in nodal mode
      call mmhypw(fid, mname, MED_NO_DT, MED_NO_IT, 0.0D0,
     &            MED_CELL, MED_TRIA3, MED_NODAL,
     &            MED_COMPACT_STMODE, MED_NO_PROFILE,
     &            MED_FULL_INTERLACE, MED_ALL_CONSTITUENT,
     &            ntria3, triacy, cret)
      if (cret .ne. 0 ) then
         print *,"ERROR : triangular cells connectivity"
         call efexit(-1)
      endif
C
C
      call mmhypw(fid, mname, MED_NO_DT, MED_NO_IT, 0.0D0,
     &            MED_CELL, MED_QUAD4, MED_NODAL,
     &            MED_COMPACT_STMODE, MED_NO_PROFILE,
     &            MED_FULL_INTERLACE, MED_ALL_CONSTITUENT,
     &            nquad4, quadcy, cret)
      if (cret .ne. 0 ) then
         print *,"ERROR : quadrangular cells connectivity"
         call efexit(-1)
      endif
C
C
C     Mesh deformation (nodes coordinates) in 2 steps 
C     A rotation by step for each node
C
C     STEP 1 : dt1 = 5.5, it = 1
      call mmhtfw(fid, mname, 1, 1, 5.5D0, trama1, cret)
C
C
C     STEP 2 : dt2 = 8.9, it = 1
      call mmhtfw(fid, mname, 2, 1, 8.9D0, trama2, cret)
C
C
C     create family 0 : by default, all mesh entities family number is 0
      call mfacre(fid, mname,MED_NO_NAME, 0, 0, MED_NO_GROUP, cret)
      if (cret .ne. 0 ) then
         print *,"ERROR : create family 0"
         call efexit(-1)
      endif
C
C
C     close file
      call mficlo(fid,cret)
      if (cret .ne. 0 ) then
         print *,"ERROR :  close file"
         call efexit(-1)
      endif        
C
C
      end
C
