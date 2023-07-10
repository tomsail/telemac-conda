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
C * How to create an structured mesh
C * Use case 4 : write a 2D structured mesh (5x3 cartesian grid)
C *
C *****************************************************************************
      program UsesCase_MEDmesh_4
C     
      implicit none
      include 'med.hf77'
C
C     
      integer cret
      integer*8 fid

      integer sdim, mdim, stype, mtype, atype
      integer axis, isize, entype, nquad4
      character*200 mdesc
      character*64  fname
      character*64  mname
C     axis name      
      character*16 axname(2)
C     unit name      
      character*16 unname(2)
      character*16 dtunit
      character*16 cnames(8)
      real*8 dt
      real*8 cooXaxis(5)
      real*8 cooYaxis(3)
      parameter (fname = "UsesCase_MEDmesh_4.med")  
      parameter (mdesc = "A 2D structured mesh")
      parameter (mname = "2D structured mesh")  
      parameter (sdim = 2, mdim = 2)
      parameter (stype=MED_SORT_DTIT, mtype=MED_STRUCTURED_MESH)
      parameter (atype=MED_CARTESIAN_GRID)
      parameter (nquad4=8)
      parameter (dt=0.0d0)
      data dtunit  /" "/
      data axname  /"x" ,"y"/
      data unname  /"cm","cm"/
      data cnames /"CELL_1","CELL_2",
     &             "CELL_3","CELL_4",
     &             "CELL_5","CELL_6",
     &             "CELL_7","CELL_8"/
      data cooXaxis /1.,2.,3.,4.,5./
      data cooYaxis /1.,2.,3./
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
C     mesh creation
      call mmhcre(fid, mname, sdim, mdim, mtype,mdesc,
     &            dtunit, stype, atype, axname, unname, cret)
      if (cret .ne. 0 ) then
         print *,'ERROR : mesh creation'
         call efexit(-1)
      endif  
C
C
C     specify grid type
      call mmhgtw(fid,mname,MED_CARTESIAN_GRID,cret)
      if (cret .ne. 0 ) then
         print *,'ERROR : write grid type'
         call efexit(-1)
      endif  
C
C
C     write axis "X" and "Y" coordinates
      axis = 1
      isize = 5
      call mmhgcw(fid,mname,MED_NO_DT,MED_NO_IT,dt, 
     &            axis,isize,cooXaxis,cret)
      if (cret .ne. 0 ) then
         print *,'ERROR : write X coordinates'
         call efexit(-1)
      endif
      axis = 2
      isize = 3
      call mmhgcw(fid,mname,MED_NO_DT,MED_NO_IT,dt, 
     &            axis,isize,cooYaxis,cret)
      if (cret .ne. 0 ) then
         print *,'ERROR : write Y coordinates'
         call efexit(-1)
      endif
C
C
C     optionnal : names for nodes or elements
C     In this case, a name is given to the cells of the mesh
      call mmheaw(fid,mname,MED_NO_DT,MED_NO_IT,MED_CELL,MED_QUAD4,
     &            nquad4,cnames,cret)
      if (cret .ne. 0 ) then
         print *,'ERROR : write names for elements'
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
