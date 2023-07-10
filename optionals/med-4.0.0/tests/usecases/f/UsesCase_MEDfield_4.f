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
C * Field use case 4 : write a field with computing steps
C *
C *****************************************************************************
      program UsesCase_MEDfield_4
C     
      implicit none
      include 'med.hf77'
C
C     
      integer cret
      integer*8 fid

C     component number, node number
      integer ncompo
C     triangular elements number, quadrangular elements number
      integer ntria3, nquad4
C     med file name,  link file name
      character*64  fname, lfname
C     mesh name, field name, component name, commponent unit
      character*64  mname, finame, cpname, cpunit
      character*16 dtunit
      real*8 dt
      integer ndt, nit
C     mesh num dt, mesh num it
      integer mnumdt, mnumit
C
      real*8 t3vs1(8)
      real*8 t3vs2(8)
      real*8 q4vs1(4)
      real*8 q4vs2(4)
C
      parameter (fname = "UsesCase_MEDfield_4.med")
      parameter (lfname = "./UsesCase_MEDmesh_1.med")
      parameter (mname = "2D unstructured mesh")
      parameter (finame = "TEMPERATURE_FIELD")
      parameter (cpname ="TEMPERATURE", cpunit = "C")
      parameter (dtunit = "ms")
      parameter (ncompo = 1 )
      parameter (ntria3 =  8, nquad4 = 4)

      data t3vs1 / 1000., 2000., 3000., 4000., 
     &             5000., 6000., 7000., 8000. /
      data q4vs1 / 10000., 20000., 30000., 4000. /
      data t3vs2 / 1500., 2500., 3500., 4500., 
     &             5500., 6500., 7500., 8500. /
      data q4vs2 / 15000., 25000., 35000., 45000. /
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
C                      UsecaseMEDmesh_1.f use case. Computation step unit in 'ms'
      call mfdcre(fid,finame,MED_FLOAT64,ncompo,cpname,cpunit,dtunit,
     &            mname,cret)
      if (cret .ne. 0 ) then
         print *,'ERROR : create field ...'
         call efexit(-1)
      endif
C
C
C     two computation steps :
C      - first  on meshname MED_NO_DT,MED_NO_IT mesh computation step
C      - second on meshname 1,3 mesh computation step
C     write values at cell centers : 8 MED_TRIA3 and 4 MED_QUAD4
C
C
C     STEP 1 : dt1 = 5.5, it = 1
C
C
C     MED_TRIA3
      dt = 5.5d0
      ndt = 1
      nit = 1
      call mfdrvw(fid,finame,ndt,nit,dt,MED_CELL,MED_TRIA3,
     &            MED_FULL_INTERLACE,MED_ALL_CONSTITUENT,
     &            ntria3,t3vs1,cret)
      if (cret .ne. 0 ) then
         print *,'ERROR : write field values on MED_TRIA3'
         call efexit(-1)
      endif
C
C
C     MED_QUAD4
      call mfdrvw(fid,finame,ndt,nit,dt,MED_CELL,MED_QUAD4,
     &            MED_FULL_INTERLACE,MED_ALL_CONSTITUENT,
     &            nquad4,q4vs1,cret)
      if (cret .ne. 0 ) then
         print *,'ERROR : write field values on MED_TRIA3'
         call efexit(-1)
      endif
C
C
C     STEP 2 : dt2 = 8.9, it = 1
C
C     MED_TRIA3
      dt = 8.9d0
      ndt = 2
      nit = 1
      call mfdrvw(fid,finame,ndt,nit,dt,MED_CELL,MED_TRIA3,
     &            MED_FULL_INTERLACE,MED_ALL_CONSTITUENT,
     &            ntria3,t3vs2,cret)
      if (cret .ne. 0 ) then
         print *,'ERROR : write field values on MED_TRIA3'
         call efexit(-1)
      endif
C
C
C     MED_QUAD4
      call mfdrvw(fid,finame,ndt,nit,dt,MED_CELL,MED_QUAD4,
     &            MED_FULL_INTERLACE,MED_ALL_CONSTITUENT,
     &            nquad4,q4vs2,cret)
      if (cret .ne. 0 ) then
         print *,'ERROR : write field values on MED_TRIA3'
         call efexit(-1)
      endif
C
C
C     Write associated mesh computation step
      mnumdt = 1
      mnumit = 3
      call mfdcmw(fid,finame,ndt,nit,mnumdt,mnumit,cret)
      if (cret .ne. 0 ) then
         print *,'ERROR : write field mesh computation step error '
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
