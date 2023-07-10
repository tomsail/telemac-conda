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
C * Tests for struct element module
C *
C *****************************************************************************
      program MEDstructElement2
C     
      implicit none
      include 'med.hf'
C
C     
      integer cret
      integer*8 fid

      character*64  fname
      parameter (fname = "Unittest_MEDstructElement_1.med")
      character*64  mname1, mname2, mname3
      parameter (mname1 = "model name 1")
      parameter (mname2 = "model name 2")
      parameter (mname3 = "model name 3")
      integer dim1, dim2, dim3
      parameter (dim1=2)
      parameter (dim2=2)
      parameter (dim3=2)
      character*64  smname1
      parameter (smname1=MED_NO_NAME)
      character*64  smname2
      parameter (smname2="support mesh name")
      integer setype1
      parameter (setype1=MED_NONE)
      integer setype2
      parameter (setype2=MED_NODE)
      integer setype3
      parameter (setype3=MED_CELL)
      integer sgtype1
      parameter (sgtype1=MED_NO_GEOTYPE)
      integer sgtype2
      parameter (sgtype2=MED_NO_GEOTYPE)
      integer sgtype3
      parameter (sgtype3=MED_SEG2)
      integer mtype1,mtype2,mtype3
      parameter (mtype1=601)
      parameter (mtype2=602)
      parameter (mtype3=603)
      integer nnode1,nnode2
      parameter (nnode1=1)
      parameter (nnode2=3)
      integer ncell2
      parameter (ncell2=2)
      integer ncell1
      parameter (ncell1=0)
      integer ncatt1,profile1,nvatt1
      parameter (ncatt1=0)
      parameter (nvatt1=0)
      parameter (profile1=0)
c
      integer mgtype,mdim,setype,snnode,sncell
      integer sgtype,ncatt,nvatt,profile
      character*64 smname
C 
C
C     open file
      call mfiope(fid,fname,MED_ACC_RDONLY,cret)
      print *,'Open file',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : file creation'
         call efexit(-1)
      endif 
C
C
C     Read information about a struct element model
C     Access by name
      call msesin(fid,mname1,mgtype,mdim,smname,
     &            setype,snnode,sncell,sgtype,
     &            ncatt,profile,nvatt,cret)
      print *,'Read information about struct element (by name)',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : information about struct element (by name) '
         call efexit(-1)
      endif 
      if ( (mgtype .ne. mtype1) .or.
     &     (mdim .ne. dim1) .or.
     &     (smname .ne. smname1) .or.
     &     (setype .ne. setype1) .or.
     &     (snnode .ne. nnode1) .or.
     &     (sncell .ne. ncell1) .or.
     &     (sgtype .ne. sgtype1) .or.
     &     (ncatt .ne. ncatt1) .or.
     &     (profile .ne. profile1) .or.
     &     (nvatt .ne. nvatt1) 
     &    )  then
         print *,'ERROR : information about struct element (by name) '
         call efexit(-1)
      endif 
C
C
C
      call msesin(fid,mname2,mgtype,mdim,smname,
     &            setype,snnode,sncell,sgtype,
     &            ncatt,profile,nvatt,cret)
      print *,'Read information about struct element (by name)',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : information about struct element (by name) '
         call efexit(-1)
      endif 
      if ( (mgtype .ne. mtype2) .or.
     &     (mdim .ne. dim2) .or.
     &     (smname .ne. smname2) .or.
     &     (setype .ne. setype2) .or.
     &     (snnode .ne. nnode2) .or.
     &     (sncell .ne. ncell1) .or.
     &     (sgtype .ne. sgtype2) .or.
     &     (ncatt .ne. ncatt1) .or.
     &     (profile .ne. profile1) .or.
     &     (nvatt .ne. nvatt1) 
     &    )  then
         print *,'ERROR : information about struct element (by name) '
         call efexit(-1)
      endif 
C
C
C
      call msesin(fid,mname3,mgtype,mdim,smname,
     &            setype,snnode,sncell,sgtype,
     &            ncatt,profile,nvatt,cret)
      print *,'Read information about struct element (by name)',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : information about struct element (by name) '
         call efexit(-1)
      endif 
      if ( (mgtype .ne. mtype3) .or.
     &     (mdim .ne. dim3) .or.
     &     (smname .ne. smname2) .or.
     &     (setype .ne. setype3) .or.
     &     (snnode .ne. nnode2) .or.
     &     (sncell .ne. ncell2) .or.
     &     (sgtype .ne. sgtype3) .or.
     &     (ncatt .ne. ncatt1) .or.
     &     (profile .ne. profile1) .or.
     &     (nvatt .ne. nvatt1) 
     &    )  then
         print *,'ERROR : information about struct element (by name) '
         call efexit(-1)
      endif 
C
C
C     Read model type from the name
      call msesgt(fid,mname1,mgtype,cret)
      print *,'Read struct element type (by name)',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : struct element type (by name)'
         call efexit(-1)
      endif 
      if (mgtype .ne. mtype1) then
         print *,'ERROR : struct element type (by name)'
         call efexit(-1)
      endif
c
c
c     Read model type from the name
      call msesgt(fid,mname2,mgtype,cret)
      print *,'Read struct element type (by name)',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : struct element type (by name)'
         call efexit(-1)
      endif 
      if (mgtype .ne. mtype2) then
         print *,'ERROR : struct element type (by name)'
         call efexit(-1)
      endif
c
c
c     Read model type from the name
      call msesgt(fid,mname3,mgtype,cret)
      print *,'Read struct element type (by name)',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : struct element type (by name)'
         call efexit(-1)
      endif 
      if (mgtype .ne. mtype3) then
         print *,'ERROR : struct element type (by name)'
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

