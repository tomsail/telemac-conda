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
      program MEDstructElement10
C     
      implicit none
      include 'med.hf'
C
C     
      integer cret
      integer*8 fid

      character*64  fname
      parameter (fname = "Unittest_MEDstructElement_9.med")
      character*64  mname2
      parameter (mname2 = "model name 2")
      integer mtype2
      character*64 aname1, aname2, aname3
      parameter (aname1="integer attribute name")
      parameter (aname2="real attribute name")
      parameter (aname3="string attribute name")
      integer atype1,atype2,atype3
      parameter (atype1=MED_ATT_INT)
      parameter (atype2=MED_ATT_FLOAT64)
      parameter (atype3=MED_ATT_NAME)
      integer anc1,anc2,anc3
      parameter (anc1=2)
      parameter (anc2=1)
      parameter (anc3=2)
      integer aval1(2)
      data aval1 /1,2/
      real*8 aval2(1)
      data aval2 /1./
      character*64 aval3(2)
      data aval3 /"VAL1","VAL2"/
      character*64 pname,cname
      parameter (cname="computation mesh")
      integer nentity
      parameter (nentity=1)
c
      integer atype,anc
      integer rval1(2)
      real*8 rval2(1)
      character*64 rval3(2)
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
C     informations about attributes     
C
      call msevni(fid,mname2,aname1,atype,anc,cret)
      print *,'Read information about attribute',aname1, cret
      if (cret .ne. 0) then
         print *,'ERROR : attribute infromation'
         call efexit(-1)
      endif
      if ( (atype .ne. atype1) .or.
     &     (anc .ne. anc1)
     &   ) then
         print *,'ERROR : attribute information'
         call efexit(-1)
      endif
c
      call msevni(fid,mname2,aname2,atype,anc,cret)
      print *,'Read information about attribute',aname2, cret
      if (cret .ne. 0) then
         print *,'ERROR : attribute infromation'
         call efexit(-1)
      endif
      if ( (atype .ne. atype2) .or.
     &     (anc .ne. anc2)
     &   ) then
         print *,'ERROR : attribute information'
         call efexit(-1)
      endif
c  
      call msevni(fid,mname2,aname3,atype,anc,cret)
      print *,'Read information about attribute',aname3, cret
      if (cret .ne. 0) then
         print *,'ERROR : attribute information'
         call efexit(-1)
      endif
      if ( (atype .ne. atype3) .or.
     &     (anc .ne. anc3)
     &   ) then
         print *,'ERROR : attribute information'
         call efexit(-1)
      endif

C
C     read attributes values
C
      call msesgt(fid,mname2,mtype2,cret)
      print *,'Read struct element type (by name) : ',mtype2, cret
      if (cret .ne. 0 ) then
         print *,'ERROR : struct element type (by name)'
         call efexit(-1)
      endif 
c
      call mmhiar(fid,cname,MED_NO_DT,MED_NO_IT,
     &            mtype2,aname1,rval1,cret)
      print *,'Read attribute values',cret
      if (cret .ne. 0) then
         print *,'ERROR : read attribute values'
         call efexit(-1)
      endif  
      if ( (aval1(1) .ne. rval1(1)) .or.
     &     (aval1(2) .ne. rval1(2))
     &   ) then
         print *,'ERROR : attribute information'
         call efexit(-1)
      endif
c
      call mmhrar(fid,cname,MED_NO_DT,MED_NO_IT,
     &            mtype2,aname2,rval2,cret)
      print *,'Read attribute values',cret
      if (cret .ne. 0) then
         print *,'ERROR : read attribute values'
         call efexit(-1)
      endif  
      if ( (aval2(1) .ne. rval2(1))
     &   ) then
         print *,'ERROR : attribute information'
         call efexit(-1)
      endif
c
      call mmhsar(fid,cname,MED_NO_DT,MED_NO_IT,
     &            mtype2,aname3,rval3,cret)
      print *,'Read attribute values',cret
      if (cret .ne. 0) then
         print *,'ERROR : read attribute values'
         call efexit(-1)
      endif  
      if ( (aval3(1) .ne. rval3(1)) .or.
     &     (aval3(2) .ne. rval3(2))
     &   ) then
         print *,'ERROR : attribute information'
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

