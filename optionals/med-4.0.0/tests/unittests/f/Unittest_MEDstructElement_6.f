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
      program MEDstructElement6
C     
      implicit none
      include 'med.hf'
C
C     
      integer cret
      integer*8 fid

      character*64  fname
      parameter (fname = "Unittest_MEDstructElement_4.med")
      character*64  mname2
      parameter (mname2 = "model name 2")
      integer dim2
      parameter (dim2=2)
      character*64  smname2
      parameter (smname2="support mesh name")
      integer setype2
      parameter (setype2=MED_NODE)
      integer sgtype2
      parameter (sgtype2=MED_NO_GEOTYPE)
      integer mtype2
      integer sdim1
      parameter (sdim1=2)
      character*200 description1
      parameter (description1="support mesh1 description")
      character*64 aname1, aname2, aname3
      parameter (aname1="integer constant attribute name")
      parameter (aname2="real constant attribute name")
      parameter (aname3="string constant attribute name")
      integer atype1,atype2,atype3
      parameter (atype1=MED_ATT_INT)
      parameter (atype2=MED_ATT_FLOAT64)
      parameter (atype3=MED_ATT_NAME)
      integer anc1,anc2,anc3
      parameter (anc1=2)
      parameter (anc2=1)
      parameter (anc3=1)
c
      integer mgtype,mdim,setype,snnode,sncell
      integer sgtype,ncatt,nvatt,profile
      character*64 pname,smname,aname
      integer      atype,anc,psize
      integer i
C 
C
C     file creation
      call mfiope(fid,fname,MED_ACC_RDONLY,cret)
      print *,'Open file',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : file creation'
         call efexit(-1)
      endif 
C
C     read information about struct model
C
      call msesin(fid,mname2,mgtype,mdim,smname,
     &            setype,snnode,sncell,sgtype,
     &            ncatt,profile,nvatt,cret)
      print *,'Read information about struct element (by name)',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : information about struct element (by name) '
         call efexit(-1)
      endif 
C
C     iteration on each constant attribute
C
      do i=1,ncatt
C
C
C     read information about constant attribute
C
      call msecai(fid,mname2,i,aname,atype,anc,
     &            setype,pname,psize,cret)
      print *,'Read information about constant attribute: ',aname1,cret
      if (cret .ne. 0 ) then
         print *,'ERROR : information about attribute'
         call efexit(-1)
      endif
c
      if (i. eq. 1) then
         if ( (atype .ne. atype1) .or.
     &        (anc .ne. anc1) .or.
     &        (setype .ne. setype2) .or.
     &        (pname .ne. MED_NO_PROFILE) .or.
     &        (psize .ne. 0)
     &       )  then
            print *,'ERROR : information about constant attribute '
            call efexit(-1)
         endif 
      endif
c
      if (i .eq. 2) then
         if ( (atype .ne. atype2) .or.
     &        (anc .ne. anc2) .or.
     &        (setype .ne. setype2) .or.
     &        (pname .ne. MED_NO_PROFILE) .or.
     &        (psize .ne. 0)
     &        )  then
            print *,'ERROR : information about constant attribute'
            call efexit(-1)
         endif
      endif
c
      if (i .eq. 3) then
         if ( (atype .ne. atype3) .or.
     &        (anc .ne. anc3) .or.
     &        (setype .ne. setype2) .or.
     &        (pname .ne. MED_NO_PROFILE) .or.
     &        (psize .ne. 0)
     &        )  then
            print *,'ERROR : information about constant attribute'
            call efexit(-1)
         endif 
      endif
c
      enddo
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

