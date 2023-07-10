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
      program MEDstructElement5
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
      character*16 nomcoo2D(2)
      character*16 unicoo2D(2)
      data  nomcoo2D /"x","y"/, unicoo2D /"cm","cm"/
      real*8 coo(2*3)
      data coo / 0.0, 0.0, 1.0,1.0, 2.0,2.0 /
      integer nnode
      parameter (nnode=3)
      integer nseg2
      parameter (nseg2=2)
      integer seg2(4)
      data seg2 /1,2, 2,3/
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
      integer aval1(3*2)
      data aval1 /1,2,3,4,5,6/
      real*8 aval2(3)
      data aval2 /1., 2., 3. /
      character*64 aval3(3)
      data aval3 /"VAL1","VAL2","VAL3"/
      integer itsize,ftsize,stsize
      parameter (itsize=4)
      parameter (ftsize=8)
      parameter (stsize=64)
c
      integer mgtype,mdim,setype,snnode,sncell
      integer sgtype,ncatt,nvatt,profile
      character*64 pname,smname
      integer      atype,anc,psize,tsize
      integer val1(2*3)
      real*8 val2(3)
      character*64 val3(3)
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
C     read constant attribute
C     with a direct access by name
C
      call msecni(fid,mname2,aname1,atype,anc,
     &            setype,pname,psize,cret)
      print *,'Read information about constant attribute: ',aname1,cret
      if (cret .ne. 0 ) then
         print *,'ERROR : information about attribute (by name)'
         call efexit(-1)
      endif
      if ( (atype .ne. atype1) .or.
     &     (anc .ne. anc1) .or.
     &     (setype .ne. setype2) .or.
     &     (pname .ne. MED_NO_PROFILE) .or.
     &     (psize .ne. 0)
     &    )  then
         print *,'ERROR : information about struct element (by name) '
         call efexit(-1)
      endif 
c     read size of attribute type
      call mseasz(atype,tsize,cret)
      print *,'Read information type size: ',tsize,cret
      if (cret .ne. 0 ) then
         print *,'ERROR : information about type size'
         call efexit(-1)
      endif

c     read values
      call mseiar(fid,mname2,aname1,val1,cret)
      print *,'Read attribute values: ',aname1,cret
      if (cret .ne. 0 ) then
         print *,'ERROR : attribute values'
         call efexit(-1)
      endif
      if ((aval1(1) .ne. val1(1)) .or.
     &    (aval1(2) .ne. val1(2)) .or.
     &    (aval1(3) .ne. val1(3)) .or.
     &    (aval1(4) .ne. val1(4)) .or.
     &    (aval1(5) .ne. val1(5)) .or.
     &    (aval1(6) .ne. val1(6)) 
     &   ) then
          print *,'ERROR : attribute values'
         call efexit(-1)
      endif
c
      call msecni(fid,mname2,aname2,atype,anc,
     &            setype,pname,psize,cret)
      print *,'Read information about constant attribute:',aname2,cret
      if (cret .ne. 0 ) then
         print *,'ERROR : information about attribute (by name)'
         call efexit(-1)
      endif
      if ( (atype .ne. atype2) .or.
     &     (anc .ne. anc2) .or.
     &     (setype .ne. setype2) .or.
     &     (pname .ne. MED_NO_PROFILE) .or.
     &     (psize .ne. 0)
     &    )  then
         print *,'ERROR : information about struct element (by name) '
         call efexit(-1)
      endif
c     read size of attribute type
      call mseasz(atype,tsize,cret)
      print *,'Read information type size: ',tsize,cret
      if (cret .ne. 0 ) then
         print *,'ERROR : information about type size'
         call efexit(-1)
      endif
      if (tsize .ne. ftsize) then
         print *,'ERROR : information about type size'
         call efexit(-1)
      endif 
c     read values
      call mserar(fid,mname2,aname2,val2,cret)
      print *,'Read attribute values: ',aname2,cret
      if (cret .ne. 0 ) then
         print *,'ERROR : attribute values'
         call efexit(-1)
      endif
      if ((aval2(1) .ne. val2(1)) .or.
     &    (aval2(2) .ne. val2(2)) .or.
     &    (aval2(3) .ne. val2(3)) 
     &   ) then
          print *,'ERROR : attribute values'
         call efexit(-1)
      endif
c
      call msecni(fid,mname2,aname3,atype,anc,
     &            setype,pname,psize,cret)
      print *,'Read information about constant attribute:',aname3,cret
      if (cret .ne. 0 ) then
         print *,'ERROR : information about attribute (by name)'
         call efexit(-1)
      endif
      if ( (atype .ne. atype3) .or.
     &     (anc .ne. anc3) .or.
     &     (setype .ne. setype2) .or.
     &     (pname .ne. MED_NO_PROFILE) .or.
     &     (psize .ne. 0)
     &    )  then
         print *,'ERROR : information about struct element (by name) '
         call efexit(-1)
      endif 
c     read size of attribute type
      call mseasz(atype,tsize,cret)
      print *,'Read information type size: ',tsize,cret
      if (cret .ne. 0 ) then
         print *,'ERROR : information about type size'
         call efexit(-1)
      endif
      if (tsize .ne. stsize) then
         print *,'ERROR : information about type size'
         call efexit(-1)
      endif 
c     read values
      call msesar(fid,mname2,aname3,val3,cret)
      print *,'Read attribute values: ',aname3,cret
      if (cret .ne. 0 ) then
         print *,'ERROR : attribute values'
         call efexit(-1)
      endif
      if ((aval3(1) .ne. val3(1)) .or.
     &    (aval3(2) .ne. val3(2)) .or.
     &    (aval3(3) .ne. val3(3)) 
     &   ) then
          print *,'ERROR : attribute values |',aval3(1),'|',aval3(2),
     &        '|',aval3(3),'|'
          print *,'ERROR : attribute values |',val3(1),'|',val3(2),
     &        '|',val3(3),'|'
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

