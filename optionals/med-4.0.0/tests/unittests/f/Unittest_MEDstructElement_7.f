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
      program MEDstructElement7
C     
      implicit none
      include 'med.hf'
C
C     
      integer cret
      integer*8 fid

      character*64  fname
      parameter (fname = "Unittest_MEDstructElement_7.med")
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
      integer aval1(2*2)
      data aval1 /1,2,5,6/
      real*8 aval2(2*1)
      data aval2 /1., 3. /
      character*64 aval3(2*1)
      data aval3 /"VAL1","VAL3"/
      character*64 pname
      parameter (pname="profil name")
      integer psize
      parameter (psize=2)
      integer profil(2)
      data profil / 1,3 /
C 
C
C     file creation
      call mfiope(fid,fname,MED_ACC_CREAT,cret)
      print *,'Open file',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : file creation'
         call efexit(-1)
      endif 
C
C
C     support mesh creation : 2D
      call msmcre(fid,smname2,dim2,dim2,description1,
     &            MED_CARTESIAN,nomcoo2D,unicoo2D,cret)
      print *,'Support mesh creation : 2D space dimension',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : support mesh creation'
        call efexit(-1)
      endif   
c
      call mmhcow(fid,smname2,MED_NO_DT,MED_NO_IT, 
     &            MED_UNDEF_DT,MED_FULL_INTERLACE, 
     &            nnode,coo,cret)
c
      call mmhcyw(fid,smname2,MED_NO_DT,MED_NO_IT,
     &            MED_UNDEF_DT,MED_CELL,MED_SEG2, 
     &            MED_NODAL,MED_FULL_INTERLACE,
     &            nseg2,seg2,cret)
C
C     struct element creation
C
      call msecre(fid,mname2,dim2,smname2,setype2,
     &            sgtype2,mtype2,cret)
      print *,'Create struct element',mtype2, cret
      if ((cret .ne. 0) .or. (mtype2 .lt. 0) ) then
         print *,'ERROR : struct element creation'
         call efexit(-1)
      endif  
C
C     write profile 
C
      call mpfprw(fid,pname,psize,profil,cret)
      print *,'Create a profile : ',pname, cret
      if (cret .ne. 0) then
         print *,'ERROR : profile creation'
         call efexit(-1)
      endif  
C
C     write constant attributes with profiles
C
      call  mseipw(fid,mname2,aname1,atype1,anc1,
     &             setype2,pname,aval1,cret)
      print *,'Create a constant attribute with profile : ',aname1, cret
      if (cret .ne. 0) then
         print *,'ERROR : constant attribute with profile creation'
         call efexit(-1)
      endif  
c
      call  mserpw(fid,mname2,aname2,atype2,anc2,
     &             setype2,pname,aval2,cret)
      print *,'Create a constant attribute with profile : ',aname2, cret
      if (cret .ne. 0) then
         print *,'ERROR : constant attribute with profile creation'
         call efexit(-1)
      endif  
c
      call  msespw(fid,mname2,aname3,atype3,anc3,
     &             setype2,pname,aval3,cret)
      print *,'Create a constant attribute with profile : ',aname3, cret
      if (cret .ne. 0) then
         print *,'ERROR : constant attribute with profile creation'
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

