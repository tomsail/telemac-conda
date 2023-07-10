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
      program MEDstructElement11
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
c
      integer atype,anc
      character*64 aname
      integer it,natt
      parameter (natt=3)
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
C
      do it=1,natt
         call msevai(fid,mname2,it,aname,atype,anc,cret)
         print *,'Read informations about attribute : ',aname,cret
         if (cret .ne. 0) then
            print *,'ERROR : attribute information'
            call efexit(-1)
         endif
c
         if (it .eq. 1) then
            if ( (atype .ne. atype1) .or.
     &           (anc .ne. anc1)
     &           ) then
               print *,'ERROR : attribute information'
               call efexit(-1)
            endif
         endif
c
         if (it .eq. 2) then
            if ( (atype .ne. atype2) .or.
     &           (anc .ne. anc2)
     &           ) then
               print *,'ERROR : attribute information'
               call efexit(-1)
            endif
         endif
c
         if (it .eq. 3) then
            if ( (atype .ne. atype3) .or.
     &           (anc .ne. anc3)
     &           ) then
               print *,'ERROR : attribute information'
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

