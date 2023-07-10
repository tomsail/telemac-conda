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
C * Tests for profile module
C *
C *****************************************************************************
      program MEDprofile2
C     
      implicit none
      include 'med.hf'
C
C     
      integer cret
      integer*8 fid

      character*64  fname, pname1, pname2
      parameter (fname="Unittest_MEDprofile_1.med")  
      parameter (pname1="Profile name1")
      parameter (pname2="Profile name 2")
      integer psize1,psize2
      parameter (psize1=4, psize2=2)
      integer profile1(4), profile2(2)
      data profile1 /1,2, 3,4/
      data profile2 /5,6/ 
      integer npro,n
      parameter (npro=2)
      integer it,psize
      character*64 pname
      integer profile(4)
C 
C
C     open file
      call mfiope(fid,fname,MED_ACC_RDONLY,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'ERROR : open file'
         call efexit(-1)
      endif  
C
C
C     how many profile 
      call mpfnpf(fid,n,cret)
      print *,cret
      print *,n
      if (cret .ne. 0 ) then
         print *,'ERROR : number of profile'
         call efexit(-1)
      endif  
      if (n .ne. npro)  then
         print *,'ERROR : number of profile'
         call efexit(-1)
      endif
C
C
C     Read profile(s) name and size
C     Then read profile array
      do it=1,n
         call mpfpfi(fid,it,pname,psize,cret)
         print *,cret
         if (cret .ne. 0 ) then
            print *,'ERROR : name and size of profile'
            call efexit(-1)
         endif
c
         call mpfprr(fid,pname,profile,cret)
         print *,cret
         if (cret .ne. 0 ) then
            print *,'ERROR : read profile'
            call efexit(-1)
         endif    
c
         if (it .eq. 1) then
            if ((pname .ne. pname2) .or.
     &          (psize .ne. psize2)) then
               print *,'ERROR : name and size of profile'
               call efexit(-1)
            endif
            if ((profile(1) .ne. profile2(1)) .or.
     &          (profile(2) .ne. profile2(2))) then
               print *,'ERROR : profile array'
               call efexit(-1)
            endif
         endif
c
         if (it .eq. 2) then
            if ((pname .ne. pname1) .or.
     &          (psize .ne. psize1)) then
                print *,'ERROR : name and size of profile'
                call efexit(-1)
             endif
            if ((profile(1) .ne. profile1(1)) .or.
     &          (profile(2) .ne. profile1(2)) .or.
     &          (profile(3) .ne. profile1(3)) .or.
     &          (profile(4) .ne. profile1(4)) )then
               print *,'ERROR : profile array'
               call efexit(-1)
            endif
         endif
      enddo
C
C
C     read profile size by the name
      call mpfpsn(fid,pname1,psize,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'ERROR : size of profile'
         call efexit(-1)
      endif  
c
      if (psize .ne. psize1) then
         print *,'ERROR : size of profile'
         call efexit(-1)
      endif
c
      call mpfpsn(fid,pname2,psize,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'ERROR : size of profile'
         call efexit(-1)
      endif  
c
      if (psize .ne. psize2) then
         print *,'ERROR : size of profile'
         call efexit(-1)
      endif
C
C
C     close file
      call mficlo(fid,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'ERROR :  close file'
         call efexit(-1)
      endif        
C
C
C
      end

