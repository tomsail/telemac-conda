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
C * Tests for filter module
C *
C *****************************************************************************
      program MEDfilter2
C     
      implicit none
      include 'med.hf'
C
C     
      integer cret
      integer*8 fid

      character*64 fname
      parameter (fname = "Unittest_MEDfilter_2.med")
      integer nflt
      parameter (nflt=1)
      integer flta(1)
      integer*8 flt(1)
      integer nent,nvale,scent
      parameter (nent=10,nvale=1,scent=2)
C 
C
C     open file
      call mfiope(fid,fname,MED_ACC_CREAT,cret)
      print *,'Open file',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : open file'
         call efexit(-1)
      endif
C
C
C     filter creation
      call mfrall(nflt,flt,cret)
      print *,'Filter array allocation',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : filter array allocation'
         call efexit(-1)
      endif
c
      call mfrcre(fid,nent,nvale,scent,MED_ALL_CONSTITUENT,
     &            MED_FULL_INTERLACE,MED_GLOBAL_STMODE, 
     &            MED_NO_PROFILE,MED_UNDEF_SIZE,flta,flt(1),
     &            cret)
      print *,'Filter creation',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : filter creation'
         call efexit(-1)
      endif
C
C
C     filter deallocation
      call mfrdea(nflt,flt,cret)
      print *,'Filter array deallocation',cret
      if (cret .ne. 0 ) then
         print *,'ERROR : filter dearray allocation'
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

