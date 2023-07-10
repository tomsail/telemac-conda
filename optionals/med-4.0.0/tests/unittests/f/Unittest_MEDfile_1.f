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
C * Tests for file module
C *
C *****************************************************************************
      program MEDfile
C     
      implicit none
      include 'med.hf'
C
C     
      integer cret
      integer*8 fid

      character*64  fname
      parameter (fname = "Unittest_MEDfile_1.med")  
      character*200 cmt1
      parameter (cmt1 = "My first comment")  
      character*200 cmt2
      parameter (cmt2 = "My second comment")  
      character*200 cmtrd
      integer hdfok, medok
      character*32 version
      integer major, minor, rel
C 
C
C     file creation
      call mfiope(fid,fname,MED_ACC_CREAT,cret)
      print *,cret
      print *,fid
      if (cret .ne. 0 ) then
         print *,'ERROR : file creation'
         call efexit(-1)
      endif  
C
C
C     write a comment
      call mficow(fid,cmt1,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'ERROR : write a comment'
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
C     open file in read only access mode
      call mfiope(fid,fname,MED_ACC_RDONLY,cret)
      print *,cret
      print *,fid
      if (cret .ne. 0 ) then
         print *,'ERROR : open file in READ_ONLY access mode'
         call efexit(-1)
      endif  
C
C
C     read med library version in the file
      call mfinvr(fid,major,minor,rel,cret)
      print *,cret
      print *,major,minor,rel
      if (cret .ne. 0 ) then
         print *,'ERROR : read MED (num) version in the file'
         call efexit(-1)
      endif  

      call mfisvr(fid,version,cret)
      print *,cret
      print *,version
      if (cret .ne. 0 ) then
         print *,'ERROR : read MED (str) version in the file'
         call efexit(-1)
      endif  
C
C
C     read a comment
      call mficor(fid,cmtrd,cret)
      print *,cret
      print *,cmtrd
      if (cret .ne. 0 ) then
         print *,'ERROR : read a comment'
         call efexit(-1)
      endif  
      if (cmtrd .ne. cmt1) then
         print *,'ERROR : file comment is not the good one'
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
C     open file in read and write access mode
      call mfiope(fid,fname,MED_ACC_RDWR,cret)
      print *,cret
      print *,fid
      if (cret .ne. 0 ) then
         print *,'ERROR : open file in READ and WRITE access mode'
         call efexit(-1)
      endif  
C
C
C     write a comment
      call mficow(fid,cmt2,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'ERROR : write a comment'
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
C     open file in read and extension access mode
      call mfiope(fid,fname,MED_ACC_RDEXT,cret)
      print *,cret
      print *,fid
      if (cret .ne. 0 ) then
         print *,'ERROR : open file in READ and WRITE access mode'
         call efexit(-1)
      endif      
C
C
C     write a comment has to be impossible because it exits
      call mficow(fid,cmt1,cret)
      print *,cret
      if (cret .eq. 0 ) then
         print *,'ERROR : write a comment has to be impossible'
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
C     test file compatiblity with hdf-5 et med
      print *,fname
      call mficom(fname,hdfok,medok,cret)
      print *,cret
      print *,medok,hdfok
      if (cret .ne. 0 ) then
         print *,'ERROR : file compatibility'
         call efexit(-1)
      endif  
      if (hdfok .ne. 1) then
         print *,'ERROR : the file must be in hdf5 format'
         call efexit(-1)
      endif  
      if (medok .ne. 1) then
         print *,'ERROR : the file must be compatible'
         call efexit(-1)
      endif  
      end

