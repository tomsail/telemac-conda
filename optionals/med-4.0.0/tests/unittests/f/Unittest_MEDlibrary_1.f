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
C * Tests for library module
C *
C *****************************************************************************
      program MEDlib
C     
      implicit none
      include 'med.hf'
C
C     
      integer cret
      integer major,minor,rel
      character*20 version
C 
C
C     get library version numbers
      call mlbnuv(major,minor,rel,cret)
      print *,cret
      print *,major,minor,rel
      if (cret .ne. 0 ) then
         print *,'ERROR : get library version numbers'
         call efexit(-1)
      endif  
C
C
C     get library version numbers in a string
      call mlbstv(version,cret)
      print *,cret
      print *,version
      if (cret .ne. 0 ) then
         print *,'ERROR : get library version numbers in a string'
         call efexit(-1)
      endif  
C
C
C     get Hdf library version numbers
      call mlbhnv(major,minor,rel,cret)
      print *,cret
      print *,major,minor,rel
      if (cret .ne. 0 ) then
         print *,'ERROR : get hdf-5 library version numbers'
         call efexit(-1)
      endif  
C
C
C     get Hdf library version numbers in a string
      call mlbhsv(version,cret)
      print *,cret
      print *,version
      if (cret .ne. 0 ) then
         print *,'ERROR : get hdf-5 library version numbers in a string'
         call efexit(-1)
      endif  
C
C
C     flush all data and clean memory
      call  mlbclo(cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'ERROR : MED library close'
         call efexit(-1)
      endif      

      end
