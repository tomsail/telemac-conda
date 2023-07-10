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

      subroutine mlbclo(cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mlbclo
c
      implicit none
      save
c
      integer cret
      integer mlbfclo
c
      cret = mlbfclo()
c      
      return
      end



      subroutine mlbnuv(major,minor,rel,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mlbnuv
c
      implicit none
      save
c
      integer major, minor, rel
      integer cret
      integer mlbfnuv
c
      cret = mlbfnuv(major,minor,rel)
c      
      return
      end


      subroutine mlbstv(version,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mlbstv
c
      implicit none
      save
c
      character*(*) version
      integer cret
      integer mlbfstv
c
      cret = mlbfstv(version,len(version))
c      
      return
      end



      subroutine mlbhnv(major,minor,rel,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mlbhnv
c
      implicit none
      save
c
      integer major, minor, rel
      integer cret
      integer mlbfhnv
c
      cret = mlbfhnv(major,minor,rel)
c      
      return
      end


      subroutine mlbhsv(version,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mlbhsv
c
      implicit none
      save
c
      character*(*) version
      integer cret
      integer mlbfhsv
c
      cret = mlbfhsv(version,len(version))
c      
      return
      end
