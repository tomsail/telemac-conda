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

      subroutine mpfope(fid, name, access, com, info, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mpfope
c
      implicit none
      save
      character *(*) name
      integer*8 fid
      integer access
      integer com,info,cret
      integer mpffope
c
      fid = mpffope(name, access, len(name), com, info)
      if (fid.lt.0) then
         cret=-1
      else
         cret=0
      endif
c      
      return
      end

