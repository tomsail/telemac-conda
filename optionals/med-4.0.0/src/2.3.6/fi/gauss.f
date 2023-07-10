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

      subroutine efngau( fid , n , cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efngau
c
      implicit none
      save
c
      integer*8 fid
      integer   n, cret
      integer edfngau
c
      n = edfngau(fid)

      if (n.lt.0) then
         cret = -1
      else
         cret = 0
      endif
c
      return
      end
c
c
c
      subroutine efgaui( fid , indice , locname, typgeo, ngauss , cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efgaui
c
      implicit none
      save
c
      integer*8 fid
      integer  indice,typgeo,ngauss,cret
      character *(*) locname
      integer edfgaui
c
      locname = ' ' 
c
      cret = edfgaui (fid,indice,locname,typgeo,ngauss)
c     
      return
      end
c
c
c
      subroutine efgaue( fid, typgeo, refcoo, mode_coo, ngauss,
     1                  gscoo, wg, locname,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efgaue
c
      implicit none
      save
c      
      integer*8 fid
      integer  typgeo,mode_coo,ngauss,cret
      real*8 refcoo(*),gscoo(*),wg(*)
      character *(*) locname
      integer edfgaue
c
      cret = edfgaue( fid, typgeo, refcoo, mode_coo, ngauss,
     1                gscoo, wg, locname,len(locname))
c
      return
      end
c
c
c
      subroutine efgaul( fid, refcoo, gscoo, wg, 
     1                   mode_coo, locname, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efgaul
c
      implicit none
      save
c      
      integer*8 fid
      integer  mode_coo,cret
      real*8 refcoo(*),gscoo(*),wg(*)
      character *(*) locname
      integer edfgaul 
c
      cret = edfgaul(fid, refcoo, gscoo, wg, 
     1     mode_coo, locname, len(locname), cret)
c
      return
      end
c
