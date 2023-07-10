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

      subroutine efnpro( fid , n , cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efnpro
c
      implicit none
      save
c
      integer*8 fid
      integer   n, cret
      integer edfnpro
c
      n = edfnpro(fid)

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
      subroutine efproi( fid , indice , pro , n , cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efproi
c
      implicit none
      save
c
      integer*8 fid
      integer  n,cret,indice
      character *(*) pro
      integer edfproi
c
      pro = ' ' 
c
      cret = edfproi (fid,indice,pro,n)
c     
      return
      end
c

      subroutine efpfle(fid,pflval,n,nom,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efpfle
c
      implicit none
      save
c      
      integer*8 fid
      integer  n,cret
      integer pflval(*)
      character *(*) nom
      integer edfpfle
c
      cret = edfpfle(fid,pflval,n,nom,len(nom))
c
      return
      end
c
      

      subroutine efnpfl(fid,nom,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efnpfl
c
      implicit none
      save
c      
      integer*8 fid
      integer  cret,n
      character *(*) nom
      integer edfnpfl
c
      n = edfnpfl(fid,nom,len(nom))
c
      if (n.lt.0) then
         cret = -1
      else
         cret = 0
      endif
c
      return
      end
c

      subroutine efpfll(fid,pflval,nom,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efpfll
c
      implicit none
      save
c      
      integer*8 fid
      integer  cret
      integer pflval(*)
      character *(*) nom
      integer edfpfll
c
      cret = edfpfll(fid,pflval,nom,len(nom))
c
      return
      end
c
