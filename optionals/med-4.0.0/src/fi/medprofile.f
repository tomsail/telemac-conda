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
c
c
c
      subroutine mpfprw(fid,pname,psize,profil,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mpfprw
c
      implicit none
      save
      character*(*) pname
      integer profil(*)
      integer*8 fid
      integer   cret,psize
      integer mpffprw
c
      cret = mpffprw(fid,pname,len(pname),psize,profil)
c      
      return
      end
c
c
c
      subroutine mpfnpf(fid,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mpfnpf
c
      implicit none
      save
      integer*8 fid
      integer   n,cret
      integer mpffnpf
c
      n = mpffnpf(fid)
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
c
c
      subroutine mpfpfi(fid, it, pname, psize, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mpfpfi
c
      implicit none
      save
c
      character *(*) pname
      integer*8 fid
      integer   psize, it, cret
      integer mpffpfi
c
      cret = mpffpfi(fid, it, pname, psize)
c     
      return
      end
c
c
c
      subroutine mpfpsn(fid, pname, psize, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mpfpsn
c
      implicit none
      save
c
      character *(*) pname
      integer*8 fid
      integer   psize, cret
      integer mpffpsn
c
      cret = mpffpsn(fid, pname, len(pname), psize)
c     
      return
      end
c
c
c
      subroutine mpfprr(fid,pname,profil,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mpfprr
c
      implicit none
      save
      character*(*) pname
      integer profil(*)
      integer*8 fid
      integer   cret
      integer mpffprr
c
      cret = mpffprr(fid,pname,len(pname),profil)
c      
      return
      end

