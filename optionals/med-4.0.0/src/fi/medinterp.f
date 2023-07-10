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
C
      subroutine mipcre(fid,name,gtype,cnode,nvar,
     &                  maxd,nmaxc,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mipcre
c
      implicit none
      save
c     
      character*(*) name
      integer*8 fid
      integer  gtype,cnode,nvar,maxd,nmaxc,cret
      integer mipfcre
c     
      cret = mipfcre(fid,name,len(name),gtype,cnode,
     &               nvar,maxd,nmaxc)
c
      return
      end
c
c
c
      subroutine mipbfw(fid,name,it,nc,pw,co,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mipbfw
c
      implicit none
      save
c     
      character*(*) name
      integer*8 fid
      integer it,nc,cret
      integer pw(*)
      real*8 co(*)
      integer mipfbfw
c     
      cret = mipfbfw(fid,name,len(name),it,nc,pw,co)
c
      return
      end
c
c
c
      subroutine mipbfr(fid,name,it,nc,pw,co,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mipbfr
c
      implicit none
      save
c     
      character*(*) name
      integer*8 fid
      integer it,nc,cret
      integer pw(*)
      real*8 co(*)
      integer mipfbfr
c     
      cret = mipfbfr(fid,name,len(name),it,nc,pw,co)
c
      return
      end

c
c
c
      subroutine mipnip(fid,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mipnip
c
      implicit none
      save
c     
      integer n, fid,  cret
      integer mipfnip
c     
      n = mipfnip(fid)
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
      subroutine mipiin(fid,name,gtype,cnode,nbf,nvar,
     &                  maxd,nmaxc,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mipiin
c
      implicit none
      save
c     
      character*(*) name
      integer*8 fid
      integer  gtype,cnode,nvar,maxd,nmaxc,cret,nbf
      integer mipfiin
c     
      cret = mipfiin(fid,name,len(name),gtype,cnode,nbf,
     &               nvar,maxd,nmaxc)
c
      return
      end
c
c
c
      subroutine mipipi(fid,it,name,gtype,cnode,nbf,nvar,
     &                  maxd,nmaxc,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mipipi
c
      implicit none
      save
c     
      character*(*) name
      integer*8 fid
      integer  gtype,cnode,nvar,maxd,nmaxc,cret,nbf
      integer it
      integer mipfipi
c     
      cret = mipfipi(fid,it,name,gtype,cnode,
     &               nbf,nvar,maxd,nmaxc)
c
      return
      end
c
c
c
      subroutine mipcsz(fid,name,it,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mipcsz
c
      implicit none
      save
c     
      character*(*) name
      integer*8 fid
      integer n, it, cret
      integer mipfcsz
c     
      n = mipfcsz(fid,name,len(name),it)
c
      if (n.lt.0) then
         cret = -1
      else
         cret = 0
      endif
c
      return
      end
