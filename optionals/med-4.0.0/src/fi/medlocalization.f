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
      subroutine mlclow(fid,lname,gtype,sdim,ecoo,swm,nip,
     &                  ipcoo, wght, giname, isname, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mlclow
c
      implicit none
      save
      character*(*) lname,giname, isname
      real*8    ecoo(*), ipcoo(*), wght(*)
      integer*8 fid
      integer   cret,gtype,sdim,swm,nip
      integer mlcflow
c
      cret = mlcflow(fid,lname,len(lname),gtype,sdim,ecoo,swm,nip,
     &     ipcoo, wght,giname,len(giname), isname, len(isname) )
c      
      return
      end
c
c
c
      subroutine mlcnlc(fid,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mlcnlc
c
      implicit none
      save
      integer*8 fid
      integer  n,cret
      integer mlcfnlc
c
      n = mlcfnlc(fid)
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
      subroutine mlclci(fid, it, lname, gtype, sdim, nip,
     &     giname, isname, nsmc, sgtype, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mlclci
c
      implicit none
      save
c
      character*(*) lname,giname, isname
      integer*8 fid
      integer   gtype, it, cret, sdim, nip
      integer    nsmc, sgtype
      integer    mlcflci
c
      cret = mlcflci(fid, it, lname, gtype, sdim, nip,
     &     giname, isname,nsmc,sgtype)
c     
      return
      end
c
c
c
      subroutine mlclni(fid, lname, gtype, sdim, nip,
     &     giname, isname, nsmc, sgtype, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mlclni
c
      implicit none
      save
c
      character*(*) lname,giname, isname
      integer*8 fid
      integer   gtype, cret, sdim, nip
      integer nsmc, sgtype
      integer mlcflni
c
      cret = mlcflni(fid, lname, len(lname), gtype, sdim, nip,
     &     giname, isname, nsmc, sgtype)
c     
      return
      end
c
c
c

      subroutine mlclor(fid,lname,swm,ecoo,ipcoo, wght, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mlclor
c
      implicit none
      save
      character*(*) lname
      real*8 ecoo(*), ipcoo(*), wght(*)
      integer*8 fid
      integer   cret,swm
      integer mlcflor
c
      cret = mlcflor(fid,lname,len(lname),swm,ecoo,ipcoo,wght)
c      
      return
      end


