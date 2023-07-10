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

      subroutine msmcre(fid , maa , sdim , mdim , des, 
     &                  atype, aname, aunit, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: msmcre
c
      implicit none
      save
c     
      character *(*) maa, des, aname, aunit
      integer*8 fid
      integer   cret, atype, sdim, mdim
      integer msmfcre
c
      cret = msmfcre(fid, maa, len(maa), sdim, mdim,
     &               des, len(des), atype, aname, 16*sdim,
     &               aunit, 16*sdim)
c
      return
      end
c
c
c
      subroutine msmnsm( fid , n , cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: msmnsm
c
      implicit none
      save
c
      integer*8 fid
      integer    n, cret
      integer msmfnsm 
c
      n = msmfnsm(fid)

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
      subroutine msmsni(fid, name, sdim, mdim, desc,
     &                  atype, aname, aunit, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: msmsni
c
      implicit none
      save
c
      character *(*) name, desc, aname, aunit
      integer*8 fid
      integer   sdim, mdim, atype, cret
      integer msmfsni 
c
      cret = msmfsni(fid, name, len(name), sdim, mdim, desc,
     &               atype, aname, aunit)
c     
      return
      end
c
c
c
      subroutine msmsmi(fid, it, name, sdim, mdim, desc,
     &                  atype, aname, aunit, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: msmsmi
c
      implicit none
      save
c
      character *(*) name, desc, aname, aunit
      integer*8 fid
      integer   sdim, mdim, atype, it, cret
      integer msmfsmi 
c
      cret = msmfsmi(fid, it, name, sdim, mdim, desc,
     &               atype, aname, aunit)
c     
      return
      end
c
c
c
      subroutine msmnax( fid , it, naxis, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: msmnax
c
      implicit none
      save
c
      integer*8 fid
      integer   it,naxis,cret
      integer msmfnax
c
      naxis =  msmfnax(fid,it)
c     
      if (naxis.lt.0) then
         cret = -1
      else
         cret = 0
      endif
      return
      end
c
c
c
      subroutine  msmnan( fid , name, naxis, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: msmnan
c
      implicit none
      save
c
      integer*8 fid
      integer   naxis,cret
      character *(*) name
      integer msmfnan
c
      naxis = msmfnan(fid,name,len(name))
c     
      if (naxis.lt.0) then
         cret = -1
      else
         cret = 0
      endif
      return
      end     
