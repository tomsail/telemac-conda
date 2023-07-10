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

      subroutine efequc ( fid , maa , eq , des , cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efequc
c
      implicit none
      save
c     
      character *(*) maa, des, eq
      integer*8 fid
      integer   cret
      integer edfequc
c
      cret = edfequc(fid, maa, len(maa), eq, len(eq), des, len(des))
c
      return
      end
c
      subroutine efeque (fid,maa,eq,corr,n,typent,typgeo,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efeque
c
      implicit none
      save
c
      character *(*) maa, eq
      integer*8 fid
      integer   cret, corr(*), n, typent, typgeo
      integer edfeque
c
      cret = edfeque(fid, maa, len(maa), eq, len(eq), corr, 
     1     n, typent, typgeo)
c
      return
      end
c
      subroutine efequl(fid,maa,eq,corr,n,typent,typgeo,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efequl
c
      implicit none
      save
c
      character *(*) maa, eq
      integer*8 fid
      integer   cret, corr(*), n,typent, typgeo
      integer edfequl
c
      cret = edfequl(fid, maa, len(maa), eq, len(eq), corr, 
     1     n, typent, typgeo)
c
      return
      end
c
      subroutine efncor(fid,maa,eq,typent,typgeo,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efncor
c
      implicit none
      save
c
      character *(*) maa, eq
      integer*8 fid
      integer   cret, n,typent, typgeo
      integer edfncor
c
      n = edfncor(fid, maa, len(maa), eq, len(eq), typent,typgeo)
      if (n.lt.0) then
         cret = -1
      else
         cret =0
      endif
c
      return
      end
c
      subroutine efnequ(fid,maa,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efnequ
c
      implicit none
      save
c
      character *(*) maa
      integer*8 fid
      integer   cret, n
      integer edfnequ
c
      n = edfnequ(fid, maa, len(maa))
      if (n.lt.0) then
         cret = -1
      else
         cret =0
      endif
c
      return
      end
c      
      subroutine efequi(fid,maa,ind,eq,des,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efequi
c
      implicit none
      save
c
      character *(*) maa,eq,des
      integer*8 fid
      integer   cret,ind
      integer edfequi
c
      cret = edfequi(fid, maa, len(maa),ind,eq,des)
c
      return
      end
c            

