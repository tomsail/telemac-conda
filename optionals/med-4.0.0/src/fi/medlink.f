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
      subroutine mlnliw(fid,mname,lname,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mlnliw
c
      implicit none
      save
      character*(*) mname,lname
      integer*8 fid
      integer   cret
      integer   mlnfliw
c
      cret = mlnfliw(fid,mname,len(mname),lname,len(lname))
c      
      return
      end
c
c
c
      subroutine mlnnln(fid,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mlnnln
c
      implicit none
      save
      integer*8 fid
      integer   n,cret
      integer   mlnfnln
c
      n = mlnfnln(fid)
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
      subroutine mlnlni(fid, it, mname, lsize, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mlnlni
c
      implicit none
      save
c
      character *(*) mname
      integer*8 fid
      integer   lsize, it, cret
      integer   mlnflni
c
      cret = mlnflni(fid, it, mname, lsize)
c     
      return
      end
c
c
c
      subroutine mlnlai(fid, mname, lsize, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mlnlai
c
      implicit none
      save
c
      character *(*) mname
      integer*8 fid
      integer   lsize, cret,n
      integer   mlnflai
c
      n = mlnflai(fid, mname, len(mname), lsize)
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
      subroutine mlnlir(fid,mname,lname,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mlnlir
c
      implicit none
      save
      character*(*) mname,lname
      integer*8 fid
      integer   cret
      integer   mlnflir
c
      cret = mlnflir(fid,mname,len(mname),lname,len(lname))
c      
      return
      end


