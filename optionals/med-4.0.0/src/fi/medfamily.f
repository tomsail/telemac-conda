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

      subroutine mfacre(fid, name, fname, fnum, ngro, gname, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfacre
c
      implicit none
      save
c
      character *(*) name, fname, gname
      integer*8 fid
      integer  fnum, ngro, cret
      integer mfafcre
c
      cret = mfafcre(fid, name, len(name), fname, len(name),
     &               fnum, ngro, gname, 80 * ngro)
c      
      return
      end
c
c
c
      subroutine mfanfa(fid,maa,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfanfa
c
      implicit none
      save
c
      integer*8 fid
      integer n,cret
      character *(*) maa
      integer mfafnfa
c
      n = mfafnfa(fid,maa,len(maa))
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
      subroutine mfanfg(fid,maa,it,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfanfg
c
      implicit none
      save
c
      integer*8 fid
      integer n,cret,it
      character *(*) maa
      integer mfafnfg
c
      n = mfafnfg(fid,maa,len(maa),it)
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
      subroutine mfafai(fid,maa,ind,fam,num,gro,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfafai
c
      implicit none
      save
c 
      integer*8 fid
      integer num,cret,ind
      character *(*) maa,fam,gro
      integer mfaffai
c
      cret = mfaffai(fid,maa,len(maa),ind,fam,num,gro)
c      
      return
      end
c
c
c
      subroutine mfaona(fid,maa,it,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfaona
c
      implicit none
      save
c
      integer*8 fid
      integer it,n,cret
      character *(*) maa
      integer mfafona
c
      n =  mfafona(fid,maa,len(maa),it)
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
      subroutine mfaofi(fid,maa,it,fam,attnum,attval,attdes,
     &                  num,gro,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfaofi
c
      implicit none
      save
c 
      integer*8 fid
      integer num,cret,it
      integer attnum(*),attval(*)
      character *(*) maa,fam,gro,attdes
      integer mfafofi
c
      cret = mfafofi(fid,maa,len(maa),it,fam,attnum,attval,attdes,
     &               num,gro)
c      
      return
      end
