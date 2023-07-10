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

      subroutine effamc(fid,maa,fam,num,attid,attval,attdes,
     1     natt, gro,ngro,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: effamc
c
      implicit none
      save
c 
      integer*8 fid
      integer  num,attid(*),attval(*),natt,ngro,cret
      character *(*) maa,fam,attdes
      character *80 gro(*)
      integer edffamc
c
      cret = edffamc (fid,maa,len(maa),fam,len(fam),
     1     num,attid,attval,attdes, 200 * natt ,natt,
     2     gro, 80 * ngro ,ngro)
c      
      return
      end
c
      subroutine effame(fid,maa,fam,n,typent,typgeo,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: effame
c
      implicit none
      save
c
      integer*8 fid
      integer  fam(*),n,typent,typgeo,cret
      character *(*) maa
      integer edffame
c      
      cret = edffame(fid,maa,len(maa),fam,n,typent,typgeo)
c
      return
      end
c
      subroutine effaml(fid,maa,fam,n,typent,typgeo,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: effaml
c
      implicit none
      save
c
      integer*8 fid
      integer  fam(*),n,typent,typgeo,cret
      character *(*) maa
      integer edffaml
c      
      cret = edffaml(fid,maa,len(maa),fam,n,typent,typgeo)
c
      return
      end
c      
      subroutine effami(fid,maa,ind,fam,num,attid,attval,attdes,
     1     natt,gro,ngro,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: effami
c
      implicit none
      save
c 
      integer*8 fid
      integer  num,attid(*),attval(*),natt,ngro,cret,ind
      character *(*) maa,fam,attdes,gro
      integer edffami
c
      cret = edffami (fid,maa,len(maa),ind,fam,
     1     num,attid,attval,attdes,natt,
     2     gro, ngro)
c      
      return
      end
c      
      subroutine efnfam(fid,maa,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efnfam
c
      implicit none
      save
c
      integer*8 fid
      integer  n,cret
      character *(*) maa
      integer edfnfam
c
      n = edfnfam(fid,maa,len(maa))
      if (n.lt.0) then
         cret = -1
      else
         cret = 0
      endif
c
      return
      end
c

      subroutine efngro(fid,maa,ind,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efngro
c
      implicit none
      save
c
      integer*8 fid
      integer  ind,n,cret
      character *(*) maa
      integer edfngro
c
      n = edfngro(fid,maa,len(maa),ind)
      if (n.lt.0) then
         cret = -1
      else
         cret = 0
      endif
c
      return
      end
c

      subroutine efnatt(fid,maa,ind,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efnatt
c
      implicit none
      save
c
      integer*8 fid
      integer  ind,n,cret
      character *(*) maa
      integer edfnatt
c
      n = edfnatt(fid,maa,len(maa),ind)
      if (n.lt.0) then
         cret = -1
      else
         cret = 0
      endif
c
      return
      end
c
      

