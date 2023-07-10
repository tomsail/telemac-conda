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
c     Nom : efouvr
c     Fonction : appelle edfouvr pour ouvrir un fichier
c     In : nom - nom du fichier a ouvrir
c           acces - mode d'acces 1 (lecture), 
c          2 (lecture et ecriture), 3 (lecture et ecriture, 
c          si le fichier existe deja il est detruit)
c     Out : cret 0 reussite, -1 echec
c
      subroutine efouvr(fid, nom, acces, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efouvr
c
      implicit none
      save
c
      character *(*) nom
      integer*8 fid
      integer acces, cret
      integer*8 edfouvr
c
      fid = edfouvr(nom, acces, len(nom))
      if (fid.eq.-1) then
         cret=-1
      else
         cret=0
      endif
c      
      return
      end
      

      subroutine efferm(fid, cret)
c
      implicit none
      save
c
      integer*8 fid
      integer  cret
      integer edfferm
c
      cret = edfferm(fid)
c      
      return
      end
c
c
      subroutine eflfde (fid,lon,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: eflfde
c
      implicit none
      save
c
      integer*8 fid
      integer  cret,lon
      integer edflfde
c
      lon = edflfde(fid)
c
      if (lon .lt. 0) then
         cret = -1
      else
         cret = 0
      endif
c
      end
c
c
      subroutine effien (fid,quoi,str,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: effien
c
      implicit none
      save
c
      integer*8 fid
      integer  quoi,cret
      integer edffien
      character*(*) str
c
      cret = edffien(fid,quoi,str)
c
      end
      
c
c
      subroutine effide (fid,des,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: effide
c
      implicit none
      save
      integer*8 fid
      integer  cret
      character*(*) des
      integer edffide
c
      cret = edffide(fid,des,len(des))
c
      end

      subroutine effoco (nom,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: effoco
c
      implicit none
      save
C
      integer cret
      character*(*) nom
      integer edffoco
c
      cret = edffoco(nom,len(nom))
c
      return
      end
c
c
      subroutine efveco (nom,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efveco
c
      implicit none
      save
C
      integer cret
      character*(*) nom
      integer edfveco
c
      cret = edfveco(nom,len(nom))
c
      return
      end
c
c
c
      subroutine efveli (fid,maj,min,rel,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efveli
c
      implicit none
      save
C
      integer*8 fid
      integer   maj, min, rel
      integer cret
      integer edfveli
c
      cret = edfveli(fid,maj,min,rel)
c
      return
      end
c
c
      subroutine efvedo (maj,min,rel,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efvedo
c
      implicit none
      save
C
      integer maj, min, rel
      integer cret
      integer edfvedo
c
      cret = edfvedo(maj,min,rel)
c
      return
      end


c
c
      subroutine efmont(fid, acces, type, mid, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efmont
c
      implicit none
      save
c
      character *(*) acces
      integer*8 fid, mid
      integer   type, cret
      integer*8 edfmont
c
      mid = edfmont(fid, acces, len(acces), type)
      if (mid .eq.-1) then
         cret=-1
      else
         cret=0
      endif
c      
      return
      end

c
c
      subroutine efdemo(fid, mid, type, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efdemo
c
      implicit none
      save
c
      integer*8 fid, mid
      integer type, cret
      integer edfdemo
c
      cret = edfdemo(fid, mid, type)
c      
      return
      end
