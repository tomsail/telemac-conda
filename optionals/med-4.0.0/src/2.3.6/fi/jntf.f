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

C**************************************************************************
C     creation d'un joint
C**************************************************************************
      subroutine efjntc ( fid , maalcl , jn , des , dom, maadst, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efjntc
c
      implicit none
      save
c     
      character *(*) maalcl, jn, des, maadst
      integer*8 fid
      integer   dom, cret
      integer edfjntc
c     
      cret = edfjntc(fid, maalcl, len(maalcl), jn, len(jn),
     $     des, len(des),
     $     dom, maadst,len(maadst))
c
      return
      end

C**************************************************************************
c     Lecture du nombre de joints 
C**************************************************************************
      subroutine efnjnt ( fid , maalcl , n , cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efnjnt
c
      implicit none
      save
c     
      character *(*) maalcl
      integer*8 fid
      integer n,  cret
      integer edfnjnt
c     
      cret = edfnjnt(fid, maalcl, len(maalcl), n)
c
      if (n.lt.0) then
         cret = -1
      else
         cret = 0
      endif

      return
      end
c




C**************************************************************************
C     Lecture des informations relatives a un joint
C**************************************************************************
      subroutine efjnti ( fid , maalcl , ind, jn , des , dom,
     $     maadst, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efjnti
c
      implicit none
      save
c     
      character *(*) maalcl, jn, des, maadst
      integer*8 fid
      integer   ind, dom, cret
      integer edfjnti
c     
      cret = edfjnti(fid, maalcl, len(maalcl), ind,
     $     jn, des, dom, maadst)
c
      return
      end
c



c
C**************************************************************************
c    ecriture du contenu d'une correspondance
C**************************************************************************
      subroutine efjnte ( fid , maalcl , jn , corrtab, n,
     $                    entlcl, geolcl, entdst, geodst,
     $                    cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efjnte
c
      implicit none
      save
c     
      character *(*) maalcl, jn
      integer*8 fid
      integer n, cret, corrtab(*)
      integer entlcl, entdst, geolcl, geodst
      integer edfjnte
c     
      cret = edfjnte(fid, maalcl, len(maalcl), jn, len(jn),
     $     corrtab, n,
     $     entlcl, geolcl, entdst, geodst)
c
      return
      end

C**************************************************************************
c    lecture du contenu d'une correspondance
C**************************************************************************
      subroutine efjntl ( fid , maalcl , jn , corrtab, n,
     $                    entlcl, geolcl, entdst, geodst,
     $                    cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efjntl
c
      implicit none
      save
c     
      character *(*) maalcl, jn
      integer*8 fid
      integer n, cret, corrtab(*)
      integer entlcl, entdst, geolcl, geodst
      integer edfjntl
c     
      cret = edfjntl(fid, maalcl, len(maalcl), jn, len(jn),
     $     corrtab, n,
     $     entlcl, geolcl, entdst, geodst)
c
      return
      end
c

C**************************************************************************
C     Lecture du nombre d'entités pur deux type en regard dans un joint
C**************************************************************************
      subroutine efjnco ( fid , maalcl , jn, 
     $     entlcl, geolcl, entdst, geodst,
     $     nent, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efjnco
c
      implicit none
      save
c     
      character *(*) maalcl, jn
      integer*8 fid
      integer   entlcl, entdst, geolcl, geodst, nent, cret
      integer edfjnco
c     
      nent = edfjnco(fid,
     $     maalcl, len(maalcl),
     $     jn, len(jn),
     $     entlcl, geolcl, entdst, geodst)
c
      if (nent.lt.0) then
         cret = -1
      else
         cret = 0
      endif

      return
      end



C**************************************************************************
C     Lecture du type des elements en regard dans un joint
C**************************************************************************
      subroutine efjtco ( fid , maalcl , jn, ind, 
     $     entlcl, geolcl, entdst, geodst,
     $     cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efjtco
c
      implicit none
      save
c     
      character *(*) maalcl, jn 
      integer*8 fid
      integer   entlcl, entdst, geolcl, geodst, nent, cret, ind
      integer edfjtco
c     
      nent = edfjtco(fid,
     $     maalcl, len(maalcl),
     $     jn, len(jn),
     $     ind,
     $     entlcl, geolcl, entdst, geodst)
c
      if (nent.lt.0) then
         cret = -1
      else
         cret = 0
      endif

      return
      end





C**************************************************************************
c    ecriture d'une numerotation globale
C**************************************************************************
      subroutine efgnme ( fid , maa , numtab, n,
     $                    ent, geo, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efgnme
c
      implicit none
      save
c     
      character *(*) maa
      integer*8 fid
      integer n,  cret, numtab(*)
      integer ent, geo
      integer edfgnme
c     
      cret = edfgnme(fid, maa, len(maa), numtab, n, ent, geo)
c
      return
      end
c


C**************************************************************************
c    lecture d'une numerotation globale
C**************************************************************************
      subroutine efgnml ( fid , maa , numtab, n,
     $                    ent, geo, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efgnml
c
      implicit none
      save
c     
      character *(*) maa
      integer*8 fid
      integer n, cret, numtab(*)
      integer ent, geo
      integer edfgnml
c     
      cret = edfgnml(fid, maa, len(maa), numtab, n, ent, geo)
c
      return
      end
c


