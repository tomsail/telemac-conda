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

      subroutine efmaac ( fid , nom , dim , type, desc, cret )
c     DEC$ ATTRIBUTES DLLEXPORT :: efmaac
c
      implicit none
      save
c
      character *(*) nom
      character *(*) desc
      integer*8 fid
      integer   dim, type, cret
      integer edfmaac
c
      cret = edfmaac (fid, nom , len(nom), dim, type, desc, len(desc))
c      
      return
      end
c
c
      subroutine efnmaa( fid , n , cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efnmaa
c
      implicit none
      save
c
      integer*8 fid
      integer   n, cret
      integer edfnmaa
c
      n = edfnmaa(fid)

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
      subroutine efmaai( fid, indice, maa, dim, type, desc, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efmaai
c
      implicit none
      save
c
      integer*8 fid
      integer  dim,cret,indice,type
      character *(*) maa
      character *(*) desc
      integer edfmaai
c
      maa = ' ' 
c
      cret = edfmaai (fid,indice,maa,dim,type,desc)
c     
      return
      end
c
      subroutine efdiml( fid , maa, dim, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efdiml
c
      implicit none
      save
c
      integer*8 fid
      integer  dim,cret
      character *(*) maa
      integer edfdiml
c
      dim = edfdiml (fid,maa,len(maa))
c     
      if (dim.lt.0) then
         cret = -1
      else
         cret = 0
      endif
      return
      end
c
c
      subroutine efnnsl (fid,maa,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efnnsl
c
      implicit none
      save
c
      integer*8 fid
      integer  n,cret
      character*(*) maa
      integer edfnnsl
c
      n = edfnnsl(fid,maa,len(maa))
c
      if (n .lt. 0) then
         cret = -1
      else
         cret = 0
      endif
c
      end
c
c
      subroutine efnnse (fid,maa,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efnnse
c
      implicit none
      save
c
      integer*8 fid
      integer  n,cret
      character*(*) maa
      integer edfnnse
c
      cret = edfnnse(fid,maa,len(maa),n)
c
      end
c
c
      subroutine efnnil (fid,maa,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efnnil
c
      implicit none
      save
c
      integer*8 fid
      integer  n,cret
      character*(*) maa
      integer edfnnil
c
      n = edfnnil(fid,maa,len(maa))
c
      if (n .lt. 0) then
         cret = -1
      else
         cret = 0
      endif
c
      end
c
c
      subroutine efnnie (fid,maa,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efnnie
c
      implicit none
      save
c
      integer*8 fid
      integer  n,cret
      character*(*) maa
      integer edfnnie
c
      cret = edfnnie(fid,maa,len(maa),n)
c
      end
c
c
      subroutine efnnml (fid,maa,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efnnml
c
      implicit none
      save
c
      integer*8 fid
      integer  n,cret
      character*(*) maa
      integer edfnnml
c
      n = edfnnml(fid,maa,len(maa))
c
      if (n .lt. 0) then
         cret = -1
      else
         cret = 0
      endif
c
      end
c
c
      subroutine efnnme (fid,maa,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efnnme
c
      implicit none
      save
c
      integer*8 fid
      integer  n,cret
      character*(*) maa
      integer edfnnme
c
      cret = edfnnme(fid,maa,len(maa),n)
c
      end


      subroutine efunvc ( fid , nom , cret )
c     DEC$ ATTRIBUTES DLLEXPORT :: efunvc
c
      implicit none
      save
c
      character *(*) nom
      integer*8 fid
      integer   cret
      integer edfunvc
c
      cret = edfunvc (fid, nom , len(nom))
c
      return
      end

      subroutine efunvl( fid , nom , nomu , cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efunvl
c
      implicit none
      save
c
      integer*8 fid
      integer  cret
      character *(*) nom,nomu
      integer edfunvl
c  
      cret = edfunvl (fid,nom,len(nom),nomu,len(nomu))
c
      return
      end
c
c
      subroutine efespc(fid, nom, dim, cret )
c     DEC$ ATTRIBUTES DLLEXPORT :: efespc
c
      implicit none
      save
c
      character *(*) nom
      integer*8 fid
      integer   dim, cret
      integer edfespc
c
      cret = edfespc (fid, nom , len(nom), dim)
c      
      return
      end
c
c
      subroutine efespl( fid , maa, dim, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efespl
c
      implicit none
      save
c
      integer*8 fid
      integer  dim,cret
      character *(*) maa
      integer edfespl
c
      dim = edfespl (fid,maa,len(maa))
c     
      if (dim.lt.0) then
         cret = -1
      else
         cret = 0
      endif
      return
      end

c
c
      subroutine efnage(fid,maa,typ,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efnage
c
      implicit none
      save
c
      integer*8 fid
      integer  typ,cret
      character*(*) maa
      integer edfnage
c
      cret = edfnage(fid,maa,len(maa),typ)
c
      end

c
c
      subroutine efnagl(fid,maa,typ,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efnagl
c
      implicit none
      save
c
      integer*8 fid
      integer  typ,cret
      character*(*) maa
      integer edfnagl
c
      cret = edfnagl(fid,maa,len(maa),typ)
c
      end
