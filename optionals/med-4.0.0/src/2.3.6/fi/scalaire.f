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

      subroutine efscac(fid,sca,type,desc,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efscac 
c
      implicit none
      save
c      
      character *(*) sca,desc
      integer*8 fid
      integer   cret, type
      integer edfscac
c
      cret = edfscac (fid,sca,len(sca),type,desc,len(desc))
c      
      return
      end
c
c     Ecriture des scalaires en differenciant ENTIERS et REELS
c
      subroutine efscee(fid,sca,val,numdt,dtunit,dt,numo,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efscee 
c
      implicit none
      save
c      
      character *32 sca
      character*(*) dtunit
      integer*8 fid
      integer  val,cret
      integer numdt,numo
      real*8 dt
      integer edfscee
c
      cret = edfscee(fid,sca,len(sca),val,
     1               numdt,dtunit,len(dtunit),dt,numo)
c      
      return
      end
c
c    
c     
      subroutine efscfe(fid,sca,val,numdt,dtunit,
     1                  dt,numo,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efscfe
c
      implicit none
      save
c      
      character *(*) sca
      character*(*) dtunit
      integer*8 fid
      integer  cret
      integer numdt,numo
      real*8 dt
      real*8 val
      integer edfscfe
c
      cret = edfscfe(fid,sca,len(sca),val,
     1               numdt,dtunit,len(dtunit),dt,numo)
c      
      return
      end
c
c
c
c     Lecture des champs en distinguant les reels et les entiers
c     
      subroutine efscel(fid,sca,val,numdt, numo,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efscel
c
      implicit none
      save
c      
      character *(*) sca
      integer*8 fid
      integer  val,cret
      integer numdt,numo
      integer edfscel
c
      cret = edfscel(fid,sca,len(sca),val,numdt,numo)
c      
      return
      end
c
c
c
      subroutine efscfl(fid,sca,val,numdt, numo,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efscfl
 
      implicit none
      save
c      
      character *(*) sca
      integer*8 fid
      integer  cret
      integer numdt,numo
      integer edfscfl
      real*8 val(*)
c
      cret = edfscfl(fid,sca,len(sca),val,numdt,numo)
c      
      return
      end
c
c
c
      subroutine efscai(fid,ind,sca,type,desc,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efscai
c
      implicit none
      save
c      
      character *(*) sca,desc
      integer*8 fid
      integer  ind,type,cret
      integer edfscai
c
      cret = edfscai(fid,ind,sca,type,desc)
c      
      return
      end
c
      subroutine efnsca(fid,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efnsca
c
      implicit none
      save
c      
      integer*8 fid
      integer  cret,n
      integer edfnsca
c
      n = edfnsca(fid)
      if (n.lt.0) then
         cret = -1
      else
         cret = 0
      endif
c
      return
      end
c
      subroutine efnspd(fid,sca,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efnspd 
c
      implicit none
      save
c      
      integer*8 fid
      integer  n,cret
      character*32 sca
      integer edfnspd
c
      n = edfnspd(fid,sca,len(sca))
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

      subroutine efspdi(fid,sca,indice,
     1                  numdt,dtunit,dt,numo,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efspdi 
c
      implicit none
      save
c      
      integer*8 fid
      integer  indice,numdt,numo,cret
      character*32 sca
      character*16 dtunit
      real*8 dt
      integer edfspdi
c
      cret = edfspdi(fid,sca,len(sca),indice,
     1               numdt,dtunit,dt,numo)
c
      return
      end
c
