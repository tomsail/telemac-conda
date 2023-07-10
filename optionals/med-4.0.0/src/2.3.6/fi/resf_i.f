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

      subroutine efchac(fid,cha,type,comp,unit,ncomp,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efchac
c
      implicit none
      save
c      
      character *(*) cha,comp,unit
      integer*8 fid
      integer   ncomp, cret, type
      integer edfchac
c
      cret = edfchac (fid, cha,len(cha),type, comp,16*ncomp,
     1     unit, 16*ncomp, ncomp)
c      
      return
      end
c
c
c
      subroutine efchae(fid,maa,cha,val,interlace,n,locname,numco,
     1                  profil,pflmod,typent,typgeo,numdt,dtunit,
     1                  dt,numo,cret)
c
      implicit none
      save
c      
      character *(*) cha,maa,profil,locname
      character*(*) dtunit
      integer*8 fid
      integer  val(*)
      integer n,pflmod,typent,typgeo,cret
      integer interlace,numco,numdt,numo
      real*8 dt
      integer edfchae
c
      cret = edfchae(fid,maa,len(maa),cha,len(cha),val,
     1     interlace,n,locname,len(locname),numco,
     1     profil,len(profil),pflmod,
     1     typent,typgeo,numdt,dtunit,len(dtunit),
     1     dt,numo)
c      
      return
      end
c
c     Ecriture des champs en differenciant ENTIERS et REELS
c
      subroutine efchie(fid,maa,cha,val,interlace,n,locname,numco,
     1                  profil,pflmod,typent,typgeo,numdt,dtunit,
     1                  dt,numo,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efchie 
c
      implicit none
      save
c      
      character *(*) cha,maa,profil,locname
      character*(*) dtunit
      integer*8 fid
      integer  val(*),n,pflmod,typent,typgeo,cret
      integer interlace,numco,numdt,numo
      real*8 dt
      integer edfchae
c
      cret = edfchae(fid,maa,len(maa),cha,len(cha),val,
     1     interlace,n,locname,len(locname),numco,
     1     profil,len(profil),pflmod,
     1     typent,typgeo,numdt,dtunit,len(dtunit),
     1     dt,numo)
c      
      return
      end
c
c    
c     

      subroutine efchal(fid,maa,cha,val,
     1                  interlace,numco,locname,
     1                  profil,pflmod,
     1                  typent,typgeo,
     1                  numdt, numo,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efchal
c
      implicit none
      save
c      
      character *(*) cha,maa,locname,profil
      integer*8 fid
      integer  val(*),pflmod,typent,typgeo,cret
      integer interlace,numco,numdt,numo
      integer edfchal
c
      cret = edfchal(fid,maa,len(maa),cha,len(cha),val,
     1     interlace,numco,locname,profil,pflmod,
     1     typent,typgeo,numdt,numo)
c      
      return
      end
c      
c
c     Lecture des champs en distinguant les reels et les entiers
c     
      subroutine efchil(fid,maa,cha,val,
     1                  interlace,numco,locname,
     1                  profil,pflmod,
     1                  typent,typgeo,
     1                  numdt, numo,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efchil
c
      implicit none
      save
c      
      character *(*) cha,maa,locname,profil
      integer*8 fid
      integer  val(*),typent,typgeo,cret,pflmod
      integer interlace,numco,numdt,numo
      integer edfchal
c
      cret = edfchal(fid,maa,len(maa),cha,len(cha),val,
     1     interlace,numco,locname,profil,pflmod,
     1     typent,typgeo,numdt,numo)
c      
      return
      end
c
c
c
      subroutine efchai(fid,ind,cha,type,comp,unit,ncomp,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efchai
c
      implicit none
      save
c      
      character *(*) cha,comp,unit
      integer*8 fid
      integer  ind,type,ncomp,cret
      integer edfchai
c
      cret = edfchai(fid,ind,cha,type,
     1     comp,unit,ncomp)
c      
      return
      end
c
      subroutine efncha(fid,ind,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efncha
c
      implicit none
      save
c      
      integer*8 fid
      integer  ind,cret,n
      integer edfncha
c
      n = edfncha(fid,ind)
      if (n.lt.0) then
         cret = -1
      else
         cret = 0
      endif
c
      return
      end
c

      subroutine efnval(fid,cha,typent,typgeo,numdt,numo,maa,pflmod,
     1     n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efnval
c
      implicit none
      save
c      
      integer*8 fid
      integer  typent,typgeo,n,cret
      integer numdt, numo, pflmod
      character *(*) cha, maa
      integer edfnval
c
      n = edfnval(fid,cha,len(cha),typent,typgeo,
     1            numdt,numo,maa,len(maa),pflmod)
      if (n.lt.0) then
         cret = -1
      else
         cret = 0
      endif
c
      return
      end
c

C Nouvelles routines pour MED V2.1


      subroutine efnpdt(fid,cha,type,geo,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efnpdt
c
      implicit none
      save
c      
      integer*8 fid
      integer  type,geo,n,cret
      character*32 cha
      integer edfnpdt
c
      n = edfnpdt(fid,cha,len(cha),type,geo)
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

      subroutine efpdti(fid,cha,typent,typgeo,ind,ngauss,
     1                  numdt,numo,dtunit,dt,maa,local,nmaa,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efpdti
c
      implicit none
      save
c      
      integer*8 fid
      integer  typent,typgeo,ind,ngauss,numdt,numo,nmaa,cret
      character*32 cha,maa
      character*16 dtunit
      real*8 dt
      logical local
      integer edfpdti
c
      cret = edfpdti(fid,cha,len(cha),typent,typgeo,ind,
     1               ngauss,numdt,numo,dtunit,dt,maa,local,nmaa)     
c
      return
      end
c
c     NOUVELLE ROUTINE MED2.2

      subroutine efnref(fid,cha,typent,typgeo,numdt,numo,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efnref 
c
      implicit none
      save
c
      integer*8 fid
      integer  typent,typgeo,n,cret
      integer numdt, numo
      character *(*) cha
      integer edfnref

      n = edfnref(fid,cha,len(cha),typent,typgeo,
     1            numdt,numo)

      if (n.lt.0) then
         cret = -1
      else
         cret = 0
      endif
c
      return
      end
c

      subroutine efrefi(fid,cha, typent, typgeo, indice, numdt, numo,
     1                  maa, local, ngauss, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efrefi
c
      implicit none
      save
c
      integer*8 fid
      integer  typent,typgeo,indice,n,cret
      integer numdt, numo, ngauss
      character *(*) cha, maa
      logical local
      integer edfrefi

      n = edfrefi(fid,cha,len(cha),typent,typgeo,indice,
     1            numdt,numo,maa,local,ngauss)

      if (n.lt.0) then
         cret = -1
      else
         cret = 0
      endif
c
      return
      end
c

      subroutine efliee(fid,lienval,maa,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efliee
      integer*8 fid
      integer  cret
      character *(*) lienval, maa
      integer   edfliee

c     
      cret = edfliee(fid,lienval,len(lienval),maa,len(maa))
c     
      return
      end
c


      subroutine efnlie(fid,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efnlie
c
      implicit none
      save
c      
      integer*8 fid
      integer  cret,n
      integer edfnlie
c
      n = edfnlie(fid)
c
      if (n.lt.0) then
         cret = -1
      else
         cret = 0
      endif
c
      return
      end

      subroutine efliei( fid , indice , maa , n , cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efliei
c
      implicit none
      save
c
      integer*8 fid
      integer  n,cret,indice
      character *(*) maa
      integer edfliei
c
      maa = ' ' 
c
      cret = edfliei (fid,indice,maa,n)
c     
      return
      end

      subroutine efnvli(fid,maa,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efnvli
c
      implicit none
      save
c      
      integer*8 fid
      integer  cret,n
      character *(*) maa
      integer edfnvli
c
      n = edfnvli(fid,maa,len(maa))
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

      subroutine efliel(fid,lienval,n,maa,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efliel
c
      implicit none
      save
c      
      integer*8 fid
      integer  n,cret
      character *(*) maa,lienval
      integer edfliel
c
      cret = edfliel(fid,lienval,n,maa,len(maa))
c     
      return
      end
c

