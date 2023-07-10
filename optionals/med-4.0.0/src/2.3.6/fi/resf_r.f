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
      subroutine efchre(fid,maa,cha,val,interlace,n,locname,numco,
     1                  profil,pflmod,typent,typgeo,numdt,dtunit,
     1                  dt,numo,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efchre
c
      implicit none
      save
c      
      character *(*) cha,maa,profil,locname
      character*(*) dtunit
      integer*8 fid
      integer  n,pflmod,typent,typgeo,cret
      integer interlace,numco,numdt,numo
      real*8 dt
      real*8 val(*)
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

      subroutine efchrl(fid,maa,cha,val,
     1                  interlace,numco,locname,profil,pflmod,
     1                  typent,typgeo,
     1                  numdt, numo,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efchrl 
c
      implicit none
      save
c      
      character *(*) cha,maa,locname,profil
      integer*8 fid
      integer  typent,typgeo,cret
      integer interlace,numco,numdt,numo,pflmod
      integer edfchal
      real*8 val(*)
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
