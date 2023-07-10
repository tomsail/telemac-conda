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

      subroutine meqcre(fid , maa , eq , des , cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: meqcre
c
      implicit none
      save
c     
      character *(*) maa, des, eq
      integer*8 fid
      integer  cret
      integer meqfcre
c
      cret = meqfcre(fid, maa, len(maa), eq, len(eq), des, len(des))
c
      return
      end
c
c
c
      subroutine meqcow(fid,maa,eq,numdt,numit,typent,typgeo,
     &                  n,corr,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: meqcow
c
      implicit none
      save
c
      character *(*) maa, eq
      integer numit, numdt
      integer*8 fid
      integer  cret, n, typent, typgeo
      integer corr(*)
      integer meqfcow
c
      cret = meqfcow(fid,maa,len(maa),eq,len(eq),
     &               numdt,numit,typent,typgeo,
     &               n,corr)
c
      return
      end  
c
c
c
      subroutine meqneq(fid,maa,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: meqneq
c
      implicit none
      save
c
      character *(*) maa
      integer*8 fid
      integer  cret, n
      integer meqfneq
c
      n = meqfneq(fid, maa, len(maa))
      if (n.lt.0) then
         cret = -1
      else
         cret =0
      endif
c
      return
      end
c
c
c
      subroutine meqeqi(fid,maa,ind,eq,des,nstep,nctcor,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: meqeqi
c
      implicit none
      save
c
      character *(*) maa,eq,des
      integer*8 fid
      integer  cret,ind,nstep,nctcor
      integer meqfeqi
c
      cret = meqfeqi(fid,maa,len(maa),ind,eq,des,
     &               nstep,nctcor)
c
      return
      end
c
c
c
      subroutine meqcsz(fid,maa,eq,numdt,numit,
     &                   typent,typgeo,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: meqcsz
c
      implicit none
      save
c
      character *(*) maa, eq
      integer*8 fid
      integer cret,n,typent,typgeo,numit,numdt
      integer meqfcsz
c
      n = meqfcsz(fid,maa,len(maa),eq,len(eq),
     &            numdt,numit,typent,typgeo)
c
      if (n.lt.0) then
         cret = -1
      else
         cret =0
      endif
c
      return
      end     
c
c
c
      subroutine meqszi(fid,maa,eq,numdt,numit,
     &                  corit,typent,typgeo,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: meqszi
c
      implicit none
      save
c
      character *(*) maa, eq
      integer*8 fid
      integer cret,n,typent,typgeo,numit,numdt
      integer corit
      integer meqfszi
c
      cret = meqfszi(fid,maa,len(maa),eq,len(eq),
     &               numdt,numit,corit,typent,typgeo,n)
c
      return
      end     
c
c
c
      subroutine meqcor(fid,maa,eq,numdt,mumit,typent,typgeo,
     &                  corr,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: meqcor
c
      implicit none
      save
c
      character *(*) maa, eq
      integer*8 fid
      integer  cret, corr(*), typent, typgeo,numdt,mumit
      integer meqfcor
c
      cret = meqfcor(fid, maa, len(maa), eq, len(eq), 
     &               numdt,mumit,typent,typgeo,corr)
c
      return
      end
c
c
c
      subroutine meqcsi(fid,maa,eq,ind,numdt,numit,ncor,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: meqcsi
c
      implicit none
      save
c
      character *(*) maa,eq
      integer*8 fid
      integer cret,ind,numdt,numit,ncor
      integer meqfcsi
c
      cret = meqfcsi(fid,maa,len(maa),eq,len(eq),
     &               numdt,numit,ncor)
c
      return
      end
