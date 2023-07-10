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

      subroutine msdjcr(fid,lmname,jname,des,dom,
     &                  rmname,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: MSDJCR
c
      implicit none
      save
c     
      character*(*) jname
      character*(*) lmname
      character*(*) des
      character*(*) rmname
      integer*8 fid
      integer   dom, cret
      integer   msdfjcr
c     
      print *,lmname
      print *,jname
      print *,des
      print *,dom
      print *,rmname
      cret = msdfjcr(fid,lmname,len(lmname),
     &               jname,len(jname),
     &               des, len(des),
     &               dom, rmname, len(rmname))
c
      return
      end
c
c
c
      subroutine msdcrw(fid,lmname,jname,numdt,numit,
     &                  entlcl,geolcl,entdst,geodst,
     &                  n,corrtab,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: msdcrw
c
      implicit none
      save
c     
      character *(*) lmname, jname
      integer n, fid, cret, corrtab(*)
      integer entlcl, entdst, geolcl, geodst, numdt,numit
      integer msdfcrw
c     
      cret = msdfcrw(fid,lmname,len(lmname),jname,len(jname),
     &               numdt, numit,  entlcl, geolcl, entdst, geodst,          
     &               n,corrtab)
c
      return
      end
c
c
c
      subroutine msdnjn(fid,maa,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: msdnjn
c
      implicit none
      save
c     
      character *(*) maa
      integer n, fid,  cret
      integer msdfnjn
c     
      n = msdfnjn(fid,maa,len(maa))
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
      subroutine msdjni(fid,lmname,ind,jname,des,dom,
     &                  rmname,nstep,ncor,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: msdjni
c
      implicit none
      save
c     
      character *(*) lmname,jname,des,rmname
      integer*8 fid
      integer   ind,dom,cret,nstep,ncor
      integer msdfjni
c     
      cret = msdfjni(fid,lmname,len(lmname),ind,
     &              jname,des,dom,rmname,nstep,ncor)
c
      return
      end
c
c
c
      subroutine msdszi(fid,mname,jname,
     &                  numdt,numit,
     &                  it,letype,lgtype,
     &                  retype,rgtype,
     &                  ncor,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: msdszi
c
      implicit none
      save
c     
      character*(*) mname,jname
      integer*8 fid
      integer cret,numdt,numit
      integer it,letype,lgtype,retype,rgtype,ncor
      integer msdfszi
c     
      cret = msdfszi(fid,mname,len(mname),
     &               jname,len(jname),
     &               numdt,numit,it,
     &               letype,lgtype,
     &               retype,rgtype,ncor)
c
      return
      end
c
c
c
      subroutine msdcsz(fid,mname,jname,
     &                  numdt,numit,
     &                  letype,lgtype,
     &                  retype,rgtype,
     &                  ncor,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: msdcsz 
c
      implicit none
      save
c     
      character*(*) mname,jname
      integer*8 fid
      integer cret,numdt,numit
      integer letype,lgtype,retype,rgtype,ncor
      integer msdfcsz
c     
      cret = msdfcsz(fid,mname,len(mname),
     &               jname,len(jname),
     &               numdt,numit,
     &               letype,lgtype,
     &               retype,rgtype,
     &               ncor)
c
      return
      end
c
c
c
      subroutine msdcrr(fid,lmname,jname,numdt,numit,
     &                  entlcl,geolcl,entdst,geodst,
     &                  corrtab,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: msdcrr
c
      implicit none
      save
c     
      character *(*) lmname, jname
      integer*8 fid
      integer cret, corrtab(*)
      integer entlcl, entdst, geolcl, geodst, numdt,numit
      integer msdfcrr
c     
      cret = msdfcrr(fid,lmname,len(lmname),jname,len(jname),
     &               numdt, numit,  entlcl, geolcl, entdst, geodst,          
     &               corrtab)
c
      return
      end
c
c
c
      subroutine msdcsi(fid,mname,jname,ind,
     &                  numdt,numit,ncor,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: msdcsi
c
      implicit none
      save
c     
      character *(*) mname,jname
      integer*8 fid
      integer ind,ncor,numdt,numit,cret
      integer msdfcsi
c     
      cret = msdfcsi(fid,mname,len(mname),
     &               jname,len(jname),ind,
     &               numdt,numit,ncor)
c
      return
      end
