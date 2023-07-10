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

      subroutine mprcre(fid , name , type , des , dtunit,  cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mprcre
c
      implicit none
      save
c     
      character *(*) name, des, dtunit
      integer*8 fid
      integer    cret,  type
      integer mprfcre
c
      cret = mprfcre(fid,name,len(name),type,des,len(des),
     &               dtunit,len(dtunit))
c
      return
      end
c
c
c
      subroutine mprrvw(fid,name,numdt,numit,dt,val,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mprrvw
c
      implicit none
      save
      character*(*) name
      integer*8 fid
      integer   numdt,numit,cret
      real*8 dt,val
      integer mprfrvw
c
      cret = mprfrvw(fid,name,len(name),numdt,numit,dt,val)
c      
      return
      end
c
c
c
      subroutine mprivw(fid,name,numdt,numit,dt,val,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mprivw
c
      implicit none
      save
      character*(*) name
      integer*8 fid
      integer   numdt,numit,cret
      real*8 dt
      integer val
      integer mprfivw
c
      cret = mprfivw(fid,name,len(name),numdt,numit,dt,val)
c      
      return
      end
c
c
c
      subroutine mprrvr(fid,name,numdt,numit,val,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mprrvr
c
      implicit none
      save
      character*(*) name
      integer*8 fid
      integer   numdt,numit,cret
      real*8 val
      integer mprfrvr
c
      cret = mprfrvr(fid,name,len(name),numdt,numit,val)
c      
      return
      end
c
c
c
      subroutine mprivr(fid,name,numdt,numit,val,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mprivr
c
      implicit none
      save
      character*(*) name
      integer*8 fid
      integer   numdt,numit,cret
      integer val
      integer mprfivr
c
      cret = mprfivr(fid,name,len(name),numdt,numit,val)
c      
      return
      end
c
c
c
      subroutine mprnpr(fid,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mprnpr
c
      implicit none
      save
      integer*8 fid
      integer   n,cret
      integer mprfnpr
c
      n = mprfnpr(fid)
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
      subroutine mprpri(fid, it, name, type, desc, 
     &                  dtunit,  nstep, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mprpri
c
      implicit none
      save
c
      character *(*) name, dtunit, desc
      integer*8 fid
      integer   it, cret,  type, nstep
      integer mprfpri 
c
      cret = mprfpri(fid, it, name, type,
     &               desc, dtunit,  nstep)
c     
      return
      end
c
c
c
      subroutine mprpin(fid, name, type, desc, 
     &                  dtunit,  nstep, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mprpin
c
      implicit none
      save
c
      character *(*) name, dtunit, desc
      integer*8 fid
      integer   cret,  type, nstep
      integer mprfpin 
c
      cret = mprfpin(fid, name, len(name), type,
     &               desc, dtunit,  nstep)
c     
      return
      end
c
c
c
      subroutine mprcsi(fid, name, it, numdt, numit,
     &                  dt, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mprcsi
c
      implicit none
      save
c
      character *(*) name
      integer*8 fid
      integer   it, cret, numdt, numit
      real*8 dt
      integer mprfcsi 
c
      cret = mprfcsi(fid, name, len(name), it, 
     &               numdt,numit,dt)
c     
      return
      end
