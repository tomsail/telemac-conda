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
      subroutine mfdcre(fid,fname,ftype,ncomp,cname,cunit,
     &                  dtunit,mname,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfdcre
c
      implicit none
      save
      character*(*) fname,cname,cunit,dtunit,mname
      integer*8 fid
      integer ncomp,cret,ftype
      integer mfdfcre
c
      cret = mfdfcre(fid,fname,len(fname),ftype,
     &               ncomp,cname,16*ncomp,cunit,16*ncomp,
     &               dtunit,len(dtunit),mname,len(mname))
c      
      return
      end
c
c
c
      subroutine mfdrvw(fid,fname,numdt,numit,dt,
     &                  etype,gtype,swm,cs,n,val,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfdrvw
c
      implicit none
      save
      character*(*) fname
      integer numdt,numit
      real*8 dt,val(*)
      integer*8 fid
      integer cret,etype,gtype,swm,cs,n
      integer mfdfrvw
c
      cret = mfdfrvw(fid,fname,len(fname),numdt,numit,dt,
     &               etype,gtype,swm,cs,n,val)
c      
      return
      end
c
c
c
      subroutine mfdivw(fid,fname,numdt,numit,dt,
     &                  etype,gtype,swm,cs,n,val,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfdivw
c
      implicit none
      save
      character*(*) fname
      integer numdt,numit
      real*8 dt
      integer val(*)
      integer*8 fid
      integer cret,etype,gtype,swm,cs,n
      integer mfdfivw
c
      cret = mfdfivw(fid,fname,len(fname),numdt,numit,dt,
     &               etype,gtype,swm,cs,n,val)
c      
      return
      end
c
c
c
      subroutine mfdrpw(fid,fname,numdt,numit,dt,
     &                  etype,gtype,
     &                  stm,pname,lname,
     &                  swm,cs,n,val,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfdrpw
c
      implicit none
      save
      character*(*) fname,pname,lname
      integer numdt,numit
      real*8 dt,val(*)
      integer*8 fid
      integer cret,etype,gtype,swm,cs,n,stm
      integer mfdfrpw
c
      cret = mfdfrpw(fid,fname,len(fname),numdt,numit,dt,
     &               etype,gtype,stm,pname,len(pname),
     &               lname,len(lname),swm,cs,n,val)
c      
      return
      end
c
c
c
      subroutine mfdipw(fid,fname,numdt,numit,dt,
     &                  etype,gtype,
     &                  stm,pname,lname,
     &                  swm,cs,n,val,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfdipw
c
      implicit none
      save
      character*(*) fname,pname,lname
      integer numdt,numit
      real*8 dt
      integer val(*)
      integer*8 fid
      integer cret,etype,gtype,swm,cs,n,stm
      integer mfdfipw
c
      cret = mfdfipw(fid,fname,len(fname),numdt,numit,dt,
     &               etype,gtype,stm,pname,len(pname),
     &               lname,len(lname),swm,cs,n,val,cret)
c      
      return
      end
c
c
c
      subroutine mfdraw(fid,fname,numdt,numit,dt,
     &                  etype,gtype,lname,flt,
     &                  val,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfdraw
c
      implicit none
      save
      character*(*) fname,lname
      integer numdt,numit
      real*8 dt,val(*)
      integer*8 flt(*)
      integer*8 fid
      integer cret,etype,gtype
      integer mfdfraw
c
      cret = mfdfraw(fid,fname,len(fname),numdt,numit,dt,
     &               etype,gtype,lname,len(lname),flt,val)
c      
      return
      end
c
c
c
      subroutine mfdiaw(fid,fname,numdt,numit,dt,
     &                  etype,gtype,lname,flt,
     &                  val,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfdiaw
c
      implicit none
      save
      character*(*) fname,lname
      integer numdt,numit
      real*8 dt
      integer val(*)
      integer*8 flt(*)
      integer*8 fid
      integer cret,etype,gtype
      integer mfdfiaw
c
      cret = mfdfiaw(fid,fname,len(fname),numdt,numit,dt,
     &               etype,gtype,lname,len(lname),flt,val)
c      
      return
      end
c
c
c
      subroutine mfdnfd(fid,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfdnfd
c
      implicit none
      save
      integer*8 fid
      integer n,cret
      integer mfdfnfd
c
      n = mfdfnfd(fid)
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
      subroutine mfdnfc(fid,ind,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfdnfc
c
      implicit none
      save
      integer*8 fid
      integer n,cret,ind
      integer mfdfnfc
c
      n = mfdfnfc(fid,ind)
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
      subroutine mfdncn(fid,fname,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfdncn
c
      implicit none
      save
      integer*8 fid
      integer n,cret
      character *(*) fname
      integer mfdfncn
c
      n = mfdfncn(fid,fname,len(fname))
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
      subroutine mfdfdi(fid, it, fname, mname, lmesh, type, 
     &                  cname, cunit, dtunit, nc, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfdfdi
c
      implicit none
      save
c
      character *(*) fname, mname, dtunit, cname, cunit
      integer lmesh, cret
      integer*8 fid
      integer  type, it, nc
      integer mfdffdi 
c
      cret = mfdffdi(fid, it, fname, mname, lmesh, type,
     &               cname, cunit, dtunit, nc)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfdffdi
c     
      return
      end
c
c
c
      subroutine mfdfin(fid, fname, mname, lmesh, type, 
     &                  cname, cunit, dtunit, nc, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfdfin
c
      implicit none
      save
c
      character *(*) fname, mname, dtunit, cname, cunit
      integer lmesh, cret
      integer*8 fid
      integer  type, nc
      integer mfdffin 
c
      cret = mfdffin(fid, fname, len(fname), mname, lmesh, type,
     &               cname, cunit, dtunit, nc)
c     
      return
      end
c
c
c
      subroutine mfdcsi(fid, fname, it, numdt, numit, dt, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfdcsi
c
      implicit none
      save
c
      character*(*) fname
      integer cret
      integer*8 fid
      integer  it, numdt, numit
      real*8  dt
      integer mfdfcsi
c
      cret = mfdfcsi(fid,fname,len(fname),it,numdt,numit,dt)
c     
      return
      end
c
c
c
      subroutine mfdcmi(fid, fname, it, numdt, numit, dt, 
     &     mnumdt, mnumit, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfdcmi
c
      implicit none
      save
c
      character*(*) fname
      integer cret
      integer*8 fid
      integer  it, numdt, numit, mnumdt, mnumit
      real*8  dt
      integer mfdfcmi
c
      cret = mfdfcmi(fid,fname,len(fname),it,numdt,numit,dt,
     &     mnumdt,mnumit)
c     
      return
      end
c
c
c
      subroutine mfdcmw(fid, fname, numdt, numit,
     &     mnumdt, mnumit, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfdcmw
c
      implicit none
      save
c
      character*(*) fname
      integer cret
      integer*8 fid
      integer  numdt, numit, mnumdt, mnumit
      integer mfdfcmw
c
      cret = mfdfcmw(fid,fname,len(fname),numdt,numit,
     &     mnumdt,mnumit)
c     
      return
      end
c
c
c
      subroutine mfdnpf(fid,fname,numdt,numit,etype,gtype,
     &                  dpname, dlname, n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfdnpf
c
      implicit none
      save
      integer*8 fid
      integer n,cret
      integer numdt,numit,etype,gtype
      character*(*) fname,dpname,dlname
      
      integer mfdfnpf
c
      n = mfdfnpf(fid,fname,len(fname),numdt,numit,etype,
     &            gtype,dpname,dlname)
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
      subroutine mfdnva(fid,fname,numdt,numit,etype,gtype,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfdnva
c
      implicit none
      save
      character*(*) fname
      integer*8 fid
      integer n,cret,numdt,numit,etype,gtype
      integer mfdfnva
c
      n = mfdfnva(fid,fname,len(fname),numdt,numit,
     &            etype,gtype,cret)
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
      subroutine mfdnvp(fid,fname,numdt,numit,etype,gtype,
     &                  pit,stm,pname,psize,lname,nip,
     &                  n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfdnvp
c
      implicit none
      save
      character*(*) fname, pname, lname
      integer pit,stm,psize
      integer*8 fid
      integer n,cret,numdt,numit,etype,gtype,nip
      integer mfdfnvp
c
      n = mfdfnvp(fid,fname,len(fname),numdt,numit,
     &            etype,gtype,pit,stm,pname,psize,lname,
     &            nip,cret)
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
      subroutine mfdnpn(fid,fname,numdt,numit,etype,gtype,
     &                  pname,stm,psize,lname,nip,
     &                  n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfdnpn
c
      implicit none
      save
      character*(*) fname, pname, lname
      integer stm,psize
      integer*8 fid
      integer n,cret,numdt,numit,etype,gtype,nip
      integer mfdfnpn
c
      n = mfdfnpn(fid,fname,len(fname),numdt,numit,
     &            etype,gtype,pname,len(pname),stm,
     &            psize,lname,nip,cret)
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
      subroutine mfdrvr(fid,fname,numdt,numit,
     &                  etype,gtype,swm,cs,val,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfdrvr
c
      implicit none
      save
      character*(*) fname
      integer numdt,numit
      real*8 val(*)
      integer*8 fid
      integer cret,etype,gtype,swm,cs
      integer mfdfrvr
c
      cret = mfdfrvr(fid,fname,len(fname),numdt,numit,
     &               etype,gtype,swm,cs,val)
c      
      return
      end
c
c
c
      subroutine mfdivr(fid,fname,numdt,numit,
     &                  etype,gtype,swm,cs,val,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfdivr
c
      implicit none
      save
      character*(*) fname
      integer numdt,numit
      integer val(*)
      integer*8 fid
      integer cret,etype,gtype,swm,cs
      integer mfdfivr
c
      cret = mfdfivr(fid,fname,len(fname),numdt,numit,
     &               etype,gtype,swm,cs,val)
c      
      return
      end
c
c
c
      subroutine mfdrpr(fid,fname,numdt,numit,
     &                  etype,gtype,
     &                  stm,pname,
     &                  swm,cs,val,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfdrpr
c
      implicit none
      save
      character*(*) fname,pname
      integer numdt,numit
      real*8 val(*)
      integer*8 fid
      integer cret,etype,gtype,swm,cs,stm
      integer mfdfrpr
c
      cret = mfdfrpr(fid,fname,len(fname),numdt,numit,
     &               etype,gtype,stm,pname,len(pname),swm,cs,val)
c      
      return
      end
c
c
c
      subroutine mfdipr(fid,fname,numdt,numit,
     &                  etype,gtype,
     &                  stm,pname,
     &                  swm,cs,val,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfdipr
c
      implicit none
      save
      character*(*) fname,pname
      integer numdt,numit
      integer val(*)
      integer*8 fid
      integer cret,etype,gtype,swm,cs,n,stm
      integer mfdfipr
c
      cret = mfdfipr(fid,fname,len(fname),numdt,numit,
     &               etype,gtype,stm,pname,len(pname),swm,cs,val,cret)
c      
      return
      end
c
c
c
      subroutine mfdrar(fid,fname,numdt,numit,
     &                  etype,gtype,flt,
     &                  val,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfdrar
c
      implicit none
      save
      character*(*) fname
      integer numdt,numit
      real*8 val(*)
      integer*8 flt(*)
      integer*8 fid
      integer cret,etype,gtype
      integer mfdfrar
c
      cret = mfdfrar(fid,fname,len(fname),numdt,numit,
     &               etype,gtype,flt,val)
c      
      return
      end
c
c
c
      subroutine mfdiar(fid,fname,numdt,numit,
     &                  etype,gtype,flt,
     &                  val,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfdiar
c
      implicit none
      save
      character*(*) fname
      integer numdt,numit
      integer val(*)
      integer*8 flt(*)
      integer*8 fid
      integer cret,etype,gtype
      integer mfdfiar
c
      cret = mfdfiar(fid,fname,len(fname),numdt,numit,
     &               etype,gtype,flt,val)
c      
      return
      end
c
c
c    
      subroutine mfdinw(fid,fname,iname,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfdinw
c
      implicit none
      save
      character*(*) fname,iname
      integer*8 fid
      integer cret
      integer mfdfinw
c
      cret = mfdfinw(fid,fname,len(fname),iname,len(iname),cret)
c      
      return
      end
c
c
c
      subroutine mfdnin(fid,fname,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfdnin
c
      implicit none
      save
      integer*8 fid
      integer n,cret
      character*(*) fname
      integer mfdfnin
c
      n = mfdfnin(fid,fname,len(fname))
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
      subroutine mfdini(fid,fname,it,iname,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfdini
c
      implicit none
      save
      character*(*) fname,iname
      integer*8 fid
      integer cret,it
      integer mfdfini
c
      cret = mfdfini(fid,fname,len(fname),it,iname,cret)
c      
      return
      end
c
c
c
      subroutine mfdoci(fid, fname, it, numdt, numit, dt, 
     &                  nmesh, mname, lmesh, 
     &                  mnumdt, mnumit, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfdoci
c
      implicit none
      save
c
      character*(*) fname,mname
      integer cret
      integer*8 fid
      integer it,numdt,numit,nmesh,lmesh
      integer mnumdt,mnumit
      real*8  dt
      integer mfdfoci
c
      cret = mfdfoci(fid,fname,len(fname),it,
     &               numdt,numit,dt,
     &               nmesh, mname, lmesh, 
     &               mnumdt, mnumit)
c     
      return
      end
c
c
c
      subroutine mfdonp(fid,fname,numdt,numit,etype,gtype,
     &                  it,mname, dpname, dlname, n, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfdonp
c
      implicit none
      save
      integer*8 fid
      integer n,cret
      integer numdt,numit,etype,gtype,it
      character*(*) fname,dpname,dlname,mname
      
      integer mfdfonp
c
      n = mfdfonp(fid,fname,len(fname),numdt,numit,etype,
     &            gtype,it,mname,dpname,dlname)
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
      subroutine mfdonv(fid,fname,numdt,numit,etype,gtype,
     &                  mname,pit,stm,pname,psize,lname,
     &                  nip,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfdonv 
c
      implicit none
      save
      character*(*) fname, pname, lname, mname
      integer pit,stm,psize
      integer*8 fid
      integer n,cret,numdt,numit,etype,gtype,nip
      integer mfdfonv
c
      n = mfdfonv(fid,fname,len(fname),numdt,numit,
     &            etype,gtype,
     &            mname,len(mname),
     &            pit,stm,pname,psize,lname,
     &            nip,cret)
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
      subroutine mfdorr(fid,fname,numdt,numit,
     &                  etype,gtype,
     &                  mname,
     &                  stm,pname,
     &                  swm,cs,val,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfdorr
c
      implicit none
      save
      character*(*) fname,pname,mname
      integer numdt,numit
      real*8 val(*)
      integer*8 fid
      integer cret,etype,gtype,swm,cs,stm
      integer mfdforr
c
      cret = mfdforr(fid,fname,len(fname),numdt,numit,
     &               etype,gtype,mname,len(mname),
     &               stm,pname,len(pname),swm,cs,val)
c      
      return
      end
c
c
c
      subroutine mfdoir(fid,fname,numdt,numit,
     &                  etype,gtype,
     &                  mname,
     &                  stm,pname,
     &                  swm,cs,val,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfdoir
c
      implicit none
      save
      character*(*) fname,pname,mname
      integer numdt,numit
      integer val(*)
      integer*8 fid
      integer cret,etype,gtype,swm,cs,n,stm
      integer mfdfoir
c
      cret = mfdfoir(fid,fname,len(fname),numdt,numit,
     &               etype,gtype,mname,len(mname),
     &               stm,pname,len(pname),swm,cs,val,cret)
c      
      return
      end
