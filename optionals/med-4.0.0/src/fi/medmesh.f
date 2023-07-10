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

      subroutine mmhcre(fid, name, sdim, mdim, mtype, desc, dtunit,
     &                  stype, atype, aname, aunit, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhcre
c
      implicit none
      save
c
      character *(*) name, desc, dtunit, aname, aunit
      integer*8 fid
      integer   sdim, mdim, mtype, stype, atype, cret
      integer mmhfcre
c
      cret = mmhfcre(fid, name, len(name), sdim, mdim, mtype, 
     &               desc, len(desc), dtunit, len(dtunit),
     &               stype, atype, aname, 16*sdim, 
     &               aunit, 16*sdim)
c      
      return
      end
c
c
c
      subroutine mmhnmh( fid , n , cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhnmh
c
      implicit none
      save
c
      integer*8 fid
      integer   n, cret
      integer mmhfnmh 
c
      n = mmhfnmh(fid)

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
      subroutine mmhnax( fid , it, naxis, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhnax
c
      implicit none
      save
c
      integer*8 fid
      integer  it,naxis,cret
      integer mmhfnax
c
      naxis =  mmhfnax(fid,it)
c     
      if (naxis.lt.0) then
         cret = -1
      else
         cret = 0
      endif
      return
      end
c
c
c
      subroutine  mmhnan( fid , name, naxis, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhnan
c
      implicit none
      save
c
      integer*8 fid
      integer  naxis,cret
      character *(*) name
      integer mmhfnan
c
      naxis = mmhfnan(fid,name,len(name))
c     
      if (naxis.lt.0) then
         cret = -1
      else
         cret = 0
      endif
      return
      end
c
c
c
      subroutine mmhmii(fid, it, name, sdim, mdim, mtype, desc, dtunit,
     &                  stype, nstep, atype, aname, aunit, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhmii
c
      implicit none
      save
c
      character *(*) name, desc, dtunit, aname, aunit
      integer*8 fid
      integer   sdim, mdim, mtype, stype, atype, cret, nstep, it
      integer mmhfmhi 
c
      cret = mmhfmhi(fid, it, name, sdim, mdim, mtype, desc, dtunit,
     &               stype, nstep, atype, aname, aunit)
c     
      return
      end
c
c
c
      subroutine mmhmin(fid, name, sdim, mdim, mtype, desc, dtunit,
     &                  stype, nstep, atype, aname, aunit, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhmin
c
      implicit none
      save
c
      character *(*) name, desc, dtunit, aname, aunit
      integer*8 fid
      integer   sdim, mdim, mtype, stype, atype, cret, nstep
      integer mmhfmin 
c
      cret = mmhfmin(fid, name, len(name), sdim, mdim, mtype, desc,
     &               dtunit, stype, nstep, atype, aname, aunit)
c     
      return
      end

c
c
c
      subroutine  mmhunw( fid , name, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhunw
c
      implicit none
      save
c
      integer*8 fid
      integer  cret
      character *(*) name
      integer mmhfunw
c
      cret = mmhfunw(fid,name,len(name))
c     
      return
      end
c
c
c
      subroutine  mmhunr( fid , mname, uname, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhunr
c
      implicit none
      save
c
      integer*8 fid
      integer  cret
      character *(*) uname
      character *(*) mname
      integer mmhfunr
c
      cret = mmhfunr(fid,mname,len(mname),uname)
c     
      return
      end
c
c
c
      subroutine  mmhatw( fid , name, nin, nvn, nnc, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhatw
c
      implicit none
      save
c
      integer*8 fid
      integer  cret,nin,nvn,nnc
      character *(*) name
      integer mmhfatw
c
      cret = mmhfatw(fid,name,len(name), nin, nvn, nnc)
c     
      return
      end
c
c
c
      subroutine  mmhatr( fid , name, nin, nvn, nnc, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhatr
c
      implicit none
      save
c
      integer*8 fid
      integer  cret,nin,nvn,nnc
      character *(*) name
      integer mmhfatr
c
      cret = mmhfatr(fid,name,len(name), nin, nvn, nnc)
c     
      return
      end
c
c
c
      subroutine  mmhgtw( fid , name, gtype, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhgtw
c
      implicit none
      save
c
      integer*8 fid
      integer  cret,gtype
      character *(*) name
      integer mmhfgtw
c
      cret = mmhfgtw(fid,name,len(name), gtype)
c     
      return
      end
c
c
c
      subroutine  mmhgtr( fid , name, gtype, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhgtr
c
      implicit none
      save
c
      integer*8 fid
      integer  cret,gtype
      character *(*) name
      integer mmhfgtr
c
      cret = mmhfgtr(fid,name,len(name), gtype)
c     
      return
      end
c
c
c
      subroutine  mmhgsw( fid , name, numdt, numit, dt, st, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhgsw
c
      implicit none
      save
c
      integer*8 fid
      integer  cret,numdt, numit
      character *(*) name
      real*8 dt
      integer st( * )
      integer mmhfgsw
c
      cret = mmhfgsw(fid,name,len(name), numdt, numit, dt, st)
c     
      return
      end
c
c
c
      subroutine mmhgsr( fid , name, numdt, numit, st, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhgsr
c
      implicit none
      save
c
      integer*8 fid
      integer  cret,numdt, numit
      character *(*) name
      integer st(*)
      integer mmhfgsr
c
      cret = mmhfgsr(fid,name,len(name), numdt, numit, st)
c     
      return
      end
c
c
c
      subroutine  mmhcow( fid , name, numdt, numit, dt, 
     &                    swm, n, coo, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhcow
c
      implicit none
      save
c
      integer*8 fid
      integer  cret,numdt,numit,n,swm
      real*8 dt
      character *(*) name
      real*8 coo(*)
      integer mmhfcow
c
      cret = mmhfcow(fid,name,len(name),numdt,numit,dt,swm,n,coo)
c     
      return
      end
c
c
c
      subroutine  mmhcor( fid , name, numdt, numit,
     &                    swm, coo, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhcor
c
      implicit none
      save
c
      integer*8 fid
      integer  cret,numdt,numit,swm
      character *(*) name
      real*8 coo(*)
      integer mmhfcor
c
      cret = mmhfcor(fid,name,len(name),numdt,numit,swm,coo)
c     
      return
      end
c
c
c
      subroutine  mmhcpw( fid , name, numdt, numit, dt, 
     &                    stm, pname, swm, dim, n, coo, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhcpw
c
      implicit none
      save
c
      integer*8 fid
      integer  cret,numdt,numit,n,swm,stm,dim
      real*8 dt
      character *(*) name, pname
      real*8 coo(*)
      integer mmhfcpw
c
      cret = mmhfcpw(fid,name,len(name),numdt,numit,dt,stm,
     &               pname,len(pname),swm,dim,n,coo)
c     
      return
      end
c
c
c
      subroutine  mmhcpr( fid , name, numdt, numit,
     &                    stm, pname, swm, dim, coo, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhcpr
c
      implicit none
      save
c
      integer*8 fid
      integer  cret,numdt,numit,swm,stm,dim
      character *(*) name, pname
      real*8 coo(*)
      integer mmhfcpr
c
      cret = mmhfcpr(fid,name,len(name),numdt,numit,stm,
     &               pname,len(pname),swm,dim,coo)
c     
      return
      end
c
c
c
      subroutine  mmhgcw( fid , name, numdt, numit, dt, 
     &                    axis, size, index, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhgcw
c
      implicit none
      save
c
      integer*8 fid
      integer  cret,numdt,numit,axis,size
      real*8 dt
      character *(*) name
      real*8 index(*)
      integer mmhfgcw
c
      cret = mmhfgcw(fid,name,len(name),numdt,numit,dt,axis,size,index)
c     
      return
      end
c
c
c
      subroutine  mmhgcr( fid , name, numdt, numit, 
     &                    axis, index, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhgcr
c
      implicit none
      save
c
      integer*8 fid
      integer  cret,numdt,numit,axis
      character *(*) name
      real*8 index(*)
      integer mmhfgcr
c
      cret = mmhfgcr(fid,name,len(name),numdt,numit,axis,index)
c     
      return
      end
c
c
c
      subroutine  mmhenw(fid,name,numdt,numit,entype,geotype, 
     &                   n,num,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhenw
c
      implicit none
      save
c
      integer*8 fid
      integer  cret,numdt, numit,n,entype,geotype
      character *(*) name
      integer num(*)
      integer mmhfenw
c
      cret = mmhfenw(fid,name,len(name),numdt,numit,entype,geotype,
     &               n,num)
c     
      return
      end
c
c
c
      subroutine  mmhenr(fid,name,numdt,numit,entype,geotype, 
     &                   num,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhenr
c
      implicit none
      save
c
      integer*8 fid
      integer  cret,numdt, numit,entype,geotype
      character *(*) name
      integer num(*)
      integer mmhfenr
c
      cret = mmhfenr(fid,name,len(name),numdt,numit,entype,geotype,
     &               num)
c     
      return
      end
c
c
c
      subroutine  mmhfnw(fid,name,numdt,numit,entype,geotype, 
     &                   n,num,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhfnw
c
      implicit none
      save
c
      integer*8 fid
      integer  cret,numdt, numit,n,entype,geotype
      character *(*) name
      integer num(*)
      integer mmhffnw
c
      cret = mmhffnw(fid,name,len(name),numdt,numit,entype,geotype,
     &               n,num)
c     
      return
      end
c
c
c
      subroutine  mmhfnr(fid,name,numdt,numit,entype,geotype, 
     &                   num,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhfnr
c
      implicit none
      save
c
      integer*8 fid
      integer  cret,numdt, numit,entype,geotype
      character *(*) name
      integer num(*)
      integer mmhffnr
c
      cret = mmhffnr(fid,name,len(name),numdt,numit,entype,geotype,
     &               num)
c     
      return
      end
c
c
c
      subroutine mmheaw(fid,mname,numdt,numit,entype,geotype,
     &                  n,ename,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmheaw
c
      implicit none
      save
c
      character *(*) mname
      character*(*) ename
      integer*8 fid
      integer   numdt, numit, entype, geotype, n, cret
      integer mmhfeaw
c
      cret = mmhfeaw(fid,mname,len(mname),numdt,numit,entype,
     &               geotype,n,ename,16*n)
c      
      return
      end
c
c
c
      subroutine mmhear(fid,mname,numdt,numit,entype,geotype,
     &                  ename,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhear
c
      implicit none
      save
c
      character *(*) mname
      character*(*) ename
      integer*8 fid
      integer   numdt, numit, entype, geotype, cret
      integer mmhfear

      cret = mmhfear(fid,mname,len(mname),numdt,numit,entype,
     &               geotype,ename)
c      
      return
      end
c
c
c
      subroutine mmhnme(fid,name,numdt,numit,
     &                  entype,geotype,datype,cmode,
     &                  chgt,tsf,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhnme
c
      implicit none
      save
c
      character *(*) name
      integer*8 fid
      integer  numdt,numit,entype,geotype,datype,cret
      integer cmode,chgt,tsf,n
      integer mmhfnme
c
      n =  mmhfnme(fid,name,len(name),numdt,numit,entype,geotype,
     &             datype,cmode,chgt,tsf)

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
      subroutine  mmhcyw(fid,name,numdt,numit,dt,entype,geotype, 
     &                   cmode,swm,n,con,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhcyw
c
      implicit none
      save
c
      integer*8 fid
      integer  cret,numdt,numit,n,entype,geotype,swm,cmode
      character *(*) name
      integer con(*)
      real*8 dt
      integer mmhfcyw
c
      cret = mmhfcyw(fid,name,len(name),numdt,numit,dt,entype,geotype,
     &               cmode,swm,n,con)
c     
      return
      end
c
c
c
      subroutine  mmhcyr(fid,name,numdt,numit,entype,geotype, 
     &                   cmode,swm,con,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhcyr
c
      implicit none
      save
c
      integer*8 fid
      integer  cret,numdt,numit,entype,geotype,swm,cmode
      character *(*) name
      integer con(*)
      integer mmhfcyr
c
      cret = mmhfcyr(fid,name,len(name),numdt,numit,entype,geotype,
     &               cmode,swm,con)
c     
      return
      end
c
c
c
      subroutine  mmhypw(fid,name,numdt,numit,dt,entype,geotype, 
     &                   cmode,stmode,pname,swm,dim,n,con,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhypw
c
      implicit none
      save
c
      integer*8 fid
      integer  cret,numdt,numit,n,entype,geotype,swm
      integer cmode,stmode,dim
      character *(*) name
      character *(*) pname
      integer con(*)
      real*8 dt
      integer mmhfypw
c
      cret = mmhfypw(fid,name,len(name),numdt,numit,dt,entype,geotype,
     &               cmode,stmode,pname,len(pname),swm,dim,n,con)
c     
      return
      end
c
c
c
      subroutine  mmhypr(fid,name,numdt,numit,entype,geotype, 
     &                   cmode,stmode,pname,swm,dim,n,con,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhypr
c
      implicit none
      save
c
      integer*8 fid
      integer  cret,numdt,numit,entype,geotype,swm
      integer cmode,n,stmode,dim
      character *(*) name
      character *(*) pname
      integer con(*)
      integer mmhfypr
c
      cret = mmhfypr(fid,name,len(name),numdt,numit,entype,geotype,
     &               cmode,stmode,pname,swm,dim,n,con)
c     
      return
      end
c
c
c
      subroutine mmhnep(fid,name,numdt,numit,
     &                  entype,geotype,datype,cmode,
     &                  stmode,pname,psize,
     &                  chgt,tsf,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhnep
c
      implicit none
      save
c
      character *(*) name
      character *(*) pname
      integer*8 fid
      integer  numdt,numit,entype,geotype,datype,cret
      integer cmode,chgt,tsf,n,stmode,psize
      integer mmhfnep
c

      n =  mmhfnep(fid,name,len(name),numdt,numit,entype,geotype,
     &             datype,cmode,stmode,pname,psize,chgt,tsf)

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
      subroutine  mmhnor(fid , name, numdt, numit, 
     &                   swm, coo, iname, nname, inum, num,
     &                   ifam,fam, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhnor
      implicit none
      save
c
      integer*8 fid
      integer  cret,numdt,numit,swm
      character *(*) name
      character *(*) nname
      real*8 coo(*)
      integer num(*),fam(*)
      integer iname,inum,ifam
      integer mmhfnor
c
      cret = mmhfnor(fid,name,len(name),numdt,numit,swm,coo,
     &               iname,nname,inum,num,ifam,fam)
c     
      return
      end
c
c
c
c
      subroutine  mmhnow(fid, name, numdt, numit, dt,
     &                   swm, n, coo, iname, nname, inum, num,
     &                   ifam, fam, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhnow
c
      implicit none
      save
c
      integer*8 fid
      integer  cret,numdt,numit,swm,n
      character *(*) name
      character *(*) nname
      real*8 coo(*), dt
      integer num(*),fam(*)
      integer iname,inum,ifam
      integer mmhfnow
c
      cret = mmhfnow(fid,name,len(name),numdt,numit,dt,swm,n,coo,
     &               iname,nname,16*n,inum,num,ifam,fam)
c     
      return
      end
c
c
c
      subroutine  mmhelw(fid,name,numdt,numit,dt,entype,geotype, 
     &                   cmode,swm,n,con,iname, nname, inum, num,
     &                   ifam,fam,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhelw
c
      implicit none
      save
c
      integer*8 fid
      integer  cret,numdt,numit,n,entype,geotype,swm,cmode
      character *(*) name
      character *(*) nname
      integer con(*)
      real*8 dt
      integer iname,inum,ifam
      integer num(*),fam(*)
      integer mmhfelw
c
      cret = mmhfelw(fid,name,len(name),numdt,numit,dt,entype,geotype,
     &               cmode,swm,n,con,
     &               iname,nname,16*n,inum,num,ifam,fam)
c     
      return
      end
c
c
c
      subroutine  mmhelr(fid,name,numdt,numit,entype,geotype, 
     &                   cmode,swm,con,iname, nname, inum, num,
     &                   ifam,fam,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhelr
c
      implicit none
      save
c
      integer*8 fid
      integer  cret,numdt,numit,entype,geotype,swm,cmode
      character *(*) name
      character *(*) nname
      integer con(*)
      integer iname,inum,ifam
      integer num(*),fam(*)
      integer mmhfelr
c
      cret = mmhfelr(fid,name,len(name),numdt,numit,entype,geotype,
     &               cmode,swm,con,iname,nname,inum,num,ifam,fam)
c     
      return
      end
c
c
c
      subroutine mmhcaw( fid , name, numdt, numit, dt, 
     &                    flt, coo, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhcaw
c
      implicit none
      save
c
      integer*8 fid
      integer  cret,numdt,numit
      real*8 dt
      character *(*) name
      real*8 coo(*)
      integer*8 flt(*)
      integer mmhfcaw
c
      cret = mmhfcaw(fid,name,len(name),numdt,numit,dt,flt,coo)
c     
      return
      end
c
c
c
      subroutine  mmhcar(fid , name, numdt, numit,
     &                   flt, coo, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhcar
c
      implicit none
      save
c
      integer*8 fid
      integer  cret,numdt,numit
      character *(*) name
      real*8 coo(*)
      integer*8 flt(*)
      integer mmhfcar
c
      cret = mmhfcar(fid,name,len(name),numdt,numit,flt,coo)
c     
      return
      end
c
c
c
      subroutine  mmhyaw(fid,name,numdt,numit,dt,entype,geotype, 
     &                   cmode,flt,con,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhyaw
c
      implicit none
      save
c
      integer*8 fid
      integer  cret,numdt,numit,entype,geotype,cmode
      character *(*) name
      integer con(*)
      integer*8 flt(*)
      real*8 dt
      integer mmhfyaw
c
      cret = mmhfyaw(fid,name,len(name),numdt,numit,dt,entype,geotype,
     &               cmode,flt,con)
c     
      return
      end
c
c
c
      subroutine  mmhyar(fid,name,numdt,numit,entype,geotype, 
     &                   cmode,flt,con,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhyar
c
      implicit none
      save
c
      integer*8 fid
      integer  cret,numdt,numit,entype,geotype,cmode
      character *(*) name
      integer con(*)
      integer*8 flt(*)
      integer mmhfyar
c
      cret = mmhfyar(fid,name,len(name),numdt,numit,entype,geotype,
     &               cmode,flt,con)
c     
      return
      end
c
c
c
      subroutine  mmhpgw(fid,name,numdt,numit,dt,entype,
     &                   cmode,isize,index,con,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhpgw
c
      implicit none
      save
c
      integer*8 fid
      integer  cret,numdt,numit,isize,entype,cmode
      character *(*) name
      integer con(*), index(*)
      real*8 dt
      integer mmhfpgw
c
      cret = mmhfpgw(fid,name,len(name),numdt,numit,dt,entype,
     &               cmode,isize,index,con)
c     
      return
      end
c
c
c
      subroutine  mmhpgr(fid,name,numdt,numit,entype,
     &                   cmode,index,con,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhpgr
c
      implicit none
      save
c
      integer*8 fid
      integer  cret,numdt,numit,entype,cmode
      character *(*) name
      integer con(*), index(*)
      integer mmhfpgr
c
      cret = mmhfpgr(fid,name,len(name),numdt,numit,entype,
     &               cmode,index,con)
c     
      return
      end
c
c
c
      subroutine  mmhphw(fid,name,numdt,numit,dt,entype,
     &                   cmode,fisize,findex,nisize,nindex,con,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhphw
c
      implicit none
      save
c
      integer*8 fid
      integer   cret,numdt,numit,fisize,nisize,entype,cmode
      character *(*) name
      integer con(*), findex(*), nindex(*) 
      real*8 dt
      integer mmhfphw
c
      cret = mmhfphw(fid,name,len(name),numdt,numit,dt,entype,
     &               cmode,fisize,findex,nisize,nindex,con)
c     
      return
      end
c
c
c
      subroutine  mmhphr(fid,name,numdt,numit,entype,
     &                   cmode,findex,nindex,con,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhphr
c
      implicit none
      save
c
      integer*8 fid
      integer   cret,numdt,numit,entype,cmode
      character *(*) name
      integer con(*), findex(*), nindex(*) 
      integer mmhfphr
c
      cret = mmhfphr(fid,name,len(name),numdt,numit,entype,
     &               cmode,findex,nindex,con)
c     
      return
      end
c
c
c
      subroutine  mmhgnw(fid,name,numdt,numit,entype,geotype, 
     &                   n,num,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhgnw
c
      implicit none
      save
c
      integer*8 fid
      integer   cret,numdt, numit,n,entype,geotype
      character *(*) name
      integer num(*)
      integer mmhfgnw
c
      cret = mmhfgnw(fid,name,len(name),numdt,numit,entype,geotype,
     &               n,num)
c     
      return
      end
c
c
c
      subroutine  mmhgnr(fid,name,numdt,numit,entype,geotype, 
     &                   num,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhgnr
c
      implicit none
      save
c
      integer*8 fid
      integer   cret,numdt, numit,entype,geotype
      character *(*) name
      integer num(*)
      integer mmhfgnr
c
      cret = mmhfgnr(fid,name,len(name),numdt,numit,entype,geotype,
     &               num)
c     
      return
      end
c
c
c
      subroutine  mmhcsc(fid,name,numdt1,numit1,numdt2,numit2,
     &                   dt2,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhcsc
c
      implicit none
      save
c
      integer*8 fid
      integer   cret,numdt1,numit1,numdt2,numit2
      character *(*) name
      real*8 dt2
      integer mmhfcsc
c
      cret = mmhfcsc(fid,name,len(name),numdt1,numit1,
     &               numdt2,numit2,dt2)
c     
      return
      end
c
c
c
      subroutine  mmhcsi(fid,name,csit,numdt,numit,dt,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhcsi
c
      implicit none
      save
c
      integer*8 fid
      integer   cret,numdt,numit,csit
      character *(*) name
      real*8 dt
      integer mmhfcsi
c
      cret = mmhfcsi(fid,name,len(name),csit,numdt,numit,dt)
c     
      return
      end
c
c
c
      subroutine  mmhcsr(fid,name,numdt,numit,dt,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhcsr
c
      implicit none
      save
c
      integer*8 fid
      integer   cret,numdt,numit
      character *(*) name
      real*8 dt
      integer mmhfcsr
c
      cret = mmhfcsr(fid,name,len(name),numdt,numit,dt)
c     
      return
      end
c
c
c
      subroutine  mmhstr(fid,name,stype,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhstr
c
      implicit none
      save
c
      integer*8 fid
      integer   cret,stype
      character *(*) name
      integer mmhfstr
c
      cret = mmhfstr(fid,name,len(name),stype)
c     
      return
      end
c
c
c
      subroutine mmhraw(fid,name,numdt,numit,
     &                  geotype,aname,n,
     &                  val,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhraw
c
      implicit none
      save
c
      character *(*) name,aname
      integer*8 fid
      integer   numdt,numit,geotype,cret
      integer   n
      real*8 val(*)
      integer mmhfraw
c
      cret =  mmhfraw(fid,name,len(name),numdt,numit,geotype,
     &                aname,len(aname),n,val)
c      
      return
      end
c
c
c
      subroutine mmhiaw(fid,name,numdt,numit,
     &                  geotype,aname,n,
     &                  val,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhiaw
c
      implicit none
      save
c
      character *(*) name,aname
      integer*8 fid
      integer   numdt,numit,geotype,cret
      integer   n
      integer   val(*)
      integer mmhfiaw
c
      cret =  mmhfiaw(fid,name,len(name),numdt,numit,geotype,
     &                aname,len(aname),n,val)
c      
      return
      end
c
c
c
      subroutine mmhsaw(fid,name,numdt,numit,
     &                  geotype,aname,n,
     &                  val,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhsaw
c
      implicit none
      save
c
      character *(*) name,aname
      integer*8 fid
      integer   numdt,numit,geotype,cret
      integer   n
      character *(*) val
      integer mmhfsaw
c
      cret =  mmhfsaw(fid,name,len(name),numdt,numit,geotype,
     &                aname,len(aname),n,val,64*n)
c      
      return
      end
c
c
c
      subroutine mmhrar(fid,name,numdt,numit,
     &                  geotype,aname,
     &                  val,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhrar
c
      implicit none
      save
c
      character *(*) name,aname
      integer*8 fid
      integer   numdt,numit,geotype,cret
      real*8    val(*)
      integer   mmhfrar
c
      cret =  mmhfrar(fid,name,len(name),numdt,numit,geotype,
     &                aname,len(aname),val)
c      
      return
      end
c
c
c
      subroutine mmhiar(fid,name,numdt,numit,
     &                  geotype,aname,val,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhfiar
c
      implicit none
      save
c
      character *(*) name,aname
      integer*8 fid
      integer   numdt,numit,geotype,cret
      integer   val(*)
      integer   mmhfiar
c
      cret =  mmhfiar(fid,name,len(name),numdt,numit,geotype,
     &                aname,len(aname),val)
c      
      return
      end
c
c
c
      subroutine mmhsar(fid,name,numdt,numit,
     &                  geotype,aname,val,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhsar
c
      implicit none
      save
c
      character *(*) name,aname
      integer*8 fid
      integer   numdt,numit,geotype,cret
      integer n
      character *(*) val
      integer mmhfsar
c
      cret =  mmhfsar(fid,name,len(name),numdt,numit,geotype,
     &                aname,len(aname),val)
c      
      return
      end
c
c
c
      subroutine  mmheni(fid,name,numdt,numit,entype,it, 
     &                   geoname,geotype,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmheni
c
      implicit none
      save
c
      integer*8 fid
      integer   cret,numdt,numit,entype,geotype,it
      character *(*) name, geoname
      integer mmhfeni
c
      cret = mmhfeni(fid,name,len(name),numdt,numit,entype,
     &               it,geoname,geotype)
c     
      return
      end
c
c
c
      subroutine  mmhtfw( fid , name, numdt, numit, dt, 
     &                    tsf,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhtfw
c
      implicit none
      save
c
      integer*8 fid
      integer   cret,numdt,numit
      real*8    dt
      character *(*) name
      real*8    tsf(*)
      integer mmhftfw
c
      cret = mmhftfw(fid,name,len(name),numdt,numit,dt,tsf)
c     
      return
      end
c
c
c
      subroutine mmhtfr( fid , name, numdt, numit, 
     &                   tsf,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhtfr
c
      implicit none
      save
c
      integer*8 fid
      integer   cret,numdt,numit
      character *(*) name
      real*8 tsf(*)
      integer mmhftfr
c
      cret = mmhftfr(fid,name,len(name),numdt,numit,tsf)
c     
      return
      end
c
c
c
      subroutine mmhaaw(fid,mname,dtype,numdt,numit,entype,geotype,
     &                  flt,val,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhaaw
c
      implicit none
      save
c
      character *(*) mname
      integer   val(*)
      integer*8 fid
      integer   numdt,numit,entype,geotype
      integer   dtype,cret
      integer   mmhfaaw
      integer*8 flt(*)
c
      cret = mmhfaaw(fid,mname,len(mname),dtype,numdt,numit,entype,
     &               geotype,flt,val)
c      
      return
      end
c
c
c
      subroutine mmhaar(fid,mname,dtype,numdt,numit,entype,geotype,
     &                  flt,val,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhaar
c
      implicit none
      save
c
      character *(*) mname
      integer  val(*)
      integer*8 fid
      integer   numdt,numit,entype,geotype
      integer   dtype,cret
      integer   mmhfaar
      integer*8 flt(*)
c
      cret = mmhfaar(fid,mname,len(mname),dtype,numdt,numit,entype,
     &               geotype,flt,val)
c      
      return
      end
c
c
c
      subroutine mmhasw(fid,mname,dtype,numdt,numit,entype,geotype,
     &                  flt,val,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhasw
c
      implicit none
      save
c
      character *(*) mname
      character *(*) val
      integer*8 fid
      integer   numdt,numit,entype,geotype
      integer   dtype,cret
      integer   mmhfasw
      integer*8 flt(*)
c
      cret = mmhfasw(fid,mname,len(mname),dtype,numdt,numit,entype,
     &               geotype,flt,val)
c      
      return
      end
c
c
c
      subroutine mmhasr(fid,mname,dtype,numdt,numit,entype,geotype,
     &                  flt,val,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mmhasr
c
      implicit none
      save
c
      character *(*) mname
      character *(*) val
      integer*8 fid
      integer   numdt,numit,entype,geotype
      integer   dtype,cret
      integer   mmhfasr
      integer*8 flt(*)
c
      cret = mmhfasr(fid,mname,len(mname),dtype,numdt,numit,entype,
     &               geotype,flt,val)
c      
      return
      end
