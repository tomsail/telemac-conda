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

      subroutine msecre(fid,mname,mdim,smname,setype,
     &                  sgtype, etype, cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: msecre
c
      implicit none
      save
      character*(*) mname,smname
      integer*8 fid
      integer   cret,mdim,setype,sgtype,etype
      integer msefcre
c
      etype = msefcre(fid,mname,len(mname),mdim,
     &               smname,len(smname),setype,sgtype)
c      
      if (etype.lt.0) then
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
      subroutine msense(fid,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: msense
c
      implicit none
      save
      integer*8 fid
      integer   n,cret
      integer msefnse
c
      n = msefnse(fid)
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
      subroutine msesei(fid,it,mname,mgtype,mdim,smname,
     &                  setype,snnode,sncell,sgtype,
     &                  ncatt,ap,nvatt,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: msesei
c
      implicit none
      save
      character*(*) mname,smname
      integer*8 fid
      integer   cret,mdim,setype,sgtype,it
      integer snnode,sncell,ncatt,ap,nvatt,mgtype
      integer msefsei
c
      cret = msefsei(fid,it,mname,mgtype,mdim,smname,
     &               setype,snnode,sncell,sgtype,
     &               ncatt,ap,nvatt)
c      
      return
      end
c
c
c
      subroutine msesin(fid,mname,mgtype,mdim,smname,
     &                  setype,snnode,sncell,sgtype,
     &                  ncatt,ap,nvatt,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: msesin
c
      implicit none
      save
      character*(*) mname,smname
      integer*8 fid
      integer   cret,mdim,setype,sgtype
      integer snnode,sncell,ncatt,ap,nvatt,mgtype
      integer msefsin
c
      cret = msefsin(fid,mname,len(mname),mgtype,
     &               mdim,smname,setype,snnode,
     &               sncell,sgtype,ncatt,ap,nvatt)
c      
      return
      end
c
c
c
      subroutine msesen(fid,mgtype,mname,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: msesen
c
      implicit none
      save
      character*(*) mname
      integer*8 fid
      integer   cret,mgtype
      integer msefsen
c
      cret = msefsen(fid,mgtype,mname)
c      
      return
      end
c
c
c
      subroutine msesgt(fid,mname,gtype,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: msesgt
c
      implicit none
      save
      character*(*) mname
      integer*8 fid
      integer   gtype,cret
      integer msefsgt
c
      gtype = msefsgt(fid,mname,len(mname))
c
      if (gtype.lt.0) then
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
      subroutine msevac(fid,mname,aname,atype,anc,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: msevac
c
      implicit none
      save
      character*(*) mname,aname
      integer*8 fid
      integer   cret,atype,anc
      integer msefvac
c
      cret = msefvac(fid,mname,len(mname),
     &               aname,len(aname),atype,
     &               anc)
c      
      return
      end
c
c
c
      subroutine msevni(fid,mname,aname,atype,anc,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: msevni
c
      implicit none
      save
      character*(*) mname,aname
      integer*8 fid
      integer   cret,atype,anc
      integer msefvni
c
      cret = msefvni(fid,mname,len(mname),
     &               aname,len(aname),
     &               atype,anc)
c      
      return
      end
c
c
c
      subroutine msevai(fid,mname,it,aname,atype,anc,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: msevai
c
      implicit none
      save
      character*(*) mname,aname
      integer*8 fid
      integer   cret,atype,anc,it
      integer msefvai
c
      cret = msefvai(fid,mname,len(mname),it,
     &               aname,atype,anc)
c      
      return
      end
c
c
c
      subroutine mseasz(atype,size,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mseasz
c
      implicit none
      save
      integer size,atype,cret
      integer msefasz
c
      size = msefasz(atype)
c
      if (size.lt.0) then
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
      subroutine mseraw(fid,mname,aname,atype,anc,
     &                  setype,val,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mseraw
c
      implicit none
      save
      character*(*) mname,aname
      integer*8 fid
      integer   cret,atype,anc,setype
      real*8 val(*)
      integer msefraw
c
      cret = msefraw(fid,mname,len(mname),
     &               aname,len(aname),atype,
     &               anc,setype,val)
c      
      return
      end
c
c
c
      subroutine mseiaw(fid,mname,aname,atype,anc,
     &                  setype,val,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mseiaw
c
      implicit none
      save
      character*(*) mname,aname
      integer*8 fid
      integer   cret,atype,anc,setype
      integer val(*)
      integer msefiaw
c
      cret = msefiaw(fid,mname,len(mname),
     &               aname,len(aname),atype,
     &               anc,setype,val)
c      
      return
      end
c
c
c
      subroutine msesaw(fid,mname,aname,atype,anc,
     &                  setype,val,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: msesaw
c
      implicit none
      save
      character*(*) mname,aname
      integer*8 fid
      integer   cret,atype,anc,setype
      character *(*) val
      integer msefsaw
c
      cret = msefsaw(fid,mname,len(mname),
     &               aname,len(aname),atype,
     &               anc,setype,val,anc*64)
c      
      return
      end
c
c
c
      subroutine mserpw(fid,mname,aname,atype,anc,
     &                  setype,pname,val,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mserpw
c
      implicit none
      save
      character*(*) mname,aname,pname
      integer*8 fid
      integer   cret,atype,anc,setype
      real*8 val(*)
      integer msefrpw
c
      cret = msefrpw(fid,mname,len(mname),
     &               aname,len(aname),atype,
     &               anc,setype,pname,len(pname),
     &               val)
c      
      return
      end
c
c
c
      subroutine mseipw(fid,mname,aname,atype,anc,
     &                  setype,pname,val,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mseipw
c
      implicit none
      save
      character*(*) mname,aname,pname
      integer*8 fid
      integer   cret,atype,anc,setype
      integer val(*)
      integer msefipw
c
      cret = msefipw(fid,mname,len(mname),
     &               aname,len(aname),atype,
     &               anc,setype,pname,len(pname),
     &               val)
c      
      return
      end
c
c
c
      subroutine msespw(fid,mname,aname,atype,anc,
     &                  setype,pname,val,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: msespw
c
      implicit none
      save
      character*(*) mname,aname,pname
      integer*8 fid
      integer   cret,atype,anc,setype
      character *(*) val
      integer msefspw
c
      cret = msefspw(fid,mname,len(mname),
     &               aname,len(aname),atype,
     &               anc,setype,pname,len(pname),
     &               val,64*anc)
c      
      return
      end
c
c
c
      subroutine msecni(fid,mname,aname,atype,anc,
     &                  setype,pname,psize,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: msecni
c
      implicit none
      save
      character*(*) mname,aname,pname
      integer*8 fid
      integer   cret,atype,anc,setype,psize
      integer msefcni
c
      cret = msefcni(fid,mname,len(mname),
     &               aname,len(aname),atype,
     &               anc,setype,pname,psize)
c      
      return
      end
c
c
c
      subroutine msecai(fid,mname,it,aname,atype,anc,
     &                  setype,pname,psize,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: msecai
c
      implicit none
      save
      character*(*) mname,aname,pname
      integer*8 fid
      integer   cret,atype,anc,setype,psize,it
      integer msefcai
c
      cret = msefcai(fid,mname,len(mname),it,
     &               aname,atype,
     &               anc,setype,pname,psize)
c      
      return
      end
c
c
c
      subroutine mserar(fid,mname,aname,val,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mserar
c
      implicit none
      save
      character*(*) mname,aname
      integer*8 fid
      integer   cret
      real*8 val(*)
      integer msefrar
c
      cret = msefrar(fid,mname,len(mname),
     &               aname,len(aname),val)
c      
      return
      end
c
c
c
      subroutine mseiar(fid,mname,aname,val,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mseiar
c
      implicit none
      save
      character*(*) mname,aname
      integer*8 fid
      integer   cret
      integer val(*)
      integer msefiar
c
      cret = msefiar(fid,mname,len(mname),
     &               aname,len(aname),val)
c      
      return
      end
c
c
c
      subroutine msesar(fid,mname,aname,val,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: msesar
c
      implicit none
      save
      character*(*) mname,aname
      integer*8 fid
      integer   cret
      character *(*) val
      integer msefsar
c
      cret = msefsar(fid,mname,len(mname),
     &               aname,len(aname),
     &               val)
c      
      return
      end
