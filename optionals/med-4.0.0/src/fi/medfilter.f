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
      subroutine mfrcre(fid,nent,nvent,ncent,cs,swm,
     &                  stm,pname,fltas,flta,flt,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfrcre
c
      implicit none
      save
      character*(*) pname 
      integer flta(*)
      integer*8 fid
      integer   nent,nvent,ncent
      integer stm,fltas
      integer*8 flt
      integer cret,cs,swm
      integer mfrfcre
c
      cret = mfrfcre(fid,nent,nvent,ncent,cs,swm,stm,
     &               pname,len(pname),fltas,flta,flt)
c      
      return
      end
c
c
c
      subroutine mfrall(nflt,flt,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfrall
c
      implicit none
      save 
      integer*8 flt(*)
      integer nflt,cret
      integer mfrfall
c
      cret = mfrfall(nflt,flt)
c      
      return
      end     
c
c
c
      subroutine mfrdea(nflt,flt,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfrdea
c
      implicit none
      save 
      integer*8 flt(*)
      integer nflt,cret
      integer mfrfdea
c
      cret = mfrfdea(nflt,flt)
c      
      return
      end     
c
c
c
      subroutine mfrblc(fid,nent,nvent,ncent,cs,swm,
     &                  stm,pname,start,stride,count,bsize,
     &                  lbsize,flt,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: mfrblc
c
      implicit none
      save
      character*(*) pname 
      integer*8 fid
      integer nent,nvent,ncent
      integer start,stride,count,bsize,lbsize
      integer stm
      integer*8 flt
      integer cret,cs,swm
      integer mfrfblc
c
      cret = mfrfblc(fid,nent,nvent,ncent,cs,swm,stm,
     &               pname,len(pname),start,stride,count,bsize,
     &               lbsize,flt)
c      
      return
      end


