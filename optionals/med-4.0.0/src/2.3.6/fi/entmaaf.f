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

      subroutine efcone (fid, maa, mdim,conn, switch, n, 
     1typent,typgeo,typcon,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efcone
c
      implicit none
      save
c
      character *(*) maa
      integer*8 fid
      integer   conn(*),typent,typgeo,typcon,cret
      integer edfcone,n,mdim, switch
c
      cret = edfcone (fid, maa ,len(maa),mdim,conn,switch,n, 
     1typent,typgeo,typcon)
c      
      return
      end
c
c
      subroutine efconl(fid, maa, mdim,conn,switch,
     1                  pfltab,psize,
     1                  typent,typgeo,typcon,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efconl
c
      implicit none
      save
c
      character *(*) maa
      integer mdim
      integer*8 fid
      integer   conn(*),typent,typgeo,typcon,cret
      integer pfltab(*), psize
      integer edfconl,switch
c
      cret = edfconl (fid, maa ,len(maa),mdim,conn,switch,
     1                pfltab,psize, 
     1                typent,typgeo,typcon)
c      
      return
      end
c
      subroutine efcooe(fid, maa, mdim, coo, modcoo,
     &                  n,typrep, 
     &                  nom,unit,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efcooe
c
      implicit none
      save
c
      character *(*) maa
      real*8 coo(*)
      integer*8 fid
      integer  typrep,mdim
      integer edfcooe,n,cret,modcoo
      character *(*) nom, unit
c
c      
         cret = edfcooe (fid, maa ,len(maa),mdim,coo,modcoo,
     &                   n, typrep,nom, 16*mdim, unit, 16*mdim )
c 
      return
      end
c
      subroutine efcool(fid, maa, mdim,coo,modcoo,
     1                  numco,pfltab,psize, 
     1                  typrep,nom,unit,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efcool
c
      implicit none
      save
c
      character *(*) maa
      real*8 coo(*)
      integer mdim
      integer*8 fid
      integer  typrep,cret,modcoo
      integer pfltab(*),psize, numco
      integer edfcool
      character *(*) nom, unit
c
      cret = edfcool (fid, maa ,len(maa),mdim,coo,modcoo,
     1                numco,pfltab,psize,
     1                typrep,nom, unit)
c      
      return
      end
c
      subroutine efnome(fid, maa, nom, n, 
     1                  typent, typgeo,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efnome
c
      implicit none
      save
c
      character *(*) maa
      character*(*) nom
      integer*8 fid
      integer  typent, typgeo,cret
      integer edfnome,n
c
      cret = edfnome (fid, maa ,len(maa),nom, 16*n, n,
     1typent, typgeo)
c      
      return
      end
c
      subroutine efnoml(fid, maa, nom, n,  
     1typent, typgeo,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efnoml
c
      implicit none
      save
c
      character *(*) maa
      character*(*) nom
      integer*8 fid
      integer  typent, typgeo,cret
      integer edfnoml,n
c
      cret = edfnoml (fid, maa ,len(maa),nom,n,
     1typent, typgeo)
c      
      return
      end
c
      subroutine efnume(fid, maa, num, n, 
     1typent, typgeo,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efnume
c
      implicit none
      save
c
      character *(*) maa
      integer num(*)
      integer*8 fid
      integer  typent, typgeo,cret
      integer edfnume, n
c
      cret = edfnume (fid, maa ,len(maa),num, n,
     1 typent, typgeo)
c      
      return
      end
c

      subroutine efnuml(fid, maa, num, n, 
     1typent, typgeo,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efnuml
c
      implicit none
      save
c
      character *(*) maa
      integer num(*)
      integer*8 fid
      integer  typent, typgeo,cret
      integer edfnuml,n
c
      cret = edfnuml (fid, maa ,len(maa),num, n,
     1typent, typgeo)
c      
      return
      end
c
      subroutine efnema(fid, maa, quoi,  
     1typent, typgeo,typcon,n,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efnema
c
      implicit none
      save
c
      character *(*) maa
      integer*8 fid
      integer  typent, typgeo,cret,typcon,n,quoi
      integer edfnema
c
      n = edfnema(fid, maa ,len(maa),quoi, 
     1typent, typgeo,typcon)

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
      subroutine efpgce(fid,maa,index,ni,conn,typent,typcon,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efpgce
c
      implicit none
      save
c
      character *32 maa
      integer*8 fid
      integer   conn(*),typent,typcon,cret,index(*)
      integer edfpgce,ni
c
      cret = edfpgce(fid,maa,len(maa),index,ni,conn,typent,typcon)
c      
      return
      end
c
c
c
      subroutine efpgcl(fid,maa,index,ni,conn,typent,typcon,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efpgcl
c
      implicit none
      save
c
      character*32 maa
      integer*8 fid
      integer   conn(*),typent,typcon,cret,index(*)
      integer edfpgcl,ni
c
      cret = edfpgcl(fid,maa,len(maa),index,ni,conn,typent,typcon)
c      
      return
      end
c
      subroutine efpygi(fid,maa,typent,typcon,consiz,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efpygi
c
      implicit none
      save
c
      character*32  maa
      integer*8 fid
      integer  typent,typcon,cret
      integer edfpygi,consiz
c
      cret = edfpygi(fid,maa,len(maa),typent,typcon,consiz)
c      
      return
      end
c
c
c
      subroutine efpece(fid,maa,indexp,np,indexf,nf,conn,typcon,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efpece
c
      implicit none
      save
c
      character *32 maa
      integer*8 fid
      integer   conn(*),typcon,cret,indexp(*),indexf(*)
      integer edfpece,np,nf
c
      cret = edfpece(fid,maa,len(maa),indexp,np,indexf,nf,conn,typcon)
c      
      return
      end
c
c
c
      subroutine efpecl(fid,maa,indexp,np,indexf,nf,conn,typcon,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efpecl
c
      implicit none
      save
c
      character*32 maa
      integer*8 fid
      integer   conn(*),typcon,cret,indexp(*),indexf(*)
      integer edfpecl,np,nf
c
      cret = edfpecl(fid,maa,len(maa),indexp,np,indexf,nf,conn,typcon)
c      
      return
      end
c
      subroutine efpyei(fid,maa,typcon,nf,consiz,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efpyei
c
      implicit none
      save
c
      character*32 maa
      integer*8 fid
      integer  typcon,cret
      integer edfpyei,consiz,nf
c
      cret = edfpyei(fid,maa,len(maa),typcon,nf,consiz)
c      
      return
      end
C
C
c
      subroutine eficoe(fid,maa,mdim,indices, 
     &                  n,axe,comp,unit,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: eficoe
c
      implicit none
      save
c
      character*32 maa
      real*8 indices(*)
      integer*8 fid
      integer  mdim,axe
      integer edficoe,n,cret
      character*(*) comp, unit
c
c      
         cret = edficoe (fid,maa,len(maa),mdim,indices,
     &                   n,axe,comp,len(comp),unit,len(unit))
c 
      return
      end
C
c
      subroutine eficol(fid,maa,mdim,indices, 
     &                  n,axe,comp,unit,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: eficol
c
      implicit none
      save
c
      character*32 maa
      real*8 indices(*)
      integer*8 fid
      integer  mdim,axe
      integer edficol,n,cret
      character*16 comp, unit
c
c      
         cret = edficol (fid,maa,len(maa),mdim,indices,
     &                   n,axe,comp,len(comp),unit,len(unit))
c 
      return
      end

c
      subroutine efscoe(fid,maa,mdim,struct,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efscoe
c
      implicit none
      save
c
      character*32  maa
      integer struct(*)
      integer*8 fid
      integer  cret,mdim
      integer edfscoe
c
      cret = edfscoe (fid, maa ,len(maa),mdim,struct)
c      
      return
      end

c
      subroutine efscol(fid,maa,mdim,struct,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efscol
c
      implicit none
      save
c
      character*32  maa
      integer struct(*)
      integer*8 fid
      integer  cret,mdim
      integer edfscol
c
      cret = edfscol (fid, maa ,len(maa),mdim,struct)
c      
      print *,mdim,struct(1),struct(2)
      return
      end
