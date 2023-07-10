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

        subroutine efnent(fid,maa,typent,typcon,cret)
        !DEC$ ATTRIBUTES DLLEXPORT :: efnent
c
        implicit none
        save
c
        integer*8 fid
        integer   typent, typcon, cret
        character*(*) maa
        integer edfnent
c
        cret = edfnent(fid,maa,len(maa),typent,typcon)
c
        end
c
c
c
	subroutine efnoel(fid,maa,mdim,coo,modcoo,rep,nomcoo,
     &                  unicoo,nom,inom,num,inum,fam,nnoe,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efnoel
c
	implicit none
	save
c
	integer*8 fid
	integer  mdim,rep,nnoe,cret,modcoo
        integer num(*),fam(*)
 	integer edfnoel
	logical inom,inum
	real*8 coo(*)
	character*(*) maa,nomcoo,unicoo,nom
	integer pnom,pnum
c
	cret = edfnoel(fid,maa,len(maa),mdim,coo,modcoo,
     &                 rep,nomcoo,
     &                 unicoo,nom,pnom,num,pnum,fam,nnoe)
c
	if (pnom .eq. 1) then
	  inom = .TRUE.
	else
	  inom = .FALSE.
	endif
c
	if (pnum .eq. 1) then
	  inum = .TRUE.
	else
	  inum = .FALSE.
	endif
c
	end
c
c
	subroutine efnoee(fid,maa,mdim,coo,modcoo,rep,nomcoo,unicoo,
     &                  nom,inom,num,inum,fam,nnoe,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efnoee
c
	implicit none
	save
c
	integer*8 fid
	integer  mdim,rep,nnoe,cret,modcoo
        integer num(*),fam(*)
 	integer edfnoee
	logical inom,inum
	real*8 coo(*)
	character*(*) maa,nomcoo,unicoo,nom
	integer pnom,pnum
c
	if (inom) then
	  pnom = 1
	else
	  pnom = 0
	endif
c
	if (inum) then
	  pnum = 1
	else
	  pnum = 0
	endif	
	cret = edfnoee(fid,maa,len(maa),mdim,coo,modcoo,
     &               rep,nomcoo,
     &               8*mdim,unicoo,
     &               8*mdim,nom,8*nnoe,
     &               pnom,num,pnum,fam,nnoe)
c
	end
c
c
	subroutine efelee (fid,maa,mdim,conn,switch,nom,inom,
     &                     num,inum,fam,nele,typent,typgeo,
     &                     typcon,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efelee
	implicit none
	save
c
	integer*8 fid
	integer  mdim,nele,cret
	integer conn(*)
        integer fam(*)
        integer num(*)
  	integer typgeo,typent,typcon,mode,edfelee,switch
        character*(*) nom
        character*(*) maa
	logical inom,inum
        integer pnum,pnom
c
	if (inom) then
	  pnom = 1
	else
	  pnom = 0
        endif
c
	if (inum) then
	  pnum = 1
	else
	  pnum = 0
        endif
c
 	cret = edfelee(fid,maa,len(maa),mdim,conn,switch,
     &               nom,8*nele,pnom,
     &               num,pnum,fam,nele,typent,typgeo,
     &               typcon,mode)
c
	end
c
c
	subroutine efelel (fid,maa,mdim,conn,switch,nom,inom,
     &                   num,inum,fam,nele,typent,typgeo,
     &                   typcon,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efelel
	implicit none
	save
c
	integer*8 fid
	integer  mdim,pnum,pnom,nele,cret,switch
  	integer typgeo,typent,typcon,edfelel
	integer conn(*),fam(*),num(*)
        character*(*) nom,maa
	logical inom,inum
c
 	cret = edfelel(fid,maa,len(maa),mdim,conn,switch,
     &               nom,pnom,
     &               num,pnum,fam,nele,typent,typgeo,
     &               typcon)
c
	if (pnom .eq. 1) then
	  inom = .TRUE.
	else
	  inom = .FALSE.
        endif
c
	if (pnum .eq. 1) then
	  inum = .TRUE.
	else
	  inum = .FALSE.
        endif
c
	end
c
c
c
      subroutine efg2fc(fid,maa,nomgro,ind,ngro,ent,nent,
     &                  typent,typgeo,indgeo,ngeo,cret)
c     DEC$ ATTRIBUTES DLLEXPORT :: efg2fc
	implicit none
	save
c     
        integer*8        fid
        integer  cret,ngro,nent,typent,ngeo
        character*32     maa
        character *80    nomgro(*)
        integer          ind(*),indgeo(*),ent(*),typgeo(*)
        integer          edfg2fc
c
        cret = edfg2fc(fid,maa,len(maa),nomgro,80*ngro,ind,ngro,
     &                 ent,nent,typent,typgeo,indgeo,ngeo)
c
        end









