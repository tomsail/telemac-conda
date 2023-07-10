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

C       ******************************************************************************
C       * - Nom du fichier : test28.f
C       *
C       * - Description : lecture des maillages structures (grille cartesienne |
C       *                 grille de-structuree ) dans le fichier test27.med
C       *
C       *****************************************************************************
	program test28
C       
	implicit none
	include 'med.hf'
C       
C       
	integer*8     fid
	integer       cret,i,j
C       ** la dimension du maillage                         **
	integer       mdim,nind,nmaa,type,quoi,rep,typmaa
        integer       edim,nstep,stype,atype, chgt, tsf
C       ** nom du maillage de longueur maxi MED_TAILLE_NOM  **
	character*64  maa
C       ** le nombre de noeuds                              **
	integer       nnoe 
C       ** table des coordonnees                            **
        real*8        coo(8)
	character*16  nomcoo(2), unicoo(2)
	character*200 desc
        integer       strgri(2)
C       ** grille cartesienne                               **
	integer       axe
        real*8        indice(4)
	character(16)  :: dtunit
        
C
C       On ouvre le fichier test27.med en lecture seule
	call mfiope(fid,'test27.med',MED_ACC_RDONLY, cret)
	if (cret .ne. 0 ) then
	   print *,'Erreur ouverture du fichier'
	   call efexit(-1)
	endif      
	print *,cret
	print *,'Ouverture du fichier test27.med'
C	
C       Combien de maillage ?
	call mmhnmh(fid,nmaa,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur lecture du nombre de maillage'
	   call efexit(-1)
	endif      
C
C       On boucle sur les maillages et on ne lit que les
C       maillages structures
	do 10 i=1,nmaa
C
C          On repere les maillages qui nous interessent
C
	   call mmhmii(fid,i,maa,edim,mdim,type,desc,
     &	               dtunit,stype,nstep,atype,
     &		       nomcoo,unicoo,cret)
	   print *,cret
	   if (cret .ne. 0 ) then
	      print *,'Erreur lecture maillage info'
	      call efexit(-1)
	   endif      
	   print *,'Maillage de nom : ',maa
	   print *,'- Dimension : ',mdim
	   if (type.eq.MED_STRUCTURED_MESH) then
	      print *,'- Type : structure'
	   else
	      print *,'- Type : non structure'   
	   endif
C       
C          On repere le type de la grille
	   if (type.eq.MED_STRUCTURED_MESH) then
	      call mmhgtr(fid,maa,typmaa,cret)
	      print *,cret
	      if (cret .ne. 0 ) then
		 print *,'Erreur lecture nature de la grille'
		 call efexit(-1)
	      endif      
	      if (typmaa.eq.MED_CARTESIAN_GRID) then
		 print *,'- Nature de la grille : cartesienne'
	      endif
	      if (typmaa.eq.MED_CURVILINEAR_GRID) then
		 print *,'- Nature de la grille : curviligne'
	      endif
	   endif
C
C          On regarde la structure et les coordonnees de la grille 
C          MED_CURVILINEAR_GRID
	   if ((typmaa.eq.MED_CURVILINEAR_GRID) 
     &           .and. (type.eq.MED_STRUCTURED_MESH)) then
C
	      call mmhnme(fid,maa,MED_NO_DT,MED_NO_IT,MED_NODE,
     &                    MED_NONE,MED_COORDINATE,MED_NO_CMODE,
     &                    chgt,tsf,nnoe,cret)
	      print *,cret
	      if (cret .ne. 0 ) then
		 print *,'Erreur lecture nombre de noeud'
		 call efexit(-1)
	      endif      
	      print *,'- Nombre de noeuds : ',nnoe
C
	      call mmhgsr(fid,maa,MED_NO_DT,MED_NO_IT,strgri,cret)
	      
	      print *,cret
	      if (cret .ne. 0 ) then
		 print *,'Erreur lecture structure de la grille'
		 call efexit(-1)
	      endif      
	      print *,'- Structure de la grille : ',strgri
C
	      call mmhcor(fid,maa,MED_NO_DT,MED_NO_IT,
     &                    MED_FULL_INTERLACE,coo,cret) 
	      print *,cret
	      if (cret .ne. 0 ) then
		 print *,'Erreur lecture des coordonnees des noeuds'
		 call efexit(-1)
	      endif      
	      print *,'- Coordonnees :'
	      do 20 j=1,nnoe*mdim
		 print *,coo(j)
 20	      continue
	   endif
C
	   if ((typmaa.eq.MED_CARTESIAN_GRID)
     &          .and. (type.eq. MED_STRUCTURED_MESH)) then
C
	      do 30 axe=1,mdim
		 if (axe.eq.1) then
		    quoi = MED_COORDINATE_AXIS1
		 endif
		 if (axe.eq.2) then
		    quoi = MED_COORDINATE_AXIS2
		 endif
		 if (axe.eq.3) then
		    quoi = MED_COORDINATE_AXIS3
		 endif
C                Lecture de la taille de l'indice selon la dimension
C                fournie par le parametre quoi
		 call mmhnme(fid,maa,MED_NO_DT,MED_NO_IT,MED_NODE,
     &	                     MED_NONE,quoi,MED_NO_CMODE,
     &	                     chgt,tsf,nind,cret)
		 print *,cret
		 if (cret .ne. 0 ) then
		    print *,'Erreur lecture taille indice'
		    call efexit(-1)
		 endif      
		 print *,'- Axe ',axe
		 print *,'- Nombre d indices : ',nind
C                Lecture des indices des coordonnees de la grille
		 call mmhgcr(fid,maa,MED_NO_DT,MED_NO_IT,
     &                       axe,indice,cret)
		 print *,cret
		 if (cret .ne. 0 ) then
		    print *,'Erreur lecture indices de coordonnées'
		    call efexit(-1)
		 endif      
		 print *,'- Axe ', nomcoo
		 print *,'  unite : ',unicoo
		 do 40 j=1,nind
		    print *,indice(j)
 40		 continue
 30	      continue
C
	   endif
C
 10	continue
C
C       On ferme le fichier
	call mficlo(fid,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur fermeture du fichier'
	   call efexit(-1)
	endif      
	print *,'Fermeture du fichier'
C
	end
	
