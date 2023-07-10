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
C       * - Nom du fichier : test27.f
C       *
C       * - Description : creation de maillages structures (grille cartesienne |
C       *                 grille standard ) dans le fichier test27.med
C       *
C       *****************************************************************************
	program test27
C       
	implicit none
	include 'med.hf'
C       
C       
	integer*8     fid
	integer       cret
C       ** la dimension du maillage                         **
	integer       mdim
C       ** nom du maillage de longueur maxi MED_TAILLE_NOM  **
	character*32  maa
C       ** le nombre de noeuds                              **
	integer       nnoe 
C       ** table des coordonnees                            **
        real*8        coo(8)
	character*16  comp, comp2(2)
	character*16  unit, unit2(2)
	character*200 desc
        integer       strgri(2)
C       ** grille cartesienne                               **
	integer       axe,nind
        real*8        indice(4)
        
C
C	
	data  coo    /0.0,0.0,1.0,0.0,0.0,1.0,1.0,1.0/
	data  comp2  /"x","y"/, unit2 /"cm","cm"/
C
C       Creation du fichier test27.med
	call efouvr(fid,'test27.med',MED_LECTURE_ECRITURE, cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur creation du fichier'
	   call efexit(-1)
	endif      
	print *,'Creation du fichier test27.med'
C	
C       Creation d'un maillage MED_NON_STRUCTURE
	mdim = 3
	maa = 'maillage vide'
	desc = 'un maillage vide'
	call efmaac(fid,maa,mdim,MED_NON_STRUCTURE,desc,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur creation du maillage'
	   call efexit(-1)
	endif      
C
C       Creation d'une grille cartesienne
	mdim = 2
	maa = 'grille cartesienne'
	desc = 'un exemple de grille cartesienne'
	call efmaac(fid,maa,mdim,MED_STRUCTURE,desc,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur creation du maillage'
	   call efexit(-1)
	endif      
	print *,'Creation d un maillage MED_STRUCTURE'
	   
C
C       On specifie la nature du maillage structure
	call efnage(fid,maa,MED_GRILLE_CARTESIENNE,cret)
	print *,cret
	print *,'On definit la nature de la grille :
     & MED_GRILLE_CARTESIENNE'
	if (cret .ne. 0 ) then
	   print *,'Erreur ecriture de la nature de la grille'
	   call efexit(-1)
	endif      
C
C       On definit les indices de la grille selon chaque dimension
	indice(1) = 1.1D0
	indice(2) = 1.2D0
	indice(3) = 1.3D0
	indice(4) = 1.4D0
	nind = 4
	axe = 1
	comp = 'X'
	unit = 'cm'
	call eficoe(fid,maa,mdim,indice,nind,axe,comp,unit,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur ecriture des indices'
	   call efexit(-1)
	endif      
	print *,'Ecriture des indices des coordonnees selon axe X'
C
	indice(1) = 2.1D0
	indice(2) = 2.2D0
	indice(3) = 2.3D0
	indice(4) = 2.4D0
	nind = 4
	axe = 2
	comp = 'Y'
	unit = 'cm'
	call eficoe(fid,maa,mdim,indice,nind,axe,comp,unit,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur ecriture des indices'
	   call efexit(-1)
	endif      
	print *,'Ecriture des indices des coordonnees selon axe Y'
C
C       Creation d'une grille MED_GRILLE_STANDARD de dimension 2
	maa = 'grille standard'
	mdim = 2
	desc = 'un exemple de grille standard'
	call efmaac(fid,maa,mdim,MED_STRUCTURE,desc,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur creation de maillage'
	   call efexit(-1)
	endif      
	print *,'Nouveau maillage MED_STRUCTURE'
C
	call efnage(fid,maa,MED_GRILLE_STANDARD,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur ecriture de la nature de la grille'
	   call efexit(-1)
	endif      
	print *,'On definit la nature du maillage : MED_GRILLE_STANDARD'
C
C       On ecrit les coordonnes de la grille
	nnoe = 4
	call efcooe(fid,maa,mdim,coo,MED_FULL_INTERLACE,nnoe,MED_CART,
     &                 comp2,unit2,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur ecriture des coordonnees des noeuds'
	   call efexit(-1)
	endif      
	print *,'Ecriture des coordonnees de la grille'
C
C       On definit la structure des coordonnees de la grille
	strgri(1) = 2
	strgri(2) = 2
	call efscoe(fid,maa,mdim,strgri,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur ecriture de la structure'
	   call efexit(-1)
	endif      
	print *,'Ecriture de la structure de la grille : / 2,2 /'
C
C       On ferme le fichier
	call efferm (fid,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur fermeture du fichier'
	   call efexit(-1)
	endif      
	print *,'Fermeture du fichier'
C	
	 end






