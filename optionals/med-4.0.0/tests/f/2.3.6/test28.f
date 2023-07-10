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
	integer       axe
        real*8        indice(4)
	integer tmp
        
C
C       On ouvre le fichier test27.med en lecture seule
	call efouvr(fid,'test27.med',MED_LECTURE, cret)
	if (cret .ne. 0 ) then
	   print *,'Erreur ouverture du fichier'
	   call efexit(-1)
	endif      
	print *,cret

	print *,'Ouverture du fichier test27.med'
C	
C       Combien de maillage ?
	call efnmaa(fid,nmaa,cret)
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
	   call efmaai(fid,i,maa,mdim,typmaa,desc,cret)
	   print *,cret
	   if (cret .ne. 0 ) then
	      print *,'Erreur lecture maillage info'
	      call efexit(-1)
	   endif      
	   print *,'Maillge de nom : ',maa
	   print *,'- Dimension : ',mdim
	   if (typmaa.eq.MED_STRUCTURE) then
	      print *,'- Type : MED_STRUCTURE'
	   else
	      print *,'- Type : MED_NON_STRUCTURE'   
	   endif
C       
C          On repere le type de la grille
	   if (typmaa.eq.MED_STRUCTURE) then
	      call efnagl(fid,maa,type,cret)
	      print *,cret
	      if (cret .ne. 0 ) then
		 print *,'Erreur lecture nature de la grille'
		 call efexit(-1)
	      endif      
	      if (type.eq.MED_GRILLE_CARTESIENNE) then
		 print *,'- Nature de la grille :',
     &                   'MED_GRILLE_CARTESIENNE'
	      endif
	      if (type.eq.MED_GRILLE_STANDARD) then
		 print *,'- Nature de la grille : MED_GRILLE_STANDARD'
	      endif
	   endif
C
C          On regarde la structure et les coordonnees de la grille MED_GRILLE_STANDARD
	   if ((type.eq.MED_GRILLE_STANDARD) 
     &           .and. (typmaa.eq.MED_STRUCTURE)) then
C
	      call efnema(fid,maa,MED_COOR,MED_NOEUD,0,0,nnoe,cret)
	      print *,cret
	      if (cret .ne. 0 ) then
		 print *,'Erreur lecture nombre de noeud'
		 call efexit(-1)
	      endif      
	      print *,'- Nombre de noeuds : ',nnoe
C
	      call efscol(fid,maa,mdim,strgri,cret)
	      print *,cret
	      if (cret .ne. 0 ) then
		 print *,'Erreur lecture structure de la grille'
		 call efexit(-1)
	      endif      
	      print *,'- Structure de la grille : ',strgri
C
	      call efcool(fid,maa,mdim,coo,
     &                        MED_FULL_INTERLACE,MED_ALL,tmp,
     &                        0,rep,comp2,unit2,cret)
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
	   if ((type.eq.MED_GRILLE_CARTESIENNE)
     &          .and. (typmaa.eq.MED_STRUCTURE)) then
C
	      do 30 axe=1,mdim
		 if (axe.eq.1) then
		    quoi = MED_COOR_IND1
		 endif
		 if (axe.eq.2) then
		    quoi = MED_COOR_IND2
		 endif
		 if (axe.eq.3) then
		    quoi = MED_COOR_IND3
		 endif
C                Lecture de la taille de l'indice selon la dimension
C                fournie par le parametre quoi
		 call efnema(fid,maa,quoi,MED_NOEUD,0,0,nind,cret)
		 print *,cret
		 if (cret .ne. 0 ) then
		    print *,'Erreur lecture taille indice'
		    call efexit(-1)
		 endif      
		 print *,'- Axe ',axe
		 print *,'- Nombre d indices : ',nind
C                Lecture des indices des coordonnees de la grille
		 call eficol(fid,maa,mdim,indice,nind,axe,comp,unit,
     &                       cret)
		 print *,cret
		 if (cret .ne. 0 ) then
		    print *,'Erreur lecture indices de coordonnées'
		    call efexit(-1)
		 endif      
		 print *,'- Axe ',comp
		 print *,'  unite : ',unit
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
	call efferm (fid,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur fermeture du fichier'
	   call efexit(-1)
	endif      
	print *,'Fermeture du fichier'
C
	end
	
