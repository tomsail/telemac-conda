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
C       * - Nom du fichier : test4.f
C       *
C       * - Description : ecriture des noeuds d'un maillage MED.
C       *
C       *****************************************************************************
	program test4
C       
	implicit none
	include 'med.hf'
C       
C       
	integer*8 fid
	integer cret
	
C       ** la dimension du maillage                         **
	integer      mdim
C       ** nom du maillage de longueur maxi MED_TAILLE_NOM  **
	character*32 maa
C       ** le nombre de noeuds                              **
	integer      nnoe 
C       ** table des coordonnees                            **
C       profil : (dimension * nombre de noeuds) ici 8       **
        real*8       coo(8)
C       ** tables des noms et des unites des coordonnees    **
C           profil : (dimension)                            **
	character*16 nomcoo(2)
	character*16 unicoo(2)
C       ** tables des noms, numeros, numeros de familles des noeuds  **
C       autant d'elements que de noeuds - les noms ont pout longueur **
C       MED_TAILLE_PNOM                                              **
	character*16 nomnoe(4)
	integer     numnoe(4)
	integer     nufano(4)
	
	parameter    ( mdim = 2, maa = "maa1",nnoe = 4 )
	data  coo    /0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0/
	data  nomcoo /"x","y"/, unicoo /"cm","cm"/
	data  nomnoe /"nom1","nom2","nom3","nom4"/
	data  numnoe /1,2,3,4/, nufano /0,1,2,2/

C       ** Creation du fichier test4.med          **
	call efouvr(fid,'test4.med',MED_LECTURE_ECRITURE, cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur creation du fichier'
	   call efexit(-1)
	endif      
	
C       ** Creation du maillage maa de dimension 2 **
C       **  et de type MED_NON_STRUCTURE           **
	call efmaac(fid,maa,mdim,MED_NON_STRUCTURE,
     &                 'un maillage pour test4',cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur creation du maillage'
	   call efexit(-1)
	endif      
	
C       ** Ecriture des coordonnees en mode MED_FULL_INTERLACE : **
C       ** (X1,Y1, X2,Y2, X3,Y3, ...)  dans un repere cartesien **
	call efcooe(fid,maa,mdim,coo,MED_FULL_INTERLACE,
     &	   nnoe,MED_CART,nomcoo,unicoo,cret)
	print *,cret         
	if (cret .ne. 0 ) then
	   print *,'Erreur ecriture des coordonnees des noeuds'
	   call efexit(-1)
	endif      
	
C       ** Ecriture des noms des noeuds (optionnel dans un maillage MED) **
	call efnome(fid,maa,nomnoe,nnoe,MED_NOEUD,0,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur ecriture des noms des noeuds'
	   call efexit(-1)
	endif      
	 
C       ** Ecriture des numeros des noeuds (optionnel dans un maillage MED) **
	call efnume(fid,maa,numnoe,nnoe,MED_NOEUD,0,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur ecriture des numeros des noeuds'
         call efexit(-1)
	endif      
	 

C       ** Ecriture des numeros de familles des noeuds **	      
	call effame(fid,maa,nufano,nnoe,MED_NOEUD,0,cret) 
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur ecriture des numeros de famille'
	   call efexit(-1)
	endif      

C       ** Fermeture du fichier **
	call efferm (fid,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur fermeture du fichier'
	   call efexit(-1)
	endif      
	
	end




