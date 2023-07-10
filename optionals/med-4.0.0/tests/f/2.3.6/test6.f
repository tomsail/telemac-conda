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

C       *******************************************************************************
C       * - Nom du fichier : test6.f
C       *
C       * - Description : exemples d'ecriture d'elements dans un maillage MED
C       *
C       ******************************************************************************
	program test6
C       
	implicit none
	include 'med.hf'
C
C	
	integer*8 fid
	integer cret
	
	integer     mdim,nse2,ntr3
	parameter  (nse2 = 5, ntr3 = 2, mdim = 2)
	integer     se2 (2*nse2)
	character*16 nomse2(nse2)
	integer     numse2(nse2),nufase2(nse2)

	integer     tr3 (3*ntr3)
	character*16 nomtr3(ntr3)
	integer     numtr3(ntr3), nufatr3(ntr3) 
	character*32 maa 

	data se2     / 1,2,1,3,2,4,3,4,2,3 /    
	data nomse2  /"se1","se2","se3","se4","se5" / 
	data numse2  / 1,2,3,4,5 /, nufase2 /-1,-1,0,-2,-3/
	data tr3     /1,2,-5,-5,3,-4 /, nomtr3 /"tr1","tr2"/,
     &                                  numtr3 /4,5/
	data nufatr3 /0,-1/,  maa /"maa1"/

C       ** Ouverture du fichier                            **
 	call efouvr(fid,'test6.med',MED_LECTURE_ECRITURE, cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur creation du fichier'
	   call efexit(-1)
	endif      

C       ** Creation du maillage maa de dimension 2         **
	call efmaac(fid,maa,mdim,MED_NON_STRUCTURE,
     &                 'un maillage pour test6',cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur creation du maillage'
	   call efexit(-1)
	endif      

C       ** Ecriture des connectivites des segments         **
	call efcone(fid,maa,mdim,se2,MED_NO_INTERLACE,
     &               nse2,MED_ARETE,
     &               MED_SEG2,MED_DESC,cret ) 
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur ecriture de la connectivite'
	   call efexit(-1)
	endif      

C       ** Ecriture (optionnelle) des noms des segments    **
	call efnome(fid,maa,nomse2,nse2,MED_ARETE,
     &	                      MED_SEG2 ,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur ecriture des noms'
	   call efexit(-1)
	endif      

C       ** Ecriture (optionnelle) des numeros des segments **
	call efnume(fid,maa,numse2,nse2,
     &              MED_ARETE ,MED_SEG2,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur ecriture des numeros'
	   call efexit(-1)
	endif      

C	** Ecriture des numeros des familles des segments  **
	call effame(fid,maa,nufase2,nse2,
     &              MED_ARETE,MED_SEG2,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur ecriture des numéros de famille'
	   call efexit(-1)
	endif      

C       ** Ecriture des connectivites des triangles        **
	call efcone(fid,maa,mdim,tr3,MED_NO_INTERLACE,
     &              ntr3,MED_MAILLE,
     &	            MED_TRIA3,MED_DESC,cret ) 
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur ecriture de la connectivite'
	   call efexit(-1)
	endif      

C       ** Ecriture (optionnelle) des noms des triangles   **
	call efnome(fid,maa,nomtr3,ntr3,MED_MAILLE,
     &	                      MED_TRIA3,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur ecriture des noms'
	   call efexit(-1)
	endif      

C       ** Ecriture (optionnelle) des numeros des triangles **
	call efnume(fid,maa,numtr3,ntr3,MED_MAILLE,
     &                       MED_TRIA3,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur ecriture des numeros'
	   call efexit(-1)
	endif      

C      ** Ecriture des numeros des familles des triangles  **
	call effame(fid,maa,nufatr3,ntr3,MED_MAILLE,
     &                      MED_TRIA3,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur ecriture des numeros de famille'
	   call efexit(-1)
	endif      

C       ** Fermeture du fichier   **
	call efferm (fid,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur a la fermeture du fichier'
	   call efexit(-1)
	endif      
C
	end
