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
C       * - Nom du fichier : test16.f
C       *
C       * - Description : ecriture d'elements d'un maillage MED
C       *                 via les routines de niveau 2
C       *                 - equivalent a test6.f
C       *
C       ******************************************************************************
	program test16         
C       
	implicit none             
	include 'med.hf'
C	
C
	integer*8    fid
	integer      cret, mdim, nse2, ntr3
	character*32 maa
	parameter    (mdim = 2,nse2 = 5,maa = "maa1", ntr3 = 2)
	integer      se2   (2*nse2)
	character*16  nomse2(nse2)
	integer      numse2(nse2),nufase2(nse2)
	integer      tr3   (3*ntr3)
	character*16  nomtr3(ntr3)
	integer      numtr3(ntr3), nufatr3(ntr3) 
	data se2    /1,2,1,3,2,4,3,4,2,3/
	data nomse2 /"se1","se2","se3","se4","se5"/
	data numse2 /1,2,3,4,5/, nufase2 /-1,-1,0,-2,-3/
	data tr3    /1,2,-5,-5,3,-4/
	data nomtr3 /"tr1","tr2"/,numtr3/4,5/,nufatr3/0,-1/
	
C       ** Creation du fichier test16.med **
	call efouvr(fid,'test16.med',MED_LECTURE_ECRITURE, cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur creation du fichier'
	   call efexit(-1)
	endif      
	
C       ** Creation du maillage **
	call efmaac(fid,maa,mdim,MED_NON_STRUCTURE,
     C                  'Un maillage pour test16',cret)
	print *,cret  
	if (cret .ne. 0 ) then
	   print *,'Erreur creation du maillage'
	   call efexit(-1)
	endif      
	
C       ** Ecriture des aretes segments MED_SEG2 :
C       - Connectivite
C       - Noms (optionnel) 
C       - Numeros (optionnel)
C       - Numeros des familles **
	call efelee(fid,maa,mdim,se2,MED_NO_INTERLACE,
     C         nomse2,MED_VRAI,numse2,MED_VRAI,
     C         nufase2,nse2,MED_ARETE,MED_SEG2,MED_DESC,cret)
	print *,cret  
	if (cret .ne. 0 ) then
	   print *,'Erreur des  elements'
	   call efexit(-1)
	endif      

C       ** Ecriture des mailles MED_TRIA3 :
C     - Connectivite
C     - Noms (optionnel) 
C     - Numeros (optionnel)
C     - Numeros des familles **
	call efelee(fid,maa,mdim,tr3,MED_NO_INTERLACE,
     C      nomtr3,MED_VRAI,numtr3,MED_VRAI,
     C      nufatr3,ntr3,MED_MAILLE,MED_TRIA3,MED_DESC,cret)
	print *,cret  
	if (cret .ne. 0 ) then
	   print *,'Erreur ecriture des elements'
	   call efexit(-1)
	endif      
	
C       ** Fermeture du fichier **
	call efferm (fid,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur fermeture du fichier'
	   call efexit(-1)
	endif      
C       
	end 

