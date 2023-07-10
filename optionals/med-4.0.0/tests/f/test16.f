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
	integer      cret, mdim, nse2, ntr3, sdim
	character*64 maa
	parameter    (mdim = 2,nse2 = 5,maa = "maa1", ntr3 = 2, sdim=2)
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
	character*16 nomcoo(2)
	character*16 unicoo(2)
	real*8 dt
	data  nomcoo /"x","y"/, unicoo /"cm","cm"/
        parameter (dt=0.0)
	
C       ** Creation du fichier test16.med **
	call mfiope(fid,'test16.med',MED_ACC_RDWR, cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur creation du fichier'
	   call efexit(-1)
	endif      
	
C       ** Creation du maillage **
	call mmhcre(fid,maa,mdim,sdim,MED_UNSTRUCTURED_MESH,
     &              'Un maillage pour test16',"",
     &              MED_SORT_DTIT,MED_CARTESIAN,nomcoo,unicoo,cret)
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
        call mmhelw(fid,maa,MED_NO_DT,MED_NO_IT,dt,MED_DESCENDING_EDGE,
     &              MED_SEG2,MED_DESCENDING,MED_NO_INTERLACE,nse2,se2,
     &              MED_TRUE,nomse2,MED_TRUE,numse2,MED_TRUE,nufase2,
     &              cret)
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
        call mmhelw(fid,maa,MED_NO_DT,MED_NO_IT,dt,MED_CELL,
     &              MED_TRIA3,MED_DESCENDING,MED_NO_INTERLACE,ntr3,tr3,
     &              MED_TRUE,nomtr3,MED_TRUE,numtr3,MED_TRUE,nufatr3,
     &              cret)
	print *,cret  
	if (cret .ne. 0 ) then
	   print *,'Erreur ecriture des elements'
	   call efexit(-1)
	endif      
	
C       ** Fermeture du fichier **
	call mficlo(fid,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur fermeture du fichier'
	   call efexit(-1)
	endif      
C       
	end 

