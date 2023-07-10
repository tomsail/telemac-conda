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
C       * - Nom du fichier : test23.f
C       *
C       * - Description : ecriture de mailles MED_POLYGONE dans un maillage MED
C       *
C       ******************************************************************************
	program test23
C       
	implicit none
	include 'med.hf'
C	
	integer*8 fid
	integer cret,mdim
	parameter  (mdim = 3)
	character*32 maa 	
        integer ni, n
	parameter (ni=4, n=3)
	integer index(ni)
	character*16 nom(n)
	integer num(n),fam(n)
        integer con(16)
C
	data con  / 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16 /    
	data nom  / "poly1", "poly2", "poly3"/ 
	data num  / 1,2,3 /, fam /0,-1,-2/
	data index /1,6,12,17/
	data maa /"maa1"/

C       ** Creation du fichier test23.med                   **
 	call efouvr(fid,'test23.med',MED_LECTURE_ECRITURE, cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur creation du fichier'
	   call efexit(-1)
	endif      
	print *,'Creation du fichier test23.med'

C       ** Creation du maillage          **
	call efmaac(fid,maa,mdim,MED_NON_STRUCTURE,
     &                 'un maillage pour test23',cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur creation du maillage'
	   call efexit(-1)
	endif      
	print *,'Creation du maillage'

C       ** Ecriture de la connectivite des mailles polygones **
	call efpgce(fid,maa,index,ni,con,MED_MAILLE,MED_NOD,cret) 
	if (cret .ne. 0 ) then
	   print *,'Erreur ecriture des connectivite polygones'
	   call efexit(-1)
	endif      
	print *,cret
	print *,'Ecriture des connectivites des mailles de type
     & MED_POLYGONE'

C       ** Ecriture des noms des mailles polygones          **
	call efnome(fid,maa,nom,n,MED_MAILLE,MED_POLYGONE,
     &		       cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur ecriture des noms polygones'
	   call efexit(-1)
	endif      
	print *,'Ecriture des noms des polygones'

C       ** Ecriture des numeros des mailles polygones **
	call efnume(fid,maa,num,n,MED_MAILLE,MED_POLYGONE,
     &                 cret)
	if (cret .ne. 0 ) then
	   print *,'Erreur ecriture des numeros polygones'
	   call efexit(-1)
	endif      
	print *,cret
	print *,'Ecriture des numeros des polygones'

C	** Ecriture des numeros des familles des segments  **
	call effame(fid,maa,fam,n,
     &              MED_MAILLE,MED_POLYGONE,cret)
	if (cret .ne. 0 ) then
	   print *,'Erreur ecriture des numeros de famille polygones'
	   call efexit(-1)
	endif      
	print *,cret
	print *,'Ecriture des numeros de familles des polygones'

C       ** Fermeture du fichier                            **
	call efferm (fid,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur fermeture du fichier'
	   call efexit(-1)
	endif      
	print *,'Fermeture du fichier'
C     
	end
