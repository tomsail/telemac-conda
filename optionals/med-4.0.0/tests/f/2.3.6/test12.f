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


C ******************************************************************************
C * - Nom du fichier : test12.f
C *
C * - Description : ecriture d'une equivalence dans un maillage MED 
C *
C ******************************************************************************
	program test12
C     
	implicit none
	include 'med.hf'
C
C
        integer*8 fid
        integer cret
        character*32 maa , equ 
        character*200 des
        integer mdim ,ncor
        integer cor(6)

        parameter (maa ="maa1",mdim = 3,ncor = 3 )
        data cor /1,2,3,4,5,6/, equ / "equivalence"/
        data des / "equivalence sur les mailles MED_TRIA3" /

C  ** Creation du fichier test12.med **
	call efouvr(fid,'test12.med',MED_LECTURE_ECRITURE, cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur creation du fichier'
	   call efexit(-1)
	endif      

  
C  ** Creation du maillage **
	call efmaac(fid,maa,mdim,MED_NON_STRUCTURE,
     &                 'Un maillage pour test12',cret)
	print *,cret  
		if (cret .ne. 0 ) then
	   print *,'Erreur creation du maillage'
	   call efexit(-1)
	endif      
  
C ** Creation de l'equivalence **
	call efequc(fid,maa,equ,des,cret)
	print *,cret  
	if (cret .ne. 0 ) then
	   print *,'Erreur creation equivalence'
	   call efexit(-1)
	endif      
        
C ** Ecriture des correspondances sur les mailles MED_TRIA3 **
	call efeque(fid,maa,equ,cor,ncor,
     &          MED_MAILLE,MED_TRIA3,cret)
	print *,cret  
	if (cret .ne. 0 ) then
	   print *,'Erreur ecriture de correspondances'
	   call efexit(-1)
	endif      
        
C ** Fermeture du fichier                                **
 	call efferm (fid,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur fermeture du fichier'
	   call efexit(-1)
	endif      
C     
	end
