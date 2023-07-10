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
C * - Nom du fichier : test29.f
C *
C * - Description : ecriture d'un joint dans un maillage MED 
C *
C ******************************************************************************
	program test29
C     
	implicit none
	include 'med.hf'
C
C
        integer*8 fid
        integer cret, domdst
        character*32 maa , jnt, maadst
        character*200 des
        integer mdim ,ncor
        integer cor(6)

        parameter (maa ="maa1",maadst="maa1", domdst=2,
     &     mdim = 3,ncor = 3 )
        data cor /1,2,3,4,5,6/, jnt / "joint"/
        data des / "joint avec le sous-domaine 2" /



C  ** Creation du fichier test29.med **
	call efouvr(fid,'test29.med',MED_LECTURE_ECRITURE, cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur creation du fichier'
	   call efexit(-1)
	endif      

  
C  ** Creation du maillage **
	call efmaac(fid,maa,mdim,MED_NON_STRUCTURE,
     &                 'Un maillage pour test29',cret)
	print *,cret  
	if (cret .ne. 0 ) then
	   print *,'Erreur creation du maillage'
	   call efexit(-1)
	endif      
  
C ** Creation du joint **
	call efjntc(fid,maa,jnt,des,domdst,maadst,cret)
	print *,cret  
	if (cret .ne. 0 ) then
	   print *,'Erreur creation joint'
	   call efexit(-1)
	endif      
        

C ** Ecriture de la correspondance Noeud, Noeud **
	call efjnte(fid,maa,jnt,cor,ncor,
     &    MED_NOEUD,0,MED_NOEUD,0,
     &    cret)
	print *,cret  
	if (cret .ne. 0 ) then
	   print *,'Erreur ecriture correspondance (Noeud,Noeud)'
	   call efexit(-1)
	endif      


C ** Ecriture de la correspondance Noeud, TRIA3 **
	call efjnte(fid,maa,jnt,cor,ncor,
     &    MED_NOEUD,0,MED_MAILLE,MED_TRIA3,
     &    cret)
	print *,cret  
	if (cret .ne. 0 ) then
	   print *,'Erreur ecriture correspondance (Noeud,Tria3)'
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
