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
C * - Nom du fichier : test31.f
C *
C * - Description : ecriture d'une numerotation globale dans un maillage MED
C *
C ******************************************************************************
	program test31
C     
	implicit none
	include 'med.hf'
C
C
        integer*8 fid
        integer cret
        character*32 maa
	character*200 des
        integer nmaa, mdim , nnoe, type, ind
        integer numglb(100),i


C  ** Ouverture du fichier test31.med **
	call efouvr(fid,'test31.med',MED_LECTURE_ECRITURE, cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur ouverture du fichier test31.med'
	   call efexit(-1)
	endif      

  
C ** lecture du nombre de maillage                      **
	
	call efnmaa(fid,nmaa,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur lecture du nombre de maillage'
	   call efexit(-1)
	endif      
	print *,'Nombre de maillages = ',nmaa

C ** lecture des infos pour le premier maillage
	
	ind=1
	call efmaai(fid,ind,maa,mdim,type,des,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur acces au premier maillage'
	   call efexit(-1)
	endif      

	nnoe = 0
	call efnema(fid,maa,MED_COOR,MED_NOEUD,0,0,nnoe,cret)   
	if (cret .ne. 0 ) then
	   print *,'Erreur acces au nombre de noeud du premier maillage'
	   call efexit(-1)
	endif      


         print '(A,I1,A,A4,A,I1,A,I4)','maillage '
     &        ,ind,' de nom ',maa,' et de dimension ',mdim,
     &        ' comportant le nombre de noeud ',nnoe

C ** construction des numeros globaux
	 
	 if (nnoe.gt.100) nnoe=100

	 do i=1,nnoe
	    numglb(i)=i+100
	 enddo

C ** ecriture de la numerotation globale

	 call efgnme(fid,maa,numglb,nnoe,MED_NOEUD,0,cret)

	if (cret .ne. 0 ) then
	   print *,'Erreur ecriture numerotation globale '
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
