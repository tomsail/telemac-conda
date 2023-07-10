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
C * - Nom du fichier : test33.f
C *
C * - Description : lecture d'une numerotation globale inexistante dans un maillage MED
C *
C ******************************************************************************
	program test33

C     
	implicit none
	include 'med.hf'
C
C
        integer*8 fid
        integer cret
        character*32 maa 
	character*200 desc
        integer nmaa,mdim,type,narr
        integer numglb(100)




C  ** Ouverture du fichier test31.med **
	call efouvr(fid,'test31.med',MED_LECTURE, cret)
	print '(I1)',cret
	if (cret .ne. 0 ) then
	   print *,'Erreur ouverture du fichier test31.med'
	   call efexit(-1)
	endif      

  
C ** lecture du nombre de maillage                      **
	
	call efnmaa(fid,nmaa,cret)
	print '(I1)',cret
	if (cret .ne. 0 ) then
	   print *,'Erreur lecture du nombre de maillage'
	   call efexit(-1)
	endif      
	print '(A,I2)','Nombre de maillages = ',nmaa

C ** lecture des infos pour le premier maillage


	call efmaai(fid,1,maa,mdim,type,desc,cret)
	print '(I1)',cret
	if (cret .ne. 0 ) then
	   print *,'Erreur acces au premier maillage'
	   call efexit(-1)
	endif      

	call efnema(fid,maa,MED_CONN,MED_ARETE,MED_SEG2,MED_DESC
     1    ,narr,cret)   
	if (cret .ne. 0 ) then
	   print *,'Erreur acces au nombre d''arretes',
     &	           ' du premier maillage'
	   call efexit(-1)
	endif      


         print '(A,I1,A,A4,A,I1,A,I4)','maillage '
     &        ,0,' de nom ',maa,' et de dimension ',mdim,
     &        ' comportant le nombre d''arretes ',narr


C ** lecture de la numerotation globale liée aux arretes 

	 call efgnml(fid,maa,numglb,max(narr,100),
     &     MED_ARETE,MED_TRIA3,cret)

	if (cret .ge. 0 ) then
	   print '(A)','Erreur lecture numerotation globale ARRETE'
	   print '(A)','cette numerotation devait etre inexistante '
	   call efexit(-1)
	endif      


C ** Fermeture du fichier                                **
 	call efferm (fid,cret)
	print '(I1)',cret
	if (cret .ne. 0 ) then
	   print *,'Erreur fermeture du fichier'
	   call efexit(-1)
	endif      
C     
	end
