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
C * - Nom du fichier : test32.f
C *
C * - Description : lecture nominale d'une numerotation globale dans un maillage MED
C *
C ******************************************************************************
	program test32
C     
	implicit none
	include 'med.hf'
C
C
        integer*8 fid
        integer cret
        character*64 maa
	character*200 des
        integer nmaa, mdim ,nnoe,type,sdim
	character*16 nomcoo(2)   
	character*16 unicoo(2)
	character(16)  :: dtunit
	integer nstep, stype, atype,chgt,tsf
        integer numglb(100),i


C  ** Ouverture du fichier test31.med **
	call mfiope(fid,'test31.med',MED_ACC_RDONLY, cret)
	print '(I1)',cret
	if (cret .ne. 0 ) then
	   print *,'Erreur ouverture du fichier test31.med'
	   call efexit(-1)
	endif      

C ** lecture des infos pour le premier maillage

	call mmhmii(fid,1,maa,sdim,mdim,type,des,dtunit,
     &              stype,nstep,atype,nomcoo,unicoo,cret)
	print '(I1)',cret
	if (cret .ne. 0 ) then
	   print *,'Erreur acces au premier maillage'
	   call efexit(-1)
	endif      

	nnoe = 0
	call mmhnme(fid,maa,MED_NO_DT,MED_NO_IT,MED_NODE,MED_NONE,
     &              MED_COORDINATE,MED_NO_CMODE,chgt,tsf,nnoe,cret)  
	if (cret .ne. 0 ) then
	   print *,'Erreur acces au nombre de noeud du premier maillage'
	   call efexit(-1)
	endif      


         print '(A,I1,A,A4,A,I1,A,I4)','maillage '
     &        ,0,' de nom ',maa,' et de dimension ',mdim,
     &        ' comportant le nombre de noeud ',nnoe


C ** lecture de la numerotation globale
	 call  mmhgnr(fid,maa,MED_NO_DT,MED_NO_IT,MED_NODE,MED_NONE, 
     &                numglb,cret)

	if (cret .ne. 0 ) then
	   print *,'Erreur lecture numerotation globale '
	   call efexit(-1)
	endif      


C ** Ecriture à l'ecran des numeros globaux

	 do i=1,min(nnoe,100)
	    print '(A,I3,A,I4)',
     &	 'Numero global du noeud ',i,' : ',numglb(i)
	 enddo


C ** Fermeture du fichier                                **
 	call mficlo(fid,cret)
	print '(I1)',cret
	if (cret .ne. 0 ) then
	   print *,'Erreur fermeture du fichier'
	   call efexit(-1)
	endif      
C     
	end
