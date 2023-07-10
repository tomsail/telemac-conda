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
        character*64 maa
	character*200 des
        integer nmaa, mdim , nnoe, type, ind,sdim
        integer numglb(100),i
	character*16 nomcoo(2)   
	character*16 unicoo(2)
	character(16)  :: dtunit
        real*8   coo(8)
	integer nstep, stype, atype,chgt,tsf
        real*8 dt
	parameter    (mdim = 2, maa = "maa1",sdim=2)
        parameter    (dt = 0.0)
	data  coo    /0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0/
	data  nomcoo /"x","y"/, unicoo /"cm","cm"/


C       ** Ouverture du fichier test4.med **
	call mfiope(fid,'test31.med',MED_ACC_RDWR, cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur ouverture du fichier test31.med'
	   call efexit(-1)
	endif      
	
C       ** Creation du maillage maa de dimension 2 **
C       **  et de type non structure               **
	nnoe=4
	call mmhcre(fid,maa,mdim,sdim,
     &              MED_UNSTRUCTURED_MESH,
     &              'un premier maillage pour test4', 
     &              "",MED_SORT_DTIT,MED_CARTESIAN,nomcoo,unicoo,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur creation du maillage'
	   call efexit(-1)
	endif      
	
C       ** Ecriture des coordonnees en mode MED_FULL_INTERLACE : **
C       ** (X1,Y1, X2,Y2, X3,Y3, ...)  dans un repere cartesien **
	call mmhcow(fid,maa,MED_NO_DT,MED_NO_IT,dt,
     &	   MED_FULL_INTERLACE,nnoe,coo,cret)
	print *,cret         
	if (cret .ne. 0 ) then
	   print *,'Erreur ecriture des coordonnees des noeuds'
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
	 call  mmhgnw(fid,maa,MED_NO_DT,MED_NO_IT,MED_NODE,MED_NONE, 
     &                nnoe,numglb,cret)

	if (cret .ne. 0 ) then
	   print *,'Erreur ecriture numerotation globale '
	   call efexit(-1)
	endif      
C ** Fermeture du fichier                                **
 	call mficlo(fid,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur fermeture du fichier'
	   call efexit(-1)
	endif      
C     
	end
