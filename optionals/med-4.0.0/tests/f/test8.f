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
C * - Nom du fichier : test8.f
C *
C * - Description : exemple d'ecriture des familles d'un maillage MED 
C *
C *****************************************************************************
	program test8
C     
	implicit none
	include 'med.hf'
C
        integer*8 fid
        integer cret
        
        character*64  maa 
        integer       mdim, sdim
        character*64  nomfam
        integer       numfam
        integer       ngro
        character*80  gro
        integer       nfamn
        character*16   str
	character*16 nomcoo(2)
	character*16 unicoo(2)
        
        parameter  ( mdim = 2, nfamn = 2 , sdim = 2)
        data       maa /"maa1"/
	data  nomcoo /"x","y"/, unicoo /"cm","cm"/
        
C     ** Creation du fichier test8.med                       **
	call mfiope(fid,'test8.med',MED_ACC_RDWR, cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur creation du fichier'
	   call efexit(-1)
	endif      

C     ** Creation du maillage maa de dimension 2         **
	call mmhcre(fid,maa,mdim,sdim,MED_UNSTRUCTURED_MESH,
     &              'un maillage pour test8',"",MED_SORT_DTIT,
     &              MED_CARTESIAN,nomcoo,unicoo,cret) 
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur creation du maillage'
	   call efexit(-1)
	endif      
      
C     ** Ecriture des familles                           **
C     * Conventions :
C       - Toujours creer une famille de numero 0 ne comportant aucun attribut
C         ni groupe (famille de reference pour les noeuds ou les elements
C         qui ne sont rattaches a aucun groupe ni attribut)
C       - Les numeros de familles de noeuds sont > 0
C       - Les numeros de familles des elements sont < 0
C       - Rien d'imposer sur les noms de familles
C     **                                                 **

C     * Creation de la famille 0                                     **
	numfam = 0
	nomfam="FAMILLE_0"
	ngro = 0
        call mfacre(fid,maa,nomfam,numfam,ngro,gro,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur creation de la famille 0'
	   call efexit(-1)
	endif      

C  * Creation pour correspondre aux cas tests precedents, 3 familles  *
C  *  d'elements (-1,-2,-3) et deux familles de noeuds (1,2)         *
	do numfam=-1,-3,-1
	   write(str,'(I1.0)') (-numfam)
	   nomfam = "FAMILLE_ELEMENT_"//str
	   gro="groupe1"
	   ngro = 1
	   call mfacre(fid,maa,nomfam,numfam,ngro,gro,cret)
	   print *,cret
	   if (cret .ne. 0 ) then
	      print *,'Erreur creation de famille'
	      call efexit(-1)
	   endif      
	end do
  
	do numfam=1,nfamn
	   write(str,'(I1.0)') numfam
	   nomfam = "FAMILLE_NOEUD_"//str
	   gro="groupe1"
	   ngro = 1
          call mfacre(fid,maa,nomfam,numfam,ngro,gro,cret)
	   print *,cret
	   if (cret .ne. 0 ) then
	      print *,'Erreur creation de famille'
	      call efexit(-1)
	   endif      
	end do
               

C     * Fermeture du fichier *
 	call mficlo(fid,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur fermeture du fichier'
	   call efexit(-1)
	endif      
C
	end 






