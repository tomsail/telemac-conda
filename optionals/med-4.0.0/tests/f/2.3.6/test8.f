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
        
        character*32  maa 
        integer       mdim
        character*32  nomfam
        integer       numfam
        character*200 attdes
        integer       natt, attide, attval
        integer       ngro
        character*80  gro
        integer       nfamn
        character*16   str
        
        parameter  ( mdim = 2, nfamn = 2 )
        data       maa /"maa1"/
        
C     ** Creation du fichier test8.med                       **
 	call efouvr(fid,'test8.med',MED_LECTURE_ECRITURE, cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur creation du fichier'
	   call efexit(-1)
	endif      
        
C     ** Creation du maillage maa de dimension 2         **
	call efmaac(fid,maa,mdim,MED_NON_STRUCTURE,
     &                 'un maillage pour test8',cret)
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
	call effamc(fid,maa,nomfam,numfam,attide,attval,attdes,
     &          0,gro,0,cret)  
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
	   attide = 1
	   attval = numfam*100
	   natt = 1
	   attdes="description attribut"
	   gro="groupe1"
	   ngro = 1
	   print *, nomfam," - ",numfam," - ",attide," - ",
     &                attval," - ",ngro

	   call effamc(fid,maa,nomfam,numfam,attide,attval,attdes,
     &                natt,gro,ngro,cret)  
	   print *,cret
	   if (cret .ne. 0 ) then
	      print *,'Erreur creation de famille'
	      call efexit(-1)
	   endif      
	end do
  
	do numfam=1,nfamn
	   write(str,'(I1.0)') numfam
	   nomfam = "FAMILLE_NOEUD_"//str
	   attide = 1
	   attval = numfam*100
	   natt = 1
	   attdes="description attribut"
	   gro="groupe1"
	   ngro = 1
	   print *, nomfam," - ",numfam," - ",attide," - ",
     &                attval," - ",ngro
	   call effamc(fid,maa,nomfam,numfam,attide,attval,attdes,
     &                natt,gro,ngro,cret)  
	   print *,cret
	   if (cret .ne. 0 ) then
	      print *,'Erreur creation de famille'
	      call efexit(-1)
	   endif      
	end do
               

C     * Fermeture du fichier *
 	call efferm (fid,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur fermeture du fichier'
	   call efexit(-1)
	endif      
C
	end 






