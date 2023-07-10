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
C       * - Nom du fichier : test24.f
C       *
C       * - Description : lecture de mailles MED_POLYGONE dans le maillage MED
C       *                 du fichier test23.med   
C       *
C       ******************************************************************************
	program test23
C       
	implicit none
	include 'med.hf'
C	
	integer*8 fid
	integer cret,mdim,nmaa,npoly,i,j,k,taille
	character*32 maa
 	character*200 desc
        integer ni, n
	parameter (ni=4, n=3)
	integer index(ni),ind1,ind2
	character*16 nom(n)
	integer num(n),fam(n)
        integer con(16)
	integer type
C
C       Ouverture du fichier test23.med en lecture seule
 	call efouvr(fid,'test23.med',MED_LECTURE, cret)
        print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur ouverture du fichier'
	   call efexit(-1)
	endif      
        print *,'Ouverture du fichier test23.med'
C
C       Lecture du nombre de maillages
	call efnmaa(fid,nmaa,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur lecture nombre de maillage'
	   call efexit(-1)
	endif      
	print *,'Nombre de maillages : ',nmaa
C   
C       Lecture de toutes les mailles MED_POLYGONE
C       dans chaque maillage
	do 10 i=1,nmaa
C
C          Info sur chaque maillage
	   call efmaai(fid,i,maa,mdim,type,desc,cret)
	   if (cret .ne. 0 ) then
	      print *,'Erreur lecture infos maillage'
	      call efexit(-1)
	   endif      
	   print *,cret
	   print *,'Maillage : ',maa
	   print *,'Dimension : ',mdim
C     
C          Combien de mailles polygones
	   call efnema(fid,maa,MED_CONN,MED_MAILLE,MED_POLYGONE,
     &                       MED_NOD,npoly,cret)
	   print *,cret
	   if (cret .ne. 0 ) then
	      print *,'Erreur lecture du nombre de polygone'
	      call efexit(-1)
	   endif      
	   print *,'Nombre de mailles MED_POLYGONE : ',npoly
C
C          Taille des connectivites
	   call efpygi(fid,maa,MED_MAILLE,MED_NOD,taille,cret)
	   print *,cret
	   if (cret .ne. 0 ) then
	      print *,'Erreur lecture infos polygones'
	      call efexit(-1)
	   endif      
	   print *,'Taille de la connectivite : ',taille
C
C          Lecture de la connectivite
	   call efpgcl(fid,maa,index,npoly+1,con,MED_MAILLE,
     &                       MED_NOD,cret)
	   print *,cret
	   if (cret .ne. 0 ) then
	      print *,'Erreur lecture des connectivites polygones'
	      call efexit(-1)
	   endif      
	   print *,'Lecture de la connectivite des polygones'
C
C          Lecture des noms
	   call efnoml(fid,maa,nom,npoly,MED_MAILLE,MED_POLYGONE,
     &                       cret)
	   print *,cret
	   if (cret .ne. 0 ) then
	      print *,'Erreur lecture des noms des polygones'
	      call efexit(-1)
	   endif      
	   print *,'Lecture des noms'
C
C          Lecture des numeros
	   call efnuml(fid,maa,num,npoly,MED_MAILLE,MED_POLYGONE,
     &                       cret)
	   print *,cret
	   if (cret .ne. 0 ) then
	      print *,'Erreur lecture des numeros des polygones'
	      call efexit(-1)
	   endif      
	   print *,'Lecture des numeros'
C
C          Lecture des numeros de familles
	   call effaml(fid,maa,fam,npoly,MED_MAILLE,MED_POLYGONE,
     &                       cret)
	   print *,cret
	   if (cret .ne. 0 ) then
	      print *,'Erreur lecture des numeros de famille des
     & polygones'
	      call efexit(-1)
	   endif      
	   print *,'Lecture des numeros de famille'
C
C          Affichage des resultats
	   print *,'Affichage des resultats'
	   do 20 j=1,npoly
C       
	      print *,'>> Maille polygone ',j
	      print *,'---- Connectivite      ---- : '
	      ind1 = index(j)
	      ind2 = index(j+1)
	      do 30 k=ind1,ind2-1
		 print *,con(k)
 30	      continue
	      print *,'---- Nom               ---- : ',nom(j)
	      print *,'---- Numero            ----:  ',num(j)
	      print *,'---- Numero de famille ---- : ',fam(j)
C
 20	   continue
C
 10	continue
C
C       Fermeture du fichier
	call efferm (fid,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur fermeture du fichier'
	   call efexit(-1)
	endif      
	print *,'Fermeture du fichier'
C
        end
