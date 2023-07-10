!*  This file is part of MED.
!*
!*  COPYRIGHT (C) 1999 - 2016  EDF R&D, CEA/DEN
!*  MED is free software: you can redistribute it and/or modify
!*  it under the terms of the GNU Lesser General Public License as published by
!*  the Free Software Foundation, either version 3 of the License, or
!*  (at your option) any later version.
!*
!*  MED is distributed in the hope that it will be useful,
!*  but WITHOUT ANY WARRANTY; without even the implied warranty of
!*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!*  GNU Lesser General Public License for more details.
!*
!*  You should have received a copy of the GNU Lesser General Public License
!*  along with MED.  If not, see <http://www.gnu.org/licenses/>.
!*

! ******************************************************************************
! * - Nom du fichier : test10.F90
! *
! * - Description : ecriture de champs de resultats MED 
! *
! ******************************************************************************

#define STR(chaine) #chaine
#define XSTR(chaine) STR(chaine)


#ifdef DEF_LECT_ECR
#define MODE_ACCES MED_ACC_RDWR
#elif DEF_LECT_AJOUT
#define MODE_ACCES MED_ACC_RDEXT
#else
#define MODE_ACCES MED_ACC_CREAT
#endif

#ifndef USER_INTERLACE
#define USER_INTERLACE MED_FULL_INTERLACE
#endif

#define USER_MODE MED_COMPACT_STMODE

#ifdef DEF_MED_FLOAT32
#define FTYPECHA MED_FLOAT32
#define ftypecha real*4
#define filename1 test10_f32
#else
#define FTYPECHA MED_DOUBLE
#define ftypecha real*8
#define filename1 test10
#endif


#ifdef  DEF_MED_INT32
#define ITYPECHA MED_INT32
#define itypecha integer*4
!Le précompilateur Fortran ne gère pas la concaténation
#define filename2 filename1 ## _i32
#elif   DEF_MED_INT64
#define ITYPECHA MED_INT64
#define itypecha integer*8
!Le précompilateur Fortran ne gère pas la concaténation
#define filename2 filename1 ## _i64
#else
#define ITYPECHA MED_INT
#define itypecha integer
!Le précompilateur Fortran ne gère pas la concaténation
!Le nom de fichier est donc défini par le Makefile
#define filename2 filename1
#endif

#define filename XSTR(filename2.med)
 

	program test10
!     
	implicit none
	include 'med.hf'
!
	integer*8    fid
	integer      ret
	real*8       a,b,p1,p2,dt

	character*64 maa1,maa2,maa3
	character*13 lien_maa2
	character*16 nomcoo(3)
	character*16 unicoo(3)
!       CHAMP N°1
	character*64 nomcha1
	character*16 comp1(2), unit1(2)
	character*16 dtunit1, nounit
	integer      ncomp1
!	MODEL N°1 DE LOC. DES PTS DE GAUSS PR CHAMP1
	integer      ngauss1_1
	character*64 gauss1_1
	real*8       refcoo1(12), gscoo1_1(12), wg1_1(6)
	integer      nval1_1, nent1_1
	ftypecha     valr1_1(1*6*2)
!	MODEL N°2 DE LOC. DES PTS DE GAUSS PR CHAMP1
	integer      ngauss1_2
	character*64 gauss1_2
	real*8       gscoo1_2(6), wg1_2(3)
	integer      nval1_2, nent1_2
	ftypecha     valr1_2(2*3*2)
	ftypecha     valr1_2p(2*3)
!	MODEL N°3 DE LOC. DES PTS DE GAUSS PR CHAMP1
	integer      ngauss1_3,nval1_3, nent1_3
	ftypecha     valr1_3(2*3*2)
	ftypecha     valr1_3p(2*2)

!	CHAMP N°2
	character*64 nomcha2
	character*16 comp2(3), unit2(3)
	integer      ncomp2, nval2
	itypecha     valr2(5*3),   valr2p(3*3)

!	CHAMP N°3
	character*64 nomcha3
	character*16 comp3(2), unit3(2)
	integer      ncomp3, nval3, nent3
	itypecha     valr3(5*4*2),   valr3p(3*4*2)

!	PROFILS UTILISES
	character*64 nomprofil1
        integer      profil1(2) , profil2(3)

        parameter ( a=0.446948490915965D0, b=0.091576213509771D0    )
        parameter ( p1=0.11169079483905D0, p2=0.0549758718227661D0  )
!       MAILLAGES
	parameter ( maa1 = "maa1", maa2 = "maa2", maa3 = "maa3" )
	parameter ( lien_maa2= "./testfoo.med"                  )
!       CHAMP N°1
	parameter ( nomcha1 = "champ reel" )
	parameter ( ncomp1 = 2 )
	parameter ( dtunit1 = "                ")
	parameter ( nounit  = "                ")
!       MODEL N°1 DE LOC. DES PTS DE GAUSS PR CHAMP1
	parameter ( gauss1_1 = "Model n1" )
	parameter ( ngauss1_1 = 6 )
!       MODEL N°2 DE LOC. DES PTS DE GAUSS PR CHAMP1
	parameter ( gauss1_2  = "Model n2" )
	parameter ( ngauss1_2 = 3 )
!       MODEL N°3 DE LOC. DES PTS DE GAUSS PR CHAMP1
	parameter ( ngauss1_3 = 6 )
	parameter ( nval1_3 = 6 )
!       CHAMP N°2
	parameter ( nomcha2="champ entier")
        parameter ( ncomp2 = 3, nval2= 5  )
!       CHAMP N°3
	parameter ( nomcha3="champ entier 3")
        parameter ( ncomp3 = 2, nval3= 5*4  )
!       PROFILS
	parameter ( nomprofil1  = "PROFIL(champ(1))" )
	

!       CHAMP N°1
	data comp1 /"comp1", "comp2"/
        data unit1 /"unit1","unit2"/
!       MODEL N°1 DE LOC. DES PTS DE GAUSS PR CHAMP1
	data nval1_1  / 1*6 /
	data nent1_1  / 1 /
	data refcoo1  / -1.0,1.0, -1.0,-1.0, 1.0,-1.0, -1.0,0.0, &
     &	                0.0,-1.0, 0.0,0.0 / 
	data valr1_1  /  0.0,1.0, 2.0,3.0, 10.0,11.0, 12.0,13.0, &
     &                   20.0,21.0, 22.0,23.0/
!       MODEL N°2 DE LOC. DES PTS DE GAUSS PR CHAMP1
        data nent1_2  / 2 /
	data valr1_2  / 0.0,1.0, 2.0,3.0, 10.0,11.0, &
     &                  12.0,13.0, 20.0,21.0, 22.0,23.0 /
	data valr1_2p / 12.0,13.0, 20.0,21.0, 22.0,23.0 /
!       MODEL N°3 DE LOC. DES PTS DE GAUSS PR CHAMP1
        data nent1_3  / 6 /
	data valr1_3  / 0.0,1.0, 2.0,3.0, 10.0,11.0, 12.0,13.0, &
     &                  20.0,21.0, 22.0,23.0 /
	data valr1_3p / 2.0,3.0, 10.0,11.0   /
!       CHAMP N°2
	data comp2 /"comp1", "comp2", "comp3"/
        data unit2 /"unit1","unit2", "unit3"/
	data valr2 / 0,1,2, 10,11,12, 20,21,22, 30,31,32, 40,41,42 /
	data valr2p / 0,1,2,           20,21,22,           40,41,42 /
!       CHAMP N°3
        data nent3 / 5 /
	data comp3 /"comp1", "comp2"/
        data unit3 /"unit1","unit2"/
	data valr3 / 0,1, 10,11, 20,21, 30,31, &
     &           40,41, 50,51, 60,61, 70,71, &
     &           80,81, 90,91, 100,101, 110,111, &
     &           120,121, 130,131, 140,141, 150,151, &
     &           160,161, 170,171, 180,181, 190,191 /
	data valr3p / 0,1, 10,11, 20,21, 30,31, &
     &            80,81, 90,91, 100,101, 110,111, &
     &            160,161, 170,171, 180,181, 190,191 /


!       PROFILS
	data profil1 /2,3/
	data profil2 /1,3,5/

	data  nomcoo /"x","y","z"/, unicoo /"cm","cm","cm"/
        
	ret = 0

	gscoo1_1(1) =  2*b-1
	gscoo1_1(2) =  1-4*b
	gscoo1_1(3) =  2*b-1
	gscoo1_1(4) =  2*b-1
	gscoo1_1(5) =  1-4*b
	gscoo1_1(6) =  2*b-1
	gscoo1_1(7) =  1-4*a
	gscoo1_1(8) =  2*a-1
	gscoo1_1(9) =  2*a-1
	gscoo1_1(10) =  1-4*a
	gscoo1_1(11) =  2*a-1
	gscoo1_1(12) =  2*a-1

	wg1_1(1) =  4*p2
	wg1_1(2) =  4*p2
	wg1_1(3) =  4*p2
	wg1_1(4) =  4*p1
	wg1_1(5) =  4*p1
	wg1_1(6) =  4*p1

	nval1_2 = 2*3
	gscoo1_2(1) = -2.0D0/3
	gscoo1_2(2) =  1.0D0/3 
	gscoo1_2(3) = -2.0D0/3
	gscoo1_2(4) = -2.0D0/3
	gscoo1_2(5) =  1.0D0/3
	gscoo1_2(6) = -2.0D0/3

	wg1_2(1) =  2.0D0/3
	wg1_2(2) =  2.0D0/3
	wg1_2(3) =  2.0D0/3 
		   
!     ** ouverture du fichier                            **
 	call mfivop(fid, TEST10_FILENAME , MED_ACC_RDWR, &
     &      MED_MAJOR_NUM, MED_MINOR_NUM, MED_RELEASE_NUM, ret)
	print *,ret
        if (ret .ne. 0 ) then
	   print *,'Erreur à l''ouverture du fichier  : ','test10.med'
	   call efexit(-1)
	endif
        
!     ** creation du maillage maa1 de dimension 3         **
	call mmhcre(fid,maa1,3,3, &
     &     MED_UNSTRUCTURED_MESH,'Maillage vide', &
     &     "",MED_SORT_DTIT,MED_CARTESIAN,nomcoo,unicoo,ret)
	print *,ret
        if (ret .ne. 0 ) then
	   print *,'Erreur à la création du maillage : ', maa1
	   call efexit(-1)
	endif
 	     
!     ** creation du maillage maa3 de dimension 3         **
	call mmhcre(fid,maa3,3,3, &
     &     MED_UNSTRUCTURED_MESH,'Maillage vide', &
     &     "",MED_SORT_DTIT,MED_CARTESIAN,nomcoo,unicoo,ret)
	print *,ret
        if (ret .ne. 0 ) then
	   print *,'Erreur à la création du maillage : ', maa3
	   call efexit(-1)
	endif
 	     

!     ** creation du champ réel n°1                        **
	call mfdcre(fid,nomcha1,FTYPECHA,ncomp1,comp1,unit1, &
     &              dtunit1,maa1,ret)
	print *,ret
        if (ret .ne. 0 ) then
	   print *,'Erreur à la création du champ : ', nomcha1
	   call efexit(-1)
	endif
 	     
!     ** creation du champ entier n°2                      **
	call mfdcre(fid,nomcha2,ITYPECHA,ncomp2,comp2,unit2, &
     &              dtunit1,maa1,ret)
	print *,ret
        if (ret .ne. 0 ) then
	   print *,'Erreur à la création du champ : ', nomcha2
	   call efexit(-1)
	endif
 
!     ** creation du lien au fichier distant contenant maa2 **
	call mlnliw(fid,maa2,lien_maa2,ret)
	print *,ret
        if (ret .ne. 0 ) then
	   print *,'Erreur à la création du lien : ', lien_maa2
	   call efexit(-1)
	endif
	
 
!     ** creation de la localisation des points de Gauss modèle n°1 **
	call mlclow(fid,gauss1_1,MED_TRIA6,2,refcoo1,USER_INTERLACE, &
     &              ngauss1_1,gscoo1_1, wg1_1,MED_NO_INTERPOLATION, &
     &              MED_NO_MESH_SUPPORT, ret)
	print *,ret
        if (ret .ne. 0 ) then
	   print *,'Erreur à la création du modèle n°1 : ', gauss1_1
	   call efexit(-1)
	endif

!     ** creation de la localisation des points de Gauss modèle n°2 **
	call mlclow(fid,gauss1_2,MED_TRIA6,2,refcoo1,USER_INTERLACE, &
     &              ngauss1_2,gscoo1_2, wg1_2,MED_NO_INTERPOLATION, &
     &              MED_NO_MESH_SUPPORT, ret)
	print *,ret
        if (ret .ne. 0 ) then
	   print *,'Erreur à la création du modèle n°2 : ', gauss1_2
	   call efexit(-1)
	endif

	
!     ** Ecriture du champ n°1
!     ** - enregistre uniquement la composante n°2 de valr1_1
!     ** - pas de pas de temps, ni de numero d'ordre
	dt = 0.0
	call mfdrpw(fid,nomcha1,MED_NO_DT,MED_NO_IT,dt,MED_CELL, &
     &              MED_TRIA6,USER_MODE,MED_ALLENTITIES_PROFILE, &
     &              gauss1_1,USER_INTERLACE,2,nent1_1,valr1_1,ret)
	print *,ret
	if (ret .ne. 0 ) then
	   print *,'Erreur à l''écriture du champ : ', nomcha1,'et.1'
	   call efexit(-1)
	endif

!     ** Nouvelle Ecriture du champ reel en mode remplacement
!     ** - complete le champ precedent en enregistrant les composantes 1
!     ** - pas de pas de temps, ni de numero d'ordre 
	call mfdrpw(fid,nomcha1,MED_NO_DT,MED_NO_IT,dt,MED_CELL, &
     &              MED_TRIA6,USER_MODE,MED_ALLENTITIES_PROFILE, &
     &              gauss1_1,USER_INTERLACE,1,nent1_1,valr1_1,ret)
	print *,ret
	if (ret .ne. 0 ) then
	   print *,'Erreur à l''écriture du champ : ', nomcha1,'et.2'
	   call efexit(-1)
	endif
	 
!     ** Ecriture sur le champ reel
!     ** - De la 1ere composante du tableau valr1_2
!     ** - Avec un pas de temps égal a 5.5
!     ** - Pas de numero d'ordre
!     ** - maa2 est distant
	dt = 5.5
	call mfdrpw(fid,nomcha1,1,MED_NO_IT,dt,MED_CELL,MED_TRIA6, &
     &              USER_MODE,MED_ALLENTITIES_PROFILE,gauss1_2, &
     &              USER_INTERLACE,1,nent1_2,valr1_2,ret)
	print *,ret
	if (ret .ne. 0 ) then
	   print *,'Erreur à l''écriture du champ : ', nomcha1,'et.3'
	   call efexit(-1)
	endif

!     ** Ecriture sur le champ reel
!     ** - De la 2ere composante du tableau valr1_2
!     ** - Avec un pas de temps égal a 5.5
!     ** - Pas de numero d'ordre
!     ** - maa1 est local
	call mfdrpw(fid,nomcha1,1,MED_NO_IT,dt,MED_CELL,MED_TRIA6, &
     &              USER_MODE,MED_ALLENTITIES_PROFILE,gauss1_2, &
     &              USER_INTERLACE,2,nent1_2,valr1_2,ret)
	print *,ret
	if (ret .ne. 0 ) then
	   print *,'Erreur à l''écriture du champ : ', nomcha1,'et.4'
	   call efexit(-1)
	endif

      
!     ** Ecriture sur le champ reel
!     ** - De la 1ere composante du tableau valr1_1
!     ** - Avec un pas de temps égal a 5.5
!     ** - Numero d'ordre egal a 2
	call mfdrpw(fid,nomcha1,1,2,dt,MED_CELL,MED_TRIA6, &
     &              USER_MODE,MED_ALLENTITIES_PROFILE,gauss1_1, &
     &              USER_INTERLACE,1,nent1_1,valr1_1,ret)
	print *,ret
	if (ret .ne. 0 ) then
	   print *,'Erreur à l''écriture du champ : ', nomcha1,'et.5'
	   call efexit(-1)
	endif
    
!     ** Creation de profil
!     ** - qui selectionne uniquement le 2e element du tableau valr1
	call mpfprw(fid,nomprofil1,1,profil1,ret)
	print *,ret
	if (ret .ne. 0 ) then
	   print *,'Erreur à la création du profil : ', nomprofil1
	   call efexit(-1)
	endif


!     ** Ecriture du champ reel 
!     ** - Toutes les composantes du 2e element de valr1_1 (MED_ALL)
!     ** - Extrait a partir du profil de nom "profil1(1)"
!     ** - Pas de temps = 5.6
!     ** - Numero d'ordre = 2 
	dt = 5.6
	call mfdrpw(fid,nomcha1,2,2,dt,MED_CELL,MED_TRIA6, &
     &              USER_MODE, nomprofil1, MED_NO_LOCALIZATION, &
     &              USER_INTERLACE,MED_ALL_CONSTITUENT, &
     &              nval1_3,valr1_3p,ret)
	print *,ret
	if (ret .ne. 0 ) then
	   print *,'Erreur à l''écriture du champ : ', nomcha1,'et.6'
	   call efexit(-1)
	endif

!     ** Ecriture du champ reel 
!     ** - Toutes les composantes du 2e element de valr1_2p (MED_ALL)
!     ** - Extrait a partir du profil de nom "profil1(1)"
!     ** - Pas de temps = 5.6
!     ** - Numero d'ordre = 2 
	call mfdrpw(fid,nomcha1,2,2,dt,MED_CELL,MED_TRIA6, &
     &              USER_MODE, nomprofil1, gauss1_2, &
     &              USER_INTERLACE,MED_ALL_CONSTITUENT, &
     &              nent1_2,valr1_2p,ret)
	print *,ret
	if (ret .ne. 0 ) then
	   print *,'Erreur à l''écriture du champ : ', nomcha1,'et.7'
	   call efexit(-1)
	endif


!     ** Ecriture du champ reel 
!     ** - 2e composante du 2e element du champ
!     ** - Extrait a partir du profil de nom "profil1(1)"
!     ** - Pas de temps = 5.7
!     ** - Numero d'ordre = 2 
	dt = 5.7
	call mfdrpw(fid,nomcha1,3,2,dt,MED_CELL,MED_TRIA6, &
     &              USER_MODE, nomprofil1, MED_NO_LOCALIZATION, &
     &              USER_INTERLACE,2, &
     &              nent1_3,valr1_3p,ret)
	print *,ret
	if (ret .ne. 0 ) then
	   print *,'Erreur à l''écriture du champ : ', nomcha1,'et.8a'
	   call efexit(-1)
	endif

!     ** Ecriture du champ reel 
!     ** - 1e composante du 2e element du champ
!     ** - Extrait a partir du profil de nom "profil1(1)"
!     ** - Pas de temps = 5.7
!     ** - Numero d'ordre = 2 
	dt = 5.7
	call mfdrpw(fid,nomcha1,3,2,dt,MED_CELL,MED_TRIA6, &
     &              USER_MODE, nomprofil1, MED_NO_LOCALIZATION, &
     &              USER_INTERLACE,1, &
     &              nent1_3,valr1_3p,ret)
	print *,ret
	if (ret .ne. 0 ) then
	   print *,'Erreur à l''écriture du champ : ', nomcha1,'et.8b'
	   call efexit(-1)
	endif


!     ** Ecriture du champ entier n°2
!     ** - 1ere composante des éléments de valr2
!     ** - pas de pas de temps, ni de numero d'ordre
	dt = 0.0
	call   mfdivw(fid,nomcha2,MED_NO_DT,MED_NO_IT,dt, &
     &               MED_DESCENDING_EDGE,MED_SEG2,USER_INTERLACE, &
     &               1,nval2,valr2,ret)
        print *,ret
	if (ret .ne. 0 ) then
	   print *,'Erreur à l''écriture du champ : ', nomcha2,'et.1'
	   call efexit(-1)
	endif   

!     ** Ecriture du champ entier n°2
!     ** - 2ere composante des éléments de valr2
!     ** - pas de pas de temps, ni de numero d'ordre
!     ** - pour des raisons de complétude des tests on change 
!     **   le type d'élément (aucun sens phys.))
	call mfdivw(fid,nomcha2,MED_NO_DT,MED_NO_IT,dt, &
     &              MED_NODE,MED_NONE,USER_INTERLACE, &
     &              2,nval2,valr2,ret)
	print *,ret
	if (ret .ne. 0 ) then
	   print *,'Erreur à l''écriture du champ : ', nomcha2,'et.2'
	   call efexit(-1)
	endif   


!     ** Ecriture du champ entier n°2
!     ** - 3ere composante des éléments de valr2
!     ** - pas de pas de temps, ni de numero d'ordre
!     ** - pour des raisons de complétude des tests on change 
!     **   le type d'élément (aucun sens phys.))
	call mfdivw(fid,nomcha2,MED_NO_DT,MED_NO_IT,dt, &
     &              MED_DESCENDING_FACE,MED_TRIA6,USER_INTERLACE, &
     &              3,nval2,valr2,ret)
	print *,ret
	if (ret .ne. 0 ) then
	   print *,'Erreur à l''écriture du champ : ', nomcha2,'et.3'
	   call efexit(-1)
	endif   

!     ** Creation de profil
!     ** - selectionne les elements 1,3,5 du tableau valr2
       	call mpfprw(fid,"PROFIL(champ2)",3,profil2,ret)
	print *,ret
	if (ret .ne. 0 ) then
	   print *,'Erreur à l''écriture du profil : ', &
     &              'profil2(champ2)'
	   call efexit(-1)
	endif   


!     ** Ecriture du champ entier n°2
!     ** - 3eme composante des éléments de valr2
!     ** - pas de pas de temps, ni de numero d'ordre
!     ** - profils 
!     ** - pour des raisons de complétude des tests on change 
!     **   le type d'élément (aucun sens phys.))
        call  mfdipw(fid,nomcha2,MED_NO_DT,MED_NO_IT,dt, &
     &               MED_CELL,MED_TRIA6,USER_MODE,"PROFIL(champ2)", &
     &               MED_NO_LOCALIZATION,USER_INTERLACE,3, &
     &               nval2,valr2p,ret)
	print *,ret
	if (ret .ne. 0 ) then
	   print *,'Erreur à l''écriture du profil : ', &
     &             'profil2(champ2)'
	   call efexit(-1)
	endif   

!     ** creation du champ entier n°3                      **
	call mfdcre(fid,nomcha3,ITYPECHA,ncomp3,comp3,unit3, &
     &              dtunit1,maa1,ret)
	print *,ret
        if (ret .ne. 0 ) then
	   print *,'Erreur à la création du champ : ', nomcha3
	   call efexit(-1)
	endif
 
!     ** Ecriture du champ entier n°3
!     ** - 1ere composante des éléments de valr3
!     ** - pas de pas de temps, ni de numero d'ordre
!     ** - pour des raisons de complétude des tests on change
!     **   le type d'élément (aucun sens phys.))
	call mfdivw(fid,nomcha3,MED_NO_DT,MED_NO_IT,dt, &
     &              MED_CELL,MED_QUAD4,USER_INTERLACE, &
     &              1,nval3,valr3,ret)
	print *,ret
	if (ret .ne. 0 ) then
	   print *,'Erreur à l''écriture du champ : ', nomcha3,'et.1'
	   call efexit(-1)
	endif   

!     ** Ecriture du champ entier n°3
!     ** - les composantes des éléments de valr3
!     ** - pas de pas de temps, ni de numero d'ordre
!     ** - pour des raisons de complétude des tests on change
!     **   le type d'élément (aucun sens phys.))
	call mfdivw(fid,nomcha3,MED_NO_DT,MED_NO_IT,dt, &
     &              MED_NODE_ELEMENT,MED_QUAD4,USER_INTERLACE, &
     &              MED_ALL_CONSTITUENT,nent3,valr3,ret)
	print *,ret
	if (ret .ne. 0 ) then
	   print *,'Erreur à l''écriture du champ : ', nomcha3,'et.2'
	   call efexit(-1)
	endif   

!     ** Ecriture du champ entier n°3
!     ** - les composantes des éléments de valr3
!     ** - pas de pas de temps, ni de numero d'ordre
!     ** - profils
!     ** - pour des raisons de complétude des tests on change
!     **   le type d'élément (aucun sens phys.))
!	call efchae(fid,maa3,nomcha3,valr3p,USER_INTERLACE,nval3, &
!     &     MED_NOGAUSS,MED_ALL,"PROFIL(champ2)",USER_MODE, &
!     &               MED_NOEUD_MAILLE, &
!     &               MED_QUAD4,MED_NOPDT,nounit,dt,MED_NONOR,ret)
        call  mfdipw(fid,nomcha3,MED_NO_DT,MED_NO_IT,dt, &
     &               MED_NODE_ELEMENT,MED_QUAD4,USER_MODE, &
     &               "PROFIL(champ2)",MED_NO_LOCALIZATION, &
     &               USER_INTERLACE,MED_ALL_CONSTITUENT, &
     &               nent3,valr3p,ret)
	print *,ret
	if (ret .ne. 0 ) then
	   print *,'Erreur à l''écriture du profil : ', &
     &             'profil2(champ2)'
	   call efexit(-1)
	endif   

!     ** Fermeture du fichier *
 	call mficlo(fid,ret)
	if (ret .ne. 0 ) then
	   print *,'Erreur à la fermeture du fichier : '
	   ret = -1
	endif   

	print *,"Le code retour : ",ret
	call efexit(ret)

	end 



