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
C * - Nom du fichier : test10.f
C *
C * - Description : ecriture de champs de resultats MED 
C *
C ******************************************************************************
	program test10
C     
	implicit none
	include 'med.hf'
C
	integer*8    fid
	integer      ret,USER_INTERLACE,USER_MODE
	real*8       a,b,p1,p2,dt

	character*32 maa1,maa2,maa3
	character*13 lien_maa2
C       CHAMP N°1
	character*32 nomcha1
	character*16 comp1(2), unit1(2)
	character*16 dtunit1, nounit
	integer      ncomp1
C	MODEL N°1 DE LOC. DES PTS DE GAUSS PR CHAMP1
	integer      ngauss1_1
	character*32 gauss1_1
	real*8       refcoo1(12), gscoo1_1(12), wg1_1(6)
	integer      nval1_1
	real*8       valr1_1(1*6*2)
C	MODEL N°2 DE LOC. DES PTS DE GAUSS PR CHAMP1
	integer      ngauss1_2
	character*32 gauss1_2
	real*8       gscoo1_2(6), wg1_2(3)
	integer      nval1_2
	real*8       valr1_2(2*3*2)
	real*8       valr1_2p(2*3)
C	MODEL N°3 DE LOC. DES PTS DE GAUSS PR CHAMP1
	integer      ngauss1_3,nval1_3
	real*8       valr1_3(2*3*2)
	real*8       valr1_3p(2*2)

C	CHAMP N°2
	character*32 nomcha2
	character*16 comp2(3), unit2(3)
	integer      ncomp2, nval2
	integer      valr2(5*3),   valr2p(3*3)

C	CHAMP N°3
	character*32 nomcha3
	character*16 comp3(2), unit3(2)
	integer      ncomp3, nval3
	integer      valr3(5*4*2),   valr3p(3*4*2)

C	PROFILS UTILISES
	character*32 nomprofil1
        integer      profil1(2) , profil2(3)

	parameter (USER_INTERLACE = MED_FULL_INTERLACE)
	parameter (USER_MODE = MED_COMPACT )
        parameter ( a=0.446948490915965D0, b=0.091576213509771D0    )
	parameter ( p1=0.11169079483905D0, p2=0.0549758718227661D0  )
C       MAILLAGES
	parameter ( maa1 = "maa1", maa2 = "maa2", maa3 = "maa3" )
	parameter ( lien_maa2= "./testfoo.med"                  )
C       CHAMP N°1
	parameter ( nomcha1 = "champ reel" )
	parameter ( ncomp1 = 2 )
	parameter ( dtunit1 = "                ")
	parameter ( nounit  = "                ")
C       MODEL N°1 DE LOC. DES PTS DE GAUSS PR CHAMP1
	parameter ( gauss1_1 = "Model n1" )
	parameter ( ngauss1_1 = 6 )
C       MODEL N°2 DE LOC. DES PTS DE GAUSS PR CHAMP1
	parameter ( gauss1_2  = "Model n2" )
	parameter ( ngauss1_2 = 3 )
C       MODEL N°3 DE LOC. DES PTS DE GAUSS PR CHAMP1
	parameter ( ngauss1_3 = 6 )
	parameter ( nval1_3 = 6 )
C       CHAMP N°2
	parameter ( nomcha2="champ entier")
        parameter ( ncomp2 = 3, nval2= 5  )
C       CHAMP N°3
	parameter ( nomcha3="champ entier 3")
        parameter ( ncomp3 = 2, nval3= 5*4  )
C       PROFILS
	parameter ( nomprofil1  = "PROFIL(champ(1))" )
	

C       CHAMP N°1
	data comp1 /"comp1", "comp2"/
        data unit1 /"unit1","unit2"/
C       MODEL N°1 DE LOC. DES PTS DE GAUSS PR CHAMP1
	data nval1_1  / 1*6 /
	data refcoo1  / -1.0,1.0, -1.0,-1.0, 1.0,-1.0, -1.0,0.0, 
     1	                0.0,-1.0, 0.0,0.0 / 
	data valr1_1  /  0.0,1.0, 2.0,3.0, 10.0,11.0, 12.0,13.0,
     1                   20.0,21.0, 22.0,23.0/
C       MODEL N°2 DE LOC. DES PTS DE GAUSS PR CHAMP1
	data valr1_2  / 0.0,1.0, 2.0,3.0, 10.0,11.0,
     1                  12.0,13.0, 20.0,21.0, 22.0,23.0 /
	data valr1_2p / 12.0,13.0, 20.0,21.0, 22.0,23.0 /
C       MODEL N°3 DE LOC. DES PTS DE GAUSS PR CHAMP1
	data valr1_3  / 0.0,1.0, 2.0,3.0, 10.0,11.0, 12.0,13.0, 
     1                  20.0,21.0, 22.0,23.0 /
	data valr1_3p / 2.0,3.0, 10.0,11.0   /
C       CHAMP N°2
	data comp2 /"comp1", "comp2", "comp3"/
        data unit2 /"unit1","unit2", "unit3"/
	data valr2 / 0,1,2, 10,11,12, 20,21,22, 30,31,32, 40,41,42 /
	data valr2p / 0,1,2,           20,21,22,           40,41,42 /
C       CHAMP N°3
	data comp3 /"comp1", "comp2"/
        data unit3 /"unit1","unit2"/
	data valr3 / 0,1, 10,11, 20,21, 30,31,
     1           40,41, 50,51, 60,61, 70,71,
     1           80,81, 90,91, 100,101, 110,111,
     1           120,121, 130,131, 140,141, 150,151,
     1           160,161, 170,171, 180,181, 190,191 /
	data valr3p / 0,1, 10,11, 20,21, 30,31,
     1            80,81, 90,91, 100,101, 110,111,
     1            160,161, 170,171, 180,181, 190,191 /


C       PROFILS
	data profil1 /2,3/
	data profil2 /1,3,5/
        
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
		   
C     ** ouverture du fichier                            **
 	call efouvr(fid,'test10.med',MED_LECTURE_ECRITURE, ret)
        if (ret .ne. 0 ) then
	   print *,'Erreur à l''ouverture du fichier  : ','test10.med'
	   call efexit(-1)
	endif
        
C     ** creation du maillage maa1 de dimension 3         **
	call efmaac(fid,maa1,3,MED_NON_STRUCTURE,
     1	               "Maillage vide",ret)
        if (ret .ne. 0 ) then
	   print *,'Erreur à la création du maillage : ', maa1
	   call efexit(-1)
	endif
 	     
C     ** creation du maillage maa3 de dimension 3         **
	call efmaac(fid,maa3,3,MED_NON_STRUCTURE,
     1	               "Maillage vide",ret)
        if (ret .ne. 0 ) then
	   print *,'Erreur à la création du maillage : ', maa3
	   call efexit(-1)
	endif
 	     

C     ** creation du champ réel n°1                        **
	call efchac(fid,nomcha1,MED_FLOAT64,comp1,unit1,ncomp1,ret)
        if (ret .ne. 0 ) then
	   print *,'Erreur à la création du champ : ', nomcha1
	   call efexit(-1)
	endif
 	     
C     ** creation du champ entier n°2                      **
	call efchac(fid,nomcha2,MED_INT32,comp2,unit2,ncomp2,ret)
        if (ret .ne. 0 ) then
	   print *,'Erreur à la création du champ : ', nomcha2
	   call efexit(-1)
	endif
 
C     ** creation du lien au fichier distant contenant maa2 **
	call efliee(fid,lien_maa2,maa2,ret)
        if (ret .ne. 0 ) then
	   print *,'Erreur à la création du lien : ', lien_maa2
	   call efexit(-1)
	endif
 
C     ** creation de la localisation des points de Gauss modèle n°1 **
	call efgaue(fid, MED_TRIA6, refcoo1, USER_INTERLACE,
     1	             ngauss1_1, gscoo1_1, wg1_1, gauss1_1, ret)
        if (ret .ne. 0 ) then
	   print *,'Erreur à la création du modèle n°1 : ', gauss1_1
	   call efexit(-1)
	endif

C     ** creation de la localisation des points de Gauss modèle n°2 **
	call efgaue(fid, MED_TRIA6, refcoo1, USER_INTERLACE,
     1	             ngauss1_2, gscoo1_2, wg1_2, gauss1_2, ret)
        if (ret .ne. 0 ) then
	   print *,'Erreur à la création du modèle n°2 : ', gauss1_2
	   call efexit(-1)
	endif

	
C     ** Ecriture du champ n°1
C     ** - enregistre uniquement la composante n°2 de valr1_1
C     ** - pas de pas de temps, ni de numero d'ordre
	dt = 0.0D0
	call efchae(fid,maa1,nomcha1,valr1_1,USER_INTERLACE,nval1_1,
     1               gauss1_1,2,MED_NOPFL,MED_NO_PFLMOD,
     2               MED_MAILLE,MED_TRIA6,
     3               MED_NOPDT,dtunit1,dt,MED_NONOR,ret)
	if (ret .ne. 0 ) then
	   print *,'Erreur à l''écriture du champ : ', nomcha1,'et.1'
	   call efexit(-1)
	endif

C     ** Nouvelle Ecriture du champ reel en mode remplacement
C     ** - complete le champ precedent en enregistrant les composantes 1
C     ** - pas de pas de temps, ni de numero d'ordre 
	call efchae(fid,maa1,nomcha1,valr1_1,USER_INTERLACE,nval1_1,
     1               gauss1_1,1,MED_NOPFL,MED_NO_PFLMOD,
     2               MED_MAILLE,MED_TRIA6,
     3               MED_NOPDT,dtunit1,dt,MED_NONOR,ret)
	if (ret .ne. 0 ) then
	   print *,'Erreur à l''écriture du champ : ', nomcha1,'et.2'
	   call efexit(-1)
	endif
	 
C     ** Ecriture sur le champ reel
C     ** - De la 1ere composante du tableau valr1_2
C     ** - Avec un pas de temps égal a 5.5
C     ** - Pas de numero d'ordre
C     ** - maa2 est distant
	dt = 5.5D0
	call efchae(fid,maa2,nomcha1,valr1_2,USER_INTERLACE,nval1_2,
     1               gauss1_2,1,MED_NOPFL,MED_NO_PFLMOD,
     2               MED_MAILLE,MED_TRIA6,
     3               1,"ms",dt,MED_NONOR,ret)
	if (ret .ne. 0 ) then
	   print *,'Erreur à l''écriture du champ : ', nomcha1,'et.3'
	   call efexit(-1)
	endif

C     ** Ecriture sur le champ reel
C     ** - De la 2ere composante du tableau valr1_2
C     ** - Avec un pas de temps égal a 5.5
C     ** - Pas de numero d'ordre
C     ** - maa1 est local
	dt = 5.5D0
	call efchae(fid,maa1,nomcha1,valr1_1,USER_INTERLACE,nval1_1,
     1               gauss1_1,2,MED_NOPFL,MED_NO_PFLMOD,
     2               MED_MAILLE,MED_TRIA6,
     3               1,"ms",dt,MED_NONOR,ret)
	if (ret .ne. 0 ) then
	   print *,'Erreur à l''écriture du champ : ', nomcha1,'et.4'
	   call efexit(-1)
	endif

      
C     ** Ecriture sur le champ reel
C     ** - De la 1ere composante du tableau valr1_1
C     ** - Avec un pas de temps égal a 5.5
C     ** - Numero d'ordre egal a 2
C     ** - maa3 est local
	dt = 5.5D0
	call efchae(fid,maa3,nomcha1,valr1_2,USER_INTERLACE,nval1_2,
     1               gauss1_2,1,MED_NOPFL,MED_NO_PFLMOD,
     2               MED_MAILLE,MED_TRIA6,
     3               1,"ms",dt,2,ret)
	if (ret .ne. 0 ) then
	   print *,'Erreur à l''écriture du champ : ', nomcha1,'et.5'
	   call efexit(-1)
	endif
    
C     ** Creation de profil
C     ** - qui selectionne uniquement le 2e element du tableau valr1
	call efpfle(fid,profil1,1,nomprofil1,ret)
	if (ret .ne. 0 ) then
	   print *,'Erreur à la création du profil : ', nomprofil1
	   call efexit(-1)
	endif


C     ** Ecriture du champ reel 
C     ** - Toutes les composantes du 2e element de valr1_1 (MED_ALL)
C     ** - Extrait a partir du profil de nom "profil1(1)"
C     ** - Pas de temps = 5.6
C     ** - Numero d'ordre = 2 
	dt = 5.6D0
	call efchae(fid,maa1,nomcha1,valr1_3p,USER_INTERLACE,nval1_3,
     1               MED_NOGAUSS,MED_ALL,nomprofil1,USER_MODE,
     2               MED_MAILLE,MED_TRIA6,
     3               2,"ms",dt,2,ret)
	if (ret .ne. 0 ) then
	   print *,'Erreur à l''écriture du champ : ', nomcha1,'et.6'
	   call efexit(-1)
	endif

C     ** Ecriture du champ reel 
C     ** - Toutes les composantes du 2e element de valr1_1 (MED_ALL)
C     ** - Extrait a partir du profil de nom "profil1(1)"
C     ** - Pas de temps = 5.6
C     ** - Numero d'ordre = 2 
	dt = 5.6D0
	call efchae(fid,maa2,nomcha1,valr1_2p,USER_INTERLACE,nval1_2,
     1               gauss1_2,MED_ALL,nomprofil1,USER_MODE,
     2               MED_MAILLE,MED_TRIA6,
     3               2,"ms",dt,2,ret)
	if (ret .ne. 0 ) then
	   print *,'Erreur à l''écriture du champ : ', nomcha1,'et.7'
	   call efexit(-1)
	endif


C     ** Ecriture du champ reel 
C     ** - 2e composante du 2e element du champ
C     ** - Extrait a partir du profil de nom "profil1(1)"
C     ** - Pas de temps = 5.7
C     ** - Numero d'ordre = 2 
	dt = 5.7D0
	call efchae(fid,maa1,nomcha1,valr1_3p,USER_INTERLACE,nval1_3,
     1               MED_NOGAUSS,2,nomprofil1,USER_MODE,
     2               MED_MAILLE,MED_TRIA6,
     3               3,"ms",dt,2,ret)
	if (ret .ne. 0 ) then
	   print *,'Erreur à l''écriture du champ : ', nomcha1,'et.8'
	   call efexit(-1)
	endif


C     ** Ecriture du champ entier n°2
C     ** - 1ere composante des éléments de valr2
C     ** - pas de pas de temps, ni de numero d'ordre
	dt = 0.0D0
	call efchae(fid,maa1,nomcha2,valr2,USER_INTERLACE,nval2,
     1     MED_NOGAUSS,1,MED_NOPFL,MED_NO_PFLMOD,MED_ARETE,
     1               MED_SEG2,MED_NOPDT,nounit,dt,MED_NONOR,ret)
	if (ret .ne. 0 ) then
	   print *,'Erreur à l''écriture du champ : ', nomcha2,'et.1'
	   call efexit(-1)
	endif   

C     ** Ecriture du champ entier n°2
C     ** - 2ere composante des éléments de valr2
C     ** - pas de pas de temps, ni de numero d'ordre
C     ** - pour des raisons de complétude des tests on change 
C     **   le type d'élément (aucun sens phys.))
	call efchae(fid,maa1,nomcha2,valr2,USER_INTERLACE,nval2,
     1     MED_NOGAUSS,2,MED_NOPFL,MED_NO_PFLMOD,MED_NOEUD,
     1               0,MED_NOPDT,nounit,dt,MED_NONOR,ret)
	if (ret .ne. 0 ) then
	   print *,'Erreur à l''écriture du champ : ', nomcha2,'et.2'
	   call efexit(-1)
	endif   


C     ** Ecriture du champ entier n°2
C     ** - 3ere composante des éléments de valr2
C     ** - pas de pas de temps, ni de numero d'ordre
C     ** - pour des raisons de complétude des tests on change 
C     **   le type d'élément (aucun sens phys.))
	call efchae(fid,maa1,nomcha2,valr2,USER_INTERLACE,nval2,
     1     MED_NOGAUSS,3,MED_NOPFL,MED_NO_PFLMOD,MED_FACE,
     1               MED_TRIA6,MED_NOPDT,nounit,dt,MED_NONOR,ret)
	if (ret .ne. 0 ) then
	   print *,'Erreur à l''écriture du champ : ', nomcha2,'et.3'
	   call efexit(-1)
	endif   

C     ** Creation de profil
C     ** - selectionne les elements 1,3,5 du tableau valr2
	call efpfle(fid,profil2,3,"PROFIL(champ2)",ret)
	if (ret .ne. 0 ) then
	   print *,'Erreur à l''écriture du profil : ',
     1              'profil2(champ2)'
	   call efexit(-1)
	endif   


C     ** Ecriture du champ entier n°2
C     ** - 3eme composante des éléments de valr2
C     ** - pas de pas de temps, ni de numero d'ordre
C     ** - profils 
C     ** - pour des raisons de complétude des tests on change 
C     **   le type d'élément (aucun sens phys.))
	call efchae(fid,maa1,nomcha2,valr2p,USER_INTERLACE,nval2,
     1     MED_NOGAUSS,3,"PROFIL(champ2)",USER_MODE,MED_MAILLE,
     1               MED_TRIA6,MED_NOPDT,nounit,dt,MED_NONOR,ret)
	if (ret .ne. 0 ) then
	   print *,'Erreur à l''écriture du profil : ',
     1             'profil2(champ2)'
	   call efexit(-1)
	endif   

C     ** creation du champ entier n°3                      **
	call efchac(fid,nomcha3,MED_INT32,comp3,unit3,ncomp3,ret)
        if (ret .ne. 0 ) then
	   print *,'Erreur à la création du champ : ', nomcha3
	   call efexit(-1)
	endif
 
C     ** Ecriture du champ entier n°3
C     ** - 1ere composante des éléments de valr3
C     ** - pas de pas de temps, ni de numero d'ordre
C     ** - pour des raisons de complétude des tests on change
C     **   le type d'élément (aucun sens phys.))
	call efchae(fid,maa1,nomcha3,valr3,USER_INTERLACE,nval3,
     1     MED_NOGAUSS,1,MED_NOPFL,MED_NO_PFLMOD,MED_NOEUD_MAILLE,
     1               MED_QUAD4,MED_NOPDT,nounit,dt,MED_NONOR,ret)
	if (ret .ne. 0 ) then
	   print *,'Erreur à l''écriture du champ : ', nomcha3,'et.1'
	   call efexit(-1)
	endif   

C     ** Ecriture du champ entier n°3
C     ** - les composantes des éléments de valr3
C     ** - pas de pas de temps, ni de numero d'ordre
C     ** - pour des raisons de complétude des tests on change
C     **   le type d'élément (aucun sens phys.))
	call efchae(fid,maa2,nomcha3,valr3,USER_INTERLACE,nval3,
     1     MED_NOGAUSS,MED_ALL,MED_NOPFL,MED_NO_PFLMOD,
     1               MED_NOEUD_MAILLE,
     1               MED_QUAD4,MED_NOPDT,nounit,dt,MED_NONOR,ret)
	if (ret .ne. 0 ) then
	   print *,'Erreur à l''écriture du champ : ', nomcha3,'et.2'
	   call efexit(-1)
	endif   

C     ** Ecriture du champ entier n°3
C     ** - les composantes des éléments de valr3
C     ** - pas de pas de temps, ni de numero d'ordre
C     ** - profils
C     ** - pour des raisons de complétude des tests on change
C     **   le type d'élément (aucun sens phys.))
	call efchae(fid,maa3,nomcha3,valr3p,USER_INTERLACE,nval3,
     1     MED_NOGAUSS,MED_ALL,"PROFIL(champ2)",USER_MODE,
     1               MED_NOEUD_MAILLE,
     1               MED_QUAD4,MED_NOPDT,nounit,dt,MED_NONOR,ret)
	if (ret .ne. 0 ) then
	   print *,'Erreur à l''écriture du profil : ',
     1             'profil2(champ2)'
	   call efexit(-1)
	endif   

C     ** Fermeture du fichier *
 	call efferm (fid,ret)
	if (ret .ne. 0 ) then
	   print *,'Erreur à la fermeture du fichier : '
	   ret = -1
	endif   

	print *,"Le code retour : ",ret
	call efexit(ret)

	end 



