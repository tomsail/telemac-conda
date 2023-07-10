!*  This file is part of MED.
!*
!*  COPYRIGHT (C) 1999 - 2019  EDF R&D, CEA/DEN
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

!     *******************************************************************************
!     * - Nom du fichier : test5.f90
!     *
!     * - Description : lecture des noeuds d'un maillage MED.
!     *
!     ******************************************************************************
      program test5
!     
      implicit none
      include 'med.hf'
!     
!     
      integer cret, ret
      integer*8 fid

      
!     ** la dimension du maillage                         **
      integer mdim
!     ** nom du maillage de longueur maxi MED_TAILLE_NOM  **
      character*32 maa
      character*200 desc
!     ** le nombre de noeuds                              **
      integer nnoe 
!     ** table des coordonnees                            **
      real*8, allocatable, dimension (:) ::  coo
      real*8, allocatable, dimension (:) ::  coo2
!     ** tables des noms et des unites des coordonnees    **
      character*16 nomcoo(2)   
      character*16 unicoo(2)
!     ** tables des noms, numeros, numeros de familles des noeuds  **
!     autant d'elements que de noeuds - les noms ont pout longueur **
!     MED_TAILLE_PNOM=8                                            
      character*16, allocatable, dimension (:) :: nomnoe
      integer,     allocatable, dimension (:) :: numnoe
      integer,     allocatable, dimension (:) :: nufano
      integer,     parameter                  :: profil(2) =  (/ 2, 3 /)
 
      integer i,rep
      logical inonoe,inunoe
      integer type

!     Ouverture du fichier en lecture seule             **
      call efouvr(fid,'test4.med',MED_LECTURE, cret)
      print *,cret

!   ** Lecture des infos concernant le premier maillage **
      if (cret.eq.0) then
         call efmaai(fid,1,maa,mdim,type,desc,cret)
      endif
      print *,cret
    

!   ** Combien de noeuds a lire  **
      if (cret.eq.0) then
         nnoe = 0
         call efnema(fid,maa,MED_COOR,MED_NOEUD,0,0,        &
     &        nnoe,cret)   
      endif
      print *,cret,' Nombre de noeuds : ',nnoe

       
!   ** Allocations memoires :  **
!   ** table des coordonnees   **
!     profil : (dimension * nombre de noeuds ) **
!   ** table des des numeros, des numeros de familles des noeuds
!   ** table des noms des noeuds ** 
      
      allocate( coo(nnoe*mdim),coo2(nnoe*mdim), numnoe(nnoe),nufano(nnoe),  &
     &     nomnoe(nnoe),STAT=ret )
      print *,ret
      

!   ** Lecture des composantes n°2 des coordonnees des noeuds      **
      if (cret.eq.0) then
         call efcool(fid,maa,mdim,coo,MED_FULL_INTERLACE,   & 
     &        2,profil,0,rep,nomcoo,unicoo,cret)
      endif
      print *,cret
      print *,'Lecture des composantes 2 des coordonnees : '
      print *,coo

!   ** Lecture des composantes n°1 des coordonnees des noeuds      **
      if (cret.eq.0) then
         call efcool(fid,maa,mdim,coo,MED_FULL_INTERLACE,   & 
     &        1,profil,0,rep,nomcoo,unicoo,cret)
      endif
      print *,cret
      print *,'Lecture des composantes 1 des coordonnees : '
      print *,coo

!   ** Lecture des composantes n°1 des coordonnees des noeuds du profil  **
      if (cret.eq.0) then
         call efcool(fid,maa,mdim,coo2,MED_FULL_INTERLACE,   & 
     &        1,profil,2,rep,nomcoo,unicoo,cret)
      endif
      print *,cret
      print *,'Lecture des composantes 1 des coordonnees avec le profil : '
      print *,coo2

!   ** Lecture des toutes les composantes des coordonnees des noeuds      **
      if (cret.eq.0) then
         call efcool(fid,maa,mdim,coo2,MED_FULL_INTERLACE,   & 
     &        MED_ALL,profil,0,rep,nomcoo,unicoo,cret)
      endif
      print *,cret
      print *,'Lecture des toutes les composantes des coordonnees : '
      print *,coo2
 
!   ** Lecture des noms des noeuds (optionnel dans un fichier MED)  **
      if (cret.eq.0) then
         call efnoml(fid,maa,nomnoe,nnoe,MED_NOEUD,         &
     &               0,ret)
      endif
    
      if (ret <0) then
         inonoe = .FALSE.
      else
         inonoe = .TRUE.
      endif

!  ** Lecture des numeros des noeuds (optionnel dans un fichier MED) **
      if (cret.eq.0) then
         call efnuml(fid,maa,numnoe,nnoe,MED_NOEUD,0,ret)
      endif
      if (ret <0) then
         inunoe = .FALSE.
      else
         inunoe = .TRUE.
      endif
      
!   ** Lecture des numeros de familles des noeuds                  **	   
      if (cret.eq.0) then
         call effaml(fid,maa,nufano,nnoe,MED_NOEUD,0,cret)
      endif
      print *,cret

!   ** Fermeture du fichier
      call efferm (fid,cret)
      print *,cret
      

!  ** Affichage des resulats                                         **
      if (cret.eq.0) then

         
         print *,"Type de repere         : ", rep
         print *,"Nom des coordonnees    : "
         print *, nomcoo
         
         print *,"Unites des coordonnees : "
         print *, unicoo
         
         print *,"Coordonnees des noeuds : "
         print *, coo 
         
         if (inonoe) then
            print *,"Noms des noeuds : "
            print *,nomnoe
         endif

         if (inunoe) then
            print *,"Numeros des noeuds : "
            print *,numnoe
         endif

         print *,"Numeros des familles des noeuds : "
         print *,nufano
         
      endif
      
! ** Liberation memoire                                            **
      deallocate(coo,nomnoe,numnoe,nufano);

! **  Code retour
      call efexit(cret)
      
      end program test5






