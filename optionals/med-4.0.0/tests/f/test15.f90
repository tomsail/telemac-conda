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

! *******************************************************************************
! * - Nom du fichier : test15.f90
! *
! * - Description : lecture des noeuds d'un maillage MED.
! *                 a l'aide des routines de niveau 2
! *                 - equivalent a test5.f90
! *
! ******************************************************************************

program test15
  
  implicit none
  include 'med.hf90'
!  
!
  integer*8  fid
  integer  ret,cret
  !  ** la dimension du maillage                        **
  integer mdim,sdim
  !  ** nom du maillage de longueur maxi MED_TAILLE_NOM **
  character*64 maa
  character*200 desc
  !  ** le nombre de noeuds                             **
  integer :: nnoe = 0
  !  ** table des coordonnees                           **
  real*8, allocatable, dimension(:) :: coo
  !  ** tables des noms et des unites des coordonnees 
  !     profil : (dimension)                            **
  character*16 nomcoo(2)
  character*16 unicoo(2)
  character*16 dtunit
  !  ** tables des noms, numeros, numeros de familles des noeuds
  !     autant d'elements que de noeuds - les noms ont pout longueur
  !     MED_SNAME_SIZE **
  character*16, allocatable, dimension(:) ::  nomnoe
  integer,      allocatable, dimension(:) ::  numnoe,nufano
  integer rep
  integer inonoe,inunoe,inufa
  character*16 str
  integer i
  character*255 argc
  integer type,nstep,stype
  integer chgt,tsf

  !  ** Ouverture du fichier **
  call mfiope(fid,"test14.med",MED_ACC_RDONLY, cret)
  print *,cret
   

  !  ** Lecture des infos concernant le premier maillage **
  if (cret.eq.0) then
     call mmhmii(fid,1,maa,sdim,mdim,type,desc,dtunit,stype,nstep,rep,nomcoo,unicoo,cret)
     print *,"Maillage de nom : ",maa," et de dimension : ",mdim
  endif
  print *,cret
  
  ! ** Lecture du nombre de noeud **
  if (cret.eq.0) then
     call mmhnme(fid,maa,MED_NO_DT,MED_NO_IT,MED_NODE,MED_NONE,MED_COORDINATE,MED_NO_CMODE,chgt,tsf,nnoe,cret)
     print *,"Nombre de noeuds : ",nnoe
  endif
  print *,cret

  ! ** Allocations memoires **
  ! ** table des coordonnees 
  ! ** profil : (dimension * nombre de noeuds ) **
  allocate (coo(nnoe*sdim),STAT=ret)
  ! ** table des des numeros, des numeros de familles des noeuds
  !   profil : (nombre de noeuds) **
  allocate (numnoe(nnoe),nufano(nnoe),STAT=ret)
  ! ** table des noms des noeuds 
  !   profil : (nnoe*MED_TAILLE_PNOM+1) **
  allocate (nomnoe(nnoe),STAT=ret)

  ! ** Lecture des noeuds : 
  !     - Coordonnees
  !     - Noms (optionnel dans un fichier MED) 
  !     - Numeros (optionnel dans un fichier MED) 
  !     - Numeros de familles	**
  if (cret.eq.0) then
     call mmhnor(fid,maa,MED_NO_DT,MED_NO_IT,MED_FULL_INTERLACE,coo,inonoe,nomnoe,inunoe,numnoe,inufa,nufano,cret)    
  endif
  
  ! ** Affichage des resulats **
  if (cret.eq.0) then
      print *,"Type de repere : ",rep
      print *,"Nom des coordonnees : ",nomcoo
    
      print *,"Unites des coordonnees : ",unicoo
     
      print *,"Coordonnees des noeuds : ",coo
     
      if (inonoe .eq. MED_TRUE) then
         print *,"Noms des noeuds : |",nomnoe,"|"
      endif

      if (inunoe .eq. MED_TRUE) then
         print *,"Numeros des noeuds : ",numnoe
      endif

      if (inufa .eq. MED_TRUE) then
         print *,"Numeros des familles des noeuds : ",nufano
      else
         print *,"Numeros des familles des noeuds : 0"
      endif

   endif

  ! ** Liberation memoire **
   deallocate(coo,nomnoe,numnoe,nufano)
 
  ! ** Fermeture du fichier **
   call mficlo(fid,cret)
   print *,cret

  ! **Code retour
   call efexit(cret)
   
 end program test15

