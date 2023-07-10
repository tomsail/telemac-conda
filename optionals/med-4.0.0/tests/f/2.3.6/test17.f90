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

! ******************************************************************************
! * - Nom du fichier : test17.f90
! *
! * - Description : lecture d'elements de maillages MED ecrits par test16
! *                 via les routines de niveau 2
! *                 - equivalent a test17.f90
! *
! ******************************************************************************

program test17
  
  implicit none
  include 'med.hf'

  integer*8    fid
  integer      :: cret,ret, nse2, mdim 
  integer,     allocatable, dimension(:) ::se2
  character*16, allocatable, dimension(:) ::nomse2
  integer,     allocatable, dimension(:) ::numse2,nufase2 
  integer      ntr3
  integer,     allocatable, dimension(:) ::tr3
  character*16, allocatable, dimension(:) ::nomtr3
  integer,     allocatable, dimension(:) ::numtr3
  integer,     allocatable, dimension(:) ::nufatr3
  character*32  :: maa = "maa1"
  character*200 :: desc
  logical      :: inoele1,inuele1,inoele2,inuele2
  integer      tse2,ttr3
  integer i,type

  !   ** Ouverture du fichier test16.med en lecture seule **
  call efouvr(fid,'test16.med',MED_LECTURE, cret)
  print *,cret

  !   ** Lecture des informations sur le 1er maillage **
  if (cret.eq.0) then
     call efmaai(fid,1,maa,mdim,type,desc,cret)
     print *,"Maillage de nom : ",maa," et de dimension ",mdim
  endif
  print *,cret

   !  ** Lecture du nombre de triangles et de segments **
  if (cret.eq.0) then
     call efnema(fid,maa,MED_CONN,MED_ARETE,MED_SEG2,MED_DESC,nse2,cret)
  endif
  print *,cret

  if (cret.eq.0) then
     call efnema(fid,maa,MED_CONN,MED_MAILLE,MED_TRIA3,MED_DESC,ntr3,cret)
  endif
  print *,cret

  print *,"Nombre de MED_SEG2 : ",nse2," - nombre de MED_TRIA3 : ",ntr3

  !  ** Allocations memoire ** 
  tse2 = 2;  
  allocate(se2(tse2*nse2),nomse2(nse2),numse2(nse2),nufase2(nse2),STAT=ret)
  ttr3 = 3;
  allocate(tr3(ntr3*ttr3),nomtr3(ntr3),numtr3(ntr3),nufatr3(ntr3),STAT=ret)
 
  !  ** Lecture des aretes segments MED_SEG2 : 
  !     - Connectivite,
  !     - Noms (optionnel)
  !     - Numeros (optionnel)
  !     - Numeros de familles **
  if (cret.eq.0) then
     call efelel(fid,maa,mdim,se2,MED_NO_INTERLACE,nomse2,inoele1,numse2,inuele1,    &
          &			  nufase2,nse2,MED_ARETE,MED_SEG2,MED_DESC,cret)
  endif
  print *,cret
        
  
  !  ** lecture des mailles triangles MED_TRIA3 : 
  !     - Connectivite,
  !     - Noms (optionnel)
  !     - Numeros (optionnel)
  !     - Numeros de familles **
  if (cret.eq.0) then
     call efelel(fid,maa,mdim,tr3,MED_NO_INTERLACE,nomtr3,inoele2,numtr3,inuele2,  &
          &			  nufatr3,ntr3,MED_MAILLE,MED_TRIA3,MED_DESC,cret)
  endif
  print *,cret
 
  ! ** Fermeture du fichier **
  call efferm (fid,cret)
  print *,cret
	
  ! ** Affichage **
  if (cret.eq.0) then
      print *,"Connectivite des segments : ",nse2
     
      if (inoele1) then
         print *,"Noms des segments : ",nomse2
      endif

      if (inuele1) then
         print *,"Numeros des segments : ",numse2
      endif

      print *,"Numeros des familles des segments : ",nufase2
  
      
      print *,"Connectivite des triangles : ",tr3
      
      if (inoele2) then
         print *,"Noms des triangles :", nomtr3
      endif

      if (inuele2) then
	  print *,"Numeros des triangles :", numtr3
      endif

      print *,"Numeros des familles des triangles :", nufatr3
      
   end if

   
   ! ** Nettoyage memoire **
   deallocate(se2,nomse2,numse2,nufase2);
   deallocate(tr3,nomtr3,numtr3,nufatr3);

   ! ** Code retour
   call efexit(cret)
   
 end program test17
