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
  include 'med.hf90'

  integer*8    fid
  integer      :: cret, ret, nse2, mdim, sdim 
  integer,     allocatable,  dimension(:) ::se2
  character*16, allocatable, dimension(:) ::nomse2
  integer,     allocatable,  dimension(:) ::numse2,nufase2 
  integer      ntr3
  integer,     allocatable,  dimension(:) ::tr3
  character*16, allocatable, dimension(:) ::nomtr3
  integer,     allocatable,  dimension(:) ::numtr3
  integer,     allocatable,  dimension(:) ::nufatr3
  character*64  :: maa
  character*200 :: desc
  integer      :: inoele1,inuele1,inoele2,inuele2,ifaele1,ifaele2
  integer      tse2,ttr3
  integer i,type,rep,nstep,stype
  integer chgt,tsf
  character*16 nomcoo(2)
  character*16 unicoo(2)
  character*16 dtunit

  !   ** Ouverture du fichier test16.med en lecture seule **
  call mfiope(fid,'test16.med',MED_ACC_RDONLY, cret)
  print *,cret

  !   ** Lecture des informations sur le 1er maillage **
  if (cret.eq.0) then
     call mmhmii(fid,1,maa,sdim,mdim,type,desc,dtunit,stype,nstep,rep,nomcoo,unicoo,cret)
     print *,"Maillage de nom : ",maa," et de dimension ",mdim
  endif
  print *,cret

   !  ** Lecture du nombre de triangles et de segments **
  if (cret.eq.0) then
     call mmhnme(fid,maa,MED_NO_DT,MED_NO_IT,MED_DESCENDING_EDGE,MED_SEG2,MED_CONNECTIVITY,MED_DESCENDING,chgt,tsf,nse2,cret)
  endif
  print *,cret

  if (cret.eq.0) then
     call mmhnme(fid,maa,MED_NO_DT,MED_NO_IT,MED_CELL,MED_TRIA3,MED_CONNECTIVITY,MED_DESCENDING,chgt,tsf,ntr3,cret)
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
     call mmhelr(fid,maa,MED_NO_DT,MED_NO_IT,MED_DESCENDING_EDGE,MED_SEG2,MED_DESCENDING,MED_NO_INTERLACE,se2,&
                 inoele1,nomse2,inuele1,numse2,ifaele1,nufase2,cret)
  endif
  print *,cret
        
  
  !  ** lecture des mailles triangles MED_TRIA3 : 
  !     - Connectivite,
  !     - Noms (optionnel)
  !     - Numeros (optionnel)
  !     - Numeros de familles **
  if (cret.eq.0) then
     call mmhelr(fid,maa,MED_NO_DT,MED_NO_IT,MED_CELL,MED_TRIA3,MED_DESCENDING,MED_NO_INTERLACE,tr3,&
                 inoele2,nomtr3,inuele2,numtr3,ifaele2,nufatr3,cret)
  endif
  print *,cret
 
  ! ** Fermeture du fichier **
  call mficlo(fid,cret)
  print *,cret
	
  ! ** Affichage **
  if (cret.eq.0) then
      print *,"Connectivite des segments : ",se2
     
      if (inoele1 .eq. MED_TRUE) then
         print *,"Noms des segments : ",nomse2
      endif

      if (inuele1 .eq. MED_TRUE) then
         print *,"Numeros des segments : ",numse2
      endif

      print *,"Numeros des familles des segments : ",nufase2
  
      
      print *,"Connectivite des triangles : ",tr3
      
      if (inoele2 .eq. MED_TRUE) then
         print *,"Noms des triangles :", nomtr3
      endif

      if (inuele2 .eq. MED_TRUE) then
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
