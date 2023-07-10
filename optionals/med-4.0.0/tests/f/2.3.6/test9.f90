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
! * - Nom du fichier : test9.f90
! *
! * - Description : lecture des familles d'un maillage MED 
! *
! ******************************************************************************
program test9
  
  implicit none
  include 'med.hf'
!
  integer*8      fid
  integer        ret,cret
  character*32   maa
  integer        mdim
  integer        nfam
  integer        i,j
  integer        natt,ngro
  character*200, allocatable, dimension (:) :: attdes    
  character*80,  allocatable, dimension (:) :: gro       
  integer,       allocatable, dimension (:) :: attval,attide
  character*32   nomfam
  character*200  desc
  integer        numfam
  integer        type


!  ** Ouverture du fichier test8.med en lecture seule **
  call efouvr(fid,'test8.med',MED_LECTURE, cret)
  print *,cret

!  ** Lecture des infos sur le 1er maillage **
  if (cret.eq.0) then
     call efmaai(fid,1,maa,mdim,type,desc,cret)
     print *,"Maillage de nom : ",maa," et de dimension : ", mdim
  endif
  print *,cret

!  ** Lecture du nombre de famille **
  if (cret .eq. 0) then
     call efnfam(fid,maa,nfam,cret)
     print *,' Nombre de familles a lire : ',nfam
  endif
  print *,cret

!  ** Lecture de chaque famille **
  if (cret .eq. 0) then
     do i=1,nfam
        
!	** Lecture du nombre de groupe **
        if (cret .eq. 0) then
           call efngro(fid,maa,i,ngro,cret)
        endif
        print *,cret

!	** Lecture du nombre d'attribut **
        if (cret .eq. 0) then
           call efnatt(fid,maa,i,natt,cret)
        endif
        print *,cret

        print *,"Famille ",i," a ",natt," attributs et ",ngro," groupes " 

!	** Lecture de : nom,numero,attributs,groupes **
        if (cret .eq. 0) then
           allocate(attide(natt),attval(natt),attdes(natt),gro(ngro),STAT=ret)
!              print *,ret

           call effami(fid,maa,i,nomfam,numfam,attide,     &
                &                     attval,attdes,natt,gro,ngro,cret)
           print *,cret
           print *,"Famille de nom ",nomfam," et de numero ",numfam
           print *,"Attributs :"
           do j=1,natt
              print *,"ide = ",attide(j)," - val = ",attval(j)," - des = ",attdes(j)
           enddo
           deallocate(attide,attval,attdes)

           do j=1,ngro
              print *,"gro = ",gro(j)
           enddo
           deallocate(gro)
        endif
     enddo
  endif
     
     
!  ** Fermeture du fichier                                           **
     call efferm (fid,cret)
     print *,cret
     
!  ** Code retour
     call efexit(cret)
     
   end program test9


