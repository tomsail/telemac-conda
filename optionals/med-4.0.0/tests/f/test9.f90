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
  include 'med.hf90'
!
  integer*8      fid
  integer        ret,cret
  character*64   maa
  integer        mdim,sdim
  integer        nfam
  integer        i,j
  integer        ngro,natt 
  character*80,  allocatable, dimension (:) :: gro  
  integer,       allocatable, dimension (:) :: attid
  integer,       allocatable, dimension (:) :: attval
  character*200, allocatable, dimension (:) :: attdes
  character*200  desc     
  character*64   nomfam
  integer        numfam
  integer        type
  character(16)  :: dtunit
  integer nstep, stype, atype
  character*16 nomcoo(2)   
  character*16 unicoo(2)


!  ** Ouverture du fichier test8.med en lecture seule **
  call mfiope(fid,'test8.med',MED_ACC_RDONLY, cret)
  print *,cret

!  ** Lecture des infos sur le 1er maillage **
  if (cret.eq.0) then
     call mmhmii(fid,1,maa,sdim,mdim,type,desc,dtunit,stype,nstep,atype,nomcoo,unicoo,cret)
     print *,"Maillage de nom : ",maa," et de dimension : ", mdim
  endif
  print *,cret

!  ** Lecture du nombre de famille **
  if (cret .eq. 0) then
     call mfanfa(fid,maa,nfam,cret)
     print *,' Nombre de familles a lire : ',nfam
  endif
  print *,cret

!  ** Lecture de chaque famille **
  if (cret .eq. 0) then
     do i=1,nfam
        
!	** Lecture du nombre de groupe **
        if (cret .eq. 0) then
           call mfanfg(fid,maa,i,ngro,cret)
        endif
        print *,cret

!	** Lecture du nombre d'attributs pour les
!          fichiers 2.3 **
        if (cret .eq. 0) then
           call mfaona(fid,maa,i,natt,cret)
        endif
        print *,cret

        print *,"Famille ",i," a ",ngro," groupes et ", natt, " attributs" 

!	** Lecture de : nom,numero,attributs,groupes **
        if (cret .eq. 0) then
           allocate(gro(ngro), attid(natt), attval(natt), attdes(natt),STAT=ret)
           print *,ret

           call mfaofi(fid,maa,i,nomfam,attid,attval,attdes,numfam,gro,cret)
           print *,cret
           print *,"Famille de nom ",nomfam," et de numero ",numfam
           do j=1,natt
              print *,"attid = ", attid(j)
              print *,"attval = ", attval(j)
              print *,"attdes =", attdes(j)
           enddo
           do j=1,ngro
              print *,"gro = ",gro(j)
           enddo

           deallocate(gro, attval, attid, attdes)
        endif
     enddo
  endif
     
     
!  ** Fermeture du fichier                                           **
     call mficlo(fid,cret)
     print *,cret
     
!  ** Code retour
     call efexit(cret)
     
   end program test9


