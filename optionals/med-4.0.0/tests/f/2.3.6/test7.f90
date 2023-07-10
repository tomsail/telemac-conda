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
! * - Nom du fichier : test7.f90
! *
! * - Description : lecture des elements du maillage MED ecrits par test6
! *
! ******************************************************************************
      program test7
    
      implicit none
      include 'med.hf'
!     
!           
      integer*8 fid
      integer   cret, ret

      integer nse2
      integer,     allocatable, dimension (:) :: se2
      character*16, allocatable, dimension (:) :: nomse2
      integer,     allocatable, dimension (:) :: numse2,nufase2
 
      integer ntr3
      integer,     allocatable, dimension (:) :: tr3
      character*16, allocatable, dimension (:) :: nomtr3
      integer,     allocatable, dimension (:) :: numtr3,nufatr3
   
!     ** nom du maillage de longueur maxi MED_TAILLE_NOM    **
      character*32  :: maa  = "maa1"
      character*200 :: desc
      integer       :: mdim
      logical inoele,inuele
      integer, parameter :: profil (2) = (/ 2,3 /) 
      integer type
      integer tse2,ttr3, i

!   ** Ouverture du fichier test6.med en lecture seule       **
      call efouvr(fid,'test6.med',MED_LECTURE, cret)
      print *,cret

!   ** Lecture des infos concernant le premier maillage      **
      if (cret.eq.0) then
         call efmaai(fid,1,maa,mdim,type,desc,cret)
         print *,"Maillage de nom : ",maa," et de dimension :", mdim
      endif
      print *,cret

!   ** Combien de segments et de triangles                   **
      if (cret.eq.0) then
         nse2 = 0
         call efnema(fid,maa,MED_CONN,MED_ARETE,MED_SEG2,MED_DESC,        &
     &        nse2,cret)   
      endif
      print *,cret

      if (cret.eq.0) then
         ntr3 = 0
         call efnema(fid,maa,MED_CONN,MED_MAILLE,MED_TRIA3,MED_DESC,        &
     &        ntr3,cret)   
      endif
      print *,cret

      if (cret.eq.0) then
         print *,"Nombre de MED_SEG2 : ",nse2," - nombre de MED_TRIA3 : ",ntr3 
      endif

!   ** Allocations memoire                                 **
      tse2 = 2
      allocate ( se2(tse2*nse2), nomse2(nse2),numse2(nse2), nufase2(nse2),STAT=ret )
!      print *,ret

      ttr3 = 3
      allocate ( tr3(ntr3*ttr3), nomtr3(ntr3), numtr3(ntr3),nufatr3(ntr3),STAT=ret )
!      print *,ret


!   ** Lecture de la connectivite des segments avec profil           **   
      if (cret.eq.0) then
         call efconl(fid,maa,mdim,se2,MED_NO_INTERLACE,profil,2,MED_ARETE, &
     &               MED_SEG2,MED_DESC,cret)
      endif
      print *,cret
      print *,se2

!   ** Lecture (optionnelle) des noms des segments         **
      if (cret.eq.0) then
         call efnoml(fid,maa,nomse2,nse2,MED_ARETE,         &
     &               MED_SEG2,ret)
      endif
    
      if (ret <0) then
         inoele = .FALSE.
      else
         inoele = .TRUE.
      endif

!  ** Lecture (optionnelle) des numeros des segments       **
      if (cret.eq.0) then
         call efnuml(fid,maa,numse2,nse2,MED_ARETE,MED_SEG2,ret)
     endif

     if (ret <0) then
        inuele = .FALSE.
     else
        inuele = .TRUE.
     endif

!  ** Lecture des numeros des familles des segments         **
     if (cret.eq.0) then
         call effaml(fid,maa,nufase2,nse2,MED_ARETE,MED_SEG2,cret)
      endif
      print *,cret

!  ** Lecture de la connectivite des triangles sans profil **
      if (cret.eq.0) then
         call efconl(fid,maa,mdim,tr3,MED_NO_INTERLACE,profil,0,MED_MAILLE, &
     &               MED_TRIA3,MED_DESC,cret)
      endif
      print *,cret

!  ** Lecture (optionnelle) des noms des triangles          **
      if (cret.eq.0) then
         call efnoml(fid,maa,nomtr3,ntr3,MED_MAILLE,         &
     &               MED_TRIA3,ret)
      endif
    
      if (ret <0) then
         inoele = .FALSE.
      else
         inoele = .TRUE.
      endif
      print *,cret

!  ** Lecture (optionnelle) des numeros des segments       **
      if (cret.eq.0) then
         call efnuml(fid,maa,numtr3,ntr3,MED_MAILLE,MED_TRIA3,ret)
     endif

     if (ret <0) then
        inuele = .FALSE.
     else
        inuele = .TRUE.
     endif
     print *,cret

!  ** Lecture des numeros des familles des segments         **
     if (cret.eq.0) then
         call effaml(fid,maa,nufatr3,ntr3,MED_MAILLE,MED_TRIA3,cret)
      endif
      print *,cret

!  ** Fermeture du fichier                                           **
     call efferm (fid,cret)
     print *,cret
 
!  ** Affichage des resulats                                         **
     if (cret.eq.0) then
        
        print *,"Connectivite des segments : "
        print *, se2
        
        if (inoele) then
           print *,"Noms des segments :"
           print *,nomse2
        endif
        
        if (inuele) then
           print *,"Numeros des segments :"
           print *,numse2
        endif
        
        print *,"Numeros des familles des segments :"
        print *,nufase2
        
        print *,"Connectivite des triangles :"
        print *,tr3
        
        if (inoele) then
           print *,"Noms des triangles :"
           print *,nomtr3
        endif
        
        if (inuele) then
           print *,"Numeros des triangles :"
           print *,numtr3
        endif
        
        print *,"Numeros des familles des triangles :"
        print *,nufatr3
        
     endif

!  ** Nettoyage memoire                                          **
      deallocate (se2,nomse2,numse2,nufase2,tr3,nomtr3,numtr3,nufatr3)

!  ** Code retour
      call efexit(cret)

    end program test7

