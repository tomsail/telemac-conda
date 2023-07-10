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
      include 'med.hf90'
!     
!           
      integer*8 fid
      integer cret, ret

      integer nse2
      integer,     allocatable, dimension (:) :: se2,se21
      character*16, allocatable, dimension (:) :: nomse2
      integer,     allocatable, dimension (:) :: numse2,nufase2
 
      integer ntr3
      integer,     allocatable, dimension (:) :: tr3
      character*16, allocatable, dimension (:) :: nomtr3
      integer,     allocatable, dimension (:) :: numtr3,nufatr3
   
!     ** nom du maillage de longueur maxi MED_TAILLE_NOM    **
      character*64  :: maa
      character*200 :: desc
      integer       :: mdim,edim,nstep,stype,atype
      logical inoele,inuele
      integer, parameter :: profil (2) = (/ 2,3 /) 
      integer type
      integer tse2,ttr3, i
      character*16 nomcoo(2)
      character*16 unicoo(2)
      character*16 dtunit
      integer :: chgt,tsf
      integer flta(1)
      integer*8 flt(1)

!   ** Ouverture du fichier test6.med en lecture seule       **
      call mfiope(fid,'test6.med',MED_ACC_RDONLY, cret)     
      print *,cret

!   ** Lecture des infos concernant le premier maillage      **
      if (cret.eq.0) then
         call mmhmii(fid,1,maa,edim,mdim,type,desc,dtunit,stype,nstep,atype,nomcoo,unicoo,cret)
         print *,"Maillage de nom : ",maa," et de dimension :", mdim
      endif
      if (cret.ne.0) then
         call efexit(-1)
      endif
!   ** Combien de segments et de triangles                   **
      if (cret.eq.0) then
         nse2 = 0
         call mmhnme(fid,maa,MED_NO_DT,MED_NO_IT,MED_DESCENDING_EDGE,MED_SEG2,MED_CONNECTIVITY,MED_DESCENDING,chgt,tsf,nse2,cret)   
      endif
      if (cret.ne.0) then
         call efexit(-1)
      endif

      if (cret.eq.0) then
         ntr3 = 0
         call mmhnme(fid,maa,MED_NO_DT,MED_NO_IT,MED_CELL,MED_TRIA3,MED_CONNECTIVITY,MED_DESCENDING,chgt,tsf,ntr3,cret)  
      endif
      if (cret.ne.0) then
         call efexit(-1)
      endif

      if (cret.eq.0) then
         print *,"Nombre de MED_SEG2 : ",nse2," - nombre de MED_TRIA3 : ",ntr3 
      endif

!   ** Allocations memoire                                 **
      tse2 = 2
      allocate (se2(tse2*nse2),se21(tse2*nse2),nomse2(nse2),numse2(nse2), nufase2(nse2),STAT=ret )
      se2(:)=0; se21(:)=0
!      print *,ret

      ttr3 = 3
      allocate (tr3(ntr3*ttr3), nomtr3(ntr3), numtr3(ntr3),nufatr3(ntr3),STAT=ret )
      tr3(:)=0
!      print *,ret


!   ** Lecture de la connectivite des segments           **   
      if (cret.eq.0) then
	call mmhcyr(fid,maa,MED_NO_DT,MED_NO_IT,MED_DESCENDING_EDGE,MED_SEG2,MED_DESCENDING,MED_FULL_INTERLACE,se2,cret)
      endif
      if (cret.ne.0) then
         call efexit(-1)
      endif
      print *,se2

!    ** Lecture de de la composante 2 de la connectivite des segments           **  
!    ** On cree un filtre  associe
     if (cret .eq. 0) then
        call mfrall(1,flt,cret)
     endif
     if (cret.ne.0) then
        call efexit(-1)
     endif

!    ** on initialise le filtre pour lire uniquement la deuxième composante.
     if (cret .eq. 0) then
        call mfrcre(fid,nse2,1,edim,2,MED_FULL_INTERLACE,MED_GLOBAL_STMODE, &
                    MED_NO_PROFILE,MED_UNDEF_SIZE,flta,flt(1),cret)
     endif
     if (cret.ne.0) then
        call efexit(-1)
     endif

!   ** Lecture des composantes n°2 des segments
     if (cret.eq.0) then
	call mmhyar(fid,maa,MED_NO_DT,MED_NO_IT,MED_DESCENDING_EDGE,MED_SEG2,MED_DESCENDING, &
                    flt(1),se21,cret)
     endif
     if (cret.ne.0) then
        call efexit(-1)
     endif
     print *,se21

!   ** On desalloue le filtre
     if (cret .eq. 0) then
        call mfrdea(1,flt,cret)
     endif
     if (cret.ne.0) then
        call efexit(-1)
     endif

!   ** Lecture (optionnelle) des noms des segments         **
      if (cret.eq.0) then
         call mmhear(fid,maa,MED_NO_DT,MED_NO_IT,MED_DESCENDING_EDGE,MED_SEG2,nomse2,cret) 
      endif
    
      if (ret <0) then
         inoele = .FALSE.
      else
         inoele = .TRUE.
      endif

!  ** Lecture (optionnelle) des numeros des segments       **
      if (cret.eq.0) then
         call mmhenr(fid,maa,MED_NO_DT,MED_NO_IT,MED_DESCENDING_EDGE,MED_SEG2,numse2,cret)
     endif

     if (ret <0) then
        inuele = .FALSE.
     else
        inuele = .TRUE.
     endif

!  ** Lecture des numeros des familles des segments         **
     if (cret.eq.0) then
        call mmhfnr(fid,maa,MED_NO_DT,MED_NO_IT,MED_DESCENDING_EDGE,MED_SEG2,nufase2,cret)
      endif
     if (cret.ne.0) then
        call efexit(-1)
     endif

!  ** Lecture de la connectivite des triangles sans profil **
      if (cret.eq.0) then
	call mmhcyr(fid,maa,MED_NO_DT,MED_NO_IT,MED_CELL,MED_TRIA3,MED_DESCENDING,MED_NO_INTERLACE,tr3,cret)
      endif
     if (cret.ne.0) then
        call efexit(-1)
     endif

!  ** Lecture (optionnelle) des noms des triangles          **
      if (cret.eq.0) then
         call mmhear(fid,maa,MED_NO_DT,MED_NO_IT,MED_CELL,MED_TRIA3,nomtr3,cret) 
      endif
    
      if (ret <0) then
         inoele = .FALSE.
      else
         inoele = .TRUE.
      endif
      print *,cret

!  ** Lecture (optionnelle) des numeros des segments       **
      if (cret.eq.0) then
        call mmhenr(fid,maa,MED_NO_DT,MED_NO_IT,MED_CELL,MED_TRIA3,numtr3,cret)
     endif

     if (ret <0) then
        inuele = .FALSE.
     else
        inuele = .TRUE.
     endif
     print *,cret

!  ** Lecture des numeros des familles des segments         **
     if (cret.eq.0) then
        call mmhfnr(fid,maa,MED_NO_DT,MED_NO_IT,MED_CELL,MED_TRIA3,nufatr3,cret)
      endif
      print *,cret

!  ** Fermeture du fichier                                           **
     call mficlo(fid,cret)
     if (cret.ne.0) then
        call efexit(-1)
     endif
 
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
      deallocate (se2,se21,nomse2,numse2,nufase2,tr3,nomtr3,numtr3,nufatr3)

!  ** Code retour
      call efexit(cret)

    end program test7

