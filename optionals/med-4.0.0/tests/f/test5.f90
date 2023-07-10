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
      include 'med.hf90'
!     
!     
      integer cret, ret
      integer*8 fid

      
!     ** la dimension du maillage et de l'espace de calcul**
      integer mdim, sdim
!     ** nom du maillage de longueur maxi MED_SIZE_NAME  **
      character*64 maa
      character*200 desc
!     ** le nombre de noeuds                              **
      integer nnoe 
!     ** table des coordonnees                            **
      real*8, allocatable, dimension (:) ::  coo,coo1
!     ** tables des noms et des unites des coordonnees    **
      character*16 nomcoo(2)   
      character*16 unicoo(2)
!     ** tables des noms, numeros, numeros de familles des noeuds  **
!     autant d'elements que de noeuds - les noms ont pout longueur **
!     MED_SNAME_SIZE=16                                            
      character*16, allocatable, dimension (:) :: nomnoe
      integer,     allocatable, dimension (:) :: numnoe
      integer,     allocatable, dimension (:) :: nufano
      integer i
      logical inonoe,inunoe
      integer type,chgt,tsf
      integer flta(1)
      integer*8 flt(1)
      character(16)  :: dtunit
      integer nstep, stype, atype
      integer swm

!     Ouverture du fichier en lecture seule             **
      call mfiope(fid,'test4.med',MED_ACC_RDONLY, cret)
      print *,cret

!   ** Lecture des infos concernant le premier maillage **
      if (cret.eq.0) then
         call mmhmii(fid,1,maa,sdim,mdim,type,desc,dtunit,stype,nstep,atype,nomcoo,unicoo,cret)
      endif
      if (cret.ne.0) then
         call efexit(-1)
      endif


!   ** Combien de noeuds a lire  **
      if (cret.eq.0) then
         nnoe = 0
         call mmhnme(fid,maa,MED_NO_DT,MED_NO_IT,MED_NODE,MED_NONE,MED_COORDINATE,MED_NO_CMODE,chgt,tsf,nnoe,cret)   
      endif
      print *,cret,' Nombre de noeuds : ',nnoe
      if (cret.ne.0) then
         call efexit(-1)
      endif

       
!   ** Allocations memoires :  **
!   ** table des coordonnees   **
!     profil : (dimension * nombre de noeuds ) **
!   ** table des des numeros, des numeros de familles des noeuds
!   ** table des noms des noeuds ** 
      
      allocate( coo(nnoe*sdim),coo1(nnoe*sdim),numnoe(nnoe),nufano(nnoe),nomnoe(nnoe),STAT=ret )
      print *,ret
      coo1(:)=0.0

!   ** Lecture des composantes des coordonnees des noeuds  avec et sans filtre     **
      if (cret.eq.0) then
         call mmhcor(fid,maa,MED_NO_DT,MED_NO_IT,MED_FULL_INTERLACE,coo,cret) 
      endif
      print *,'Lecture des toutes les composantes des coordonnees : '
      print *,coo
      if (cret.ne.0) then
         call efexit(-1)
      endif

!    ** On cree un filtre  
     if (cret .eq. 0) then
        call mfrall(1,flt,cret)
     endif
     if (cret.ne.0) then
        call efexit(-1)
     endif

     if (cret .eq. 0) then
        call mfrcre(fid,nnoe,1,sdim,2,MED_FULL_INTERLACE,MED_GLOBAL_STMODE, &
                    MED_NO_PROFILE,MED_UNDEF_SIZE,flta,flt(1),cret)
     endif
     if (cret.ne.0) then
        call efexit(-1)
     endif

!   ** Lecture des composantes n°2 des coordonnees des noeuds
      if (cret.eq.0) then
         call mmhcar(fid,maa,MED_NO_DT,MED_NO_IT,flt(1),coo1,cret) 
      endif
      print *,'Lecture de la composante numero 2 des coordonnees : '
      print *,coo1

!   ** On desalloue le filtre
     if (cret .eq. 0) then
        call mfrdea(1,flt,cret)
     endif
     if (cret.ne.0) then
        call efexit(-1)
     endif

 
!   ** Lecture des noms des noeuds (optionnel dans un fichier MED)  **
      if (cret.eq.0) then
         call mmhear(fid,maa,MED_NO_DT,MED_NO_IT,MED_NODE,MED_NONE,nomnoe,cret) 
      endif
    
      if (ret <0) then
         inonoe = .FALSE.
      else
         inonoe = .TRUE.
      endif

!  ** Lecture des numeros des noeuds (optionnel dans un fichier MED) **
      if (cret.eq.0) then
         call mmhenr(fid,maa,MED_NO_DT,MED_NO_IT,MED_NODE,MED_NONE,numnoe,cret)
      endif
      if (ret <0) then
         inunoe = .FALSE.
      else
         inunoe = .TRUE.
      endif
      
!   ** Lecture des numeros de familles des noeuds                  **	   
      if (cret.eq.0) then
         call mmhfnr(fid,maa,MED_NO_DT,MED_NO_IT,MED_NODE,MED_NONE,nufano,cret)
      endif
      print *,cret
 

!   ** Fermeture du fichier
      call mficlo(fid,cret)
      if (cret.ne.0) then
         call efexit(-1)
      endif
      

!  ** Affichage des resulats                                         **
      if (cret.eq.0) then

         
         print *,"Type de repere         : ", atype
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
      deallocate(coo,coo1,nomnoe,numnoe,nufano);


! **  Code retour
      call efexit(cret)
      
      end program test5






