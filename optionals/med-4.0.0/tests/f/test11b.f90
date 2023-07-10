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
! * - Nom du fichier : test11.f90
! *
! * - Description : lecture de champs de resultats MED 
! *
! ***************************************************************************** 

program test11

  implicit none
  include 'med.hf90'


  integer*8     fid
  integer       cret,ret,lret,retmem
  integer       USER_INTERLACE,USER_MODE
  character*64  :: maa,nomcha,pflname,nomlien,locname
  character*200 desc
  character*255 argc
  character*16, allocatable, dimension(:) :: comp,unit
  character*16  dtunit
  integer       mdim,ncomp,ncha,npro,nln,pflsize,nval
  integer,      allocatable, dimension(:) :: pflval
  integer       ngauss,nloc
  integer       t1,t2,t3,typcha,type,type_geo
  real*8,       allocatable, dimension(:) :: refcoo, gscoo, wg
  character*255 lien
  integer       i,j
  integer       getFieldsOn
  integer nstep, stype, atype,sdim
  character*16 nomcoo(3)   
  character*16 unicoo(3)
  integer lmesh, ncst
  character*64  :: giname, isname
  integer nsmc, sgtype

  parameter (USER_INTERLACE = MED_FULL_INTERLACE)
  parameter (USER_MODE = MED_COMPACT_STMODE)

  cret=0;ret=0;lret=0;retmem=0
  print *,"Indiquez le fichier med a decrire : "
  !!read(*,'(A)') argc
  argc="test10.med"

  !  ** ouverture du fichier **
  call mfiope(fid,argc,MED_ACC_RDONLY, ret)
  if (ret .ne. 0) call efexit(-1)

  !  ** info sur le premier maillage **
  if (ret.eq.0) then
     call mmhmii(fid,1,maa,sdim,mdim,type,desc,dtunit,stype,nstep,atype,nomcoo,unicoo,ret)
  endif
  if (ret.ne.0) then
     print *, "Erreur a la lecture des informations sur le maillage : ", &
          & maa,mdim,type,desc
     call efexit(-1)
  endif

  write (*,'(/A,A,A,I1)') "Maillage de nom |",TRIM(maa),"| et de dimension ",mdim

  !  ** combien de champs dans le fichier **
  call mfdnfd(fid,ncha,ret)
  if (ret.ne.0) then
     print *, "Impossible de lire le nombre de champs : ",ncha
     call efexit(-1)
  endif

  write (*,'(A,I1/)') "Nombre de champs : ",ncha


  ! ** lecture de tous les champs associes a <maa> **
  do i=1,ncha
     lret = 0
     write(*,'(A,I5)') "- Champ numero : ",i

     ! ** combien de composantes **
     call mfdnfc(fid,i,ncomp,ret)
     !   print *,ncomp,ret
     if (ret.ne.0) then
        print *, "Erreur a la lecture du nombre de composantes : ",ncomp
        cret = -1
     endif

     ! ** allocation memoire de comp et unit **
     allocate(comp(ncomp),unit(ncomp),STAT=retmem)
     if (retmem .ne. 0) then
        print *, "Erreur a l'allocation mémoire de comp et unit : "
        call efexit(-1)
     endif

     ! ** Info sur les champs
     call mfdfdi(fid,i,nomcha,maa,lmesh,typcha,comp,unit,dtunit,ncst,ret)
     if (ret .ne. 0) then
        print *, "Erreur a la demande d'information sur les champs : ",nomcha,typcha,comp,unit,ncomp,ncst
        cret = -1
        continue
     endif

     write(*,'(/5X,A,A)') 'Nom du champ  : ', TRIM(nomcha)
     write(*,'(/5X,A,A)') 'Nom du maillage : ',TRIM(maa)
     write(*,'(5X,A,I5)') 'Type du champ : ', typcha
     do j=1,ncomp
        write(*,'(5X,A,I1,A,A,A,A)') 'Composante ',j,'  : ',TRIM(comp(j)),' ',TRIM(unit(j))
     enddo
     write(*,'(5X,A,I1)') 'Nombre de pas de temps = ',ncst
     print *,""

     deallocate(comp,unit)
 
     lret = getFieldsOn(fid, nomcha, typcha, ncomp, MED_NODE, USER_INTERLACE, ncst)
     ! print *,lret

     if (lret .eq. 0) then 
        lret = getFieldsOn(fid, nomcha, typcha, ncomp, MED_CELL, USER_INTERLACE, ncst)
     else 
        print *, "Erreur a la lecture des champs aux noeuds "; cret = -1; continue 
     endif

     if (lret .eq. 0) then
        lret = getFieldsOn(fid, nomcha, typcha, ncomp, MED_DESCENDING_FACE,USER_INTERLACE, ncst)
     else 
        print *,"Erreur a la lecture des champs aux mailles "; cret = -1; continue
     endif

     if (lret .eq. 0) then
        lret = getFieldsOn(fid, nomcha, typcha, ncomp, MED_DESCENDING_EDGE,USER_INTERLACE, ncst)
     else 
        print *,"Erreur a la lecture des champs aux faces "; cret = -1; continue
     endif

     if (lret .eq. 0) then
        lret = getFieldsOn(fid, nomcha, typcha, ncomp, MED_NODE_ELEMENT,USER_INTERLACE, ncst)
     else 
        print *,"Erreur a la lecture des champs aux aretes "; cret = -1; continue
     endif

     if  (lret .ne. 0) then
        print *,"Erreur a la lecture des champs aux noeuds des mailles "; cret = -1
     endif

  enddo


  call mpfnpf(fid,nval,ret)
  write (*,'(5X,A,I2)') 'Nombre de profils stockés : ', nval

  if (nval .gt. 0 ) then
     do i=1,nval
           call mpfpfi(fid,i,pflname,nval,ret)
        write (*,'(5X,A,I2,A,A,A,I2)') 'Profil n ',i,' : ',pflname, ' et de taille',nval
     enddo
  endif


  !  ** Interrogation des liens **
  call mlnnln(fid,nln,ret)
  if (ret.ne.0) then
     print *,"Erreur a la lecture du nombre de liens : " & 
          & ,nln
     cret = -1; 
  else
     print *,""
     write (*,'(5X,A,I5)') "Nombre de liens stockes : ",nln;print *,"";print *,""
     do i=1,nln
        call mlnlni(fid, i, nomlien, nval, ret)
        if (ret.ne.0) then
           print *,"Erreur a la demande d'information sur le lien n° : ",i
           cret = -1;continue;
        endif
        write (*,'(5X,A,I4,A,A,A,I4)') "- Lien n°",i," de nom |",TRIM(nomlien),"| et de taille ",nval
        !! allocate
        lien = ""
        call mlnlir(fid,nomlien,lien,ret)
        if (ret.ne.0) then
           print *,"Erreur a la lecture du lien : ", lien,nval,nomlien
           ret = -1; 
        else
           write (*,'(5X,A,A,A)') "|",TRIM(lien),"|";print *,"";print *,""
        endif
        !!deallocate
     end do
  endif


  !  ** Interrogation des localisations des points de GAUSS **
  call mlcnlc(fid,nloc,ret)
  if (ret.ne.0) then
     print *,"Erreur a la lecture du nombre de points de Gauss : " & 
          & ,nloc
     cret = -1; 
  else  
     print *,"Nombre de localisations stockees : ",nloc;print *,"";print *,""
     do i=1,nloc
        call mlclci(fid, i, locname, type_geo, sdim, ngauss, giname, isname, nsmc, sgtype, ret)
        if (ret.ne.0) then
           print *,"Erreur a la demande d'information sur la localisation n° : ",i
           cret = -1;continue;
        endif
        write (*,'(5X,A,I4,A,A,A,I4,A,I4)') "- Loc n°",i," de nom |",TRIM(locname) &
             &,"| à",ngauss, " points d'intégration dans un espace de dimension ",sdim
        t1 = MOD(type_geo,100)*sdim
        t2 = ngauss*sdim
        t3 = ngauss
        allocate(refcoo(t1),STAT=retmem)
        if (retmem .ne. 0) then
           print *, "Erreur a l'allocation mémoire de refcoo : "
           call efexit(-1)
        endif;
        allocate(gscoo(t2),STAT=retmem)
        if (retmem .ne. 0) then
           print *, "Erreur a l'allocation mémoire de gscoo : "
           call efexit(-1)
        endif;
        allocate(wg(t3),STAT=retmem)
        if (retmem .ne. 0) then
           print *, "Erreur a l'allocation mémoire de wg : "
           call efexit(-1)
        endif;
        call mlclor(fid, locname,USER_INTERLACE,refcoo,gscoo,wg, ret )
        if (ret.ne.0) then
           print *,"Erreur a la lecture  des valeurs de la localisation : " & 
                & ,locname
           cret = -1;
        else
           write (*,'(5X,A,I4)') "Coordonnees de l'element de reference de type ",type_geo
           do j=1,t1
              write (*,'(5X,E20.8)') refcoo(j)
           enddo
           print *,""
           write (*,'(5X,A)') "Localisation des points de GAUSS : "
           do j=1,t2
              write (*,'(5X,E20.8)') gscoo(j)
           enddo
           print *,""
           write (*,'(5X,A)') "Poids associes aux points de GAUSS "
           do j=1,t3
              write (*,'(5X,E20.8)') wg(j)
           enddo
           print *,""
        endif
        deallocate(refcoo)
        deallocate(gscoo)
        deallocate(wg)  
     enddo
  endif

  call mficlo(fid,ret)
  !print *,ret

  call efexit(cret)

end program test11


integer function getFieldsOn(fid, nomcha, typcha, ncomp, entite, stockage, ncst)
  implicit none
  include 'med.hf90'

  integer*8    fid
  integer      ::typcha,ncomp,entite,stockage, ncst
  character(LEN=*)  nomcha

  integer      :: itm,j,k,l,m,n,nb_geo,cret,ret,retmem,nvl,nref
  integer      :: nbpdtnor,pflsize,ngauss,ngroup,nent,nprofile
  integer,     allocatable, dimension(:) :: pflval
  integer,     allocatable, dimension(:) :: vale
  integer      :: numdt,numo,lnsize,nbrefmaa
  real*8,      allocatable, dimension(:) :: valr
  real*8       dt
  logical      local
  character*64 :: pflname,locname,maa_ass,mname
  character*16 :: dt_unit
  character*255:: lien
  integer       USER_MODE
  integer      :: nmesh,lmesh, mnumdt, mnumit

  integer,pointer,dimension(:) :: type_geo
  integer,target  :: typ_noeud(1) = (/ MED_NONE /)

  integer :: MY_NOF_CELL_TYPE = 17 
  integer :: MY_NOF_DESCENDING_FACE_TYPE =  5
  integer :: MY_NOF_DESCENDING_EDGE_TYPE =  2

  integer,target  :: typmai(17) =  (/ MED_POINT1,MED_SEG2,   &
       &  MED_SEG3,MED_TRIA3,     &
       &  MED_QUAD4,MED_TRIA6,    &
       &  MED_QUAD8,MED_TETRA4,   &
       &  MED_PYRA5,MED_PENTA6,   &
       &  MED_HEXA8,MED_TETRA10,  &
       &  MED_PYRA13,MED_PENTA15,  &
       &  MED_HEXA20,MED_POLYGON,&
       &  MED_POLYHEDRON/)

  integer,target :: typfac(5) = (/MED_TRIA3,MED_TRIA6,       &
       &  MED_QUAD4,MED_QUAD8,MED_POLYGON/)
  integer,target ::typare(2) = (/MED_SEG2,MED_SEG3/)

  character(LEN=15),pointer,dimension(:) :: AFF
  character(LEN=15),target,dimension(17) :: FMED_GEOMETRIE_MAILLE_AFF = (/&
       &  "MED_POINT1     ",&
       &  "MED_SEG2       ",&
       &  "MED_SEG3       ",&
       &  "MED_TRIA3      ",&
       &  "MED_QUAD4      ",&
       &  "MED_TRIA6      ",&
       &  "MED_QUAD8      ",&
       &  "MED_TETRA4     ",&
       &  "MED_PYRA5      ",&
       &  "MED_PENTA6     ",&
       &  "MED_HEXA8      ",&
       &  "MED_TETRA10    ",&
       &  "MED_PYRA13     ",&
       &  "MED_PENTA15    ",&
       &  "MED_HEXA20     ",&
       &  "MED_POLYGON    ",&
       &  "MED_POLYHEDRON "  /)

  character(LEN=15),target,dimension(5) :: FMED_GEOMETRIE_FACE_AFF = (/&
       &  "MED_TRIA3      ",&
       &  "MED_TRIA6      ",&
       &  "MED_QUAD4      ",&
       &  "MED_QUAD8      ",&
       &  "MED_POLYGON    " /)

  character(LEN=15),target,dimension(2) :: FMED_GEOMETRIE_ARETE_AFF = (/&
       &  "MED_SEG2       ",&
       &  "MED_SEG3       " /)   

  character(LEN=15),target,dimension(1) :: FMED_GEOMETRIE_NOEUD_AFF = (/ &
       &  "(AUCUN)        "/)  


  character(LEN=20),target,dimension(0:4) :: FMED_ENTITE_MAILLAGE_AFF =(/ &
       &  "MED_CELL            ", &
       &  "MED_DESCENDING_FACE ", &
       &  "MED_DESCENDING_EDGE ", &
       &  "MED_NODE            ", &
       &  "MED_NODE_ELEMENT    "/)

  parameter (USER_MODE = MED_COMPACT_STMODE )

  !!  write (*,'(A0)')  FMED_GEOMETRIE_NOEUD_AFF(1)
  !!  write (*,'(A0)')  FMED_GEOMETRIE_MAILLE_AFF(1)
  !!  write (*,'(A0)')  FMED_GEOMETRIE_FACE_AFF(1)
  !!  write (*,'(A0)')  FMED_GEOMETRIE_ARETE_AFF(1)

  locname=''
  nbpdtnor=0;pflsize=0;ngauss=0;nent=0
  numdt = 0;numo=0;retmem=0
  cret=0;ret=0

  nullify(type_geo)
  nullify(AFF)


  select case (entite)
  case (MED_NODE)
     type_geo => typ_noeud
     nb_geo   = 1
     AFF      => FMED_GEOMETRIE_NOEUD_AFF
  case (MED_CELL)  
     type_geo => typmai
     nb_geo   = 17
     AFF      => FMED_GEOMETRIE_MAILLE_AFF
  case (MED_NODE_ELEMENT)  
     type_geo => typmai
     nb_geo   = 17
     AFF      => FMED_GEOMETRIE_MAILLE_AFF
  case (MED_DESCENDING_FACE) 
     type_geo => typfac;
     nb_geo   = 5
     AFF      =>  FMED_GEOMETRIE_FACE_AFF
  case  (MED_DESCENDING_EDGE)
     type_geo => typare
     nb_geo   = MY_NOF_DESCENDING_EDGE_TYPE
     AFF      =>  FMED_GEOMETRIE_ARETE_AFF
  end select

  do k=1,nb_geo

     ! ** Combien de (PDT,NOR) a lire **
     nbpdtnor = ncst
     if(nbpdtnor < 1 ) continue

     do j=1,ncst

        call mfdoci(fid,nomcha,j,numdt,numo,dt, nmesh, mname, lmesh, mnumdt, mnumit, ret)
        ! print *,'ret=',ret
        if (ret.ne.0) then
           print *, "Erreur a la demande d'information sur (pdt,nor) : " &
                & ,nomcha,entite, numdt, numo, dt
           cret = -1
        end if

        do itm=1,nmesh

           call mfdonp(fid,nomcha,numdt,numo,entite,type_geo(k),itm,mname,pflname,locname,nprofile,ret)
           ! print *,'ret=',ret
           if (ret.ne.0) then
              print *, "Erreur a la lecture du nombre de profil : " &
                   & ,nomcha,entite, type_geo(k),numdt, numo
              cret = -1
              call efexit(cret)
           end if
           
           do l=1,nprofile
              
              ! ** Combien de valeurs à lire ? **
              call mfdonv(fid,nomcha,numdt,numo,entite,type_geo(k),mname,l, &
                     &    USER_MODE,pflname,pflsize,locname,ngauss,nent,ret)

              ! print *,'ret=',ret
              if (ret.ne.0) then
                 print *,"Erreur a la lecture du nombre de valeurs du champ : " & 
                      & ,nomcha,entite,type_geo(k), &
                      & numdt, numo
                 cret = -1; continue
              endif
              !write(*,'(5X,A,I5,A)')  'Il y a ', nent ,' valeurs a lire '
              
              write(*,'(5X,A,I2,A,I2,A,I2,A,E10.5,A)')  'Séquence de calcul n° ',l,' (',numdt,',',numo,'), dt=(',dt,')'
              write(*,'(5X,A,I5,A,I2,A,A,A,A,A,A,I2,A,A)') &
                   & 'Il y a ',nent,' valeurs en mode ',USER_MODE, &
                   & '. Chaque entite ',TRIM(FMED_ENTITE_MAILLAGE_AFF(entite)), &
                   & ' de type geometrique ',TRIM(AFF(k)),' associes au profil |',&
                   & TRIM(pflname)//'| a ',ngauss,' valeur(s) par entité une localization de nom |',TRIM(locname)//'|'
              print *,'Le maillage associe est ', mname 
              
              ! **Lecture des valeurs du champ **
              if (typcha .eq. MED_FLOAT64) then
                 allocate(valr(ncomp*nent*ngauss),STAT=retmem)
                 
                 call mfdorr(fid,nomcha,numdt,numo,entite,type_geo(k),mname,USER_MODE, &
                      &      pflname,stockage,MED_ALL_CONSTITUENT,valr,ret)
                 ! print *,'ret=',ret
                 if (ret.ne.0) then
                    print *,"Erreur a la lecture des valeurs du champ : ", &
                         &  nomcha,valr,stockage,MED_ALL_CONSTITUENT, &
                         &  pflname,USER_MODE,entite,type_geo(k),numdt,numo
                    cret = -1;
                    call efexit(cret)
                 endif
              else	  
                 allocate(vale(ncomp*nent*ngauss),STAT=retmem)
                 
                 call mfdoir(fid,nomcha,numdt,numo,entite,type_geo(k),mname,USER_MODE, &
                      &      pflname,stockage,MED_ALL_CONSTITUENT,vale,ret)
                 ! print *,'ret=',ret
                 if (ret.ne.0) then
                    print *,"Erreur a la lecture des valeurs du champ : ",&
                         & nomcha,vale,stockage,MED_ALL_CONSTITUENT, &
                         & pflname,USER_MODE,entite,type_geo(k),numdt,numo
                    cret = -1;
                 endif

              endif

              if (ngauss .gt. 1 ) then
                 write (*,'(5X,A,A,A)') "- Modèle de localisation des ", &
                      & "points de Gauss de nom ", TRIM(locname)
              end if
              
              if ( entite .eq. MED_NODE_ELEMENT ) then
                 ngroup = MOD(type_geo(k),100)
              else
                 ngroup = ngauss
              end if
              
              select case (stockage) 
              case (MED_FULL_INTERLACE)  
                 write(*,'(5X,A)') "- Valeurs :";  write(*,'(5X,A)') ""
                 do m=0,nent-1
                    write(*,*) "|"
                    do n=0,(ngroup*ncomp-1)
                       if (typcha .eq. MED_FLOAT64) then
                          write (*,'(1X,E20.5,1X)') valr( m*ngroup*ncomp+n +1 )
                       else
                          write (*,'(1X,I8,1X)') vale( m*ngroup*ncomp+n +1 )
                       end if
                    enddo
                 enddo
              case (MED_NO_INTERLACE)
                 write(*,'(5X,A)') "- Valeurs :";  write(*,'(5X,A)') ""
                 do m=0,ncomp-1
                    write(*,*) "|"
                    do n=0,nent-1 
                       if (typcha .eq. MED_FLOAT64) then
                          write (*,'(1X,E20.5,1X)') valr(m*nent+n +1)
                       else
                          write (*,'(1X,I8,1X)') vale(m*nent+n +1)
                       endif
                    enddo
                 enddo
              end select
              
              write(*,*) "|"
              if (typcha .eq. MED_FLOAT64) then
                 deallocate(valr)
              else
                 deallocate(vale)
              endif

              !* Profils
              if (pflname .eq. MED_NO_PROFILE) then
                 !write(*,'(5X,A)') 'Pas de profil'
              else
                 write(*,'(5X,A,A)') 'Profil :',pflname
                 call mpfpsn(fid,pflname,pflsize,ret)
                 if (ret .ne. 0) then
                    print *,"Erreur a la lecture du nombre de valeurs du profil : ", &
                         & pflname,pflsize
                    cret = -1;continue
                 endif
                 write(*,'(5X,A,I5)') 'Taille du profil : ',pflsize
                 
                 ! ** allocation memoire de pflval **
                 allocate(pflval(pflsize),STAT=retmem)
                 if (retmem .ne. 0) then
                    print *, "Erreur a l'allocation mémoire de pflsize : "
                    call efexit(-1)
                 endif
                 
                 call mpfprr(fid,pflname,pflval,ret)
                 if (cret .ne. 0) write(*,'(I1)') cret
                 if (ret .ne. 0) then
                    print *,"Erreur a la lecture du profil : ", &
                         & pflname,pflval
                    cret = -1;continue
                 endif
                 write(*,'(5X,A)') 'Valeurs du profil : '
                 do m=1,pflsize
                    write (*,'(5X,I6)') pflval(m)          
                 enddo
                 
                 deallocate(pflval)

              endif

           enddo
           
        enddo
  
     enddo
     
  enddo


  print *,""
  getFieldsOn=ret

end function getFieldsOn
