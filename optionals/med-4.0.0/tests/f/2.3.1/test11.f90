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
  include 'med.hf'
  
  
  integer*8     fid
  integer       cret,ret,lret,retmem
  integer       USER_INTERLACE,USER_MODE
  character*32  :: maa,nomcha,pflname,nomlien,locname
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

  parameter (USER_INTERLACE = MED_FULL_INTERLACE)
  parameter (USER_MODE = MED_COMPACT )

  cret=0;ret=0;lret=0;retmem=0
  print *,"Indiquez le fichier med a decrire : "
  !!read(*,'(A)') argc
  argc="test10.med"

  !  ** ouverture du fichier **
  call efouvr(fid,argc,MED_LECTURE, ret)
  if (ret .ne. 0) call efexit(-1)

  !  ** info sur le premier maillage **
  call efmaai(fid,1,maa,mdim,type,desc,ret)
  if (ret.ne.0) then
     print *, "Erreur a la lecture des informations sur le maillage : ", &
          & maa,mdim,type,desc
     call efexit(-1)
  endif

  write (*,'(/A,A,A,I1)') "Maillage de nom |",TRIM(maa),"| et de dimension ",mdim

  !  ** combien de champs dans le fichier **
  call efncha(fid,0,ncha,ret)
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
     call efncha(fid,i,ncomp,ret)
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
     call efchai(fid,i,nomcha,typcha,comp,unit,ncomp,ret)
     if (ret .ne. 0) then
        print *, "Erreur a la demande d'information sur les champs : ",nomcha,typcha,comp,unit,ncomp
        cret = -1
        continue
     endif

     write(*,'(/5X,A,A)') 'Nom du champ  : ', TRIM(nomcha)
     write(*,'(5X,A,I5)') 'Type du champ : ', typcha
     do j=1,ncomp
        write(*,'(5X,A,I1,A,A,A,A)') 'Composante ',j,'  : ',TRIM(comp(j)),' ',TRIM(unit(j))
     enddo

     deallocate(comp,unit)
     print *,""

     lret = getFieldsOn(fid, nomcha, typcha, ncomp, MED_NOEUD, USER_INTERLACE )

     if (lret .eq. 0) then 
        lret = getFieldsOn(fid, nomcha, typcha, ncomp, MED_MAILLE, USER_INTERLACE )
     else 
        print *, "Erreur a la lecture des champs aux noeuds "; cret = -1; continue 
     endif

     if (lret .eq. 0) then
        lret = getFieldsOn(fid, nomcha, typcha, ncomp, MED_FACE,USER_INTERLACE)
     else 
        print *,"Erreur a la lecture des champs aux mailles "; cret = -1; continue
     endif

     if (lret .eq. 0) then
        lret = getFieldsOn(fid, nomcha, typcha, ncomp, MED_ARETE,USER_INTERLACE)
     else 
        print *,"Erreur a la lecture des champs aux faces "; cret = -1; continue
     endif

     if  (lret .ne. 0) then 
        print *,"Erreur a la lecture des champs aux aretes "; cret = -1
     endif

  enddo


  call efnpro(fid,nval,ret)
  write (*,'(5X,A,I2)') 'Nombre de profils stockés : ', nval

  if (nval .gt. 0 ) then
     do i=1,nval
        call efproi(fid,i,pflname,nval,ret)
        write (*,'(5X,A,I2,A,A,A,I2)') 'Profil n ',i,' : ',pflname, ' et de taille',nval
     enddo
  endif

  !  ** Interrogation des liens **
  call efnlie(fid,nln,ret)
  if (ret.ne.0) then
     print *,"Erreur a la lecture du nombre de liens : " & 
          & ,nln
     cret = -1; 
  else
     print *,""
     print *,"Nombre de liens stockes : ",nln;print *,"";print *,""
     do i=1,nln
        call efliei(fid, i, nomlien, nval, ret)
        if (ret.ne.0) then
           print *,"Erreur a la demande d'information sur le lien n° : ",i
           cret = -1;continue;
        endif
        write (*,'(5X,A,I4,A,A,A,I4)') "- Lien n°",i," de nom |",TRIM(nomlien),"| et de taille ",nval
        !! allocate
        lien = ""
        call efliel(fid,lien,nval,nomlien,ret)
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
  call efngau(fid,nloc,ret)
  if (ret.ne.0) then
     print *,"Erreur a la lecture du nombre de points de Gauss : " & 
          & ,nloc
     cret = -1; 
  else  
     print *,"Nombre de localisations stockees : ",nloc;print *,"";print *,""
     do i=1,nloc
        call efgaui(fid, i, locname, type_geo, ngauss, ret)
        if (ret.ne.0) then
           print *,"Erreur a la demande d'information sur la localisation n° : ",i
           cret = -1;continue;
        endif
        write (*,'(5X,A,I4,A,A,A,I4)') "- Loc n°",i," de nom |",TRIM(locname) &
             &,"| et nbr. de pts Gauss ",ngauss
        t1 = MOD(type_geo,100)*(type_geo/100)
        t2 = ngauss*(type_geo/100)
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
        call efgaul(fid, refcoo, gscoo, wg, USER_INTERLACE, locname, ret )
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

  call efferm (fid,ret)
  
  call efexit(cret)

end program test11


integer function getFieldsOn(fid, nomcha, typcha, ncomp, entite, stockage)
  implicit none
  include 'med.hf'
  
  integer*8    fid
  integer      ::typcha,ncomp,entite,stockage
  character(LEN=*)  nomcha

  integer      :: j,k,l,m,n,nb_geo,cret,ret,retmem,nvl,nref
  integer      :: nbpdtnor,pflsize,ngauss,nval
  integer,     allocatable, dimension(:) :: pflval
  integer,     allocatable, dimension(:) :: vale
  integer      :: numdt,numo,lnsize,nbrefmaa
  real*8,      allocatable, dimension(:) :: valr
  real*8       dt
  logical      local
  character*32 :: pflname,locname,maa_ass
  character*16 :: dt_unit
  character*255:: lien
  integer       USER_MODE

  integer,pointer,dimension(:) :: type_geo
  integer,target  :: typ_noeud(1) = (/ MED_NONE /)
  integer,target  :: typmai(MED_NBR_GEOMETRIE_MAILLE+2) =  (/ MED_POINT1,MED_SEG2,   &
       &  MED_SEG3,MED_TRIA3,     &
       &  MED_QUAD4,MED_TRIA6,    &
       &  MED_QUAD8,MED_TETRA4,   &
       &  MED_PYRA5,MED_PENTA6,   &
       &  MED_HEXA8,MED_TETRA10,  &
       &  MED_PYRA13,MED_PENTA15,  &
       &  MED_HEXA20,MED_POLYGONE,&
       &  MED_POLYEDRE/)

  integer,target :: typfac(MED_NBR_GEOMETRIE_FACE+1) = (/MED_TRIA3,MED_TRIA6,       &
       &  MED_QUAD4,MED_QUAD8,MED_POLYGONE/)
  integer,target ::typare(MED_NBR_GEOMETRIE_ARETE) = (/MED_SEG2,MED_SEG3/)
  
  character(LEN=12),pointer,dimension(:) :: AFF
  character(LEN=12),target,dimension(MED_NBR_GEOMETRIE_MAILLE+2) :: FMED_GEOMETRIE_MAILLE_AFF = (/&
       &  "MED_POINT1  ",&
       &  "MED_SEG2    ",&
       &  "MED_SEG3    ",&
       &  "MED_TRIA3   ",&
       &  "MED_QUAD4   ",&
       &  "MED_TRIA6   ",&
       &  "MED_QUAD8   ",&
       &  "MED_TETRA4  ",&
       &  "MED_PYRA5   ",&
       &  "MED_PENTA6  ",&
       &  "MED_HEXA8   ",&
       &  "MED_TETRA10 ",&
       &  "MED_PYRA13  ",&
       &  "MED_PENTA15 ",&
       &  "MED_HEXA20  ",&
       &  "MED_POLYGONE",&
       &  "MED_POLYEDRE"  /)

  character(LEN=12),target,dimension(MED_NBR_GEOMETRIE_FACE+1) :: FMED_GEOMETRIE_FACE_AFF = (/&
       &  "MED_TRIA3   ",&
       &  "MED_TRIA6   ",&
       &  "MED_QUAD4   ",&
       &  "MED_QUAD8   ",&
       &  "MED_POLYGONE" /)
  
  character(LEN=12),target,dimension(MED_NBR_GEOMETRIE_ARETE) :: FMED_GEOMETRIE_ARETE_AFF = (/&
       &  "MED_SEG2    ",&
       &  "MED_SEG3    " /)   
  
  character(LEN=12),target,dimension(1) :: FMED_GEOMETRIE_NOEUD_AFF = (/ &
       &  "(AUCUN)     "/)  
  
  character(LEN=12),target,dimension(0:3) :: FMED_ENTITE_MAILLAGE_AFF =(/ &
       &  "MED_MAILLE  ", & 
       &  "MED_FACE    ", &
       &  "MED_ARETE   ", &
       &  "MED_NOEUD   "/)

  parameter (USER_MODE = MED_COMPACT )

!!  write (*,'(A0)')  FMED_GEOMETRIE_NOEUD_AFF(1)
!!  write (*,'(A0)')  FMED_GEOMETRIE_MAILLE_AFF(1)
!!  write (*,'(A0)')  FMED_GEOMETRIE_FACE_AFF(1)
!!  write (*,'(A0)')  FMED_GEOMETRIE_ARETE_AFF(1)

  nbpdtnor=0;pflsize=0;ngauss=0;nval=0
  numdt = 0;numo=0;retmem=0
  cret=0;ret=0

  nullify(type_geo)
  nullify(AFF)


  select case (entite)
  case (MED_NOEUD)
     type_geo => typ_noeud
        nb_geo   = 1
        AFF      => FMED_GEOMETRIE_NOEUD_AFF
     case (MED_MAILLE)  
        type_geo => typmai
           nb_geo   = MED_NBR_GEOMETRIE_MAILLE+2
           AFF      => FMED_GEOMETRIE_MAILLE_AFF
        case (MED_FACE) 
           type_geo => typfac;
              nb_geo   = MED_NBR_GEOMETRIE_FACE+1
              AFF      =>  FMED_GEOMETRIE_FACE_AFF
           case  (MED_ARETE)  
              type_geo => typare
                 nb_geo   = MED_NBR_GEOMETRIE_ARETE
                 AFF      =>  FMED_GEOMETRIE_ARETE_AFF
              end select

              do k=1,nb_geo

                 ! ** Combien de (PDT,NOR) a lire **
                 call efnpdt(fid,nomcha,entite,type_geo(k),nbpdtnor,ret) 
                 if (ret.ne.0) then
                    print *, "Impossible de lire le nombre de pas de temps : " &
                         & ,k,nomcha,entite,FMED_ENTITE_MAILLAGE_AFF(entite) &
                         & ,type_geo(k),AFF(type_geo(k))
                    cret = -1
                 end if
                 if(nbpdtnor < 1 ) continue

                 do j=1,nbpdtnor


                    call efpdti(fid, nomcha, entite, type_geo(k), &
                         & j, ngauss, numdt, numo, dt_unit,  &
                         & dt, maa_ass, local, nbrefmaa, ret ) 
                    if (ret.ne.0) then
                       print *, "Erreur a la demande d'information sur (pdt,nor) : " &
                            & ,nomcha,entite, type_geo(k), ngauss, numdt, numo, dt_unit &
                            & ,dt, maa_ass, local, nbrefmaa
                       cret = -1
                    end if

                    if (numdt .eq. MED_NOPDT) then
                       write(*,'(5X,A)') 'Pas de pas de temps'
                    else
                       write(*,'(5X,A,I5,A,E20.8,A,A,A)') 'Pas de temps n° ' &
                            &  ,numdt,' (', dt ,') ', 'et d''unite ',TRIM(dt_unit)
                    endif
                    if (numo .eq. MED_NONOR) then
                       write(*,'(5X,A)')     'Pas de numero d''ordre'
                    else
                       write(*,'(5X,A,I5)')  'Numero d ordre            : ', numo
                    endif
                    write(*,'(5X,A,I5)') 'Nombre de points de gauss : ',ngauss
                    write(*,'(5X,A,A)')  'Maillage associe          : ', TRIM(maa_ass)

                    ! ** Le maillage reference est-il porte par un autre fichier **
                    if ( .not. local ) then
                       call efnvli(fid,maa_ass,nvl,ret)
                      if (ret.ne.0) then
                          print *, "Erreur a la lecture de la taille du lien : " &
                               & , maa_ass, local, nvl
                          cret = -1
                       end if
                       !! allocate(lien(nvl),STAT=retmem)
                       if (retmem .ne. 0) then
                          print *, "Erreur a l'allocation mémoire de lien : "
                          call efexit(-1)
                       endif
                       lien =""
                       call efliel(fid,lien,nvl,maa_ass,ret)
                       if (ret.ne.0) then
                          print *,"Erreur a la lecture du lien : " & 
                               & ,maa_ass,lien
                          cret = -1
                       else 
                          write (*,'(5X,A,A,A,A,A)') 'Le maillage |',TRIM(maa_ass), &
                               & '| est porte par un fichier distant |', &
                               & TRIM(lien),'|'
                       endif
                       !! deallocate(lien)
                    endif

                    ! ** Combien de maillages lies aux (nomcha,ent,geo,numdt,numo)  **
                    ! ** Notons que cette information est egalement disponible ** 
                    ! ** a partir de MEDpasdetempsInfo **
                   call efnref(fid,nomcha,entite,type_geo(k),numdt,numo,nref,ret)
                   if (ret.ne.0) then
                       print *,"Erreur a la demande du nombre de maillages references par le champ : ", & 
                            & nomcha,numdt,numo
                       cret = -1; continue
                    endif

                    do l=1,nbrefmaa
                       
                       call efrefi(fid,nomcha,entite,type_geo(k), &
                            & l,numdt, numo, maa_ass, local, ngauss, ret)
                       if (ret.ne.0) then
                          print *,"Erreur a la demande d'information sur le maillage utilise par le champ n° : " & 
                               & ,nomcha,entite,type_geo(k), &
                               & l,numdt, numo, maa_ass
                          cret = -1; continue
                       endif

                       ! ** Prend en compte le nbre de pt de gauss automatiquement **
                       call efnval(fid,nomcha,entite,type_geo(k),numdt,numo,maa_ass,USER_MODE,nval,cret)
                       if (ret.ne.0) then
                          print *,"Erreur a la lecture du nombre de valeurs du champ : " & 
                               & ,nomcha,entite,type_geo(k), &
                               & numdt, numo, maa_ass
                          cret = -1; continue
                       endif
                       write(*,'(5X,A,I5,A,I5,A,A,A,A,A,A,A,I5,A)') &
                            & 'Il y a ',nval,' valeurs en mode ',USER_MODE, &
                            & ' . Chaque entite ',TRIM(FMED_ENTITE_MAILLAGE_AFF(entite)), &
                            & ' de type geometrique ',TRIM(AFF(k)),' associes au maillage |',maa_ass, & 
                            & '| a ',ngauss,' pts de gauss '

                       ! ** Le maillage reference est-il porte par un autre fichier **
                       if ( .not. local ) then
              
                          call efnvli(fid,maa_ass,nvl,ret)
                          if (ret.ne.0) then
                             print *, "Erreur a la lecture de la taille du lien : " &
                                  & , maa_ass, local, nvl
                             cret = -1
                          end if

                          !! allocate(lien(nvl),STAT=retmem)
                          if (retmem .ne. 0) then
                             print *, "Erreur a l'allocation mémoire de comp et unit : "
                             call efexit(-1)
                          endif

                          call efliel(fid,lien,nvl,maa_ass,ret)
                          if (ret.ne.0) then
                             print *,"Erreur a la lecture du lien : " & 
                                  & ,maa_ass,lien
                             cret = -1
                          else 
                             write(*,'(5X,A,A,A,A,A)') 'Le maillage |',TRIM(maa_ass), &
                                  & '| est porte par un fichier distant |',TRIM(lien),'|'
                          endif
                          !! deallocate(lien)
                       endif

                       ! **Lecture des valeurs du champ **
                       if (typcha .eq. MED_FLOAT64) then
                          allocate(valr(ncomp*nval),STAT=retmem)

                          call efchal(fid,maa_ass,nomcha,valr,stockage,MED_ALL,locname, &
                               & pflname,USER_MODE,entite,type_geo(k),numdt,numo,ret)

                          if (ret.ne.0) then
                             print *,"Erreur a la lecture du nombre de valeurs du champ : ", &
                                  &  maa_ass,nomcha,valr,stockage,MED_ALL,locname, &
                                  &  pflname,USER_MODE,entite,type_geo(k),numdt,numo
                             cret = -1;
                          endif
                       else	  
                          allocate(vale(ncomp*nval),STAT=retmem)

                          call efchal(fid,maa_ass,nomcha,vale,stockage,MED_ALL,locname, &
                               &  pflname,USER_MODE,entite,type_geo(k),numdt,numo,ret)
                          if (ret.ne.0) then
                             print *,"Erreur a la lecture des valeurs du champ : ",&
                                  & maa_ass,nomcha,vale,stockage,MED_ALL,locname, &
                                  & pflname,USER_MODE,entite,type_geo(k),numdt,numo
                             cret = -1;
                          endif

                       endif

                       if (ngauss .gt. 1 ) then
                          write (*,'(5X,A,A,A)') "- Modèle de localisation des ", &
                               & "points de Gauss de nom ", TRIM(locname)
                       end if

                       select case (stockage) 
                       case (MED_FULL_INTERLACE)  
                          write(*,'(5X,A)') "- Valeurs :";  write(*,'(5X,A)') ""
                          do m=0,(nval/ngauss-1)
                             write(*,*) "|"
                             do n=0,(ngauss*ncomp-1)     
                                if (typcha .eq. MED_FLOAT64) then
                                   write (*,'(1X,E20.5,1X)') valr( m*ngauss*ncomp+n +1 )
                                else
                                   write (*,'(1X,I8,1X)') vale( m*ngauss*ncomp+n +1 )
                                end if
                             enddo
                          enddo
                       case (MED_NO_INTERLACE) 
                          write(*,'(5X,A)') "- Valeurs :";  write(*,'(5X,A)') ""
                          do m=0,ncomp-1
                             write(*,*) "|"
                             do n=0,nval-1 
                                if (typcha .eq. MED_FLOAT64) then
                                   write (*,'(1X,E20.5,1X)') valr(m*nval+n +1)
                                else
                                   write (*,'(1X,I8,1X)') vale(m*nval+n +1)
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
                       if (pflname .eq. MED_NOPFL) then
                          write(*,'(5X,A)') 'Pas de profil'
                       else
                          write(*,'(5X,A,A)') 'Profil :',pflname
                          call efnpfl(fid,pflname,pflsize,ret)
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

                          call efpfll(fid,pflval,pflname,ret)
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
              
              print *,""
              getFieldsOn=ret
              
            end function getFieldsOn
