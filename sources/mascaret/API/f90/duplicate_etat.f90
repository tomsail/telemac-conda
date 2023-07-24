!== Copyright (C) 2000-2022 EDF-CEREMA ==
!
!   This file is part of MASCARET.
!
!   MASCARET is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   MASCARET is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with MASCARET.  If not, see <http://www.gnu.org/licenses/>
!

! *********************************************************************
! PROGICIEL : MASCARET       J.-M. LACOMBE
!
! VERSION : V8P4R0              EDF-CEREMA
! *********************************************************************
  function DUPLICATE_ETAT(Source, Dest)
    use M_ETAT_MASCARET_T
    implicit none
    integer                                 :: DUPLICATE_ETAT ! etat dupliquer et alloue
    type(ETAT_MASCARET_T),    intent(in)    :: Source         ! Instance du type derive que l'on souhaite dupliquer
    type(ETAT_MASCARET_T),    intent(inout) :: Dest           ! etat dupliquer et alloue

    interface
        function dupliqueR1(Source, Dest)
          implicit none
          integer                                        :: dupliqueR1 ! retourne 1 si erreur
          real(8), dimension(:), pointer, intent(in)     :: Source     ! Instance du type derive que l'on souhaite dupliquer
          real(8), dimension(:), pointer, intent(inout)  :: Dest       ! etat dupliquer et alloue
        end function dupliqueR1
        function dupliqueR2(Source, Dest)
          implicit none
          integer                                          :: dupliqueR2 ! retourne 1 si erreur
          real(8), dimension(:,:), pointer, intent(in)     :: Source     ! Instance du type derive que l'on souhaite dupliquer
          real(8), dimension(:,:), pointer, intent(inout)  :: Dest       ! etat dupliquer et alloue
        end function dupliqueR2
        function dupliqueR3(Source, Dest)
          implicit none
          integer                                          :: dupliqueR3 ! retourne 1 si erreur
          real(8), dimension(:,:,:), pointer, intent(in)     :: Source     ! Instance du type derive que l'on souhaite dupliquer
          real(8), dimension(:,:,:), pointer, intent(inout)  :: Dest       ! etat dupliquer et alloue
        end function dupliqueR3
        function dupliqueI1(Source, Dest)
          implicit none
          integer                                        :: dupliqueI1 ! retourne 1 si erreur
          integer, dimension(:), pointer, intent(in)     :: Source     ! Instance du type derive que l'on souhaite dupliquer
          integer, dimension(:), pointer, intent(inout)  :: Dest       ! etat dupliquer et alloue
        end function dupliqueI1
        function dupliqueI2(Source, Dest)
          implicit none
          integer                                          :: dupliqueI2 ! retourne 1 si erreur
          integer, dimension(:,:), pointer, intent(in)     :: Source     ! Instance du type derive que l'on souhaite dupliquer
          integer, dimension(:,:), pointer, intent(inout)  :: Dest       ! etat dupliquer et alloue
        end function dupliqueI2
    end interface

    integer err
    integer taille , i

    DUPLICATE_ETAT = 0

    err = dupliqueR1(Source%DPDZ2 , Dest%DPDZ2)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR1(Source%DPDZ1 , Dest%DPDZ1)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR1(Source%QDeverse , Dest%QDeverse)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR1(Source%Qinjec , Dest%Qinjec)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR2(Source%Airs , Dest%Airs)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR3(Source%W , Dest%W)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR1(Source%YNode , Dest%YNode)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR1(Source%CNode , Dest%CNode)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR1(Source%UNode , Dest%UNode)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR1(Source%XFron , Dest%XFron)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR1(Source%RH2 , Dest%RH2)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR1(Source%RH1 , Dest%RH1)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR1(Source%BS , Dest%BS)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR1(Source%B2 , Dest%B2)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR1(Source%B1 , Dest%B1)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR1(Source%P2 , Dest%P2)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR1(Source%P1 , Dest%P1)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR1(Source%Froude , Dest%Froude)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR1(Source%Beta , Dest%Beta)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR1(Source%S2 , Dest%S2)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR1(Source%S1 , Dest%S1)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR1(Source%SS , Dest%SS)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR1(Source%Q2 , Dest%Q2)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR1(Source%Q1 , Dest%Q1)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR1(Source%V1 , Dest%V1)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR1(Source%V2 , Dest%V2)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR1(Source%Y , Dest%Y)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR1(Source%VOL , Dest%VOL)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR1(Source%VOLS , Dest%VOLS)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    Dest%tempsPrecedent  = Source%tempsPrecedent
    Dest%numPasTps       = Source%numPasTps
    Dest%phaseSimulation = Source%phaseSimulation
    Dest%DT              = Source%DT

    err = dupliqueR1(Source%Q , Dest%Q)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR1(Source%Z , Dest%Z)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    if (ASSOCIATED(Source%Liaisons)) then
       taille = size(Source%Liaisons)
    else
       taille = 0
    endif
    if(.not.associated(Dest%Liaisons)) ALLOCATE(Dest%Liaisons(taille))
    do i=1, taille
        Dest%Liaisons(i) = Source%Liaisons(i)
    enddo

    if (ASSOCIATED(Source%Casiers)) then
       taille = size(Source%Casiers)
    else
       taille = 0
    endif

    if(.not.associated(Dest%Casiers)) ALLOCATE(Dest%Casiers(taille))
    do i=1, taille
       Dest%Casiers(i) = Source%Casiers(i)
    enddo

    err = dupliqueI1(Source%JGNODE , Dest%JGNODE)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueI1(Source%JDNODE , Dest%JDNODE)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueI1(Source%IFIGE , Dest%IFIGE)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR2(Source%FLUX , Dest%FLUX)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR1(Source%DebitFlux , Dest%DebitFlux)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    ! dupliquer Tracer

    ! FIN dupliquer Tracer
    err = dupliqueR2(Source%Tracer%Ctraceur, Dest%Tracer%Ctraceur)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR1(Source%Tracer%QT, Dest%Tracer%QT)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR1(Source%Tracer%ST, Dest%Tracer%ST)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR1(Source%Tracer%BT, Dest%Tracer%BT)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR1(Source%Tracer%QT_ANT, Dest%Tracer%QT_ANT)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR1(Source%Tracer%ST_ANT, Dest%Tracer%ST_ANT)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR1(Source%Tracer%BT_ANT, Dest%Tracer%BT_ANT)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR1(Source%Tracer%NbCourant, Dest%Tracer%NbCourant)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR2(Source%Tracer%MASS, Dest%Tracer%MASS)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR2(Source%Tracer%FLUMAS, Dest%Tracer%FLUMAS)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR2(Source%Tracer%FLUENT, Dest%Tracer%FLUENT)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR2(Source%Tracer%FLUSOR, Dest%Tracer%FLUSOR)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    err = dupliqueR2(Source%Tracer%FLUSRC, Dest%Tracer%FLUSRC)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    ! dupliquer rezomat_t
    Dest%MatriceRezo%SOLV  = Source%MatriceRezo%SOLV
    Dest%MatriceRezo%N  = Source%MatriceRezo%N
    Dest%MatriceRezo%NNZ  = Source%MatriceRezo%NNZ
    Dest%MatriceRezo%NN  = Source%MatriceRezo%NN
    Dest%MatriceRezo%NN1  = Source%MatriceRezo%NN1
    Dest%MatriceRezo%IHA  = Source%MatriceRezo%IHA
    Dest%MatriceRezo%KL  = Source%MatriceRezo%KL
    Dest%MatriceRezo%KU  = Source%MatriceRezo%KU
    Dest%MatriceRezo%LDAB  = Source%MatriceRezo%LDAB
    Dest%MatriceRezo%IFAIL  = Source%MatriceRezo%IFAIL
    err = dupliqueI1(Source%MatriceRezo%ipiv , Dest%MatriceRezo%ipiv)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif
    err = dupliqueI1(Source%MatriceRezo%IFLAG , Dest%MatriceRezo%IFLAG)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif
    err = dupliqueI1(Source%MatriceRezo%rowA , Dest%MatriceRezo%rowA)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif
    err = dupliqueI1(Source%MatriceRezo%colA , Dest%MatriceRezo%colA)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif
    err = dupliqueI1(Source%MatriceRezo%snr , Dest%MatriceRezo%snr)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif
    err = dupliqueI1(Source%MatriceRezo%rnr , Dest%MatriceRezo%rnr)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif
    err = dupliqueI2(Source%MatriceRezo%ha , Dest%MatriceRezo%ha)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif
    err = dupliqueI1(Source%MatriceRezo%noVarDQ , Dest%MatriceRezo%noVarDQ)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif
    err = dupliqueI1(Source%MatriceRezo%noVarDZ , Dest%MatriceRezo%noVarDZ)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif
    err = dupliqueI1(Source%MatriceRezo%noVarDQl , Dest%MatriceRezo%noVarDQl)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif
    err = dupliqueI1(Source%MatriceRezo%noVarDZc , Dest%MatriceRezo%noVarDZc)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif
    err = dupliqueI1(Source%MatriceRezo%typSec , Dest%MatriceRezo%typSec)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif
    err = dupliqueI1(Source%MatriceRezo%headConflu , Dest%MatriceRezo%headConflu)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif
    err = dupliqueI1(Source%MatriceRezo%nextSecConflu , Dest%MatriceRezo%nextSecConflu)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif
    err = dupliqueI1(Source%MatriceRezo%SecSin , Dest%MatriceRezo%SecSin)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif
    err = dupliqueI1(Source%MatriceRezo%SecLiai , Dest%MatriceRezo%SecLiai)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif
    err = dupliqueI1(Source%MatriceRezo%LiaiSec , Dest%MatriceRezo%LiaiSec)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif
    err = dupliqueR1(Source%MatriceRezo%AFLAG , Dest%MatriceRezo%AFLAG)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif
    err = dupliqueR1(Source%MatriceRezo%valA , Dest%MatriceRezo%valA)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif
    err = dupliqueR1(Source%MatriceRezo%b , Dest%MatriceRezo%b)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif
    err = dupliqueR1(Source%MatriceRezo%pivot , Dest%MatriceRezo%pivot)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif
    err = dupliqueR2(Source%MatriceRezo%AB , Dest%MatriceRezo%AB)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif
    ! FIN dupliquer rezomat_t

    Dest%NBARAD  = Source%NBARAD

    err = dupliqueI1(Source%IDEB , Dest%IDEB)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif
    err = dupliqueI1(Source%IFIN , Dest%IFIN)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif
    err = dupliqueI1(Source%ITEM0 , Dest%ITEM0)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

    ! dupliquer Sauve_t
    err = dupliqueR1(Source%Sauve%H2OIB , Dest%Sauve%H2OIB)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif
    err = dupliqueR1(Source%Sauve%H2OTB , Dest%Sauve%H2OTB)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif
    err = dupliqueR1(Source%Sauve%H2OEB , Dest%Sauve%H2OEB)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif
    err = dupliqueR1(Source%Sauve%H2OSB , Dest%Sauve%H2OSB)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif
    err = dupliqueR1(Source%Sauve%H2OIC , Dest%Sauve%H2OIC)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif
    err = dupliqueR1(Source%Sauve%H2OTC , Dest%Sauve%H2OTC)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif
    err = dupliqueR1(Source%Sauve%H2OEC , Dest%Sauve%H2OEC)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif
    err = dupliqueR1(Source%Sauve%H2OSC , Dest%Sauve%H2OSC)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif
    err = dupliqueR1(Source%Sauve%H2OTBS , Dest%Sauve%H2OTBS)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif
    err = dupliqueR1(Source%Sauve%H2OIBS , Dest%Sauve%H2OIBS)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif
    err = dupliqueR1(Source%Sauve%SPREC , Dest%Sauve%SPREC)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif
    err = dupliqueR1(Source%Sauve%QPREC , Dest%Sauve%QPREC)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif
    Dest%Sauve%H2OIG = Source%Sauve%H2OIG
    Dest%Sauve%H2OIGS = Source%Sauve%H2OIGS
    Dest%Sauve%H2OTG = Source%Sauve%H2OTG
    Dest%Sauve%H2OTGS = Source%Sauve%H2OTGS
    Dest%Sauve%H2OEG = Source%Sauve%H2OEG
    Dest%Sauve%H2OSG = Source%Sauve%H2OSG
    ! FIN dupliquer Sauve_t

    err = dupliqueR1(Source%ZINIT , Dest%ZINIT)
    if (err /= 0) then
       DUPLICATE_ETAT = 1
       return
    endif

  end function DUPLICATE_ETAT

  function dupliqueR1(Source, Dest)
    implicit none
    integer                             :: dupliqueR1 ! retourne 1 si erreur
    real(8), dimension(:), pointer, intent(in)     :: Source     ! Instance du type derive que l'on souhaite dupliquer
    real(8), dimension(:), pointer, intent(inout)  :: Dest       ! etat dupliquer et alloue

    integer err
    integer taille
    dupliqueR1 = 0

    if (ASSOCIATED(Source)) then
       taille = size(Source)
    else
       return
    endif
    if(.not.associated(Dest)) ALLOCATE(Dest(taille), STAT=err)
    if (err /= 0) then
       dupliqueR1 = 1
       return
    endif

    if (taille /= 0) then
       Dest(:) = Source(:)
    endif

    return
  end function dupliqueR1

  subroutine dupliqueR11(erreur, so, de)
    implicit none
    integer , intent(out)              :: erreur ! retourne 1 si erreur
    real(8), dimension(:), pointer     :: so     ! Instance du type derive que l'on souhaite dupliquer
    real(8), dimension(:), pointer     :: de       ! etat dupliquer et alloue

    integer taille
    erreur = 0

    if (ASSOCIATED(so)) then
       taille = size(so)
    else
       return
    endif
    if(.not.associated(de)) ALLOCATE(de(taille), STAT=erreur)
    if (erreur /= 0) then
       erreur = 1
       return
    endif

    if (taille /= 0) then
      de(:) = so(:)
    endif

    return
  end subroutine dupliqueR11

  function dupliqueR2(Source, Dest)
    implicit none
    integer                                         :: dupliqueR2 ! retourne 1 si erreur
    real(8), dimension(:,:), pointer, intent(in)    :: Source     ! Instance du type derive que l'on souhaite dupliquer
    real(8), dimension(:,:), pointer, intent(inout) :: Dest       ! etat dupliquer et alloue

    integer err
    integer taille1, taille2
    dupliqueR2 = 0

    if (ASSOCIATED(Source)) then
       taille1 = size(Source,1)
       taille2 = size(Source,2)
    else
       return
    endif
    if(.not.associated(Dest)) ALLOCATE(Dest(taille1, taille2), STAT=err)

    if (err /= 0) then
       dupliqueR2 = 1
       return
    endif

    if (taille1 /= 0) then
       Dest(:,:) = Source(:,:)
    endif

    return
  end function dupliqueR2

  function dupliqueR3(Source, Dest)
    implicit none
    integer                                           :: dupliqueR3 ! retourne 1 si erreur
    real(8), dimension(:,:,:), pointer, intent(in)    :: Source     ! Instance du type derive que l'on souhaite dupliquer
    real(8), dimension(:,:,:), pointer, intent(inout) :: Dest       ! etat dupliquer et alloue

    integer err
    integer taille1, taille2, taille3

    dupliqueR3 = 0

    if (ASSOCIATED(Source)) then
       taille1 = size(Source,1)
       taille2 = size(Source,2)
       taille3 = size(Source,3)
    else
       return
    endif
    if(.not.associated(Dest)) ALLOCATE(Dest(taille1, taille2, taille3), STAT=err)
    if (err /= 0) then
      dupliqueR3 = 1
      return
    endif

    if (taille1 /= 0) then
       Dest(:,:,:) = Source(:,:,:)
    endif

    return
  end function dupliqueR3

  function dupliqueI1(Source, Dest)
    implicit none
    integer                                       :: dupliqueI1 ! retourne 1 si erreur
    integer, dimension(:), pointer, intent(in)    :: Source         ! Instance du type derive que l'on souhaite dupliquer
    integer, dimension(:), pointer, intent(inout) :: Dest           ! etat dupliquer et alloue

    integer err
    integer taille
    dupliqueI1 = 0

    if (ASSOCIATED(Source)) then
       taille = size(Source)
    else
       return
    endif
    if(.not.associated(Dest)) ALLOCATE(Dest(taille), STAT=err)
    if (err /= 0) then
       dupliqueI1 = 1
       return
     endif

     if (taille /= 0) then
        Dest(:) = Source(:)
     endif

     return
  end function dupliqueI1

  function dupliqueI2(Source, Dest)
    implicit none
    integer                                         :: dupliqueI2 ! retourne 1 si erreur
    integer, dimension(:,:), pointer, intent(in)    :: Source         ! Instance du type derive que l'on souhaite dupliquer
    integer, dimension(:,:), pointer, intent(inout) :: Dest           ! etat dupliquer et alloue

    integer err
    integer taille1, taille2
    dupliqueI2 = 0

    if (ASSOCIATED(Source)) then
       taille1 = size(Source,1)
       taille2 = size(Source,2)
    else
       return
    endif
    if(.not.associated(Dest)) ALLOCATE(Dest(taille1, taille2), STAT=err)

    if (err /= 0) then
       dupliqueI2 = 1
       return
    endif

    if (taille1 /= 0) then
       Dest(:,:) = Source(:,:)
    endif

    return
  end function dupliqueI2
