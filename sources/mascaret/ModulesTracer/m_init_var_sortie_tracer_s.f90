!== Copyright (C) 2000-2022 EDF-CEREMA ==
!
!   This file is part of MASCARET-TRACER.
!
!   MASCARET-TRACER is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   MASCARET-TRACER is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with MASCARET-TRACER.  If not, see <http://www.gnu.org/licenses/>
!

module M_INIT_VAR_SORTIE_TRACER_S
!***********************************************************************
! PROGICIEL : TRACER         A. LEBOSSE
!                            P. CHERUBINI
!                            S. PERON
!                            S. MANDELKERN
!                            D. POIZAT
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!
! MODULE DECLARANT LES TYPES ET DEFINISSANT LES STRUCTURES
! POUR LES VARIABLES A SORTIR (I.E. A STOCKER OU A IMPRIMER SUR LISTING)
!
!***********************************************************************

   !=========================== Declarations ==============================
   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   ! Definition de 2 structures qui permettront de realiser des boucles
   ! sur les variables a stocker ou a imprimer sur listing
   ! 1. Definition de la structure contenant toutes les informations constantes
   ! sur les variables a sortir.
   !........................    definition du type de la structure VAR_NOM_T ...
   !                                                                           !
   integer , parameter :: LEN_NOM_LONG = 39                                    !
   integer , parameter :: LEN_NOM_A4   =  4                                    !
   integer , parameter :: LEN_UNITE    =  6                                    !
   !                                                                           !
   type VAR_NOM_T                                                              !
      sequence                                                                 !
      logical                 :: Obligatoire                                   !
      character(LEN_NOM_LONG) :: NomLong                                       !
      character(LEN_NOM_A4)   :: NomA4                                         !
      character(LEN_UNITE)    :: Unite                                         !
      integer                 :: Precision                                     !
      logical                 :: DependantTemps                                !
   end type VAR_NOM_T                                                          !
   !
   !................. fin de la definition du type de la structure VAR_NOM_T ...
   ! 2. Definition de la structure contenant toutes les informations dependantes
   ! de la simulation sur les variables a sortir.
   !........................    definition du type de la structure GDR_STO_T ...
   !                                                                           !
   type GDR_STO_T                                                              !
      sequence                                                                 !
      logical                             :: ASortir                           !
      real(DOUBLE), dimension(:), pointer :: Valeur                            !
   end type GDR_STO_T                                                          !
   !                                                                           !
   !................. fin de la definition du type de la structure GDR_STO_T ...

   contains

   subroutine  INIT_VAR_SORTIE_TRACER_S( &
                           Var_nom     , &
                           Gdr         , &
                           X           , &
                           ZREF        , &
                           QT   , AT   , &
                           Ctraceur    , &
                           nb_trac     , &
                           VarASortir  , &
                       PhaseSimulation   &
                                       )

   ! .....................................................................
   !
   !  FONCTION :
   !  --------
   !
   !             INITIALISATION DES VARIABLES A STOCKER ET A IMPRIMER
   !
   !-----------------------------------------------------------------------
   !                             ARGUMENTS
   ! .__________________.____._______________________________________________
   ! !    NOM      !TYPE!MODE!                   ROLE
   ! !_____________!____!____!_______________________________________________
   ! ! Var_nom     ! T  !<-- ! Structure sur les informations constantes
   ! ! Gdr         ! T  !<-- ! Structure sur les informations non constantes
   ! ! X           ! R  ! -->! Maillage
   ! ! ZREF        ! R  ! -->! Cote du fond
   ! ! Q1          ! R  ! -->! Debit mineur
   ! ! Q2          ! R  ! -->! Debit majeur
   ! ! Ctraceur    ! R  ! -->! Concentrations des traceurs
   ! ! nb_trac     ! I  ! -->: Nombre de traceurs
   ! ! VarASortir  ! L  ! -->! Drapeaux sur les variables a sortir
   ! ! PhaseSimulation!I! -->! Variable indiquant la phase de la simulation
   ! !_____________!____!____!_______________________________________________
   !
   !  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
   !               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
   !-------------------------------------------------------------------------
   !
   !   FICHIERS ENTREE/SORTIE :       FichSto
   !   ----------------------
   !
   !   SOUS-PROGRAMME(S) APPELANT(S) :  STOCK_TRACER
   !   -----------------------------
   !   SOUS-PROGRAMME(S) APPELE(S)   :  Neant
   !   -----------------------------
   !
   !***********************************************************************
   !
   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_INDEX_VARIABLE_TRACER_C    ! Constantes servant a reperer les variables
   use M_CONSTANTES_CALCUL_C ! Constante servant a reperer la phase de calcul

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments .. 
   !---------------
   type(VAR_NOM_T), dimension(:)        , intent(out):: Var_nom
   type(GDR_STO_T), dimension(:)        , intent(out):: Gdr
   real(DOUBLE)   , dimension(:)  , pointer          :: X
   real(DOUBLE)   , dimension(:)  , pointer          :: ZREF
   real(DOUBLE)   , dimension(:)  , pointer          :: QT,AT
   real(DOUBLE)   , dimension(:,:), pointer          :: Ctraceur
   integer                              , intent(in) :: nb_trac
   logical        , dimension(:)        , intent(in) :: VarASortir
   integer                              , intent(in) :: PhaseSimulation
   integer :: i
   character(2) :: i_in_letter

   !============================ Initialisations =========================== 
   label_initialisation: if (PhaseSimulation == PHASE_INITIALISATION) then

      Var_nom(:) = VAR_NOM_T(.false.,"                               ","    ","      ",0,.false.)
      !.....................     initialisation du tableau de structure Var_nom ........................
      Var_nom(VARTR_X    ) = VAR_NOM_T(.true.,"Maillage                       ","X   ","m     ",2,.false.)!
      Var_nom(VARTR_ZREF ) = VAR_NOM_T(.true.,"Cote du fond                   ","ZREF","m     ",4,.false.)!
      Var_nom(VARTR_Q    ) = VAR_NOM_T(.true.,"Debit total                    ","QTOT","m3/s  ",3,.true. )!
      Var_nom(VARTR_A    ) = VAR_NOM_T(.true.,"Section mouillee totale        ","ATOT","m2    ",3,.true. )!
      do i=1,nb_trac
         write(i_in_letter,'(I2)') i
         Var_nom(VARTR_CONC(i))=&
            & VAR_NOM_T(.true.,"Concentration traceur "//trim(adjustl(I_in_letter)),&
            &                  "C"//trim(adjustl(I_in_letter)),"unit  ",11,.true. )
      enddo
   end if label_initialisation

   !============================ Instructions ==============================
   ! Une variable a sortir est :
   ! - soit une variable     obligatoire `Var_nom(:)%Obligatoire == .true.'
   ! - soit une variable non obligatoire mais qui a ete choisie comme etant a
   !   sortir `Var_nom(:)%Obligatoire == .false. .and. VarASortir(:) == .true.'
   Gdr(:)%ASortir = Var_nom(:)%Obligatoire .or. VarASortir(:)
   Gdr(VARTR_X)%Valeur     => X
   Gdr(VARTR_ZREF)%Valeur  => ZREF
   Gdr(VARTR_Q)%Valeur     => QT
   Gdr(VARTR_A)%Valeur     => AT
   do i = 1 , nb_trac
      Gdr(VARTR_CONC(i))%Valeur    => Ctraceur(:,i)
   enddo

   end subroutine INIT_VAR_SORTIE_TRACER_S

end module M_INIT_VAR_SORTIE_TRACER_S
