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

module M_INIT_VAR_SORTIE_S
!***********************************************************************
! PROGICIEL : MASCARET
!                             A. LEBOSSE
!                             P. CHERUBINI
!                             S. PERON
!                             S. MANDELKERN
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

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
   !                                                                           !
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

   subroutine  INIT_VAR_SORTIE_S( &
       Var_nom     , &
       Gdr         , &
       X           , &
       ZREF        , &
       RGC  , RDC  , &
       CF1  , CF2  , &
       Z           , &
       Q           , &
       Q1   , Q2   , &
       DebitFlux   , &
       S1   , S2   , &
       B1   , B2   , &
       BS          , &
       P1   , P2   , &
       RH1  , RH2  , &
       FR   , BETA , &
       TAUF        , &
       Y    , HMOY , &
       Q2G  , Q2D  , &
       VOL  , VOLS , &
       CHARG       , &
       SS          , &
       V1   , V2   , &
       ZMAX , TZMAX, &
       VZMAX       , &
       ZMIN , TZMIN, &
       V1MIN, V1MAX, &
       BMAX        , &
       TOND        , &
       QMAX , TQMAX, &
       EMAX        , &
       YVRAI, QVRAI, &
       Qdeverse    , &
       VarASortir  , &
       PhaseSimulation &
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
   ! ! RGC         ! R  ! -->! Cote de la rive gauche
   ! ! RDC         ! R  ! -->! Cote de la rive droite
   ! ! CF1         ! R  ! -->! Coefficient de Strickler mineur
   ! ! CF2         ! R  ! -->! Coefficient de Strickler majeur
   ! ! Z           ! R  ! -->! Cote d'eau a une section de calcul
   ! ! Q1          ! R  ! -->! Debit mineur
   ! ! Q2          ! R  ! -->! Debit majeur
   ! ! S1          ! R  ! -->! Section mouillee mineure
   ! ! S2          ! R  ! -->! Section mouillee majeure
   ! ! B1          ! R  ! -->! Largeur au miroir mineure
   ! ! B2          ! R  ! -->! Largeur au miroir majeure
   ! ! BS          ! R  ! -->! Largeur au miroir des zones de stockage
   ! ! P1          ! R  ! -->! Perimetre mouille mineur
   ! ! P2          ! R  ! -->! Perimetre mouille majeur
   ! ! RH1         ! R  ! -->! Rayon hydraulique mineur
   ! ! RH2         ! R  ! -->! Rayon hydraulique majeur
   ! ! FR          ! R  ! -->! Nombre de Froude
   ! ! BETA        ! R  ! -->! Coefficient beta du modele DEBORD
   ! ! TAUF        ! R  ! -->! Contrainte au fond
   ! ! Y           ! R  ! -->! Hauteur d'eau maximale
   ! ! HMOY        ! R  ! -->! Hauteur d'eau moyenne
   ! ! Q2G         ! R  ! -->! Debit majeur gauche
   ! ! Q2D         ! R  ! -->! Debit majeur droit
   ! ! VOL         ! R  ! -->! Volume du lit actif
   ! ! VOLS        ! R  ! -->! Volume de stockage
   ! ! CHARG       ! R  ! -->! Charge
   ! ! SS          ! R  ! -->! Section mouillee des zones de stockage
   ! ! V1          ! R  ! -->! Vitesse mineure
   ! ! V2          ! R  ! -->! Vitesse majeure
   ! ! ZMAX        ! R  ! -->! Cote maximale atteinte
   ! ! TZMAX       ! R  ! -->! Instant de cote maximale atteinte
   ! ! VZMAX       ! R  ! -->! Vitesse a la cote maximale
   ! ! ZMIN        ! R  ! -->! Cote minimale atteinte
   ! ! TZMIN       ! R  ! -->! Instant de cote minimale atteinte
   ! ! V1MIN       ! R  ! -->! Vitesse mineure minimale
   ! ! V1MAX       ! R  ! -->! Vitesse majeure minimale
   ! ! BMAX        ! R  ! -->! Largeur au miroir maximale
   ! ! TOND        ! R  ! -->! Instant d'arrivee d'onde
   ! ! QMAX        ! R  ! -->! Debit maximal
   ! ! TQMAX       ! R  ! -->! Instant de debit maximal
   ! ! EMAX        ! R  ! -->! Energie maximale
   ! ! Qdeverse    ! R  ! -->! debit deverse par les deversoirs
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
   !   SOUS-PROGRAMME(S) APPELANT(S) :  STOCK, POST_IMP
   !   -----------------------------
   !   SOUS-PROGRAMME(S) APPELE(S)   :  Neant
   !   ---------------------------
   !
   !   COMMENTAIRES :
   !   ------------
   !                     Comment rajouter une variable a sortir ?
   !                     -----------------------------------------
   !
   !   Soit VARNEW le nom de la nouvelle variable a sortir.
   !   A. Il faut modifier 1 ligne :
   !      I. Dans le module M_INDEX_VARIABLE_C :
   !         1. Rajouter 1 unite a la constante NB_TOT_VAR
   !            qui represente le nombre total de variables stockables
   !            `  integer , parameter :: NB_TOT_VAR = ...'
   !   B. Il faut introduire 3 lignes :
   !      I.  Dans le module M_INDEX_VARIABLE_C :
   !          2. Introduire la constante definissant l'indice de la
   !             nouvelle variable dans les tableaux de variables
   !             `  integer , parameter :: VAR_VARNEW = ...'
   !      II. Dans le sous-programme STOCK :
   !          3. Definir l'initialisation des informations constantes
   !             de cette nouvelle variable
   !             a. Si son stokage est obligatoire
   !             b. Son nom long
   !             c. Son nom sur 4 caracteres
   !             d. Le nom de son unite physique
   !             e. La precision souhaitee pour sa valeur numerique
   !             f. Si elle est dependante du temps
   !             Par exemple :
   !             `Var_nom(VAR_VARNEW )=VAR_NOM_T(.false.,"Variable Nouvelle              ","VNEW","m/s   ",3,.true. )'
   !          4. Definir ses valeurs dans la structure contenant les
   !             informations dependantes de la simulation
   !             `gdr(VAR_VARNEW)%Valeur     => ...'
   !             ou l'objet pointe est le tableau des valeurs par section
   !             de cette nouvelle variables.
   !             (Bien entendu le tableau des valeurs par section devra
   !              etre defini, par exemple en le passant en argument
   !              de la routine STOCK)
   !
   !
   !   DOCUMENTATION EXTERNE :
   !   ---------------------

   !***********************************************************************
   !
   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_INDEX_VARIABLE_C    ! Constantes servant a reperer les variables
   use M_CONSTANTES_CALCUL_C ! Constante servant a reperer la phase de calcul

   !.. Declarations explicites .. 
   !-----------------------------
   implicit none

   !.. Arguments .. 
   !---------------
   type(VAR_NOM_T), dimension(:)        , intent(out):: Var_nom
   type(GDR_STO_T), dimension(:)        , intent(out):: Gdr
   real(DOUBLE)   , dimension(:), pointer  :: X
   real(DOUBLE)   , dimension(:), pointer  :: ZREF
   real(DOUBLE)   , dimension(:), pointer  :: RGC
   real(DOUBLE)   , dimension(:), pointer  :: RDC
   real(DOUBLE)   , dimension(:), pointer  :: CF1
   real(DOUBLE)   , dimension(:), pointer  :: CF2
   real(DOUBLE)   , dimension(:), pointer  :: Z
   real(DOUBLE)   , dimension(:), pointer  :: Q
   real(DOUBLE)   , dimension(:), pointer  :: Q1
   real(DOUBLE)   , dimension(:), pointer  :: Q2
   real(DOUBLE)   , dimension(:), pointer  :: S1
   real(DOUBLE)   , dimension(:), pointer  :: S2
   real(DOUBLE)   , dimension(:), pointer  :: B1
   real(DOUBLE)   , dimension(:), pointer  :: B2
   real(DOUBLE)   , dimension(:), pointer  :: BS
   real(DOUBLE)   , dimension(:), pointer  :: P1
   real(DOUBLE)   , dimension(:), pointer  :: P2
   real(DOUBLE)   , dimension(:), pointer  :: RH1
   real(DOUBLE)   , dimension(:), pointer  :: RH2
   real(DOUBLE)   , dimension(:), pointer  :: FR
   real(DOUBLE)   , dimension(:), pointer  :: BETA
   real(DOUBLE)   , dimension(:), pointer  :: TAUF
   real(DOUBLE)   , dimension(:), pointer  :: Y
   real(DOUBLE)   , dimension(:), pointer  :: HMOY
   real(DOUBLE)   , dimension(:), pointer  :: Q2G
   real(DOUBLE)   , dimension(:), pointer  :: Q2D
   real(DOUBLE)   , dimension(:), pointer  :: VOL
   real(DOUBLE)   , dimension(:), pointer  :: VOLS
   real(DOUBLE)   , dimension(:), pointer  :: CHARG
   real(DOUBLE)   , dimension(:), pointer  :: SS
   real(DOUBLE)   , dimension(:), pointer  :: V1
   real(DOUBLE)   , dimension(:), pointer  :: V2
   real(DOUBLE)   , dimension(:), pointer  :: ZMAX
   real(DOUBLE)   , dimension(:), pointer  :: TZMAX
   real(DOUBLE)   , dimension(:), pointer  :: VZMAX
   real(DOUBLE)   , dimension(:), pointer  :: ZMIN
   real(DOUBLE)   , dimension(:), pointer  :: TZMIN
   real(DOUBLE)   , dimension(:), pointer  :: V1MIN
   real(DOUBLE)   , dimension(:), pointer  :: V1MAX
   real(DOUBLE)   , dimension(:), pointer  :: BMAX
   real(DOUBLE)   , dimension(:), pointer  :: TOND
   real(DOUBLE)   , dimension(:), pointer  :: QMAX
   real(DOUBLE)   , dimension(:), pointer  :: TQMAX
   real(DOUBLE)   , dimension(:), pointer  :: EMAX
   real(DOUBLE)   , dimension (:),pointer :: YVRAI
   real(DOUBLE)   , dimension (:),pointer :: QVRAI
   real(DOUBLE)   , dimension (:),pointer :: Qdeverse
   real(DOUBLE)   , dimension (:),pointer :: DebitFlux
   logical        , dimension(:)        , intent(in) :: VarASortir
   integer                              , intent(in) :: PhaseSimulation

   !.. Constantes ..
   !----------------

   !.. Variables locales ..
   !-----------------------

   !============================ Initialisations ===========================
   label_initialisation : if( PhaseSimulation == PHASE_INITIALISATION ) then
      !.....................     initialisation du tableau de structure Var_nom ........................
      Var_nom(VAR_X    )=VAR_NOM_T(.true. ,"Maillage                       ","X   ","m     ",2,.false.)!
      Var_nom(VAR_ZREF )=VAR_NOM_T(.true.,"Cote du fond                   ","ZREF","m     ",4, .false.)!
      Var_nom(VAR_RGC  )=VAR_NOM_T(.false.,"Cote de la rive gauche         ","RGC ","m     ",4,.false.)!
      Var_nom(VAR_RDC  )=VAR_NOM_T(.false.,"Cote de la rive droite         ","RDC ","m     ",4,.false.)!
      Var_nom(VAR_CF1  )=VAR_NOM_T(.true.,"Coefficient de frottement mineur","KMIN","m1/3/s",0,.false.)!
      Var_nom(VAR_CF2  )=VAR_NOM_T(.true.,"Coefficient de frottement majeur","KMAJ","m1/3/s",0,.false.)!
      Var_nom(VAR_Z    )=VAR_NOM_T(.true. ,"Cote de l eau                  ","Z   ","m     ",3,.true. )!
      Var_nom(VAR_Q    )=VAR_NOM_T(.true. ,"Debit total                    ","Q   ","m3/s  ",3,.true.) !
      Var_nom(VAR_Q1   )=VAR_NOM_T(.true.,"Debit mineur                    ","QMIN","m3/s  ",3,.true. )!
      Var_nom(VAR_Q2   )=VAR_NOM_T(.true.,"Debit majeur                    ","QMAJ","m3/s  ",3,.true. )!
      Var_nom(VAR_S1   )=VAR_NOM_T(.false.,"Section mouillee mineure       ","S1  ","m2    ",2,.true. )!
      Var_nom(VAR_S2   )=VAR_NOM_T(.false.,"Section mouillee majeure       ","S2  ","m2    ",2,.true. )!
      Var_nom(VAR_B1   )=VAR_NOM_T(.false.,"Largeur au miroir mineure      ","B1  ","m     ",2,.true. )!
      Var_nom(VAR_B2   )=VAR_NOM_T(.false.,"Largeur au miroir majeure      ","B2  ","m     ",2,.true. )!
      Var_nom(VAR_BS   )=VAR_NOM_T(.false.,"Largeur au miroir du stockage  ","BS  ","m     ",2,.true. )!
      Var_nom(VAR_P1   )=VAR_NOM_T(.false.,"Perimetre mouille mineur       ","P1  ","m     ",2,.true. )!
      Var_nom(VAR_P2   )=VAR_NOM_T(.false.,"Perimetre mouille majeur       ","P2  ","m     ",2,.true. )!
      Var_nom(VAR_RH1  )=VAR_NOM_T(.false.,"Rayon hydraulique mineur       ","RH1 ","m     ",2,.true. )!
      Var_nom(VAR_RH2  )=VAR_NOM_T(.false.,"Rayon hydraulique majeur       ","RH2 ","m     ",2,.true. )!
      Var_nom(VAR_FR   )=VAR_NOM_T(.true.,"Nombre de Froude                ","FR  ","      ",5,.true. )!
      Var_nom(VAR_BETA )=VAR_NOM_T(.false.,"Coefficient beta modele DEBORD ","BETA","      ",2,.true. )!
      Var_nom(VAR_TAUF )=VAR_NOM_T(.false.,"Contrainte au fond             ","TAUF","Pa    ",6,.true. )!
      Var_nom(VAR_Y    )=VAR_NOM_T(.false.,"Hauteur d'eau                  ","Y   ","m     ",3,.true. )!
      Var_nom(VAR_HMOY )=VAR_NOM_T(.false.,"Hauteur d'eau moyenne          ","HMOY","m     ",3,.true. )!
      Var_nom(VAR_Q2G  )=VAR_NOM_T(.false.,"Debit majeur gauche            ","Q2G ","m3/s  ",2,.true. )!
      Var_nom(VAR_Q2D  )=VAR_NOM_T(.false.,"Debit majeur droit             ","Q2D ","m3/s  ",2,.true. )!
      Var_nom(VAR_VOL  )=VAR_NOM_T(.false.,"Volume du lit actif            ","VOL ","m3    ",0,.true. )!
      Var_nom(VAR_VOLS )=VAR_NOM_T(.false.,"Volume de stockage             ","VOLS","m3    ",0,.true. )!
      Var_nom(VAR_CHARG)=VAR_NOM_T(.false.,"Charge                         ","CHAR","m     ",2,.true. )!
      Var_nom(VAR_SS   )=VAR_NOM_T(.false.,"Section mouillee du stockage   ","SS  ","m2    ",2,.true. )!
      Var_nom(VAR_V1   )=VAR_NOM_T(.false.,"Vitesse mineure                ","VMIN","m/s   ",4,.true. )!
      Var_nom(VAR_V2   )=VAR_NOM_T(.false.,"Vitesse majeure                ","VMAJ","m/s   ",4,.true. )!
      Var_nom(VAR_ZMAX )=VAR_NOM_T(.false.,"Cote maximale atteinte         ","ZMAX","m     ",2,.true. )!
      Var_nom(VAR_TZMAX)=VAR_NOM_T(.false.,"Instant de cote maximale       ","TZMA","s     ",1,.true. )!
      Var_nom(VAR_VZMAX)=VAR_NOM_T(.false.,"Vitesse a la cote maximale     ","VZMX","m/s   ",4,.true. )!
      Var_nom(VAR_ZMIN )=VAR_NOM_T(.false.,"Cote minimale atteinte         ","ZMIN","m     ",2,.true. )!
      Var_nom(VAR_TZMIN)=VAR_NOM_T(.false.,"Instant de cote minimale       ","TZMI","s     ",1,.true. )!
      Var_nom(VAR_V1MIN)=VAR_NOM_T(.false.,"Vitesse mineure minimale       ","VINF","m/s   ",4,.true. )!
      Var_nom(VAR_V1MAX)=VAR_NOM_T(.false.,"Vitesse mineure maximale       ","VSUP","m/s   ",4,.true. )!
      Var_nom(VAR_BMAX )=VAR_NOM_T(.false.,"Largeur au miroir maximale     ","BMAX","m     ",2,.true. )!
      Var_nom(VAR_TOND )=VAR_NOM_T(.false.,"Instant d'arrivee d'onde       ","TOND","s     ",1,.true. )!
      Var_nom(VAR_QMAX )=VAR_NOM_T(.false.,"Debit maximal                  ","QMAX","m3/s  ",2,.true. )!
      Var_nom(VAR_TQMAX)=VAR_NOM_T(.false.,"Instant de debit maximal       ","TQMA","s     ",1,.true. )!
      Var_nom(VAR_EMAX )=VAR_NOM_T(.false.,"Energie maximale               ","EMAX","J     ",3,.true. )!
      Var_nom(VAR_YVRAI )=VAR_NOM_T(.false.,"Hauteur d eau analy           ","YVRAI","m    ",3,.true. )!!..................... fin initialisation du tableau de structure Var_nom .......................!
      Var_nom(VAR_QVRAI )=VAR_NOM_T(.false.,"Debit         analy           ","QVRAI","m    ",3,.true. )!!.
      Var_nom(VAR_QDEV  )=VAR_NOM_T(.false.,"Debit deverse                 ","QDEV","m3/s  ",3,.true. )!!
      Var_nom(VAR_Debi )=VAR_NOM_T(.false.,"Flux de masse            ","Debi","m3/s  ",3,.true. )!!..
   end if label_initialisation

   !============================ Instructions ============================== 
   ! Une variable a sortir est :
   ! - soit une variable     obligatoire `Var_nom(:)%Obligatoire == .true.'
   ! - soit une variable non obligatoire mais qui a ete choisie comme etant a
   !   sortir `Var_nom(:)%Obligatoire == .false. .and. VarASortir(:) == .true.'
   Gdr(:)%ASortir = Var_nom(:)%Obligatoire .or. VarASortir(:)
   Gdr(VAR_X)%Valeur     => X
   Gdr(VAR_ZREF)%Valeur  => ZREF
   Gdr(VAR_RGC)%Valeur   => RGC
   Gdr(VAR_RDC)%Valeur   => RDC
   Gdr(VAR_CF1)%Valeur   => CF1
   Gdr(VAR_CF2)%Valeur   => CF2
   Gdr(VAR_Z)%Valeur     => Z
   Gdr(Var_Q)%Valeur     => Q
   Gdr(VAR_Q1)%Valeur    => Q1
   Gdr(VAR_Q2)%Valeur    => Q2
   Gdr(VAR_S1)%Valeur    => S1
   Gdr(VAR_S2)%Valeur    => S2
   Gdr(VAR_B1)%Valeur    => B1
   Gdr(VAR_B2)%Valeur    => B2
   Gdr(VAR_BS)%Valeur    => BS
   Gdr(VAR_P1)%Valeur    => P1
   Gdr(VAR_P2)%Valeur    => P2
   Gdr(VAR_RH1)%Valeur   => RH1
   Gdr(VAR_RH2)%Valeur   => RH2
   Gdr(VAR_FR)%Valeur    => FR
   Gdr(VAR_BETA)%Valeur  => BETA
   Gdr(VAR_TAUF)%Valeur  => TAUF
   Gdr(VAR_Y)%Valeur     => Y
   Gdr(VAR_HMOY)%Valeur  => HMOY
   Gdr(VAR_Q2G)%Valeur   => Q2G
   Gdr(VAR_Q2D)%Valeur   => Q2D
   Gdr(VAR_VOL)%Valeur   => VOL
   Gdr(VAR_VOLS)%Valeur  => VOLS
   Gdr(VAR_CHARG)%Valeur => CHARG
   Gdr(VAR_SS)%Valeur    => SS
   Gdr(VAR_V1)%Valeur    => V1
   Gdr(VAR_V2)%Valeur    => V2
   Gdr(VAR_ZMAX)%Valeur  => ZMAX
   Gdr(VAR_TZMAX)%Valeur => TZMAX
   Gdr(VAR_VZMAX)%Valeur => VZMAX
   Gdr(VAR_ZMIN)%Valeur  => ZMIN
   Gdr(VAR_TZMIN)%Valeur => TZMIN
   Gdr(VAR_V1MIN)%Valeur => V1MIN
   Gdr(VAR_V1MAX)%Valeur => V1MAX
   Gdr(VAR_BMAX)%Valeur  => BMAX
   Gdr(VAR_TOND)%Valeur  => TOND
   Gdr(VAR_QMAX)%Valeur  => QMAX
   Gdr(VAR_TQMAX)%Valeur => TQMAX
   Gdr(VAR_EMAX)%Valeur  => EMAX
   Gdr(VAR_YVRAI)%Valeur  => YVRAI
   Gdr(VAR_QVRAI)%Valeur  => QVRAI
   Gdr(VAR_QDEV)%Valeur  => Qdeverse
   Gdr(VAR_Debi)%Valeur => DebitFlux

   end subroutine INIT_VAR_SORTIE_S

end module M_INIT_VAR_SORTIE_S
