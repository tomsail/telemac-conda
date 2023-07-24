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

module M_MASCARET_I
! *********************************************************************
! PROGICIEL : MASCARET         N. GOUTAL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************

   interface

   subroutine MASCARET( &
                        ! Entree/Sorties
                        !---------------
                    ZNODE , & ! Cote
              QMIN , QMAJ , & ! debits  mineur, majeur
              SMIN , SMAJ , & ! section mineur, majeur
                 W , AIRS , & ! Etats 2D  pour le confluent
  YNODE,UNODE,CNODE ,FLUX , & ! Etats 1D pour Mascaret
           DebitFlux      , & ! Flux de masse
     JGNODE,JDNODE ,IFIGE , & ! Indices de planimetrage mascaret
            BETA , Froude , & ! Coefficient de repartition mineur/majeur
                    XFRON , & ! Abscisse du front d'onde
       DT ,Temps, num_pas , & ! Pas de temps optimal
                    Npmax , & !nombre de pas de temps max
             TempsMaximun , & ! Temps maximun
                        ! Conditions aux limites
        Extremite ,apport , & ! Extremites libres
                      Qin , & ! Debit injecte
                 Qdeverse , & !Debit Deverse
                        ! Variables temporels
          PhaseSimulation , &
           Phase_post_imp , &
                  DZ ,DZD , &
                       XD , &
                    NbPas , &
                        ! Planimetrage
              SectionPlan , &
                        ! Modele
                        !-------
                        ! Maillage, Strickler
                        X , & ! Maillage
      STMIN , STMAJ, COTR , & ! Cote du fond
                  nb_sect , & ! nombre de sections
                        ! Reseau
                ZoneSeche , & ! Zone Seche 
                  Connect , & ! Table de connectivite
              Singularite , & ! Singularites 
                  Barrage , & ! XBARP,ZBARP 
                   PCSing , & ! Pertes de charges singulieres
                Deversoir , & ! Deversoirs
                Confluent , & ! Confluents 2D
             SVRAI, QVRAI , & ! VALIDATION
            ZVRAI , ZINIT , & 
                        ! Parametres de calcul
             HEPS  , SEPS , & ! Hauteur et section minimale
                     GPES , & ! acceleration de la pesanteur
                   CALCOS , & ! Calcul d'une onde de submersion
            CALCVA,IVALID , & ! Indicateur et numero de validation
                      REP , & ! Indicateur de reprise de calcul 
                   FROLIM , & ! Indicateur de condition aux limites
                   FRTIMP , & ! Indicateur pour l'impliciation du frottement
        Impli_Trans , Opt , & ! Indicateur pour l'implicitation du solveur
  PerteElargissementTrans , & ! Perte de Charge  
               Boussinesq , & ! Prise en compte de termes non hydro-statiques
                     CQMV , & ! Apport qmv pour les debits d'apport 
                 STOCKAGE , & ! Indicateur de zones de stockage
         PastempsVariable , & ! Indicateur de pas de temps variable 
            NombreCourant , & ! Nombre de Courant limite
                        ! Parametres
               Impression , & ! Flag d'impression
             UniteListing , & ! Unite logique fichier listing
                        ! Etats
             VOLS , Sauve , & ! apport/ provenant des zones de stockage
                   Erreur )

!***********************************************************************
!  FONCTION :
!  --------
!
!       SOUS-PROGRAMME DE CALCUL DU CODE MASCARET
!
!-----------------------------------------------------------------------
!
!                        VARIABLES EN ARGUMENTS
! .____________________________________________________________________
! !   X      ! I  !<-- !   POINTEURS DU TABLEAU   X
! !   XP     ! I  !<-- !   POINTEURS DU TABLEAU   XP
! !   DB     ! I  !<-- !   POINTEURS DU TABLEAU   DB
! !   PZREF  ! I  !<-- !   POINTEURS DU TABLEAU   ZREFP
! !   STP    ! I  !<-- !   POINTEURS DU TABLEAU   STP
! !   XDD    ! I  !<-- !   POINTEURS DU TABLEAU   XD
! !   FM1    ! I  !<-- !   POINTEURS DU TABLEAU   FM1
! !   FP1    ! I  !<-- !   POINTEURS DU TABLEAU   FP1
! !   UNP1   ! I  !<-- !   POINTEURS DU TABLEAU   UNP1
! !   YNP1   ! I  !<-- !   POINTEURS DU TABLEAU   YNP1
! !   SNP1   ! I  !<-- !   POINTEURS DU TABLEAU   SNP1
! !   CNP1   ! I  !<-- !   POINTEURS DU TABLEAU   CNP1
! !   FRNO   ! I  !<-- !   POINTEURS DU TABLEAU   FRNODE
! !   ST     ! I  !<-- !   POINTEURS DU TABLEAU   ST
! !   PRAD   ! I  !<-- !   POINTEURS DU TABLEAU   PRAD
! !   COTR   ! I  !<-- !   POINTEURS DU TABLEAU   COTR
! !   COTRD  ! I  !<-- !   POINTEURS DU TABLEAU   COTRD
! !   YDNP   ! I  !<-- !   POINTEURS DU TABLEAU   YDNP
! !   SDNP   ! I  !<-- !   POINTEURS DU TABLEAU   SDNP
! !   CDNP   ! I  !<-- !   POINTEURS DU TABLEAU   CDNP
! !   QDNP   ! I  !<-- !   POINTEURS DU TABLEAU   QDNP
! !   UDNP   ! I  !<-- !   POINTEURS DU TABLEAU   UDNP
! !   PRADD  ! I  !<-- !   POINTEURS DU TABLEAU   PRADD
! !   YNODE  ! I  !<-- !   POINTEURS DU TABLEAU   YNODE
! !   SNODE  ! I  !<-- !   POINTEURS DU TABLEAU   SNODE
! !   CNODE  ! I  !<-- !   POINTEURS DU TABLEAU   CNODE
! !   KNODE  ! I  !<-- !   POINTEURS DU TABLEAU   AKNODE
! !   QNODE  ! I  !<-- !   POINTEURS DU TABLEAU   QNODE
!-----------------------------------------------------------------------

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_CONSTANTES_CALCUL_C !
   use M_MESSAGE_C
   use M_APPORT_T
   use M_BARRAGE_T
   use M_CONFLUENT_T
   use M_CONNECT_T
   use M_DEVERSOIR_T     ! Definition du type DEVERSOIR_T
   use M_ERREUR_T        ! Definition du type ERREUR_T
   use M_EXTREMITE_T
   use M_SECTION_PLAN_T
   use M_SINGULARITE_T
   use M_ZONE_SECHE_T    ! Type Zone_Seche_T
   use M_SAUVE_T         ! Type Sauve_T
   use M_INTERPOLATION_S
   use M_LIMITE_I      ! Interface du sous-programme LIMITE
   use M_BILAN_I       ! Interface du sous-programme BILAN
   use M_BORNE_I       ! Interface du sous-programme BORNE
   use M_CALCFL_I      ! Interface du sous-programme CALCFL
   use M_CALVAR_I      ! Interface du sous-programme CALVAR
   use M_CONFLU_I      ! Interface du sous-programme CONFLU
   use M_CQINJ_I
   use M_CSUR_I        ! Interface de la fonction    CSUR
   use M_CSURM1_I      ! Interface de la fonction    CSURM1
   use M_DECBAR_I      ! Interface du sous-programme DECBAR
   use M_FROTTD_I      ! Interface du sous-programme FROTTD
   use M_PRECAL_I      ! Interface du sous-programme PRECAL
   use M_PRES_I        ! Interface de la fonction    PRES
   use M_PRESD_I       ! Interface de la fonction    PRESD
   use M_RESOL_I       ! Interface du sous-programme RESOL
   use M_VALIDA_I      ! Interface du sous-programme VALIDA
   use M_TRAITER_ERREUR_I ! Interface du programme de traitement des erreurs 

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   ! Entrees/Sorties
   real(DOUBLE), dimension(:), intent(inout) :: ZNODE
   real(DOUBLE), dimension(:), intent(inout) :: SMIN
   real(DOUBLE), dimension(:), intent(inout) :: SMAJ
   real(DOUBLE), dimension(:), intent(inout) :: QMIN
   real(DOUBLE), dimension(:), intent(inout) :: QMAJ
   real(DOUBLE), dimension(:), intent(inout) :: BETA
   real(DOUBLE), dimension(:), intent(inout) :: FROUDE
   real(DOUBLE), dimension(:), intent(inout) :: YNODE
   real(DOUBLE), dimension(:), intent(inout) :: CNODE
   integer,      dimension(:), intent(inout) :: JGNODE,JDNODE,IFIGE
   real(DOUBLE) ,dimension(:,:),intent(inout):: FLUX
   real(DOUBLE) ,dimension(:)  ,intent(inout):: DebitFlux
   real(DOUBLE), dimension(:), intent(inout) :: UNODE
   ! 1ere dimension nb_bief
   real(DOUBLE), dimension(:), intent(inout) :: XFRON
   real(DOUBLE)              , intent(in)    :: Temps
   integer                   , intent(in   ) :: PhaseSimulation
   integer                   , intent(in   ) :: Phase_post_imp
   type(APPORT_T)         , dimension(:), intent(in   ) :: Apport
   type(EXTREMITE_T)      , dimension(:), intent(in   ) :: Extremite
   Real(DOUBLE)           , dimension(:), intent(inout) :: Qdeverse ,QIN
   type(ZONE_SECHE_T)     , dimension(:), pointer       :: ZoneSeche
   type(SAUVE_T)     ,                    intent(inout) :: Sauve
   ! Maillage
   real(DOUBLE), dimension(:), intent(in   ) ::  X
   real(DOUBLE), dimension(:), intent(in   ) ::  XD
   real(DOUBLE), dimension(:), intent(in   ) ::  STMIN
   real(DOUBLE), dimension(:), intent(in   ) ::  STMAJ
   real(DOUBLE), dimension(:), intent(in   ) ::  COTR
   ! Reseau
   type(CONNECT_T), intent(in   ) :: CONNECT
   ! Planimetrage
   real(DOUBLE), dimension(:), intent(in   ) ::  DZ
   real(DOUBLE), dimension(:), intent(in   ) ::  DZD
   integer                   , intent(in   ) ::  NbPas
   integer                   , intent(in   ) ::  num_pas,Npmax
   integer                   , intent(in   ) ::  Nb_sect
   !  ! 1ere dimension IM
   !  ! 1ere dimension IM1
   type(SECTION_PLAN_T),              intent(in   ) :: SectionPlan
   ! Donnees temporelles
   real(DOUBLE)             , intent(inout   ) :: DT
   ! Confluents
   type(CONFLUENT_T)     , dimension(:), intent(in   ) :: Confluent
   ! Pertes de charge singulieres
   real(DOUBLE)        , dimension(:), intent(in   ) :: PCSing
   ! Deversoirs
   type (DEVERSOIR_T)  , dimension(:), intent(in   ) :: Deversoir
   ! Singularite
   type(SINGULARITE_T) , dimension(:), intent(inout) :: Singularite
   ! Barrage Principal 
   type(BARRAGE_T)                   , intent(in   ) :: Barrage 
   !  ! 2nde dimension 12, 3eme dimension nb_noeud
   real(DOUBLE), dimension (:,:,:)  , intent(inout)  ::  W
   !  ! 1ere dimension 12, 2nde dimension nb_noeud
   real(DOUBLE) , dimension (:,:)   , intent(inout)  :: AIRS
   ! Resultats analytiques
   real(DOUBLE), dimension(:) :: SVRAI
   real(DOUBLE), dimension(:) :: QVRAI
   real(DOUBLE), dimension(:) :: ZVRAI
   real(DOUBLE), dimension(:) :: ZINIT
   ! Parametres de calcul
   real(DOUBLE), intent(in   ) :: FROLIM
   real(DOUBLE), intent(in   ) :: HEPS
   real(DOUBLE), intent(in   ) :: SEPS
   real(DOUBLE), intent(in   ) :: GPES
   real(DOUBLE), intent(in   ) :: NombreCourant 
   real(DOUBLE), intent(in   ) :: TempsMaximun
   integer     , intent(in   ) :: IVALID
   logical     , intent(in   ) :: STOCKAGE
   logical     , intent(in   ) :: FRTIMP
   logical     , intent(in   ) :: Impli_Trans,Opt
   logical     , intent(in   ) :: PerteElargissementTrans
   logical     , intent(in   ) :: Boussinesq
   integer     , intent(in   ) :: CQMV
   logical     , intent(in   ) :: CALCOS
   logical     , intent(in   ) :: CALCVA
   logical     , intent(in   ) :: REP
   logical     , intent(in   ) :: Impression
   integer     , intent(in   ) :: UniteListing
   logical     , intent(in   ) :: PasTempsVariable
   ! Etat
   ! Apport et soutirage zones de stockage
   real(DOUBLE), dimension(:), pointer :: VOLS
   ! Erreur
   type(ERREUR_T)                    , intent(inout) :: Erreur

   end subroutine MASCARET

   end interface

end module M_MASCARET_I
