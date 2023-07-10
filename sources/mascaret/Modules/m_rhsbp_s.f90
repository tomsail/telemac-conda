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

module M_RHSBP_S
!***********************************************************************
! PROGICIEL : MASCARET      S. MANDELKERN
!
! VERSION : V8P4R0             EDF-CEREMA
!***********************************************************************
   contains

   subroutine RHSBP_GENERIQUE_S ( &
        var_sect                , & ! Variable a interpoler avec les profils
        var_prof                , & ! Variable des profils de donnees
        ZREF                    , & ! Cote du fond
        Z                       , & ! Cote d'eau a une section de calcul
        IDT                     , & ! Indices   des sections de donnees
        XDT                     , & ! Positions des sections de donnees
        PROF                    , & ! Structure des profils  de donnees
        NB_SECT                 , & ! Nombre de sections
        Erreur                    & ! Erreur
                               )

   ! **********************************************************************
   !  FONCTION :
   !  --------
   !         CALCUL D'<<UNE>> VARIABLE INTERPOLEE SUR LES PROFILS DE DONNEES
   !         POUR TOUTES LES SECTIONS DE CALCUL
   !
   !  SOUS-PROGRAMME(S) APPELANT(S) : POST,
   !  -----------------------------
   !  SOUS-PROGRAMME(S) APPELE(S)   : Neant
   !  ---------------------------
   !***********************************************************************

   !============================= Declarations ===========================
   !.. Modules importes ..
   use M_PRECISION
   use M_PROFIL_T              ! Definition du type PROFIL
   use M_ERREUR_T              ! Definition de la structure ERREUR_T
   use M_TRAITER_ERREUR_I      ! Traitement de l'erreur
   use M_PARAMETRE_C           ! EPS3, EPS6

   !.. Declaration Explicite ..
   implicit none

   !.. Arguments ..
   real(DOUBLE)   , dimension(:)  , intent(  out) :: var_sect
   real(DOUBLE)   , dimension(:,:), intent(in   ) :: var_prof
   real(DOUBLE)   , dimension(:)  , intent(in   ) :: ZREF
   real(DOUBLE)   , dimension(:)  , intent(in   ) :: Z
   integer        , dimension(:)  , intent(in   ) :: IDT
   real(DOUBLE)   , dimension(:)  , intent(in   ) :: XDT
   type (PROFIL_T), dimension(:)  , intent(in   ) :: PROF
   integer                        , intent(in   ) :: NB_SECT
   type (ERREUR_T)                , intent(inout) :: Erreur

   !.. Variables locales ..
   real(DOUBLE), dimension(NB_SECT)   :: pas, yd
   real(DOUBLE), dimension(NB_SECT)   :: y_loc
   real(DOUBLE), dimension(NB_SECT,2) :: fs
   integer     , dimension(NB_SECT)   :: kdt, ip1t, nbpas
   integer        :: i                ! Compteur sur les sections
   !character(132) :: !arbredappel_old

   !============================ Instructions =================================
   Erreur%Numero = 0
   !arbredappel_old = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>RHSBP_GENERIQUE'

   y_loc(:) = Z(:) - ZREF(:)

   ! Cas des sections a sec
   where( y_loc(:) <= EPS3 )
      y_loc(:) = EPS3
   end where

   where( XDT(:) <= EPS6 )
      ip1t(:) = IDT(:)
   elsewhere
      ip1t(:) = IDT(:) + 1
   end where

   pas(:)  = PROF(IDT(:))%Pas + ( PROF(ip1t(:))%Pas - PROF(IDT(:))%Pas ) * XDT(:)
   nbpas(:)= PROF(IDT(:))%NbPas

   ! TIRANT D'EAU CORRECT (POSITIF)
   kdt(:) = int( y_loc(:) / pas(:) ) + 1
   where( kdt(:) >= nbpas(:) )
       kdt(:) = nbpas(:) - 1
   end where
   yd(:) = y_loc(:) - ( kdt(:) - 1 ) * pas(:)

   do i = 1 , NB_SECT
      if( y_loc(i) >= EPS6 ) then
         fs(i,1) = var_prof(idt(i),kdt(i)) + ( var_prof(ip1t(i),kdt(i)) - var_prof(idt(i),kdt(i)) ) * XDT(i)
         fs(i,2) = var_prof(idt(i),kdt(i)+1) + ( var_prof(ip1t(i),kdt(i)+1) - var_prof(idt(i),kdt(i)+1) ) * XDT(i)
         var_sect(i) = fs(i,1) + (fs(i,2) - fs(i,1)) * yd(i) / pas(i)
      else
         var_sect(i) = 0._DOUBLE
      end if
   end do

   !Erreur%arbredappel = !arbredappel_old

   return

   end subroutine RHSBP_GENERIQUE_S

   !========================================================================
   subroutine RHSBP_S                   ( &
          B1,B2,BS,P1,P2,S1,S2,R1,R2    , & ! Variables hydrauliques
          Section                       , & ! Indice de la section de calcul
          Z,ZREF                        , & ! Cote et cote de reference
          IDT,XDT                       , & ! positionnement section/profils
         Profil, PROF                   , & ! Profil et Profils planimetres
          UniteListing                  , & ! Unite logique fichier listing
          Erreur                          & ! Erreur
                                         )

   !***********************************************************************
   ! PROGICIEL : MASCARET      A. LEBOSSE
   !                             P. CHERUBINI
   !                             S. PERON
   !                             S. MANDELKERN
   !
   ! VERSION : V8P4R0             EDF-CEREMA
   !***********************************************************************
   !   FONCTION :
   !   --------
   !
   !   CALCUL DU RAYON HYDAULIQUE, DE LA SURFACE MOUILLEE, DE LA LARGEUR
   !   AU MIROIR ET DU PERIMETRE MOUILLE DANS UNE SECTION DE
   !   DE CALCUL (LIT MINEUR, ,LIT MAJEUR)
   !
   ! ----------------------------------------------------------------------
   ! ARGUMENTS
   ! .________________.____._______________________________________________
   ! !    NOM    !TYPE!MODE!                   ROLE
   ! !___________!____!____!______________________________________________
   ! ! B1,B2,BS  ! R  !<-- ! LARGEUR AU MIROIR   )
   ! ! P1,P2     ! R  !<-- ! PERIMETRE MOUILLE   )
   ! ! S1,S2     ! R  !<-- ! SECTION MOUILLEE    ) INDICE 1 LIT MINEUR
   ! ! R1,R2     ! R  !<-- ! RAYON HYDRAULIQUE   )        2 LIT MAJEUR
   ! ! Section   ! I  ! -->! SECTION DE CALCUL            S STOCKAGE
   ! ! Z         ! R  ! -->! COTE D'EAU DANS LA SECTION Section
   ! ! ZREF      ! R  ! -->! COTE DU FOND
   ! ! IDT       ! I  ! -->!  )PERMET DE DETERMINER LA SECTION DE CALCUL A
   ! ! XDT       ! R  ! -->!  )PARTIR DES SECTIONS DE DONNEES
   ! !DB1,DB2,DBS! T  ! -->! LARGEUR AU MIROIR  )  PLANIMETRAGE
   ! !DP1,DP2    ! T  ! -->! PERIMETRE MOUILLE  )  1= MINEUR   2= MAJEUR
   ! !DS1,DS2    ! T  ! -->! SECTION MOUILEE    )  S= STOCKAGE
   ! !___________!____!____!______________________________________________
   !
   !  VARIABLES LOCALES
   ! .___________.________________________________________________________
   ! !   FB1,FB2 ! R  ! -- !
   ! !   FP1,FP2 ! R  ! -- !
   ! !   FS1,FS2 ! R  ! -- !
   ! !   FS2G    ! R  ! -- !
   ! !   PAS     ! R  ! -->! PAS EN HAUTEUR DU PLANIMETRAGE D'UN PROFIL
   ! !   NBPAS   ! R  ! -->! NOMBRE DE PAS DE PLANIMETRAGE
   ! !___________!____!____!______________________________________________
   !  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
   !               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
   ! ----------------------------------------------------------------------
   !
   !   FICHIERS ENTREE/SORTIE :    - UniteListing : IMPRESSION DES RESULTATS GLOBAUX
   !   ----------------------
   !   SOUS PROGRAMMES APPELANTS :  REZO, CRITIQ, PERMAT, QREPAR, SARAP
   !   ---------------------------
   !   SOUS PROGRAMMES APPELES :    ---
   !   -------------------------

   !============================ Declarations ==============================
   use M_PRECISION
   use M_MESSAGE_C
   use M_PARAMETRE_C
   use M_PROFIL_T
   use M_PROFIL_PLAN_T
   use M_FICHIER_T
   use M_ERREUR_T
   use M_TRAITER_ERREUR_I      ! Traitement de l'erreur

   implicit none

   !.. Formal Arguments ..
   real(DOUBLE), intent(out) :: B1, B2, BS, P1, P2, S1, S2, R1, R2
   type (PROFIL_T), dimension(:), intent(in) :: Profil
   type (PROFIL_PLAN_T)         , intent(in) :: Prof
   integer     , intent(in   ) :: Section
   real(DOUBLE), intent(inout) :: Z
   real(DOUBLE), intent(in   ) :: ZREF
   real(DOUBLE), dimension(:), intent(in   ) :: XDT
   integer     , dimension(:), intent(in   ) :: IDT
   integer     , intent(in   ) :: UniteListing
   type (ERREUR_T)           , intent(inout) :: Erreur

   !.. Local Scalars ..
   integer        :: I,IP1,J,K
   real(DOUBLE)   :: PAS,XD,Y,YD
   integer        :: NBPAS
   !character(132) :: !arbredappel_old

   ! Les Constantes sont declares dans le module M_PARAMETRES_C
   !.. Local Arrays ..
   real(DOUBLE), dimension(2) :: FB1,FB2,FBS,FP1,FP2,FS1,FS2

   !.. Intrinsic Functions ..
   intrinsic INT

   !============================= Instructions =============================
   ! INITIALISATIONS
   ! ---------------
   Erreur%Numero = 0
   !arbredappel_old = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>RHSBP'

   S1  = W0
   S2  = W0
   B1  = W0
   B2  = W0
   BS  = W0
   R1  = W0
   R2  = W0
   FS1 = W0
   Y   = Z - ZREF

   ! Cas des sections a sec
   if( Y <= EPS3 ) then
      Erreur%Numero = 1
      Erreur%ft   = err_1
      Erreur%ft_c = err_1c
      call TRAITER_ERREUR (Erreur, Section)
      return
   end if

   XD  = XDT(Section)
   I   = IDT(Section)
   IP1 = I + 1
   if( XD <= EPS6 ) then
      IP1 = I
   end if

   PAS   = Profil(I)%Pas + ( Profil(IP1)%Pas - Profil(I)%Pas ) * XD
   NBPAS = Profil(I)%NbPas

   ! TIRANT D'EAU CORRECT (POSITIF)
   ! ------------------------------
   K = INT( Y / PAS ) + 1
   if( K >= NBPAS ) then
      K = NBPAS - 1
      if (UniteListing>0) then
         write (UniteListing,10000) Section, Y, NBPAS, PAS
      endif
   end if

   YD = Y - ( K - 1 ) * PAS

   do J = 1 , 2
      FB1(J) = Prof%B1(I,K) + ( Prof%B1(IP1,K) - Prof%B1(I,K) ) * XD
      FB2(J) = Prof%B2(I,K) + ( Prof%B2(IP1,K) - Prof%B2(I,K) ) * XD
      FP1(J) = Prof%P1(I,K) + ( Prof%P1(IP1,K) - Prof%P1(I,K) ) * XD
      FP2(J) = Prof%P2(I,K) + ( Prof%P2(IP1,K) - Prof%P2(I,K) ) * XD
      FS1(J) = Prof%S1(I,K) + ( Prof%S1(IP1,K) - Prof%S1(I,K) ) * XD
      FS2(J) = Prof%S2(I,K) + ( Prof%S2(IP1,K) - Prof%S2(I,K) ) * XD
      FBS(J) = Prof%BS(I,K) + ( Prof%BS(IP1,K) - Prof%BS(I,K) ) * XD
      K = K + 1
   end do

   B1 = FB1(1) + ( FB1(2) - FB1(1) ) * YD / PAS
   B2 = FB2(1) + ( FB2(2) - FB2(1) ) * YD / PAS
   P1 = FP1(1) + ( FP1(2) - FP1(1) ) * YD / PAS
   P2 = FP2(1) + ( FP2(2) - FP2(1) ) * YD / PAS
   S1 = FS1(1) + ( FS1(2) - FS1(1) ) * YD / PAS
   S2 = FS2(1) + ( FS2(2) - FS2(1) ) * YD / PAS
   BS = FBS(1) + ( FBS(2) - FBS(1) ) * YD / PAS
   R1 = S1 / P1

   if( BS < EPS3 ) then
      BS = 0._DOUBLE
   end if

   if( P2 > EPS3 ) then
      R2 = S2 / P2
   else
      R2 = 0._DOUBLE
      S2 = 0._DOUBLE
      P2 = 0._DOUBLE
      B2 = 0._DOUBLE
   end if

   !Erreur%arbredappel = !arbredappel_old

   return

! ... Format Declarations ...
   10000 format (                                                                &
        '<< ATTENTION >>',                                                      &
        'Dans la section de calcul n0 ',i4,/,                                   &
        'Tirant d''eau = ',g15.7,' depassant la hauteur du profil',/,           &
        'Augmenter le nombre de pas de planimetrage : ',i4,' ou le pas : ',f7.2)

end subroutine RHSBP_S

!=================================================================

   subroutine RHSBP_SECTION_S  ( &
     VarSect                   , & ! Variable a interpoler sur les profils
     ZREF                      , & ! Cote du fond a la section de calcul
     Z                         , & ! Cote d'eau   a la section de calcul
     IDT                       , & ! Indices du profil de donnees amont
     XDT                       , & ! Position de la section / profils
     Profil                    , & ! Profils geometriques
     VarProf                   , & ! Variable planimetree
     Erreur                      & ! Erreur
                               )

   !***********************************************************************
   ! PROGICIEL : MASCARET      S. MANDELKERN
   !
   ! VERSION : V8P4R0             EDF-CEREMA
   !***********************************************************************
   !
   !  FONCTION :
   !  --------
   !         CALCUL D'<<UNE>> VARIABLE INTERPOLEE SUR LES PROFILS DE DONNEES
   !         A <<UNE>> SECTION DE CALCUL
   !
   !  SOUS-PROGRAMME(S) APPELANT(S) : - PSING
   !  -----------------------------
   !  SOUS-PROGRAMME(S) APPELE(S)   : ---
   !  ---------------------------
   !***********************************************************************

   !============================= Declarations ===========================
   !.. Modules importes ..
   use M_PRECISION
   use M_PROFIL_T              ! Definition du type PROFIL
   use M_ERREUR_T              ! Definition de la structure ERREUR_T
   use M_TRAITER_ERREUR_I      ! Traitement de l'erreur
   use M_PARAMETRE_C           ! EPS3, EPS6
   !.. Declaration Explicite ..
   implicit none

   !.. Arguments ..
   real(DOUBLE)                    , intent(  out) :: VarSect
   real(DOUBLE)   , dimension(:,:) , intent(in   ) :: VarProf
   real(DOUBLE)                    , intent(in   ) :: ZREF
   real(DOUBLE)                    , intent(in   ) :: Z
   integer                         , intent(in   ) :: IDT
   real(DOUBLE)                    , intent(in   ) :: XDT
   type(PROFIL_T) , dimension(:)   , intent(in   ) :: Profil
   type(ERREUR_T)                  , intent(inout) :: Erreur
   !.. Variables locales ..
   real(DOUBLE)               :: pas     ! Pas de planimetrage
   real(DOUBLE)               :: yd      ! Hauteur d'eau a la section
   real(DOUBLE)               :: y_loc   ! Hauteur d'eau a la section
   real(DOUBLE), dimension(2) :: fs      ! Fonction locale d'interpolation
   integer                    :: kdt     ! Indice
   integer                    :: ip1t    ! indice du profil aval de la section
   integer                    :: nbpas   ! Nombre de pas de planimetrage
   !character(132)             :: !arbredappel_old

   !============================ Instructions =================================
   Erreur%Numero = 0
   !arbredappel_old = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>RHSBP_SECTION_S'

   y_loc = Z - ZREF

   !------------------------
   ! Cas d'une section a sec
   !------------------------
   if( y_loc <= EPS3 ) then
      y_loc = EPS3
   end if

   !---------------------------------------
   ! Si on est tres proche du profil amont,
   ! pas d'interpolation amont-aval
   !---------------------------------------
   if( XDT <= EPS6 ) then
      ip1t = IDT
   else
      ip1t = IDT + 1
   end if

   !-----------------------------------------------
   ! Calcul du pas et du nombre de pas a la section
   !-----------------------------------------------
   pas   = Profil(IDT)%Pas + ( Profil(ip1t)%Pas - Profil(IDT)%Pas ) * XDT
   nbpas = Profil(IDT)%NbPas

   ! TIRANT D'EAU CORRECT (POSITIF)
   kdt = int( y_loc / pas ) + 1
   if( kdt >= nbpas ) then
       kdt = nbpas - 1
   end if

   yd = y_loc - ( kdt - 1 ) * pas
   if( y_loc >= EPS6 ) then
      fs(1) = VarProf(IDT,kdt) + ( VarProf(ip1t,kdt) - VarProf(IDT,kdt) ) * XDT
      fs(2) = VarProf(IDT,kdt+1) + ( VarProf(ip1t,kdt+1) - VarProf(IDT,kdt+1) ) * XDT
      VarSect = fs(1) + ( fs(2) - fs(1) ) * yd / pas
   else
      VarSect = 0._DOUBLE
   end if

   !Erreur%arbredappel = !arbredappel_old

   return

   end subroutine RHSBP_SECTION_S

end module M_RHSBP_S
