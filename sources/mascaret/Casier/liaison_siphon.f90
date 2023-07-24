!== Copyright (C) 2000-2017 EDF-CEREMA-ARTELIA ==
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

subroutine  LIAISON_SIPHON      ( &
            AS,BS,CS            , & ! Coeff de l'equation discretisee de la singularite
            ZAM,ZAV             , & !
            ZfAM, ZfAV          , &
            Liaison             , &
            Erreur              & ! Erreur
                                )

! *********************************************************************
! PROGICIEL : MASCARET        S. DELMAS    C. COULET
!                             
! VERSION : V8P4R0         EDF-CEREMA-ARTELIA
! *********************************************************************
!  FONCTION :
!  --------
!   CALCUL DES COEFFICIENTS DE L'EQUATION DISCRETISEE D'UN LIAISON
!   DEFINI DE TYPE SIPHON.
!-----------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :
!   ----------------------
!   SOUS PROGRAMME APPELANT :  KLIAI
!   -------------------------
!   SOUS PROGRAMMES APPELES :  ---
!   -------------------------
!   COMMENTAIRES :
!   ------------
!
!------------------------------------------------------------------------

!========================== Declarations ==============================

   use M_PRECISION    ! type DOUBLE
   use M_PARAMETRE_C  ! constantes numeriques 2/3
   use M_LIAISON_CHENAL_I     ! interface du sous-programme CHENAL
   use M_LIAISON_T
   use M_ERREUR_T         ! Definition du type ERREUR_T
   use M_TRAITER_ERREUR_I ! Traitement des erreurs

   implicit none

   !.. Formal Arguments ..
   real(DOUBLE)              , intent(  out) :: AS, BS, CS
   real(DOUBLE)              , intent(in   ) :: ZAM, ZAV, ZfAM, ZfAV
   type(LIAISON_T)           , intent(inout) :: Liaison
   type(ERREUR_T)            , intent(inout) :: Erreur

!.. Constantes ..
   real(DOUBLE), parameter :: DZMIN = EPS6     ! tous les termes sont annules lorsque
                                               ! hamont est inferieur ou egal a la constante DZMIN
   integer, parameter      :: DE_1_VERS_2 = 1  ! constante reperant le sens de l ecoulement
   integer, parameter      :: DE_2_VERS_1 = 2

!.. Variables locales ..
   real(DOUBLE) ::  h1             , &  ! tirant d eau casier1
                    h2             , &  ! tirant d eau casier2
                    hamont         , &  ! tirant d eau amont
                    haval          , &  ! tirant d eau aval
                    coef_geom      , &  ! coefficient de l equation de la liaison qui ne depend 
                                        ! que des caracteristiques geometriques
                    debit          , &  ! debit brut de la liaison 
                    ddebit_dzamont , &  ! variation du debit par rapport a Zamont
                    ddebit_dzaval  , &  ! variation du debit par rapport a Zaval
                    deltaz         , &  ! tirant d eau amont - tirant d eau aval
                    racine_sect    , &  ! racine carree de la section du siphon
                    K_chenal            ! coefficient de rugosite du chenal equivalent si 
                                        ! siphon non en charge
   integer      :: sens_ecoul           ! repere du sens de l ecoulement

!========================== Instructions ==============================  

!
! INITIALISATIONS
! ---------------

   h1 = ZAM - Liaison%Cote
   h2 = ZAV - Liaison%Cote

   ! calcul du sens de l ecoulement
   if( h1 >= h2 ) then
      hamont     = h1
      haval      = h2
      sens_ecoul = DE_1_VERS_2
   else
      hamont     = h2
      haval      = h1
      sens_ecoul = DE_2_VERS_1
   end if

   ! correction des termes
   if( hamont <= DZMIN ) return
   if( haval <= 0._DOUBLE ) then
      haval = 0._DOUBLE
   end if

   deltaz = hamont - haval

   if( deltaz <= DZMIN ) then
      deltaz = DZMIN
   end if

   racine_sect = DSQRT( Liaison%Section )
   coef_geom   = DSQRT( ( 2._DOUBLE * GPES * racine_sect ) / ( Liaison%CoefPerteCharge &
                * Liaison%Longueur ) ) * Liaison%Section

!
! SIPHON NON EN CHARGE
! --------------------

   if( hamont < racine_sect ) then
      K_chenal         = DSQRT( 2._DOUBLE * GPES / Liaison%CoefPerteCharge ) / racine_sect**W16 
      Liaison%Largeur  = racine_sect
      Liaison%Rugosite = K_chenal

      call LIAISON_CHENAL                       ( &
                     AS,BS,CS                   , &
                     ZAM, ZAV, ZfAM, ZfAV       , &
                     Liaison                    , &
                     Erreur                     )
   else
!
! SIPHON EN CHARGE
! ----------------
!
! CALCUL DE BASE
!
      debit          = coef_geom * DSQRT( deltaz )
      ddebit_dzamont = coef_geom * W12 / DSQRT( deltaz )
      ddebit_dzaval  = coef_geom * (-W12) / DSQRT( deltaz )
!
! RESULTATS DEFINITIFS
!
        select case( sens_ecoul )

            case( DE_1_VERS_2 )

                CS      = debit
                AS      = ddebit_dzamont
                BS      = ddebit_dzaval

            case( DE_2_VERS_1 )

                CS      = - debit
                AS      = - ddebit_dzaval
                BS      = - ddebit_dzamont

        end select
   end if

   return

end subroutine LIAISON_SIPHON