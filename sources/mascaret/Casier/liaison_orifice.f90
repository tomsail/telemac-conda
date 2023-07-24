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

subroutine  LIAISON_ORIFICE             ( &
             AS, BS, CS                 , &
             ZAM, ZAV, ZfAM, ZfAV       , &
             Liaison                    , &
             Erreur                     )

! *********************************************************************
! PROGICIEL : MASCARET        S. DELMAS    C. COULET
!                             
! VERSION : V8P4R0         EDF-CEREMA-ARTELIA
! *********************************************************************
!  FONCTION :
!  --------
!   CALCUL DES COEFFICIENTS DE L'EQUATION DISCRETISEE D'UN LIAISON
!   DEFINI DE TYPE ORIFICE.
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

   use M_PRECISION          ! type DOUBLE
   use M_PARAMETRE_C        ! constantes numeriques 2/3
   use M_LIAISON_CHENAL_I   ! interface du sous-programme CHENAL
   use M_LIAISON_T
   use M_ERREUR_T           ! Definition du type ERREUR_T
   use M_TRAITER_ERREUR_I   ! Traitement des erreurs

   implicit none

   !.. Formal Arguments ..
   real(DOUBLE)              , intent(  out) :: AS, BS, CS
   real(DOUBLE)              , intent(in   ) :: ZAM, ZAV, ZfAM, ZfAV
   type(LIAISON_T)           , intent(inout) :: Liaison
   type(ERREUR_T)            , intent(inout) :: Erreur
   
   !.. Constantes ..
   integer , parameter :: DE_1_VERS_2 = 1   ! constante reperant le sens de l ecoulement
   integer , parameter :: DE_2_VERS_1 = 2

   !.. Variables locales ..
   real(DOUBLE) :: h1 ,               &  ! tirant d eau casier1
                   h2 ,               &  ! tirant d eau casier2
                   hamont ,           &  ! tirant d eau amont
                   haval ,            &  ! tirant d eau aval
                   coef_geom ,        &  ! coefficient de l equation de la liaison qui ne depend 
                                         ! que des caracteristiques geometriques
                   debit ,            &  ! debit brut de la liaison 
                   ddebit_dzamont ,   &  ! variation du debit par rapport a Zamont
                   ddebit_dzaval ,    &  ! variation du debit par rapport a Zaval
                   deltaz ,           &  ! tirant d eau amont - tirant d eau aval
                   Zplafond ,         &  ! cote du plafond de l'orifice
                   a ,                &  ! hauteur de l'orifice
                   epsil                 ! coefficient de contraction verticale
   integer       :: sens_ecoul           ! repere du sens de l ecoulement


!========================== Instructions ==============================  

   h1 = ZAM - Liaison%Cote
   h2 = ZAV - Liaison%Cote

   ! calcul du sens de l ecoulement
   if( h1 >= h2 ) then
      hamont = h1
      haval = h2
      sens_ecoul = DE_1_VERS_2
   else
      hamont = h2
      haval = h1
      sens_ecoul = DE_2_VERS_1
   end if

   ! correction des termes
   if( hamont <= EPS6 ) return
   if( haval <= 0._DOUBLE ) then
      haval = 0._DOUBLE
   end if

   ! selection sens de l'ecoulement selon type clapet et retour si sens oppose
   select case( sens_ecoul )
      case( DE_1_VERS_2 ) !sens riv-casier ou amont-aval
         select case( Liaison%NatureLiaison )
            case( LIAISON_TYPE_RIVIERE_CASIER )
               if( liaison%TypeOrifice == 2 ) return !le debit est nul pour ce tyoe d'orifice
               !(sens casier-riv)
               case( LIAISON_TYPE_CASIER_CASIER )
               if( liaison%TypeOrifice == 3 ) return
         end select
      case( DE_2_VERS_1 ) !sens casier - riv ou aval-amont
         select case( Liaison%NatureLiaison )
            case( LIAISON_TYPE_RIVIERE_CASIER )
               if( liaison%TypeOrifice == 3 ) return !le debit est nul pour ce type d'orifice
               !(sens riv-casier)
            case( LIAISON_TYPE_CASIER_CASIER )
               if( liaison%TypeOrifice == 2 ) return
         end select
   end select

   deltaz = hamont - haval
   if( deltaz <= EPS6 ) then
      deltaz = EPS6
   end if
   a        = Liaison%Section / Liaison%Largeur  ! hauteur de l'orifice
   Zplafond = Liaison%Cote + a

   !
   ! ORIFICE NON EN CHARGE
   ! --------------------
   if( hamont < a ) then    ! deversoir a surface libre
      Liaison%CoefNoye = 0.2_DOUBLE
      
      call  LIAISON_SEUIL( &
            AS,BS,CS    , & ! Coeff de l'equation discretisee de la singularite
            ZAM,ZAV     , & !
            Liaison     , &
            Erreur      )
      
      
   else
      !
      ! ORIFICE SUBMERGE (hamont > a)
      ! ----------------
      !
      ! CALCUL DE EPSILON
      if( ( a / hamont > 0) .and. ( a / hamont <= 0.55_DOUBLE ) ) then
         epsil = 0.65_DOUBLE
      else if( ( a / hamont > 0.55_DOUBLE) .and. ( a / hamont <= 0.90_DOUBLE ) ) then
         epsil = 0.5_DOUBLE + 0.268_DOUBLE * ( a / hamont )
      else if( ( a / hamont > 0.90_DOUBLE ) .and. ( a / hamont <= 1.0_DOUBLE ) ) then
         epsil = 0.745_DOUBLE + 0.255_DOUBLE * ( a / hamont - 0.9_DOUBLE )
      end if

      ! CALCUL DE BASE
      coef_geom = Liaison%CoefDebitOrifice * epsil * Liaison%Section * DSQRT( 2._DOUBLE * GPES )
      if( haval >= a / 2.D0 ) then  ! ecoulement noye
         debit          = coef_geom * DSQRT( deltaz )
         ddebit_dzamont = coef_geom * W12 / DSQRT( deltaz )
         ddebit_dzaval  = coef_geom * ( -W12 ) / DSQRT( deltaz )
      else if( haval < a / 2._DOUBLE ) then  ! ecoulement denoye
         debit          = coef_geom * DSQRT( hamont - ( a / 2._DOUBLE ) )
         ddebit_dzamont = coef_geom * W12 / DSQRT( hamont - ( a / 2._DOUBLE ) )
         ddebit_dzaval = 0._DOUBLE
      end if

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

end subroutine LIAISON_ORIFICE