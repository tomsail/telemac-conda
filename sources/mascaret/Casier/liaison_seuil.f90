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

subroutine  LIAISON_SEUIL       ( &
            AS,BS,CS    , & ! Coeff de l'equation discretisee de la singularite
            ZAM,ZAV     , & !
            Liaison     , &
            Erreur        & ! Erreur
                        )

! *********************************************************************
! PROGICIEL : MASCARET               S. DELMAS    C. COULET
!
!
! VERSION : V8P4R0         EDF-CEREMA-ARTELIA
! *********************************************************************
!  FONCTION :
!  --------
!
!   CALCUL DES COEFFICIENTS DE L'EQUATION DISCRETISEE D'UN LIAISON
!   DEFINI DE TYPE SEUIL.
!
!
!-----------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :
!   ----------------------
!
!   SOUS PROGRAMME APPELANT :  KLIAI
!   -------------------------
!   SOUS PROGRAMMES APPELES :  ---
!   -------------------------
!
!   COMMENTAIRES :
!   ------------
!
!
! . Q EST > 0 DE L'INDICE 1 VERS L'INDICE 2
!   EN REGIME DENOYE , LA LOI STANDARD EST APPLIQUEE
!   EN REGIME NOYE , LA CORRECTION EST DONNEE PAR LE COEFFICIENT C :
!
! . RH=(ZAVAL-CoteSeuil)/(ZAMONT-CoteSeuil)
!      ---          RH < 0.8   C= +1
!      ---   0.8  < RH < 1.0   C=  C1*RH**3 + C2*RH**2 + C3 *RH + C4
!
! . AS,BS ET CS SONT LES COEFFICIENTS DE L'EQUATION DISCRETISEE :
!   Q = AS*DZAMONT + BS*DZAVAL + CS
!   DZAMONT REPRESENTE DZ1 SI L'ECOULEMENT A LIEU DANS LE SENS 1->2
!   DZAMONT REPRESENTE DZ2 SI L'ECOULEMENT A LIEU DANS LE SENS 2->1
!   DZAVAL  REPRESENTE DZ2 SI L'ECOULEMENT A LIEU DANS LE SENS 1->2
!   DZAVAL  REPRESENTE DZ1 SI L'ECOULEMENT A LIEU DANS LE SENS 2->1
!
!------------------------------------------------------------------------

   !============================ Declarations ==============================
   use M_PRECISION        ! Type DOUBLE
   use M_PARAMETRE_C      ! Parametres de calcul
   use M_MESSAGE_C        ! Liste des messages d'erreur
   use M_LIAISON_T
   use M_ERREUR_T         ! Definition du type ERREUR_T
   use M_TRAITER_ERREUR_I ! Traitement des erreurs

   implicit none

   !.. Formal Arguments ..
   real(DOUBLE)              , intent(  out) :: AS, BS, CS
   real(DOUBLE)              , intent(in   ) :: ZAM, ZAV
   type(LIAISON_T)           , intent(inout) :: Liaison
   type(ERREUR_T)            , intent(inout) :: Erreur

  !.. Constantes ..
   real(DOUBLE) , parameter :: DZMIN = EPS6 ! tous les termes sont annules lorsque
                                            ! hamont est inferieur ou egal a la constante DZMIN
   integer , parameter      :: DE_1_VERS_2 = 1   ! constante reperant le sens de l ecoulement
   integer , parameter      :: DE_2_VERS_1 = 2

   !.. Variables locales ..
   real(DOUBLE) ::  h1,               &  ! tirant d eau casier1
                    h2,               &  ! tirant d eau casier2
                    hamont,           &  ! tirant d eau amont
                    haval,            &  ! tirant d eau aval
                    coef_geom,        &  ! coefficient de l equation de la liaison qui ne depend
                                         ! que des caracteristiques geometriques
                    debit,            &  ! debit brut de la liaison
                    ddebit_dzamont,   &  ! variation du debit par rapport a Zamont
                    ddebit_dzaval,    &  ! variation du debit par rapport a Zaval
                    C,                &  ! represente une fonction dont le but est de faire tendre
                                         ! rapidement ddebit_dzamont et ddebit_dzaval vers zero
                                         ! lorsque haval/hamont tend vers 1, tout en respectant
                                         ! la continuite des derivees; ce coef. est actif lorsque
                                         ! haval/hamont est superieur a la constante COEF
                    dC_dzamont,       &  ! variation de C par rapport a hamont
                    dC_dzaval,        &  ! variation de C par rapport a haval
                    coef_beta,        &  ! valeur de la fonction BETA en hamont et haval
                    LargeurEcoulement

   integer      :: sens_ecoul            ! repere le sens de l ecoulement

   !========================== Instructions ==============================

   !
   ! INITIALISATIONS
   ! ---------------
   Liaison%DebitEchange = 0._DOUBLE

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

   ! correction des termes negligeables
   if( hamont <= DZMIN ) return
   if( haval <= 0._DOUBLE ) then
      haval = 0._DOUBLE
   end if

! DIMINUTION DE LA LARGEUR SI HAMONT FAIBLE
!    hlim = Liaison%Largeur / 1000
!    if( hamont < hlim) then
!        LargeurEcoulement = (hamont * Liaison%Largeur) / hlim
!    else
        LargeurEcoulement = Liaison%Largeur
!    endif

   coef_geom = Liaison%CoefDebitSeuil * LargeurEcoulement * DSQRT( 2._DOUBLE * GPES )

   !
   ! CALCUL DE BASE
   ! --------------
   debit          = coef_geom * hamont**W32
   ddebit_dzamont = coef_geom * W32 * hamont**W12
   ddebit_dzaval  = 0._DOUBLE

   !
   ! CORRECTION COEFFICIENT BETA / partie a commentariser pour test sur coefficient correcteur
   ! ---------------------------
   coef_beta = beta(hamont,haval)
   ! hamont /= 0, cf test sur DZMIN
   if( coef_beta < 1._DOUBLE ) then
      C              = lis(coef_beta)
      dC_dzamont     = dlis(coef_beta) * dbetax(hamont,haval)
      dC_dzaval      = dlis(coef_beta) * dbetay(hamont,haval)
      ddebit_dzamont = dC_dzamont * debit + C * ddebit_dzamont
      ddebit_dzaval  = dC_dzaval * debit + C * ddebit_dzaval
      debit          = C * debit
   end if

   !
   ! RESULTATS DEFINITIFS
   ! --------------------
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

   return

   contains

     !.. Fonctions locales ..
     real(DOUBLE) function lis(X)
       real(DOUBLE), intent(in) :: X
       lis = -2._DOUBLE * X**3._DOUBLE + 3._DOUBLE * X**2._DOUBLE
     end function

     real(DOUBLE) function dlis(X)
       real(DOUBLE), intent(in) :: X
       dlis = -6._DOUBLE * X**2._DOUBLE + 6._DOUBLE * X
     end function

     real(DOUBLE) function beta(X,Y)
       real(DOUBLE), intent(in) :: X,Y
       beta = ( -1._DOUBLE / ( 1._DOUBLE - Liaison%CoefNoye ) ) * ( Y / X - 1._DOUBLE )
     end function

     real(DOUBLE) function dbetax(X,Y)
       real(DOUBLE), intent(in) :: X,Y
       dbetax = ( -1._DOUBLE / ( 1._DOUBLE - Liaison%CoefNoye ) ) * ( -Y / X**2._DOUBLE )
     end function

     real(DOUBLE) function dbetay(X,Y)
       real(DOUBLE), intent(in) :: X,Y
       dbetay = ( -1._DOUBLE / ( 1._DOUBLE - Liaison%CoefNoye ) ) * ( 1._DOUBLE / X )
     end function

end subroutine LIAISON_SEUIL
