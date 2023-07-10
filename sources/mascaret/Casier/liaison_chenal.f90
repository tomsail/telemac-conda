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

subroutine  LIAISON_CHENAL       ( &
            AS,BS,CS            , & ! Coeff de l'equation discretisee de la singularite
            ZAM,ZAV             , & !
            ZfAM, ZfAV          , &
            Liaison             , &
            Erreur              & ! Erreur
                                )

! *********************************************************************
! PROGICIEL : MASCARET    S. DELMAS    C. COULET
!
!
!
! VERSION : V8P4R0         EDF-CEREMA-ARTELIA
! *********************************************************************
!  FONCTION :
!  --------
!
!   CALCUL DES COEFFICIENTS DE L'EQUATION DISCRETISEE D'UN LIAISON
!   DEFINI DE TYPE CHENAL.
!
!
!-----------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :
!   ----------------------
!
!   SOUS PROGRAMME APPELANT :  KLIAISON
!   -------------------------
!   SOUS PROGRAMMES APPELES :  ---
!   -------------------------
!
!   COMMENTAIRES :
!   ------------
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
   real(DOUBLE)              , intent(in   ) :: ZAM, ZAV, ZfAM, ZfAV
   type(LIAISON_T)           , intent(in   ) :: Liaison
   type(ERREUR_T)            , intent(inout) :: Erreur

   !.. Constantes ..
   real(DOUBLE), parameter :: DZMIN = EPS6       ! tous les termes sont annules lorsque
                                                 ! hamont est inferieur ou egal a la constante DZMIN
   real(DOUBLE), parameter :: COEF = 0.8_DOUBLE  ! cf. commentaire C
   integer, parameter      :: DE_1_VERS_2 = 1    ! constante reperant le sens de l ecoulement
   integer, parameter      :: DE_2_VERS_1 = 2

   !.. Variables locales ..
   real(DOUBLE) :: h1 , &  ! tirant d eau casier1 (par rapport a la cote moyenne du chenal)
                   h2 , &  ! tirant d eau casier2
               hamont , &  ! tirant d eau reel a l'amont physique du chenal
                haval , &  ! tirant d eau reel a l'aval physique du chenal
                 hmax , &  ! tirant d'eau a l'amont du chenal (par rapport a la cote moy)
                 hmin , &  ! tirant d'eau a l'aval du chenal (par rapport a la cote moy)
               deltaz , &  ! tirant d eau amont - tirant d eau aval
            coef_geom , &  ! coefficient de l equation de la liaison qui ne depend
                           ! que des caracteristiques geometriques
               debit ,  &  ! debit brut de la liaison
      ddebit_dzamont ,  &  ! variation du debit par rapport a Zamont
       ddebit_dzaval ,  &  ! variation du debit par rapport a Zaval
              hmoyen ,  &  ! moyenne de hamont et haval
                   C ,  &  ! represente une fonction dont le but est de faire tendre
                           ! rapidement ddebit_dzamont et ddebit_dzaval vers zero
                           ! lorsque haval/hamont tend vers 1, tout en respectant
                           ! la continuite des derivees; ce coef. est actif
                           ! lorsque haval/hamont est superieur a la constante COEF
          dC_dzamont , &   ! variation de C par rapport a hamont
           dC_dzaval , &   ! variation de C par rapport a haval
           coef_beta , &   ! valeur de la fonction BETA en hamont et haval
   LargeurEcoulement , &   ! largeur de l'ecoulement dans la liaison
 CoteAmont, CoteAval , &
       Pamont, Paval , &
            h, hlim



   integer :: sens_ecoul   ! repere du sens de l ecoulement


   !========================== Instructions ==============================

   !
   ! INITIALISATIONS
   ! ---------------

   AS = 0._DOUBLE
   BS = 0._DOUBLE
   CS = 0._DOUBLE

   h1 = ZAM - Liaison%Cote ! comparaison par rapport a la cote de reference du chenal
   h2 = ZAV - Liaison%Cote ! (cote moyenne)


   if( ( Liaison%Cote < ZfAM ) .or. ( Liaison%Cote < ZfAV ) ) then ! chenal avec pente uniforme
       if( ZAM >= ZAV ) then
           CoteAmont  = ZAM
           CoteAval   = ZAV
           Pamont     = ZfAM
           Paval      = ZfAV
           sens_ecoul = DE_1_VERS_2
       else
           CoteAmont  = ZAV
           CoteAval   = ZAM
           Pamont     = ZfAV
           Paval      = ZfAM
           sens_ecoul = DE_2_VERS_1
       end if

   else   ! cas du chenal horizontal
      Pamont     = Liaison%Cote
      Paval      = Liaison%Cote
      if( ZAM >= ZAV ) then
         CoteAmont  = ZAM
         CoteAval   = ZAV
         sens_ecoul = DE_1_VERS_2
      else
         CoteAmont  = ZAV
         CoteAval   = ZAM
         sens_ecoul = DE_2_VERS_1
      end if
   end if

   hamont = CoteAmont - Pamont
   haval  = CoteAval  - Paval

   ! correction des termes negligeables
   !
    if( hamont <= DZMIN ) return !le debit reste nul si le casier est vide

    if( haval <= 0._DOUBLE ) then
        haval    = 0._DOUBLE
        CoteAval = Paval
    end if

    deltaz = CoteAmont - CoteAval  ! On previent les faibles valeurs de deltaz pour eviter les instabilites?
    if( deltaz <= DZMIN ) then
        deltaz = DZMIN
    end if

    ! DIMINUTION DE LA LARGEUR SI HAMONT <<< LARGEUR              :::: INDISPENSABLE ::::
    hlim = Liaison%Largeur / 1000
    if( hamont < hlim ) then
        LargeurEcoulement = (hamont * Liaison%Largeur) / hlim
    else
        LargeurEcoulement = Liaison%Largeur
    endif

    hmoyen = W12 * ( hamont + haval )
    h      = min(hamont, hmoyen)

    coef_geom = ( Liaison%Rugosite * LargeurEcoulement ) / DSQRT( Liaison%Longueur ) ! Longueur /= 0 => PRETRAIT_CASIER

    !
    ! CALCUL DE BASE
    ! --------------
    debit          = coef_geom * h**W53 * DSQRT( deltaz )
    ddebit_dzamont = coef_geom * W53 * W12 * h**W23 * DSQRT( deltaz ) &
                   + coef_geom * h**W53 * ( W12 / DSQRT( deltaz ) )
!    if( CoteAval > Liaison%Cote ) then
    ddebit_dzaval  = coef_geom * W53 * W12 * h**W23 * DSQRT( deltaz ) &
                   + coef_geom * h**W53 * (-W12 / DSQRT( deltaz ) )
!    else
!        ddebit_dzaval  = 0
!    endif

    !
    ! CORRECTION COEFFICIENT BETA / partie a commentariser pour test sur coefficient correcteur
    ! ---------------------------
    if( h1 <= 0._DOUBLE ) then   ! reutilisation des valeurs h1 et h2 pour calculer les
        h1 = 0._DOUBLE            ! tirants d'eau max et min
    end if
    if( h2 <= 0._DOUBLE ) then
        h2 = 0._DOUBLE
    end if

    hmax      = dmax1( h1 , h2 )  ! tirant d'eau a l'amont physique de la liaison (par rapport a la cote moyenne)
    hmin      = dmin1( h1 , h2 )  ! tirant d'eau a l'aval de la liaison

    coef_beta = beta( hmax , hmin )
    ! hamont /= 0 ---> cf. test sur DZMIN

    if( coef_beta < 1._DOUBLE ) then
        C               = lis(coef_beta)
        dC_dzamont      = dlis(coef_beta) * dbetax(hmax,hmin)
        dC_dzaval       = dlis(coef_beta) * dbetay(hmax,hmin)
        ddebit_dzamont  = dC_dzamont * debit + C * ddebit_dzamont
        ddebit_dzaval   = dC_dzaval * debit  + C * ddebit_dzaval
        debit           = C * debit
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
       beta = ( -1._DOUBLE / ( 1._DOUBLE - COEF ) ) * ( Y / X - 1._DOUBLE )
     end function

     real(DOUBLE) function dbetax(X,Y)
       real(DOUBLE), intent(in) :: X,Y
       dbetax = ( -1._DOUBLE / ( 1._DOUBLE - COEF ) ) * ( -Y / X**2._DOUBLE )
     end function

     real(DOUBLE) function dbetay(X,Y)
       real(DOUBLE), intent(in) :: X,Y
       dbetay = ( -1._DOUBLE / ( 1._DOUBLE - COEF ) ) * ( 1._DOUBLE / X )
     end function

end subroutine LIAISON_CHENAL
