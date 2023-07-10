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

subroutine  KLIAISON                    ( &
            ALiai,BLiai,CLiai,DLiai     , & ! Coefficients du seuil
            QLiai                       , & ! Debit au seuil
            Liaison                     , & ! Singularites
            Appel                       , & ! Flag
            ZAM                         , & ! Cote amont
            ZAV                         , & ! Cote aval
            ZfAM                        , & ! Cote Fond amont
            ZfAV                        , & ! Cote Fond aval
            Connect                     , & ! Table de connectivite du reseau
            Impression                  , & ! Flag d'impression
            UniteListing                , & ! Unite logique du fichier listing
            Erreur                        & ! Erreur
                                        )

! *********************************************************************
! PROGICIEL : MASCARET      S. DELMAS    C. COULET
!
! VERSION : V8P4R0         EDF-CEREMA-ARTELIA
! *********************************************************************
!   FONCTION :
!   --------
!
!   . CALCUL DES COEFFICIENTS A,B,C,D, DE L'EQUATION DISCRETISEE D'UNE
!     LIAISON :
!
!     A*DQ_liaison + B*DZAMONT + C*DZAVAL = D
!
!     Appel = 1 : LES COEFFICIENTS DE L'EQUATION
!                 DE LA LIAISON ET LE DEBIT SONT CALCULES
!     Appel = 2 : SEUL LE DEBIT THEORIQUE EST CALCULE
!_____________________________________________________________________________
!
!   FICHIERS ENTREE/SORTIE :    - UniteListing : IMPRESSION DES RESULTATS GLOBAUX
!
!   SOUS-PROGRAMMES APPELANT :  - REZODT
!   ------------------------
!
!   SOUS-PROGRAMMES APPELES :
!   -----------------------
!
! . INTERPOLATION_S : SOUS-PROGRAMME D'INTERPOLATION
!
!   COMMENTAIRES :
!   ------------
!
! . LES CALCULS DEPENDENT DU TYPE DE LA LIAISON
! . SI LA LIAISON EST EST DEFINIE AU MOYEN D'UNE LOI
!   Q = F ( ZAMONT , ZAVAL) ALORS :
!   A=-1.   B=DF/DZAMONT   C=DF/DZAVAL D=0.
!
!------------------------------------------------------------------------

   !============================ Declarations ==============================
   use M_PRECISION        ! Type DOUBLE
   use M_PARAMETRE_C      ! Parametres de calcul
   use M_MESSAGE_C        ! Liste des messages d'erreur
   use M_CONNECT_T        ! Definition du type CONNECT_T
   use M_LIAISON_T        ! Definition du type SINGULARITE_T
   use M_ERREUR_T         ! Definition du type ERREUR_T
   use M_TRAITER_ERREUR_I ! Traitement des erreurs
   use M_TRAITER_ERREUR_CASIER_I ! traitement des erreurs
   use M_MESSAGE_CASIER_C        ! messages d erreur propres a CASIER
   use M_NUM_BIEF_S       ! Calcul du num d'un bief d'apres num section calcul

   use M_INTERPOLATION_S  ! Interpolation
   use M_RHSBP_S          ! Sous programme

   use M_LIAISON_SEUIL_I    ! Interface de sous-programme
   use M_LIAISON_CHENAL_I   ! Interface de sous-programme
   use M_LIAISON_SIPHON_I   ! Interface de sous-programme
   use M_LIAISON_ORIFICE_I  ! Interface de sous-programme


   implicit none

   !.. Arguments ..
   real(DOUBLE)                     , intent(  out) :: Aliai, Bliai
   real(DOUBLE)                     , intent(  out) :: Cliai, Dliai
   real(DOUBLE)                     , intent(  out) :: Qliai
   TYPE(LIAISON_T)                  , intent(inout) :: Liaison
   integer                          , intent(in   ) :: Appel
   real(DOUBLE)                     , intent(in   ) :: ZAM, ZAV, ZfAM, ZfAV
   logical                          , intent(in   ) :: Impression
   integer                          , intent(in   ) :: UniteListing
   type(CONNECT_T)                  , intent(in   ) :: Connect
   type(ERREUR_T)                   , intent(inout) :: Erreur

   !.. Scalaires locaux ..
   integer :: type   ! Type de la singularite
   real(DOUBLE)   :: DQDZAM,DQDZAV
   real(DOUBLE)   :: Zplafond

   !character(132) :: arbredappel_old
   !integer        :: num_section     ! numero d'une section
   !integer        :: num_bief        ! numero du bief correspondant
   !real(DOUBLE)   :: abs_rel         ! abscisse relative correspondante

   !.. Intrinsic Functions ..
   intrinsic DABS, DMAX1, DMIN1

   !============================ Instructions ==============================

   ! INITIALISATIONS
   ! ---------------
   Erreur%Numero = 0
   !arbredappel_old    = trim(Erreur%arbredappel)
   !Erreur%arbredappel = trim(Erreur%arbredappel)//'=>KLIAISON'

    DQDZAM = 0._DOUBLE
    DQDZAV = 0._DOUBLE
    Qliai  = 0._DOUBLE

   !calcul de cote moyenne de la surface libre au dessus de la liaison
   if( dmin1( ZAM , ZAV ) <= Liaison%Cote ) then
      liaison%CoteMoyenne = 0.5_DOUBLE * ( dmax1( ZAM , ZAV ) + Liaison%Cote ) !haval est nulle
   else
      liaison%CoteMoyenne = 0.5_DOUBLE * ( ZAM + ZAV )
   end if

   ! -----------------------------------------------------------------------
   ! CALCUL DES PARAMETRES CARACTERISTIQUES DES SINGULARITES
   ! -----------------------------------------------------------------------
    type   = liaison%Typeliaison
    select case( type )

        case ( LIAISON_TYPE_SEUIL )     !************************************

            if( ZAM > Liaison%Cote .or. ZAV > Liaison%Cote ) then

               call LIAISON_SEUIL                 ( &
                     DQDZAM,DQDZAV,Qliai          , &
                     ZAM,ZAV                      , &
                     Liaison                      , &
                     Erreur                         )

               if( Erreur%Numero /= 0 ) then
                  return
               end if
            end if

            if( abs(Liaison%CoteMoyenne - Liaison%Cote) .LT.EPS6 ) then
                Liaison%VitesseEchange = Liaison%DebitEchange / ( Liaison%Largeur * ( Liaison%CoteMoyenne - Liaison%Cote ) )
            end if

        case( LIAISON_TYPE_CHENAL )     !************************************

            if( ZAM > Liaison%Cote .or. ZAV > Liaison%Cote ) then

                call LIAISON_CHENAL             ( &
                    DQDZAM,DQDZAV,Qliai         , &
                    ZAM, ZAV, ZfAM, ZfAV        , &
                    Liaison                     , &
                    Erreur                       )

                if( Erreur%Numero /= 0 ) then
                    return
                end if
            endif

            ! Largeur /=0, test dans PRETRAIT_CASIER
            if( abs(Liaison%DebitEchange).GT.EPS6 ) then
                Liaison%VitesseEchange = Liaison%DebitEchange / ( Liaison%Largeur * ( liaison%CoteMoyenne - Liaison%Cote ) )
            end if

        case( LIAISON_TYPE_SIPHON )     !************************************

            if( ZAM > Liaison%Cote .or. ZAV > Liaison%Cote ) then

               call LIAISON_SIPHON              ( &
                     DQDZAM,DQDZAV,Qliai        , &
                     ZAM, ZAV, ZfAM, ZfAV       , &
                     Liaison                    , &
                     Erreur                     )

               if( Erreur%Numero /= 0 ) then
                  return
               end if
            endif
            ! Section /=0, test dans PRETRAIT_CASIER
            Liaison%VitesseEchange = Liaison%DebitEchange / Liaison%Section

        case( LIAISON_TYPE_ORIFICE )    !************************************

            if( ZAM > Liaison%Cote .or. ZAV > Liaison%Cote ) then

               call LIAISON_ORIFICE             ( &
                     DQDZAM,DQDZAV,Qliai        , &
                     ZAM, ZAV, ZfAM, ZfAV       , &
                     Liaison                    , &
                     Erreur                     )

               if( Erreur%Numero /= 0 ) then
                  return
               end if
            endif

            Zplafond = Liaison%cote + Liaison%section/liaison%largeur    !orifice rectangulaire
            if( ( ZAM >= Zplafond ) .or. ( ZAV >= Zplafond ) ) then   !orifice submerge
                ! Section /=0, test dans PRETRAIT_CASIER
                Liaison%VitesseEchange = Liaison%DebitEchange / Liaison%Section
            elseif( ( liaison%CoteMoyenne >= Liaison%Cote ) .and. ( abs(Liaison%DebitEchange).GT.EPS6 ) ) then
                Liaison%VitesseEchange = Liaison%DebitEchange / ( Liaison%Largeur * ( liaison%CoteMoyenne - Liaison%Cote ) )
            else
                Liaison%VitesseEchange = 0
            endif

!                if( ( liaison%CoteMoyenne == Liaison%Cote ) .and. ( Liaison%DebitEchange /= 0._DOUBLE ) ) then
!                    Erreur%Numero = 802
!                    Erreur%ft     = err_802
!                    Erreur%ft_c   = err_802c
!                    call TRAITER_ERREUR_CASIER( Erreur , NumeroLiaison )
!                    return
!                end if
!                ! Largeur /=0, test dans PRETRAIT_CASIER
!                if( Liaison%DebitEchange /= 0._DOUBLE ) then
!                    Liaison%VitesseEchange = Liaison%DebitEchange / ( Liaison%Largeur * ( liaison%CoteMoyenne - Liaison%Cote ) )
!                end if
!            end if

        case default                    !************************************

            Erreur%Numero = 602
            Erreur%ft     = err_602
            Erreur%ft_c   = err_602c
            call TRAITER_ERREUR( Erreur , type )
            return

    end select

   ! ----------------------------------------
   ! CALCUL DE ASING,BSING,CSING,DSING,QSING
   ! ----------------------------------------

    Aliai = -1._DOUBLE
    Bliai = DQDZAM
    Cliai = DQDZAV
    Dliai = 0._DOUBLE


!    if( Impression ) then
!            num_section = Singularite(NumSeuil)%Section
!            num_bief    = NUM_BIEF_S( Connect , num_section , Erreur )
!            abs_rel     = X(num_section) - X(Connect%OrigineBief(num_bief))
!            write (UniteListing,10001) NumSeuil, num_bief, abs_rel,           &
!                                       QAM, QAV, ZAM, ZAV,                    &
!                                       ASING, BSING, CSING, DSING, QSING
!    end if


   return
!  10001 format ( /,                                             &
!           ' LIAISON NUMERO = ',i4,/,                         &
!           ' Bief n0 ',i4,' Abscisse relative X = ',f12.3,/,      &
!           ' QAM=',f7.2,' QAV=',f7.2,' ZAM=',f7.2,' ZAV=',f7.2,/, &
!           ' A=',e12.4,' B=',e12.4,' C=',e12.4,' D=',e12.4,/,     &
!           ' QLIAI=',f7.2)


end subroutine KLIAISON
