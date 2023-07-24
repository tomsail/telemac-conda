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

module M_CALINT_I
!***********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE       S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine  CALINT          ( &
            ! Resultats
            DeltaZ          , & ! Variation de la cote de la surface libre
            DeltaQ          , & ! Variation du debit
            ! Donnees
            NumeroPas       , & ! Numero du pas de temps traite
            NumBief         , & ! Numero du bief
            I1, I2          , & ! Sections origine et fin du bief
            FA,FB,FC        , & ! Coefficients de la relation
            FD,FE,FF        , & ! de transfert I <-- I-1
            GA,GB,GC        , & ! Coefficients de la relation
            GD,GE,GF        , & ! de transfert I-1 <-- I
            SectionSing     , & ! Numero section precedant la singularite
            ASING, BSING    , & ! Coeff de l'equation discretisee
            CSING, DSING    , & ! de la singularite
            QSING           , & ! Debit passant sur la singularite
            DELZI1, DELQI1  , & ! Variation de cote et debit dans
            DELZI2, DELQI2  , & ! les sections origine et fin du bief
            R               , & ! Coefficients de
            S               , & ! la relation d'impedance
            T               , & ! R*DQI + S*DZI = T
            Impression      , & ! Flag d'impression
            UniteListing    , & ! Unite logique fichier listing
            Erreur            & ! Erreur
                            )
!
! .....................................................................
!   FONCTION :
!   --------
! . CALCUL DES DZ , DQ  DANS TOUTES LES  SECTIONS D'UN BIEF           .
! . CONNAISSANT LES DZ , DQ DANS LES SECTIONS EXTREMES                .
! .                                                                   .
! . TRAITEMENT DES SINGULARITES                                       .
! .....................................................................
!
! . C'EST LE CALCUL CLASSIQUE REALISE , POUR LE CODE LIDO , DANS LE   .
! . SOUS-PROGRAMME FLINEA : METHODE DU DOUBLE BALAYAGE UTILISANT LES  .
! . RELATIONS DE TRANSFERT , CALCULEES DANS LE SOUS-PROGRAMME COEFFS ,.
! . ET LES RELATONS D'IMPEDANCE ( R(I)*DELQ(I)+S(I)*DELZ(I)=T(I) )    .
! .....................................................................
! .....................................................................
! . FICHIERS EN SORTIE : UniteListing . IMPRESSION LISTING
! .....................................................................
! . SOUS-PROGRAMME APPELANT  :  CALCUL                                .
! .....................................................................
! .....................................................................
! . PRINCIPALES VARIABLES AUXILIAIRES                                 .
! .....................................................................
! . NOM    . SIGNIFICATION                                            .
! .....................................................................
! . DELZ1  . VALEUR CALCULEE DE DELZI1                                .
! . DELQ1  . VALEUR CALCULEE DE DELQI1                                .
! . DELZ2  . VALEUR CALCULEE DE DELZI2                                .
! . DELQ2  . VALEUR CALCULEE DE DELQI2                                .
! .....................................................................

   !============================= Declarations ===========================
   use M_PRECISION       ! Type DOUBLE
   use M_PARAMETRE_C     ! Parametres de calcul
   use M_MESSAGE_C       ! Liste des messages d'erreur
   use M_ERREUR_T        ! Definition du type ERREUR_T
   use M_TRAITER_ERREUR_I ! Traitement des erreurs

   implicit none

   !.. Arguments .. 
   real(DOUBLE), dimension(:), intent(  out) :: DeltaZ, DeltaQ
   real(DOUBLE), dimension(:), intent(in   ) :: FA, FB, FC
   real(DOUBLE), dimension(:), intent(in   ) :: FD, FE, FF
   real(DOUBLE), dimension(:), intent(in   ) :: GA, GB, GC
   real(DOUBLE), dimension(:), intent(in   ) :: GD, GE, GF
   real(DOUBLE), dimension(:), intent(inout) :: R, S, T
   real(DOUBLE)              , intent(in   ) :: ASING, BSING
   real(DOUBLE)              , intent(in   ) :: CSING, DSING
   real(DOUBLE)              , intent(in   ) :: QSING
   real(DOUBLE)              , intent(in   ) :: DELZI1, DELQI1
   real(DOUBLE)              , intent(in   ) :: DELZI2, DELQI2
   integer                   , intent(in   ) :: NumBief
   integer                   , intent(in   ) :: NumeroPas
   integer                   , intent(in   ) :: I1, I2
   integer                   , intent(in   ) :: SectionSing
   logical                   , intent(in   ) :: Impression
   integer                   , intent(in   ) :: UniteListing
   type(ERREUR_T)            , intent(inout) :: Erreur

   end subroutine CALINT

   end interface

end module M_CALINT_I
