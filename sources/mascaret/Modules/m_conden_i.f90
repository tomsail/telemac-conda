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

module M_CONDEN_I
!***********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE         S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine  CONDEN                ( &
        ! Resultats
        ZCA,ZCB,ZCC,                & ! Coeff de la rel de condensation 
        ZTA,ZTB,ZTC,                & ! Coeff de la rel de condensation
        ! Donnees non modifiees
        I1,I2,                      & ! Section origine et fin du bief traite
        FA,FB,FC,FD,FE,FF,          & ! Coefficient de la relation de transfert I <-- I-1
        GA,GB,GC,GD,GE,GF,          & ! Coefficient de la relation de transfert I-1 <- I
        SectionSing,                & ! Numero de la section precedant la sing
        ASING,BSING,CSING,DSING,    & ! Coeff de l'equ discretisee de la sing
        QSING,                      & ! Debit passant sur la singularite
        Erreur                      &
                                  )
!**********************************************************************
!   FONCTION :
!   --------
! .                                                                   .
! . CALCUL DES COEFFICIENTS ZCA,ZCCB,ZCC,ZTA,ZTB,ZTC RELIANT LES DZ , .
! . DQ DANS LES 2 SECTIONS EXTREMES D'UN BIEF , RELATIONS OBTENUES    .
! . PAR LA METHODE DE CONDENSATION :                                  .
! .                                                                   .
! . DQ(ORIGINE)   = ZCA*DZ(ORIGINE) + ZCB + ZCC*DZ(EXTREMITE)         .
! . DQ(EXTREMITE) = ZTA*DZ(EXTREMITE) + ZTB + ZTC*DZ(ORIGINE)         .
! .....................................................................
! . LA METHODE EST DECRITE DANS LE RAPPORT HE-43/92.64                .
! . ELLE PREND EN COMPTE LES SINGULARITES                             .
! .....................................................................
! . SOUS-PROGRAMME APPELANT  :  REZO                                .
! .....................................................................
! .....................................................................
! . SOUS-PROGRAMME APPELE : SPECTR : CALCUL DU RAYON SPECTRAL
! .                                  D'UNE MATRICE 2X2
! .....................................................................
!
   !=============================Declarations================================
   use M_PRECISION        ! Type DOUBLE
   use M_PARAMETRE_C      ! Parametres de calcul
   use M_MESSAGE_C        ! Liste des messages d'erreur
   use M_ERREUR_T         ! Definition du type ERREUR_T
   use M_TRAITER_ERREUR_I ! Traitement des erreurs

   implicit none

   !.. Arguments .. 
   real(DOUBLE)              , intent(  out) :: ZCA, ZCB, ZCC
   real(DOUBLE)              , intent(  out) :: ZTA, ZTB, ZTC
   real(DOUBLE), dimension(:), intent(in   ) :: FA, FB, FC, FD, FE, FF
   real(DOUBLE), dimension(:), intent(in   ) :: GA, GB, GC, GD, GE, GF
   real(DOUBLE)              , intent(in   ) :: ASING, BSING, CSING, DSING
   real(DOUBLE)              , intent(in   ) :: QSING
   integer                   , intent(in   ) :: I1, I2
   integer                   , intent(in   ) :: SectionSing
   type(ERREUR_T)            , intent(inout) :: Erreur

   end subroutine CONDEN

   end interface

end module M_CONDEN_I
