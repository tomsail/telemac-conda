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

module M_DIRECT_I
!***********************************************************************
! PROGICIEL : MASCARET       A. LEBOSSE
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   SUBROUTINE DIRECT( &
              FCOUTI , &
               Z_CAL , &   ! Variables propres au calage
                   Z , &   ! Cote de la surface libre
                   Q , &   ! Debit mineur +majeur
              Qinjec , &   ! Qinjecte
              PCSing , &   ! Pertes de charge singulieres
               ZINIT , &
                  ic   &   ! Numero de la crue
               )

! *********************************************************************
!
! VARIABLES LOCALES
!
! .________________.____.______________________________________________
! ! FCOUT     ! R  ! -- ! VALEUR DE LA FONCTION DE COUT POUR LE
! !           !    !    ! STRICKLER INITIAL
! !___________!____!____!______________________________________________
!
! *********************************************************************
!
! FONCTION :
! --------
!
! CALCUL DU GRADIENT DE LA FONCTION DE COUT DANS LE CAS PERMANENT
!
!
! FICHIERS ENTREE/SORTIE :
! ----------------------
!
! IOTER  : IMPRESSION DE SORTIES LIDO (SANS INTERET)
!
! SOUS-PROGRAMME APPELANT :  CALAGE
! -----------------------
!
! SOUS-PROGRAMMES APPELES :
! -----------------------
!
! - COUT   : CALCUL DE LA FONCTION DE COUT
!
! - PERMAT : CALCUL D'UNE LIGNE D'EAU EN REGIME PERMANENT
!
! - STRICK : IMPLANTATION DES COEFFICIENTS DE RUGOSITE SUR LES
!            SECTIONS DE CALCUL
!
! COMMENTAIRES :  LE GRADIENT EST CALCULE PAR DIFFERENCES FINIES , I.E.
! ------------    EN FAISANT VARIER 1 A 1 CHAQUE COEFFICIENT DE
!                 RUGOSITE , ET EN CALCULANT A CHAQUE FOIS LA VALEUR DE
!                 LA FONCTION DE COUT : IL Y A DONC AUTANT D'APPELS DE
!                 PERMAT QUE DE COEFFICIENTS A ESTIMER
!                LES VALEURS INITIALES DU DEBIT (LUES DANS LA LIGNE
!                 D'EAU INITIALE) SONT INDISPENSABLES CAR ELLES DONNENT
!                 LES DEBITS D'APPORT . CES VALEURS NE SONT JAMAIS
!                 MODIFIEES QUELLE QUE SOIT LA RUGOSITE
!
!**********************************************************************

   !  VARIABLES LIEES A L'HYDRAULIQUE
   !
   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_SHARE_VAR
   ! Constantes nommees
   use M_PARAMETRE_C       ! GPES
   use M_MESSAGE_C         ! Messages d'erreur
   ! Procedures-module
   use M_RHSBP_S           ! Sous-programme RHSBP_S
   use M_NUM_BIEF_S        ! Numero de bief d'une section
   use M_FROUDE_S          ! Calcul du nombre de Froude
   use M_TRAITER_ERREUR_I  ! Interface generique de traitement des erreurs
   ! Interfaces
   use M_COUT_I
   use M_STRICK_I
   use M_PERMAT_I
   use M_CQINJ_I

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !
   !.. Arguments ..
   !---------------
   integer , intent(in) :: ic
   ! TABLEAU  DIMENSIONNE  A NbSect
   !
   real(DOUBLE)       , dimension(:)  , intent(out)   :: Z
   !
   ! TABLEAU  DIMENSIONNE  A NbSect
   real(DOUBLE)       , dimension(:)  , intent(inout) :: Q
   real(DOUBLE)       ,                 intent(inout) :: ZINIT
   ! TABLEAUX DIMENSIONNES A Nbsect
   real(DOUBLE)       , dimension(:)  , intent(in)    :: PCSing
   real(DOUBLE)       , dimension(:)  , intent(inout) :: Qinjec
   !
   ! VARIABLES CASTOR
   !
   real(DOUBLE)  , dimension(:) ,      intent(inout)  :: Z_cal
   real(DOUBLE)                 ,      intent(inout)  :: FCOUTI


   end subroutine DIRECT

   end interface

end module M_DIRECT_I
