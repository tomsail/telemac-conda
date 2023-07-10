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

module M_LOILIM_I
!***********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE
!                             P. CHERUBINI
!                             S. PERON
!                             S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine  LOILIM( &
                   R,S,T , & ! Coefficients de la loi R*delta(Z)+S*delta(Q)=T
                     CLZ , & ! Ordonnee de la loi Z(Q)
                     CLQ , & ! Abscisse de la loi Z(Q)
                     NBP , & ! Nombre de points de la loi Z(Q)
                       Z , & ! Cote de la surface libre
                       Q , & ! Debit
                  NumLoi , & ! Numero de la loi (pour messages d'erreur)
                  Erreur & ! Erreur
                        )

   !***********************************************************************
   !  FONCTION :
   !  --------
   !
   !           CALCUL DES COEFFIENTS R,S,T DE LA CONDITION LIMITE Z(Q)
   !
   !               R*DELTA(Q) + S*DELTA(Z) = T
   !
   !-----------------------------------------------------------------------
   !                             ARGUMENTS
   ! ._________.___._____._______________________________________________
   ! !   NOM   !TYPE!MODE!                   ROLE
   ! !_________!____!____!__________________________________________________
   ! !   R     ! R  !<-- !   ) COEFFICIENTS DE LA RELATION D'IMPEDANCE
   ! !   S     ! R  !<-- !   ) A LA LIMITE A TRAITER :
   ! !   T     ! R  !<-- !   ) R*DELTA(Q) + S*DELTA(Z) = T
   ! !_________!____!____!______________________________________________
   !
   !                          VARIABLES LOCALES
   ! .____________________________________________________________________
   ! !   DQ    ! R  ! -- ! ECART DE DEBIT DEPUIS LE PAS PRECEDENT .
   ! !   dqdz  ! R  ! -- ! VALEUR INTERMEDIAIRE D'ECART DE DEBIT .
   ! !   DZ    ! R  ! -- ! ECART DE COTE DEPUIS LE PAS PRECEDENT .
   ! !   qm1   ! R  ! -- ! VALEUR DE DEBIT POUR UNE COTE MINOREE,LOI Z(Q)
   ! !   qp1   ! R  ! -- ! VALEUR DE DEBIT POUR UNE COTE MAJOREE,LOI Z(Q)
   ! !_________!____!____!______________________________________________
   !  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
   !               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
   !-----------------------------------------------------------------------
   !
   !   FICHIERS ENTREE/SORTIE :
   !   ----------------------
   !   SOUS PROGRAMMES APPELANTS :  - CCL
   !   ---------------------------
   !   SOUS PROGRAMMES APPELES :   - INTERPOLATION_S : INTERPOLATION 
   !   -------------------------     DE LAGRANGE D'ORDRE N
   !
   !   COMMENTAIRES :
   !   ------------
   !
   !   DOCUMENTATION EXTERNE :
   !   ---------------------
   !

   !============================ Declarations ==============================
   use M_PRECISION       ! Type DOUBLE
   use M_PARAMETRE_C     ! Parametres de calcul
   use M_MESSAGE_C       ! Liste des messages d'erreur
   use M_INTERPOLATION_S
   use M_ERREUR_T        ! Definition du type ERREUR_T
   use M_TRAITER_ERREUR_I ! Traitement des erreurs

   implicit none

   !.. Arguments .. 
   real(DOUBLE)              , intent(  out) :: R , S , T
   real(DOUBLE), dimension(:), intent(in   ) :: CLZ , CLQ
   integer                   , intent(in   ) :: NBP
   real(DOUBLE)              , intent(in   ) :: Z , Q
   integer                   , intent(in   ) :: NumLoi
   type(ERREUR_T)            , intent(inout) :: Erreur

   end subroutine LOILIM

   end interface

end module M_LOILIM_I
