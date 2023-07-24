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

module M_QNODE_I
!***********************************************************************
! PROGICIEL : MASCARET      A. LEBOSSE
!                           S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine QNODE (   &
        Q             , & !/DONNEES MODIFIEES/
        Z             , &
        NumConfluence , & !/DONNEES NON MODIFIEES/
        NumPassage    , &
        Connect       , &
        Erreur          & !/ERREUR/
                     )
   !
   !***********************************************************************
   !
   !  FONCTION :
   !  --------
   !
   !        REPARTITION DES DEBITS A UN NOEUD AVEC EGALITE DES COTES :
   !
   !          CAS AVEC UNE SEULE BRANCHE AVAL
   !                   ---------
   !
   !-----------------------------------------------------------------------
   !                              ARGUMENTS
   ! ._______________.____.____._______________________________________________
   ! !    NOM        !TYPE!MODE!                   ROLE
   ! !_______________!____!____!______________________________________________
   ! ! Q             ! R  !<-->! TABLEAU DES DEBITS
   ! ! Z             ! R  !<-->! TABLEAU DES COTES DE LA SURFACE LIBRE
   ! ! NumConfluence ! I  ! -->! Numero de la confluence a traiter
   ! ! NumPassage    ! I  ! -->! VARIABLE INDICATRICE :
   ! !               !    !    !  = 1  CALCUL DU DEBIT DANS LA BRANCHE AVAL
   ! !               !    !    !  = 2  CALCUL DES COTES DANS LES BRANCHES AMONT
   ! ! Connect       ! T  ! -->! Structure contenant la table de connectivite
   ! ! Erreur        ! T  !<-->! ERREUR
   ! !_______________!____!____!______________________________________________
   !
   !                         VARIABLES LOCALES
   ! ._______________.____.____.______________________________________________
   ! !               !    !    !
   ! !_______________!____!____!______________________________________________
   !  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
   !               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
   !-----------------------------------------------------------------------
   !
   !   FICHIERS ENTREE/SORTIE :
   !   ----------------------
   !   SOUS PROGRAMME APPELANT :  PERSAR
   !   -------------------------
   !   SOUS PROGRAMMES APPELES :  ---
   !   -------------------------
   !
   !   COMMENTAIRE:
   !   -----------
   !
   !   DOCUMENTATION EXTERNE :
   !   ---------------------
   !
   !***********************************************************************

   !============================= Declarations ===========================
   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_MESSAGE_C        ! Messages d'erreur
   use M_CONNECT_T        ! Type CONNECT_T
   use M_ERREUR_T         ! Type ERREUR_T
   use M_TRAITER_ERREUR_I ! Traitement de l'erreur

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   ! TABLEAUX DIMENSIONNES A NbSection
   real(DOUBLE)       , dimension(:)  , intent(inout) :: Q
   real(DOUBLE)       , dimension(:)  , intent(inout) :: Z
   integer            ,                 intent(in   ) :: NumConfluence
   integer            ,                 intent(in   ) :: NumPassage
   type(CONNECT_T)    ,                 intent(in   ) :: Connect
   type(ERREUR_T)     ,                 intent(inout) :: Erreur

   end subroutine QNODE

   end interface

end module M_QNODE_I
