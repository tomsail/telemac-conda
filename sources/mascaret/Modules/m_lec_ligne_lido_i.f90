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

module M_LEC_LIGNE_LIDO_I
!***********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE
!                             P. CHERUBINI
!                             S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
interface

   subroutine LEC_LIGNE_LIDO( &
           x_ini              , & ! Tableau des abscisses initiales
           z_ini              , & ! Tableau des cotes     initiales
           q_ini              , & ! Tableau des debits    initiaux
           cf1_ini            , & ! Tableau des coefficients de frottement initiales
           cf2_ini            , & ! Tableau des coefficients de frottement initiaux
           PresenceCoeffFrott , & ! test de presence de coefficients de frottement
           FichierLigne       , & ! fichier de la ligne d'eau initiale
           Erreur               &
                                       )

   !***********************************************************************
   !  FONCTION :
   !  --------
   !
   !         LECTURE DE LA LIGNE D'EAU INITIALE ,
   !         DES COEFFICIENTS DE STRICKLER SI presence_strickler est vrai
   !
   !----------------------------------------------------------------------
   !
   !   FICHIERS ENTREE/SORTIE :
   !   ----------------------
   !   SOUS PROGRAMME APPELANT :  LEC_LIGNE
   !   -------------------------
   !   SOUS PROGRAMMES APPELES :
   !   -------------------------
   !
   !                 - INTERP * INTERPOLATION DE LAGRANGE D'ORDRE N .
   !
   !   COMMENTAIRES :
   !   ------------
   !
   !
   !   **  TypeMaillage = 4 **   LES SECTIONS ONT ETE DEFINIES DANS LE CALCUL
   !                       PERMANENT ET SONT LUES DANS LE FICHIER
   !                       CONTENANT LA LIGNE D'EAU INITIALE .
   !
   !   DOCUMENTATION EXTERNE :
   !   ---------------------
   !
   !***********************************************************************

   !============================= declarations ============================
   use M_PRECISION
   use M_PARAMETRE_C      ! EPS5
   use M_MESSAGE_C        ! messages d'erreur
   use M_ERREUR_T         ! Definition du type ERREUR_T
   use M_FICHIER_T        !                    FICHIER_T
   use M_TRAITER_ERREUR_I

   ! Arguments
   real(DOUBLE), dimension(:), pointer       :: x_ini   ! abscisse lue sur le fichier
   real(DOUBLE), dimension(:), pointer       :: z_ini   ! cote lue sur le fichier
   real(DOUBLE), dimension(:), pointer       :: q_ini   ! debit lu sur le fichier
   real(DOUBLE), dimension(:), pointer       :: cf1_ini ! coeff de frottement mineur lu sur le fichier
   real(DOUBLE), dimension(:), pointer       :: cf2_ini ! coeff de frottement majeur lu sur le fichier
   logical                   , intent(  out) :: PresenceCoeffFrott ! presence dans la ligne d'eau initiale
   type(FICHIER_T)           , intent(in   ) :: FichierLigne
   type(ERREUR_T)            , intent(inout) :: Erreur

   end subroutine LEC_LIGNE_LIDO

   end interface

end module M_LEC_LIGNE_LIDO_I
