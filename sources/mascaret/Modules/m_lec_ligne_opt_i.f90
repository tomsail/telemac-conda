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

module M_LEC_LIGNE_OPT_I
!***********************************************************************
! PROGICIEL : MASCARET        S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine LEC_LIGNE_OPT( &
                         x_ini , & ! Tableau des abscisses initiales
                         z_ini , & ! Tableau des cotes initiales
                         q_ini , & ! Tableau des debits initiaux
                       cf1_ini , & ! Tableau des coefficients de frottement initiales
                       cf2_ini , & ! Tableau des coefficients de frottement initiaux
            PresenceCoeffFrott , & ! test de presence du strickler
                  FichierLigne , & ! fichier de la ligne d'eau initiale
                        Erreur &
                               )

   !***********************************************************************
   !  FONCTION :   LECTURE DU FICHIER D'UNE LIGNE D'EAU INITIALE
   !  --------     AU FORMAT OPTHYCA
   !
   !  SOUS PROGRAMMES APPELANT(S) : LEC_LIGNE
   !  ---------------------------
   !  SOUS PROGRAMMES APPELE(S) :   Neant
   !  -------------------------
   !***********************************************************************

   !============================= Declarations ===========================
   use M_PRECISION
   use M_MESSAGE_C        ! messages d'erreur
   use M_PARAMETRE_C
   use M_ERREUR_T         ! Definition du type ERREUR_T
   use M_FICHIER_T        !                    FICHIER_T
   use M_TRAITER_ERREUR_I ! traitement des erreurs
   use M_LIRE_CHAINE_S    ! lecture d'une chaine de caracteres
   use M_OPT2FORT_I       ! interface de sous-programme

   !.. Declarations implicites ..
   implicit none

   !.. Arguments ..
   real(DOUBLE), dimension(:), pointer       :: x_ini   ! abscisse lue sur le fichier
   real(DOUBLE), dimension(:), pointer       :: z_ini   ! cote lue sur le fichier
   real(DOUBLE), dimension(:), pointer       :: q_ini   ! debit lu sur le fichier
   real(DOUBLE), dimension(:), pointer       :: cf1_ini ! Coeff de frottement mineur lu sur le fichier
   real(DOUBLE), dimension(:), pointer       :: cf2_ini ! Coeff de frottement majeur lu sur le fichier
   logical                   , intent(  out) :: PresenceCoeffFrott ! presence dans la ligne d'eau initiale
   type(FICHIER_T)           , intent(in   ) :: FichierLigne
   type(ERREUR_T)            , intent(inout) :: Erreur

   end subroutine LEC_LIGNE_OPT

   end interface

end module M_LEC_LIGNE_OPT_I
