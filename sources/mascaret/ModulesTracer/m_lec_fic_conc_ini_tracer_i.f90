!== Copyright (C) 2000-2022 EDF-CEREMA ==
!
!   This file is part of MASCARET-TRACER.
!
!   MASCARET-TRACER is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   MASCARET-TRACER is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with MASCARET-TRACER.  If not, see <http://www.gnu.org/licenses/>
!

module M_LEC_FIC_CONC_INI_TRACER_I
!***********************************************************************
! PROGICIEL : TRACER         S.MANDELKERN - M.LUCK
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   interface

   subroutine LEC_FIC_CONC_INI_TRACER( &
                               x_ini , & ! Tableau des abscisses initiales
                               c_ini , & ! Tableau des concentrations initiales
                             nb_trac , & ! nombre de traceurs
                     FichierConcInit , & ! fichier des concentrations initiales
                              Erreur   &
                                     )

   !***********************************************************************
   !
   !  FONCTION :   LECTURE DU FICHIER D'UNE LIGNE D'EAU INITIALE
   !  --------     AU FORMAT OPTHYCA
   !
   !  SOUS PROGRAMMES APPELANT(S) : LEC_CONC_INI_TRACER
   !  ---------------------------
   !  SOUS PROGRAMMES APPELE(S) :   - LIRE_CHAINE_S
   !  -------------------------     - OPT2FORT
   !
   !***********************************************************************

   !============================= Declarations ===========================
   use M_PRECISION
   use M_PARAMETRE_C
   use M_MESSAGE_C        ! messages d'erreur
   use M_MESSAGE_TRACER_C
   use M_ERREUR_T         ! Definition du type ERREUR_T
   use M_FICHIER_T        !                    FICHIER_T
   use M_TRAITER_ERREUR_I ! traitement des erreurs
   use M_LIRE_CHAINE_S    ! lecture d'une chaine de caracteres
   use M_OPT2FORT_I       ! interface de sous-programme

   !.. Declarations implicites ..
   implicit none

   !.. Arguments ..
   real(DOUBLE), dimension(:), pointer       :: x_ini   ! abscisse lue sur le fichier
   real(DOUBLE), dimension(:,:), pointer     :: c_ini   ! cote lue sur le fichier
   integer                   , intent(in   ) :: nb_trac  ! nombre de traceurs
   type(FICHIER_T)           , intent(in   ) :: FichierConcInit
   type(ERREUR_T)            , intent(inout) :: Erreur

   end subroutine LEC_FIC_CONC_INI_TRACER

   end interface

end module M_LEC_FIC_CONC_INI_TRACER_I
