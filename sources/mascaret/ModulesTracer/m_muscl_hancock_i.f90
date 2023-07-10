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

module M_MUSCL_HANCOCK_I
!***********************************************************************
! PROGICIEL : TRACER         S.MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   interface

   subroutine MUSCL_HANCOCK( &
                             ! Donnees Initiales et Resultat
                        GT , & ! Grandeur (AC) apres convection
                         G , & ! Grandeur (AC) donnee
                            ! Donnees hydrauliques
                         U , & ! Vitesse
                            ! Donnees TRACER
                  ConsTrac , & ! Constantes liees au transport-diffusion
                      CLAM , & ! Condition Limite Amont
                      CLAV , & ! Condition Limire Aval
                             ! Modele
                         X , & ! Abscisse des concentrations
                        DT , & ! Pas de temps
                    Nbsect , & ! Nombre de sections de calcul (donc de mailles VF)
                    FLUENT , &
                    FLUSOR , &
                    Erreur )

   !***********************************************************************
   !
   !  FONCTION :
   !  --------
   !
   !  RESOLUTION DE L'EQUATION DE CONVECTION EN FORMULATION CONSERVATIVE
   !     PAR UNE METHODE VOLUMES FINIS D'ORDRE 2 OU 3 (MUSCL-HANCOCK METHOD)
   !     AVEC OU SANS LIMITEUR DE PENTE (DE TYPE SUPERBEE)
   !
   !***********************************************************************
   !   SOUS-PROGRAMMES APPELANT : RESEQU
   !   ------------------------
   !***********************************************************************

   use M_PRECISION
   use M_PARAMETRE_C
   use M_CONSTANTES_TRACER_T
   use M_ERREUR_T

   implicit none

   ! Variables Globales
   ! ------------------
   !.. Donnees Initiales et Resultats
   real (DOUBLE), dimension(:)             , intent(inout) :: G
   real (DOUBLE), dimension(:)             , intent(  out) :: GT
   !.. Donnees hydrauliques
   real (DOUBLE), dimension(:)             , intent(inout) :: U
   !.. Donnees TRACER
   type (CONSTANTES_TRACER_T)              , intent(inout) :: ConsTrac
   real (DOUBLE)                           , intent(inout) :: CLAM
   real (DOUBLE)                           , intent(inout) :: CLAV
   !.. Modele
   real (DOUBLE)                           , intent(in   ) :: DT
   real (DOUBLE), dimension(:)             , intent(in   ) :: X
   integer                                 , intent(in   ) :: Nbsect
   real (DOUBLE)                           , intent(inout) :: FLUENT,FLUSOR
   type (ERREUR_T)                         , intent(inout) :: Erreur

   end subroutine MUSCL_HANCOCK

   end interface

end module M_MUSCL_HANCOCK_I
