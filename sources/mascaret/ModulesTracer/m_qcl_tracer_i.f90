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

module M_QCL_TRACER_I
!***********************************************************************
! PROGICIEL : TRACER         S.MANDELKERN - M.LUCK
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   interface

   subroutine QCL_TRACER( &
                          ! Resultats : Objets dont l'etat est mis a jour
               Cond_Lim , & ! Conditions limites amont 
          Source_Tracer , & ! Tableau des sources
                          ! Donnees
              Extremite , & ! Tableau des extremites libres
              LoiTracer , & ! Tableau des lois tracer
                 Nbtrac , & ! Nombre de traceurs
                  Temps , & ! Temps
                          ! Modele
                 Erreur   & ! Erreur
                        )

   !***********************************************************************
   !
   !  FONCTION :
   !  --------
   !  La fonctionnalite Qcl_Tracer fournit les valeurs au temps t
   !  de chaque concentration aux limites (C)
   !
   !
   !   SOUS-PROGRAMME(S) APPELANT(S) : SUPERVISEUR
   !   -----------------------------
   !   SOUS-PROGRAMME(S) APPELE(S)   : INTERPOLATION_S
   !   ---------------------------
   !
   !***********************************************************************

   use M_PRECISION
   use M_CONSTANTES_CALCUL_C  ! Constantes pour les phases et modeles de calcul
   use M_SOURCE_TRACER_T      ! Definition du type SOURCE_TRACEUR_T
   use M_COND_LIM_TRACER_T    ! Definition du type COND_LIM_TRACEUR_T
   use M_LOI_TRACER_T         ! Definition du type LOI_TRACER_T
   use M_EXTREMITE_T          ! Definition du type EXTREMITE_T
   use M_INTERPOLATION_S      ! Sous-programme INTERPOLATION_S
   use M_ERREUR_T             ! Definition du type ERREUR_T
   use M_MESSAGE_C            ! Messages d'erreur
   use M_TRAITER_ERREUR_I     ! Traitement des erreurs

   !==============================================================
   !           Declarations
   !==============================================================
   !.. Declaration Implicite ..
   implicit none

   !.. Arguments .. 
   type(SOURCE_TRACER_T)  , dimension(:), intent(inout)  :: Source_Tracer
   type(COND_LIM_TRACER_T), dimension(:), pointer        :: Cond_lim
   type(EXTREMITE_T)       , dimension(:), pointer       :: Extremite
   !.. Donnees ..
   type(LOI_TRACER_T)      , dimension(:), intent(in   ) :: LoiTracer
   integer                               , intent(in   ) :: Nbtrac
   real(DOUBLE)                          , intent(in   ) :: Temps
   !.. Gestion des erreurs ..
   type(ERREUR_T)                        , intent(inout) :: Erreur
   !.. Constante  pour les interpolations ..
   integer , parameter :: ORDRE_INTERPOLATION = 1

   end subroutine QCL_TRACER

   end interface

end module M_QCL_TRACER_I 
