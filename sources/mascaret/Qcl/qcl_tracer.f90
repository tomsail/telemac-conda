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

subroutine QCL_TRACER( &
                       ! Resultats : Objets dont l'etat est mis a jour
            Cond_Lim , & ! Conditions limites amont
       Source_Tracer , & ! Tableau des sources
                       ! Donnees
           Extremite , & ! Tableau des extremites libres
           LoiTracer , & ! Tableau des lois de concentrations des traceurs
              Nbtrac , & ! Nombre de traceurs
               Temps , & ! Temps
                       ! Modele
              Erreur   & ! Erreur
                       )

!*****************************************************************************
! PROGICIEL : TRACER         S.MANDELKERN - M. LUCK - F. ZAOUI
!
! VERSION : V8P4R0              EDF-CEREMA
!*****************************************************************************
!
!  FONCTION :
!  --------
!  La fonctionnalite Qcl_Tracer fournit les valeurs au temps t
!  de chaque concentration aux limites (C)
!
!   SOUS-PROGRAMME(S) APPELANT(S) : SUPERVISEUR
!   -----------------------------
!   SOUS-PROGRAMME(S) APPELE(S)   : INTERPOLATION_S
!   ---------------------------
!
!***********************************************************************

   use M_PRECISION
   use M_CONSTANTES_CALCUL_C  ! Constantes pour les phases et modeles de calcul
   use M_SOURCE_TRACER_T      ! Definition du type SOURCE_TRACER_T
   use M_COND_LIM_TRACER_T    ! Definition du type COND_LIM_TRACER_T
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
   !.. Variables locales ..
   integer  :: nb_source
   integer  :: nb_extremite
   integer  :: nb_loi_tracer
   integer  :: is, itrac ,iext   ! Compteur sur les sources
   integer  :: num_loi           ! Numero de la loi utilisee
   integer  :: nb_points
   integer  :: retour

   !==============================================================
   !           Initialisations
   !==============================================================
   Erreur%Numero = 0
   !arbredappel_old    = trim(Erreur%arbredappel)
   !Erreur%arbredappel = trim(Erreur%arbredappel)//'=>QCL_TRACER'

   nb_loi_tracer = size(LoiTracer)
   nb_extremite  = size(Extremite)

   !==============================================================
   !           Calcul de la source de traceur au temps T
   !==============================================================
   nb_source = size(Source_Tracer)

   if( nb_source /= 0 ) then

      do is = 1 , nb_source

         if(.not.associated(Source_Tracer(is)%Apport_source)) allocate( Source_Tracer(is)%Apport_source(Nbtrac) , STAT = Retour )

         num_loi   = Source_Tracer(is)%NumeroLoi
         nb_points = size(LoiTracer(num_loi)%Temps)

         !--------------------------------------------
         ! Interpolation de la source au temps Temps
         ! a partir de la loi LoiTracer correspondante
         !--------------------------------------------

         do itrac = 1 , Nbtrac

            call INTERPOLATION_S (                           &
                 Source_Tracer(is)%Apport_source(itrac)    , &
                 Temps                                     , &
                 ORDRE_INTERPOLATION                       , &
                 LoiTracer(num_loi)%Temps                  , &
                 LoiTracer(num_loi)%Conc(1:nb_points,itrac), &
                 size(LoiTracer(num_loi)%Temps)            , &
                 Erreur                                      )

            if( Erreur%Numero /= 0 ) then
               return
            end if

         enddo

      end do

   end if

   !==============================================================
   ! Calcul de la condition limite amont du traceur au temps Temps
   !==============================================================
   if( Nbtrac /= 0 ) then
      do iext = 1 , Nb_Extremite

         if(.not.associated(Cond_Lim(iext)%Conc_lim)) allocate( Cond_Lim(iext)%Conc_lim(Nbtrac) , STAT = Retour )
         num_loi   = Cond_Lim(iext)%NumeroLoi
         nb_points = size(LoiTracer(num_loi)%Temps)

         !---------------------------------------------
         ! Interpolation de la cote au temps Temps
         !  a partir de la loi LoiHydrau correspondante
         !---------------------------------------------

         do itrac = 1 , Nbtrac

            call INTERPOLATION_S (                           &
                 Cond_Lim(iext)%Conc_lim(itrac)            , &
                 Temps                                     , &
                 ORDRE_INTERPOLATION                       , &
                 LoiTracer(num_loi)%Temps                  , &
                 LoiTracer(num_loi)%Conc(1:nb_points,itrac), &
                 size(LoiTracer(num_loi)%Temps)            , &
                 Erreur                                      )
            if( Erreur%Numero /= 0 ) then
               return
            end if

         end do

      end do

   end if

   !Erreur%arbredappel = arbredappel_old

   return

end subroutine QCL_TRACER
