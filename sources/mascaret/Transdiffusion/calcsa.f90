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

subroutine CALCSA( SAD , &  ! Terme Source ajoutee  (kg/m3/s)
         Source_Tracer , &  ! Structure de donnees Traceur
                Qinjec , &  ! Injection de debit (dans le calcul hydraulique)
                     H , &  ! Hauteur d'eau
                     X , &  ! Maillage longitudinal
                     A , &  ! Section mouillee totale
                Nbsect , &  ! Nombre de sections de calcul 
                 Nbtra , &  ! Nombre de traceurs
                Erreur  )

!*****************************************************************************
! PROGICIEL : TRACER         S.MANDELKERN - N.GOUTAL - M.LUCK
!
! VERSION : V8P4R0              EDF-CEREMA
!*****************************************************************************
!
!  FONCTION :
!  --------
!   Calcul des termes Sources ajoutees (surfaciques et volumiques)
!   a l'instant t
!
!  SOUS-PROGRAMMES APPELANT : TRACER
!  ------------------------
!
!**********************************************************************

   use  M_PRECISION
   use  M_SOURCE_TRACER_T
   use  M_APPORT_T
   use  M_CONSTANTES_TRACER_T
   use  M_PARAMETRES_QUALITE_EAU_T 
   use  M_XINDIC_S
   use  M_ERREUR_T

   !==============================================================
   !           Declarations
   !==============================================================
   implicit none
   !
   !.. Variables transmises ..
   !
   type (SOURCE_TRACER_T), dimension(:), intent(inout) :: Source_tracer
   type (ERREUR_T)       ,               intent(inout) :: Erreur
   real (DOUBLE) , dimension(:,:)      , intent(  out) :: SAD ! sources ajoutees a l'instant t
   real (DOUBLE) , dimension(:)        , intent(in   ) :: Qinjec
   real (DOUBLE) , dimension (:)       , intent(in   ) :: X ,A , H
   integer                             , intent(in   ) :: Nbsect , Nbtra
   !
   !.. Variables locales ..
   !
   real (DOUBLE), dimension (nbsect,nbtra) :: SSA , SVA
   real (DOUBLE)                           :: Flux_source , Volume
   real (DOUBLE)                           :: Long , long_modele
   integer                                 :: i , j , k , Kaval , Kamont

   !==============================================================
   !   Initialisations
   !==============================================================
   do k = 1 , Nbtra
      do i = 1 , Nbsect
         SVA(i,k) = 0.d0
         SSA(i,k) = 0.d0
      enddo
   enddo

   if( size(Source_tracer) /= 0 ) then
      !==============================================================
      !   Calcul des sources volumiques et surfaciques
      !             de traceur(s) ajoutees
      !==============================================================
      do k = 1 , Nbtra   ! boucle sur les traceurs
         do j = 1 , size(Source_tracer)   ! boucle sur les sources

            Kamont      = Source_tracer(j)%SectionAm
            Kaval       = Source_tracer(j)%SectionAv
            Flux_source = Source_tracer(j)%Apport_source(k)
            Long        = Source_tracer(j)%Longueur
            long_modele = X(Kaval) - X(Kamont)

            ! Sources volumiques
            if( Source_tracer(j)%type == SOURCE_TRACER_TYPE_VOLUMIQUE ) then
               if( Source_tracer(j)%SuperpositionApport ) then
                  ! Flux_source = concentration (kg/m3)
                  do i = Kamont , Kaval
                     Volume =  0.5d0 * A(i) * ( X(i+1) - X(i-1) )
                     SVA(i,k) = SVA(i,k) + Flux_source * Qinjec(i) / Volume
                     ! (si longueur non nulle, correction de longueur deja faite dans Qinjec)
                  enddo
               else
                  if( Kaval.eq.Kamont ) then
                     ! Flux_source = flux volumique (kg/m3/s)
                     SVA(Kamont,k) = SVA(Kamont,k) + Flux_source
                  else
                     ! Flux_source en kg/m3/s/m
                     do i = Kamont , Kaval - 1
                        SVA(i,k) = SVA(i,k) + Flux_source * ( X(i+1) - X(i) )
                     enddo
                     SVA(Kaval,k) = SVA(Kaval,k) + Flux_source * ( Long - long_modele )
                  endif
               endif

            ! Sources surfaciques
            elseif( Source_tracer(j)%type == SOURCE_TRACER_TYPE_SURFACIQUE ) then
               if( Kaval.eq.Kamont ) then
                  ! Flux_source = flux surfacique kg/m2/s
                  SSA(Kamont,k) = SSA(Kamont,k) + Flux_source
               else
                  ! Flux_source en kg/m2/s/m
                  do i = Kamont , Kaval - 1
                     SSA(i,k) = SSA(i,k) + Flux_source * ( X(i+1) - X(i) )
                  enddo
                  SSA(Kaval,k) = SSA(Kaval,k) + Flux_source * ( Long - long_modele )
               endif
            ! Sources de type flux temporel
            elseif( Source_tracer(j)%type == SOURCE_TRACER_TYPE_FLUX_TEMP ) then
               if( Kaval.eq.Kamont ) then
                  ! Flux_source = flux temporel en kg/s
                  Volume        = 0.5d0 * A(Kamont) * ( X(Kamont+1) - X(Kamont-1) )
                  SVA(Kamont,k) = SVA(Kamont,k) + Flux_source / Volume
               else
                  ! Flux_source en kg/s/m
                  do i = Kamont , Kaval - 1
                     Volume   =  0.5d0 * A(i) * ( X(i+1) - X(i-1) )
                     SVA(i,k) = SVA(i,k) + Flux_source / Volume * ( X(i+1) - X(i) )
                  enddo
                  SVA(Kaval,k) = SVA(Kaval,k) + Flux_source * ( Long - long_modele )
               endif
            endif
         enddo  ! fin de la boucle sur les sources
      enddo ! fin de la boucle sur les traceurs
   endif ! de if ( size(Source_tracer) /= 0 )

   !==============================================================
   !   Cumul des sources volumiques et surfaciques
   !==============================================================
   do k = 1 , Nbtra
      do i = 1 , Nbsect
         SAD(i,k) = SSA(i,k) / H(i) + SVA(i,k)
      enddo
   enddo

   return

end subroutine CALCSA
