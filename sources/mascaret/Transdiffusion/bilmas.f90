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

SUBROUTINE BILMAS( MASSE , FLUMAS , FLUENT , FLUSOR , FLUSRC  , &
                                       Source_ajoutee , itrac , &
                      ibief , Nb_sect , DT , NSCMP , Constrac , &
                                            C , A , X , B , U , &
                                                   RK , IPASS )

!*****************************************************************************
! PROGICIEL : TRACER         S.MANDELKERN - N.GOUTAL - M.LUCK
!
! VERSION : V8P4R0              EDF-CEREMA
!*****************************************************************************
!
!  FONCTION :
!  --------
!
! SOUS PROGRAMME CALCULANT LA MASSE TOTALE
! DE CHAQUE TRACEUR PRESENT DANS UN BIEF
!
!___________________________________________________________________
!   NOM   !TYPE!MODE!                      ROLE                     !
!_________!____!____!_______________________________________________!
!                                                                   !
!                              PARAMETRES D'APPEL                   !
!___________________________________________________________________!
!  NBTRA  ! E  ! >  ! Nbre de traceurs                              !
!  DT     ! R  ! >  ! Pas de temps                                  !
! IM     ! R  ! >  ! Nombre de sections de calcul                   !
! NSCMP  ! E  ! >  ! Section de calcul ou on calcul la masse passee !
! CONV   ! C  ! >  ! Convection des traceur                         !
!   C     ! TR ! >  ! Concentration de traceur                      !
!   A     ! TR ! >  ! Section mouillee                              !
!   X     ! TR ! >  ! Abscisse de la section de calcul              !
!   B     ! TR ! >  ! Compteur du Numero du pas de LIDO (en NP)     !
!   U     ! TR ! >  ! Vitesse de l'eau                              !
!   RK    ! TR ! >  ! Coefficient de dispersion                     !
! MASSE   ! TR !    ! Masse du traceur j dans le domaine            !
! FLUMAS  ! TR !    ! Masse du traceur j passant au pt NSCMP du maillage!
! FLUENT  ! TR !    ! Masse du traceur j entrant dans le domaine    !
! FLUSOR  ! TR !    ! Masse du traceur j sortant du domaine         !
!_________!____!____!_______________________________________________!
!                                                                   !
!                            VARIABLES INTERNES                     !
!___________________________________________________________________!
! RMANN   ! R  !    ! Masse de traceur au point i du maillage       !
! NSCMP2  ! E  !    ! NSCMP pour calcul du gradient                 !
!-------------------------------------------------------------------
!                                                                   !
!                            VARIABLES EN COMMON                    !
!___________________________________________________________________!
! NFM     ! E  !    ! Numero du fichier de stockage des masses      !
! NFM2    ! E  !    ! Num du fich de stock masse passee en 1 section!
!-------------------------------------------------------------------
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
!-----------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :
!   ----------------------
!
!   SOUS-PROGRAMMES APPELANT : TRACER
!   ------------------------
!
!   SOUS-PROGRAMMES APPELES :
!   -----------------------
!
!   COMMENTAIRES :
!   ------------
!
!***********************************************************************

   use M_PRECISION
   use M_CONSTANTES_TRACER_T
   use M_CONSTANTES_CALCUL_C

   implicit none

   Type (CONSTANTES_TRACER_T) , dimension(:) , intent(inout) :: ConsTrac
   real (DOUBLE)                             , intent(inout) :: MASSE
   real (DOUBLE)                             , intent(inout) :: FLUMAS , FLUENT , FLUSOR , FLUSRC
   real (DOUBLE)              , dimension(:) , intent(in)    :: C , Source_ajoutee
   real (DOUBLE)              , dimension(:) , intent(in)    :: A , B , U , X , RK
   real (DOUBLE) DT
   integer NSCMP , IPASS
   integer itrac , ibief
   integer Nb_sect
   LOGICAL CONV
   real(DOUBLE) , dimension(Nb_sect) :: Bord , dXi
   integer I , IM , NSCMP2 , scheco
   real(DOUBLE) RMANN

   CONV   = ConsTrac(itrac)%Conv
   SCHECO = ConsTrac(itrac)%Scheconv

   !
   ! * CALCUL DE LA MASSE TOTALE DE TRACEUR *
   ! * ------------------------------------ *
   !
   MASSE = 0.d0
   RMANN = 0.d0

   if( scheco.eq.2 ) then

      do I = 2 , Nb_sect - 1

         if( CONV ) then
            RMANN = RMANN + ( C(I) + C(I+1) ) * 0.25d0   &
                   * ( A(I) + A(I+1) ) * ( X(i+1) - X(i) )
         else ! variable de fond
            RMANN = RMANN + ( C(I) + C(I+1) ) * 0.25d0   &
                   * ( B(I) + B(I+1) ) * ( X(i+1) - X(i) )
         endif
         MASSE = RMANN
      enddo

   elseif (scheco.eq.3) then

      do I = 2 , Nb_sect - 1

         if( CONV ) then
            RMANN = RMANN + ( A(I) * C(I) ) * ( X(i+1) - X(i) )
         else ! variable de fond
            RMANN = RMANN + ( B(I) * C(I) ) * ( X(i+1) - X(i) )
         endif
         MASSE = RMANN

      enddo

   else

      Bord(1) = ( X(2) + X(1) ) / 2.d0

      do I = 2 , Nb_sect - 1

         Bord(I) = ( X(I) + X(I+1) ) / 2.d0
         DXi(I)  = Bord(I) - Bord(I-1)

      end do

      do I = 2 , Nb_sect - 1

         if( CONV ) then
            RMANN = RMANN + C(I) * A(I) * DXi(I)
         else ! variable de fond => A VOIR
            RMANN = RMANN + C(I) * B(I) * DXi(I)
         endif
         MASSE = RMANN

      enddo

   endif

   !
   ! * CALCUL DES FLUX DE TRACEUR *
   ! * -------------------------- *
   !
   NSCMP = Nb_sect / 2
   if( NSCMP /= Nb_sect ) then
      NSCMP2 = NSCMP
   else
      NSCMP2 = NSCMP - 1
   endif

   if( CONV .and. IPASS == PHASE_CALCUL ) then

      IM = Nb_sect

      if( ( scheco.eq.2 ).or.( scheco.eq.3 ) ) then

         FLUMAS = FLUMAS + C(NSCMP) * DT * A(NSCMP) * U(NSCMP)  &
                  - RK(NSCMP2) * ( C(NSCMP2+1)-C(NSCMP2) )      &
                  / (X(NSCMP2+1)-X(NSCMP2))

         FLUENT = FLUENT + C(1) * DT * A(1) * U(1)              &
                  - RK(1) * ( C(2)-C(1) )                       &
                  / ( X(2)-X(1) )

         FLUSOR = FLUSOR + C(IM) * DT * A(IM) * U(IM)           &
                  - RK(IM) * ( C(IM) - C(IM-1) )                &
                  / ( X(IM) - X(IM-1) )

         do I = 2 , IM - 1

            FLUSRC = FLUSRC + Source_ajoutee(I)                 &
                    * A(I) * 0.5*(X(I+1)-X(I-1)) * DT
         enddo
         FLUSRC = FLUSRC                                             &
                 + Source_ajoutee(1)  * A(1)  * (X(2)-X(1))     * DT &
                 + Source_ajoutee(IM) * A(IM) * (X(IM)-X(IM-1)) * DT

      else

         FLUMAS = 0.d0
         FLUENT = FLUENT ! calcule dans la routine GODUNOV ou MUSCL_HANCOCK
         FLUSOR = FLUSOR ! calcule dans la routine GODUNOV ou MUSCL_HANCOCK
         do I = 2 , IM - 1
            FLUSRC = FLUSRC + Source_ajoutee(I)                 &
                    * A(I) * 0.5*(X(I+1)-X(I-1)) * DT
         enddo
         FLUSRC = FLUSRC                                              &
                  + Source_ajoutee(1)  * A(1)  * (X(2)-X(1))     * DT &
                  + Source_ajoutee(IM) * A(IM) * (X(IM)-X(IM-1)) * DT

      endif

   else

      FLUMAS = 0.d0
      FLUENT = 0.d0
      FLUSOR = 0.d0
      FLUSRC = 0.d0

   endif

   return

end subroutine BILMAS
