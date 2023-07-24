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

subroutine GODUNOV( &
                    ! Donnees Initiales et Resultat
               GT , & ! Grandeur (AC) apres convection
                G , & ! Grandeur (AC) donnee
                    ! Donnees hydrauliques
                U , & ! Vitesse
                    ! Donnees TRACER
             CLAM , & ! Condition Limite Amont
             CLAV , & ! Condition Limire Aval
                    ! Modele
                X , & ! Abscisses des concentrations 
               DT , &	! ...
           Nbsect , &   !
           FLUENT , &   ! Flux entrant dans le bief
           FLUSOR , &   ! Flux sortant du bief
           Erreur )

!*****************************************************************************
! PROGICIEL : TRACER         S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!*****************************************************************************
!  FONCTION :
!  --------
!   RESOLUTION DE L'EQUATION DE CONVECTION
!   EN FORMULATION CONSERVATIVE
!   PAR UNE METHODE VOLUMES FINIS D'ORDRE 1 (GODUNOV 1959)
!
!   SOUS-PROGRAMMES APPELANT :   RESEQU
!   ------------------------
!
!*****************************************************************************

   use M_PRECISION
   use M_ERREUR_T

   !.. Implicit Declarations ..
   implicit none

   ! Variables Globales
   ! ------------------
   !.. Donnees Initiales et Resultat
   real (DOUBLE), dimension(:), intent(inout)   :: G
   real (DOUBLE), dimension(:), intent(  out)   :: GT
   !.. Donnees hydrauliques
   real (DOUBLE), dimension(:), intent(inout)   :: U
   !.. Donnees TRACER
   real (DOUBLE), intent(inout)                 :: CLAM
   real (DOUBLE), intent(inout)                 :: CLAV
   !.. Modele
   real (DOUBLE)              , intent(in  )    :: DT
   real (DOUBLE), dimension(:), intent(in  )    :: X
   integer                    , intent(in  )    :: Nbsect
   real (DOUBLE)              , intent(inout)   :: FLUENT , FLUSOR
   type (ERREUR_T)            , intent(inout)   :: Erreur
   ! Variables Locales
   ! -----------------
   !.. Resultats
   real (DOUBLE), dimension(Nbsect)             :: dXi ! Taille des mailles
   !.. Les flux
   real (DOUBLE), dimension(Nbsect+1)           :: Flux_int ! Flux aux interfaces
   !.. CFL
   real (DOUBLE)                                :: CFL ! Nombre de Courant
   !.. Modification maillage
   real (DOUBLE), dimension(Nbsect)             :: Bord ! Abscisses des frontieres de mailles
                                                        ! [ Bord(i) = X(i+1/2) ]
   integer                                      :: i    ! Indice de boucle

   !.. Intrinsic Functions .. 
   intrinsic DMAX1,DABS,DMIN1

   ! Initialisations
   ! ---------------
   ! Maillage
   ! --------
   Bord(1) = ( X(2) + X(1) ) / 2.d0
   dXi(1)  = Bord(1) - X(1)

   do i = 2 , Nbsect - 1
      Bord(i) = ( X(i) + X(i+1) ) / 2.d0
      dXi(i)  = Bord(i) - Bord(i-1)
   end do

   dXi(Nbsect) = X(Nbsect) - Bord(Nbsect-1)

   ! Critere de CFL
   ! --------------
   do i = 2 , Nbsect - 1
      CFL = U(i) * DT / dXi(i)
      if( CFL.GT.1.d0 ) then
         print*, 'Problem with the Courant–Friedrichs–Lewy condition : '
         print*, '=================================================='
         print*, 'Index : ',i,'	Abscissa : ', X(i)
         print*, 'Cell size              : ', dXi(i)
         print*, 'Speed                  : ', U(i)
         print*, 'Time step              : ', DT
         print*, 'Courant number         : ', CFL
         exit
      end if
   end do

   ! Calcul des flux aux interfaces (i-1/2)
   ! --------------------------------------
   Flux_int(1) = U(1) * G(1)       ! 1ere section = 1ere interface
   do i = 2 , Nbsect               ! Nbsect interfaces "internes" au bief
      if( U(i-1).ge.0.d0 ) then
         Flux_int(i) = U(i-1) * G(i-1)
      else
         Flux_int(i) = U(i  ) * G(i  )
      endif
   enddo

   Flux_int(Nbsect+1) = U(Nbsect) * G(Nbsect) ! (Nbsect)eme section = (Nbsect+1)eme interface

   ! Resolution du probleme de Riemann aux interfaces
   ! ------------------------------------------------
   do i = 1 , Nbsect
      GT(i) = G(i) + DT * ( Flux_int(i) - Flux_int(i+1) ) / dXi(i)
   enddo

   ! Conditions aux limites amont / aval de type Dirichlet
   ! -----------------------------------------------------
   if( U(1).GE.0.D0 ) then
      GT(1) = CLAM
   endif
   if( U(Nbsect).LT.0.D0 ) then
      GT(Nbsect) = CLAV
   endif

   ! Calcul des flux entrant et sortant (pour le bilan de masse)
   ! -----------------------------------------------------------
   FLUENT = FLUENT + Flux_int(1) * DT
   FLUSOR = FLUSOR + Flux_int(Nbsect+1) * DT

   if( Erreur%Numero /= 0 ) Then
      return
   endif

end subroutine GODUNOV
