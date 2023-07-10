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

subroutine MUSCL_HANCOCK( &
                          ! Donnees Initiales et Resultat
                     GT , & ! Grandeur (AC) apres convection
                      G , & ! Grandeur (AC) donnee
                          ! Donnees hydrauliques
                      U , & ! Vitesse
                          ! Donnees TRACER
               ConsTrac , &   ! Constantes liees au transport-diffusion
                   CLAM , &   ! Condition Limite Amont
                   CLAV , &   ! Condition Limire Aval
                          ! Modele
                      X , & ! Abscisse des concentrations
                     DT , & ! Pas de temps
                 Nbsect , & ! Nombre de sections de calcul (donc de mailles VF)
                 FLUENT , &
                 FLUSOR , &
                 Erreur )

!*****************************************************************************
! PROGICIEL : TRACER         S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!*****************************************************************************
!
!  FONCTION :
!  --------
!
! RESOLUTION DE L'EQUATION DE CONVECTION EN FORMULATION CONSERVATIVE
!   PAR UNE METHODE VOLUMES FINIS D'ORDRE 2 OU 3 (MUSCL-HANCOCK METHOD)
!   AVEC OU SANS LIMITEUR DE PENTE (DE TYPE SUPERBEE)
!
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
   real (DOUBLE)                           , intent(inout) :: FLUENT, FLUSOR
   type (ERREUR_T)                         , intent(inout) :: Erreur
   ! Variables Locales
   ! -----------------
   integer                                                 :: ordre ! Ordre (2 ou 3)
   real (DOUBLE)                                           :: w     ! Parametre de pente
   logical                                                 :: limiteur_pente   ! Limiteur de pente
   !
   real (DOUBLE), dimension(Nbsect)                        :: dXi        ! Taille des mailles
   real (DOUBLE), dimension(Nbsect)                        :: Bord , XD  ! Abscisses des interfaces
   integer                                                 :: i          ! Indice de boucle
   real (DOUBLE)                                           :: CFL        ! Nombre de courant
   real (DOUBLE), dimension(Nbsect)                        :: Gamont , Gaval , GL , GR
   real (DOUBLE), dimension(Nbsect)                        :: penteS  ! Pente
   real (DOUBLE)                                           :: pente_amont, pente_aval, pente
   real (DOUBLE)                                           :: beta , r , xsi  ! Parametres de correcteur de pente
   real (DOUBLE), dimension(Nbsect+1)                      :: Flux_int       ! Flux aux interfaces

   ! Intrinsic Functions
   ! -------------------
   intrinsic MIN

   ! Initialisations
   ! ---------------
   ordre          = ConsTrac%OrdreVF
   w              = ConsTrac%ParamW
   limiteur_pente = ConsTrac%LimiteurPente

   !.. Maillage
   Bord(1) = ( X(2) + X(1) ) / 2.d0
   XD(1)   = X(1)
   DXi(1)  = Bord(1) - X(1)

   do i = 2 , Nbsect - 1
      Bord(i) = ( X(i) + X(i+1) ) / 2.d0
      DXi(i)  = Bord(i) - Bord(i-1)
      XD(i)   = ( Bord(i) + Bord(i-1) ) / 2.d0
   end do

   DXi(Nbsect) = X(Nbsect) - Bord(Nbsect-1)
   XD(NBsect)  = X(Nbsect)
   Bord(Nbsect) = Bord(Nbsect-1) + U(Nbsect) * DT

   !.. Critere de CFL
   do i = 1 , Nbsect - 1
      CFL = U(i) * DT / dXi(i)
      if( CFL.GT.1.d0 ) then
         print*, 'Problem with the Courant–Friedrichs–Lewy condition : '
         print*, '=================================================='
         print*, 'Index : ',i,' Abscissa : ', X(i)
         print*, 'Cell size              : ', dXi(i)
         print*, 'Speed                  : ', U(i)
         print*, 'Time step              : ', DT
         print*, 'Courant number         : ', CFL
         exit
      end if
   end do
   !
   ! 1. Calcul des pentes
   ! --------------------

   !.. Premiere maille
   penteS(1) = ( G(2) - G(1) ) / ( X(2) - X(1) )

   !.. Mailles "internes"
   do i = 2 , Nbsect - 1

      pente_amont = ( G(i)   - G(i-1) ) / ( XD(i  ) - XD(i-1) )
      pente_aval  = ( G(i+1) - G(i)   ) / ( XD(i+1) - XD(i  ) )
      if( ordre.EQ.3 ) then
         w = ( 1.d0 / 3.d0 ) * ( 2.d0 * ( U(i) * DT / dXi(i) ) - 1.d0 )
      endif
      pente = ( 1.d0 / 2.d0 ) * ( 1.d0 + w ) * pente_amont + ( 1.d0 / 2.d0 ) * ( 1.d0 - w ) * pente_aval

      if( limiteur_pente ) then ! Limiteur de pente de type Superbee
         if( abs(pente_aval).lt.EPS15 ) then
            if( abs(pente_amont).lt.EPS15 ) then
               r = 0.d0
            else
               r = 1.1d0
            endif
         else
            r = pente_amont / pente_aval
         endif

         beta = 2.d0 / ( 1.d0 - ( U(i) * DT / dXi(i) ) )

         !MS2019 handle an infinity case
         IF( W == 1.d0 .AND. r == 0.d0) THEN
           xsi = 3.d0
         ELSE
           xsi  = ( 2.d0 * beta ) / ( 1.d0 - w + ( 1.d0 + w ) * r )
         ENDIF

         if( r.LE.0.d0 ) then
            penteS(i) = 0.d0
         elseif( ( r.GE.0.d0 ).and.( r.LE.0.5d0 ) ) then
            penteS(i) = 2.d0 * r * pente
         elseif( ( r.GE.0.5d0 ).and.( r.LE.1.d0 ) ) then
            penteS(i) = pente
         elseif( r.GE.1.d0 ) then
            penteS(i) = MIN( r , xsi , 2.d0 ) * pente
         endif

      else                     ! Pas de limiteur de pente

         penteS(i) = pente

      endif

   enddo

   !.. Derniere maille

   penteS(Nbsect) = ( G(Nbsect) - G(Nbsect-1) ) / ( X(Nbsect) - X(Nbsect-1) )
   !
   ! 2. Valeurs aux interfaces
   ! -------------------------
   ! Attention : avec ce calcul de Gaval(1), GR(1) ne reste pas egal a 1 et le flux
   !             entrant (Flux_int(2) n'est plus exactement egal a A1*C1*U1 )
   Gaval(1)  = G(1) + ( Bord(1) - X(1) ) * penteS(1)

   do i = 2 , Nbsect
      Gamont(i) = G(i) + ( Bord(i-1) - X(i) ) * penteS(i)
      Gaval(i)  = G(i) + ( Bord(i)   - X(i) ) * penteS(i)
   enddo
   !
   ! 3. Evolution des valeurs aux interfaces sur un demi pas de temps
   ! ----------------------------------------------------------------
   GR(1) = Gaval(1)

   !.. Mailles "internes"
   do i = 2 , Nbsect
      GL(i) = Gamont(i) + DT / 2.d0 * U(i) * ( Gamont(i)-Gaval(i) ) / dXi(i)
      GR(i) = Gaval(i)  + DT / 2.d0 * U(i) * ( Gamont(i)-Gaval(i) ) / dXi(i)
   enddo
   !
   ! 4. Resolution du probleme de Riemann / Calcul des valeurs convectees
   ! --------------------------------------------------------------------

   ! Calcul des flux aux interfaces (i-1/2)

   Flux_int(1) = U(1) * G(1)                   ! 1ere section = 1ere interface

   do i = 2 , Nbsect                           ! Nbsect interfaces "internes" au bief
      if( U(i-1).ge.0.d0 ) then
         Flux_int(i) = U(i-1) * GR(i-1)
      else
         Flux_int(i) = U(i  ) * GL(i  )
      endif
   enddo

   Flux_int(Nbsect+1) = U(Nbsect) * GR(Nbsect) ! (Nbsect)eme section = (Nbsect+1)eme interface

   ! Resolution du probleme de Riemann aux interfaces
   do i = 1 , Nbsect
      GT(i) = G(i) + DT * ( Flux_int(i) - Flux_int(i+1) ) / dXi(i)
   enddo

   ! Conditions aux limites amont / aval de type Dirichlet
   if( U(1).GE.0.D0 ) then
      GT(1)      = CLAM
   endif
   if( U(Nbsect).LT.0.D0 ) then
     GT(Nbsect) = CLAV
   endif

   ! Calcul des flux entrant et sortant (pour le bilan de masse)
   FLUENT = FLUENT + Flux_int(1) * DT
   FLUSOR = FLUSOR + Flux_int(Nbsect+1) * DT

   if( Erreur%Numero /= 0 ) Then
      return
   endif

end subroutine MUSCL_HANCOCK
