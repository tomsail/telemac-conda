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

Subroutine Diffu_tracer( &
                  Conc , & ! Concentration a t+Dt
                    T2 , & ! Grandeur convectee (C ou Sm.C) a t+Dt
                  Flux , & ! Flux lineaire de MES (kg/s/m)
                    Sm , & ! Surface mouillee a t+DT
                   Vit , & ! Vitesse a t+dt
                  Absc , & ! Abscisse des sections de calcul
                    Dt , & ! Pas de temps
                    Rk , & ! Coefficient de diffusion
                  conv , & ! Convection
                scheco , & ! Schema pour la convection
                QINJEC , & ! Debits d'apport
                NbProf , & ! Nb de sections de calcul
         type_cl_amont , & ! Type de conditions aux
          type_cl_aval , & ! limites aval / amont
                Erreur )

!*****************************************************************************
! PROGICIEL : TRACER         Ch. BERTIER - M. LUCK
!
! VERSION : V8P4R0              EDF-CEREMA
!*****************************************************************************
!=========================================================================
!
!  Fonction : Calcul de la diffusion de la concentration
!  --------
!
!  Sous-programme appelant : DansLo - Resequ
!  -----------------------
!
!  Sous-programme appele : Bissnd_tracer
!  ---------------------
!
!=========================================================================

   use M_PRECISION ! Definition de la precision DOUBLE ou SIMPLE
   use M_PARAMETRE_C ! Definition des constante tq EPS*, W0, ...
   use M_ERREUR_T ! Type ERREUR_T
   use M_MESSAGE_C! Messages d'erreur
   use M_TRAITER_ERREUR_I ! Traitement de l'errreur
   use M_BISSND_TRACER_I

   !=========================================================================
   ! DECLARATIONS
   !=========================================================================
   !.. Implicit Declarations ..
   implicit none

   ! Variables d'entree
   real(DOUBLE), intent(in) :: Dt
   real(DOUBLE), dimension(:), intent(in   ) :: T2
   real(DOUBLE), dimension(:), intent(in   ) :: Flux
   real(DOUBLE), dimension(:), intent(in   ) :: Sm
   real(DOUBLE), dimension(:), intent(in   ) :: Vit
   real(DOUBLE), dimension(:), intent(in   ) :: Absc
   real(DOUBLE), dimension(:), intent(in   ) :: Rk
   integer                   , intent(in   ) :: scheco, NbProf
   logical                   , intent(in   ) :: CONV
   real(DOUBLE), dimension(:), intent(in   ) :: QINJEC
   integer     , intent(in)                  :: type_cl_amont
   integer     , intent(in)                  :: type_cl_aval
   ! Variables de sortie
   real(DOUBLE), dimension(:), intent(  out)  :: Conc
   ! Variables locales
   integer :: i ! Compteur
   real(DOUBLE), dimension(:), allocatable :: AAA, BBB, CCC, DDD ! coefficients de la matrice
   real(DOUBLE), dimension(:), allocatable :: A, B, C, D ! coefficients de la matrice
   real(DOUBLE) :: Alpha ! Variable de calcul
   real(DOUBLE) :: Ai, APi, Ki, KPi ! Variable de calcul
   real(DOUBLE) :: Hi, HiP1, HHi ! Variable de calcul
   real(DOUBLE) :: Cst1, Cst2, Cst3 ! Variable de calcul
   ! Traitement des erreurs
   type(ERREUR_T), intent(inout) :: Erreur
   integer :: retour ! Code de retour de la fonction read, allocate


   !=========================================================================
   !           Initialisations
   !=========================================================================
   Erreur%Numero = 0
   retour        = 0
   ! arbredappel_old    = trim(Erreur%arbredappel)
   ! Erreur%arbredappel = trim(Erreur%arbredappel)//'=>Diffu_tracer'

   !=========================================================================
   !    Allocation des tableaux locaux
   !=========================================================================
   Allocate( AAA(NbProf) ,STAT = retour )
   If( retour /= 0 ) Then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'AAA' )
      return
   End if

   Allocate( BBB(NbProf) , STAT = retour )
   If( retour /= 0 ) Then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'BBB' )
      return
   End if

   Allocate( CCC(NbProf) , STAT = retour )
   If( retour /= 0 ) Then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'CCC' )
      return
   End if

   Allocate( DDD(NbProf) , STAT = retour )
   If (retour /= 0) Then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'DDD' )
      return
   End if

   Allocate( A(NbProf) , STAT = retour )
   If( retour /= 0 ) Then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'A' )
      return
   End if

   Allocate( B(NbProf) , STAT = retour )
   If( retour /= 0 ) Then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'B' )
      return
   End if

   Allocate( C(NbProf) , STAT = retour )
   If (retour /= 0) Then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'C' )
      return
   End if

   Allocate( D(NbProf) , STAT = retour )
   If( retour /= 0 ) Then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'D' )
      return
   End if

   !=========================================================================
   !      Conditions limites amont
   !=========================================================================
   If( type_cl_amont == 2 ) then
      BBB(1) = W1
      CCC(1) = W0
      if( SCHECO.EQ.2 ) then
         DDD(1) = T2(1)          ! Forme non conservative => T2 = C
      else
         DDD(1) = T2(1) / Sm(1)  ! Forme conservative     => T2 = A.C
      endif
   Else
      BBB(1) = W1
      CCC(1) = -W1
      DDD(1) = W0
   Endif

   !=========================================================================
   !      Conditions limites aval
   !=========================================================================
   If( ( Vit(NbProf) >= W0 ).OR.( type_cl_aval == 1 ) ) Then
      AAA(NbProf) = -W1
      BBB(NbProf) =  W1
      DDD(NbProf) =  W0
   Else
      AAA(NbProf) =  W0
      BBB(NbProf) =  W1
      if( SCHECO.EQ.2 ) then
         DDD(NbProf) = T2(NbProf)               ! Forme non conservative => T2 = C
      else
         DDD(NbProf) = T2(NbProf) / Sm(NbProf)  ! Forme conservative     => T2 = A.C
      endif
   Endif

   !=========================================================================
   !      Calcul des coefficients courants
   !=========================================================================
   Alpha = W1 / Dt

   Do i = 2 , NbProf - 1

      Hi   = Absc(i  ) - Absc(i-1)
      HiP1 = Absc(i+1) - Absc(i  )
      HHi  = Absc(i+1) - Absc(i-1)

      Ai   =  Sm(i)
      APi  = (Sm(i) - Sm(i-1)) / Hi
      Ki   =  RK(i)
      KPi  = (RK(i) - RK(i-1)) / Hi

      Cst1 =  W2 / (HiP1 * HHi )
      Cst2 =  W2 / (Hi   * HHi )
      Cst3 = -W2 / (Hi   * HiP1)

      AAA(i) =            - Ki * Ai * Cst2 + Ki * APi / Hi + KPi * Ai / Hi
      BBB(i) = Alpha * Ai - Ki * Ai * Cst3 - Ki * APi / Hi - KPi * Ai / Hi
      CCC(i) =            - Ki * Ai * Cst1
      DDD(i) = T2(i) * Alpha

      ! === Traitement de la forme non conservative ===
      if( SCHECO.EQ.2 ) then
         AAA(i) = AAA(i) / Ai
         BBB(i) = BBB(i) / Ai
         CCC(i) = CCC(i) / Ai
      endif

   Enddo

   ! === Ajout des termes provenant de l'equation de continuite ===
   ! === en cas de debit d'apport  (en forme non conservative)  ===
   !
   if( CONV.AND.( SCHECO == 2 ) ) then
      do I = 2 , NbProf
         Hi = Absc(i  ) - Absc(i-1)
         Ai = Sm(i)
         if( SCHECO.EQ.2 ) AAA(I) = AAA(I) + QINJEC(I) / ( Ai * Hi )
     enddo
   endif

   Do i = 1 , NbProf
      A(i) = AAA(i)
      B(i) = BBB(i)
      C(i) = CCC(i)
      if (SCHECO.EQ.2) then
         D(i) = DDD(i) + Flux(i)
      else
         D(i) = DDD(i) + Flux(i) * Sm(i)
      endif
      Conc(i) = D(i)
   Enddo

   !=========================================================================
   !      Resolution du systeme
   !=========================================================================
   call Bissnd_tracer( &
                Conc , & ! Resultat de la diffusion
                   A , & ! ! Coefficients
                   B , & ! ! de la
                   C , & ! ! matrice de diffusion
              NbProf , & ! Nombre de point des tableaux
                   1 , & ! NFU
              Erreur )
   If( Erreur%Numero /= 0 ) Then
      return
   Endif

   !=========================================================================
   !      Dellocation des tableaux locaux
   !=========================================================================

   deallocate( AAA , BBB , CCC , DDD )
   deallocate( A , B , C , D )

   !Erreur%arbredappel = arbredappel_old

   return

End Subroutine Diffu_tracer
