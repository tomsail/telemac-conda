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

module M_RESEQU_I
!***********************************************************************
! PROGICIEL : TRACER         S.MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   interface

   subroutine RESEQU( &
                      ! Resultats
                  C , &   ! Concentration du traceur le long du bief
                      ! Donnees hydrauliques
                  A , &   ! Section mouillee totale le long du bief
              A_ANT , &   ! Section mouillee totale le long du bief au pas de temps precedent
             QINJEC , &   ! Debits d apport le long du bief
                  U , &   ! Vitesse de l'eau
                      ! Donnees TRACER
                 RK , &   ! Coefficient de diffusion du traceur
                  S , &   ! Source interne du traceur
                TNU , &   ! termes sources implicites
      type_cl_amont , &   !
       type_cl_aval , &   !
               CLAM , &   ! Concentration limite amont
               CLAV , &   ! Concentration limite aval 
                      ! Modele
                  X , &   ! Abscisses des sections de calcul
           ConsTrac , &   ! Constantes lies au transport-diffusion
                 IM , &   ! Nombre de section du bief
                 DT , &   ! Pas de temps
             FLUENT , &
             FLUSOR , &
            Message , &   ! ERREUR
             Erreur )     ! Canal de sortie pour fichier d impression

   !***********************************************************************
   !  FONCTION :
   !  --------
   !            RESOLUTION DE L'EQUATION DE CONVECTION-DIFFUSION
   !            EN FORMULATION NON CONSERVATIVE ET CONSERVATIVE
   !_________________________________________________________________________________
   !   NOM   !TYPE!MODE!                      ROLE                                  !
   !_________!____!____!____________________________________________________________!
   !                                                                                !
   !                              PARAMETRES D'APPEL                                !
   !________________________________________________________________________________!
   !   DT    ! R  !    ! Pas de temps                                               !
   !   IM    ! E  !    ! Dimension du systeme                                       !
   !   X     ! TR !    ! Position de la section (abscisse)                          !
   !   A     ! TR !    ! Section mouillee                                           !
   !   U     ! TR !    ! Champ du vecteur vitesse                                   !
   !   RK    ! TR !    ! Coefficient de diffusion                                   !
   !   C     ! TR !    ! Concentration initiale                                     !
   !   S     ! TR !    ! Vecteur source volumique                                   !
   !  ZERO   ! TR !    ! Tableau nul pour hyp1fa                                    !
   !  QINJEC ! TR !    ! Debits d'apport                                            !
   !   TNU   ! TR !    ! Terme source  implicit                                     !
   !   CLAM  ! R  !    ! condition amont                                            !
   !   CLAV  ! R  !    ! condition aval                                             !
   !type_cl_amont ! CH ! TYPE DE LA CONDITION A LA LIM AMONT                        !
   !type_cl_aval  ! CH ! TYPE DE LA CONDITION A LA LIM AVAL                         !
   !   CONV  ! L  !    ! Convection du traceurs                                     !
   !_________!____!____!____________________________________________________________!
   !                                                                                !
   !                            VARIABLES INTERNES                                  !
   !________________________________________________________________________________!
   !   GT    ! TR !    ! G TILDA Grandeur (C ou A.C) intermediaire 1<=i<=im         !
   !   RK    ! TR !    ! Coefficient de dispersion (si var. de fond >> RK=0         !
   !________________________________________________________________________________!
   !  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
   !               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
   !-----------------------------------------------------------------------
   !
   !   FICHIERS ENTREE/SORTIE :
   !   ----------------------
   !
   !   SOUS-PROGRAMMES APPELES : DIFFU_TRACER, HYP1FA
   !   -----------------------
   !
   !   SOUS-PROGRAMMES APPELANT : TRACER
   !   ------------------------
   !
   !   COMMENTAIRES :
   !   ------------
   !
   !   DOCUMENTATION EXTERNE :
   !   ---------------------
   !
   !***********************************************************************

   use M_PRECISION
   use M_PARAMETRE_C
   use M_CONSTANTES_TRACER_T
   use M_BISSND_TRACER_I
   use M_HYP1FA_I
   use M_GODUNOV_I
   use M_MUSCL_HANCOCK_I
   use M_ERREUR_T
   use M_DIFFU_TRACER_I

   !.. Implicit Declarations ..
   implicit none

   !.. Formal Arguments ..
   type (ERREUR_T)            , intent(inout)   :: Erreur
   ! Resultat
   real (DOUBLE), dimension(:), intent(inout)   :: C
   ! Donnees hydrauliques
   real (DOUBLE), dimension(:), intent(in)      :: A
   real (DOUBLE), dimension(:), intent(in)      :: QINJEC
   real (DOUBLE), dimension(:), intent(inout)   :: U
   real(DOUBLE), dimension(:)  , intent(in)     :: A_ANT
   ! Donnees TRACER
   integer, intent(in)                          :: type_cl_amont
   integer, intent(in)                          :: type_cl_aval
   real (DOUBLE), intent(inout)                 :: CLAM
   real (DOUBLE), intent(inout)                 :: CLAV
   real (DOUBLE), dimension(:), intent(in)      :: RK
   real (DOUBLE), dimension(:), intent(in)      :: S
   real (DOUBLE), dimension(:), intent(in)      :: TNU
   type (CONSTANTES_TRACER_T) , intent(inout)   :: ConsTrac
   ! Modeles
   integer, intent(in)                          :: IM
   real (DOUBLE), intent(in)                    :: DT
   real (DOUBLE), dimension(:), intent(in)      :: X
   real (DOUBLE), intent(inout)                 :: FLUENT,FLUSOR
   character(132), intent(in)                   :: Message

   end subroutine RESEQU

   end interface

end module M_RESEQU_I
