!== Copyright (C) 2000-2022 EDF-CEREMA ==
!
!   This file is part of MASCARET.
!
!   MASCARET is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   MASCARET is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with MASCARET.  If not, see <http://www.gnu.org/licenses/>
!

subroutine CQINJ( &
             Qinjec , & ! Vecteur Debit d'apport
                  X , & ! Abscisse des sections de calcul
                  Z , & ! Cotes
             Apport , & ! Apports
          Deversoir , & ! Deversoirs
           Qdeverse , &
             Erreur   & ! Erreur
                      )

! *********************************************************************
! PROGICIEL : MASCARET        C. RISSOAN      N. GOUTAL
!
! VERSION : V8P4R0               EDF-CEREMA
! *********************************************************************
!   FONCTION : CALCUL DU TABLEAU QINJEC DES APPORTS (DEBITS + DEVRESOIRS)
!   --------
!
!
! ----------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :     - UL_LST : Sortie listing
!   ----------------------
!   SOUS PROGRAMMES APPELANTS :  - REZO, SARAP
!   ---------------------------
!   SOUS PROGRAMMES APPELES :    - INTERPOLATION_S
!   -------------------------
!=========================================================================

   !=========================== Declarations ================================
   use M_PRECISION       ! Type DOUBLE
   use M_PARAMETRE_C     ! Parametres de calcul
   use M_MESSAGE_C       ! Liste des messages d'erreur
   use M_FICHIER_T       ! Numero du canal UL_LST
   use M_DEVERSOIR_T     ! Definition du type DEVERSOIR_T
   use M_APPORT_T        ! Definition du type APPORT_T
   use M_ERREUR_T        ! Definition du type ERREUR_T
   use M_INTERPOLATION_S  ! Interpolation
   use M_TRAITER_ERREUR_I ! Traitement des erreurs

   implicit none

   !.. Arguments ..
   real(DOUBLE)     , dimension(:), intent(  out) :: Qinjec
   real(DOUBLE)     , dimension(:), intent(in   ) :: X
   real(DOUBLE)     , dimension(:), intent(in   ) :: Z
   type(APPORT_T)   , dimension(:), intent(in   ) :: Apport
   type(DEVERSOIR_T), dimension(:), intent(in   ) :: Deversoir
   real(DOUBLE)     , dimension(:), intent(inout) :: Qdeverse
   type(ERREUR_T)                 , intent(inout) :: Erreur

   !.. Variables locales ..
   real(DOUBLE), dimension(:), allocatable :: q_dever
   integer        :: IS             ! Indice de section de calcul
   integer        :: JJ, K          ! Compteurs
   integer        :: nb_point
   integer        :: ns_debut, ns_fin
   real(DOUBLE)   :: q_apport
   real(DOUBLE)   :: long_apport_modele
   real(DOUBLE)   :: coeff_debit
   real(DOUBLE)   :: cote_crete
   integer        :: retour    ! retour de fonction intrinseque
   !character(132) :: arbredappel_old

   !========================== Instructions =============================
   ! INITIALISATION
   !---------------
   Erreur%Numero = 0
   retour        = 0
   !arbredappel_old    = trim(Erreur%arbredappel)
   !Erreur%arbredappel = trim(Erreur%arbredappel)//'=>CQINJ'
   Qinjec(:)   = 0._DOUBLE
   Qdeverse(:) = 0._DOUBLE

   ! CAS DES DEBITS D'APPORT
   !------------------------
   if( size(Apport) /= 0 ) then

      do K = 1 , size(Apport)

         ns_debut = Apport(K)%SectionAm
         ns_fin   = Apport(K)%SectionAv
         q_apport = Apport(K)%Debit

         ! DEBIT PONCTUEL
         if( ABS(Apport(K)%Longueur).LT.EPS15 ) then
            Qinjec(ns_debut) = Qinjec(ns_debut) + q_apport
         else
         ! DEBIT LINEIQUE (EXPRIME EN M3/M)
            long_apport_modele = X(ns_fin) - X(ns_debut)
            if( long_apport_modele > 0._DOUBLE ) then
               do IS = ns_debut , ns_fin - 1
                  Qinjec(IS) = Qinjec(IS) + q_apport * ( X(IS+1) - X(IS) )
               end do
            end if

            Qinjec(ns_fin) = Qinjec(ns_fin) + q_apport * ( Apport(K)%Longueur - long_apport_modele )
         end if
      end do
   end if

   ! CAS DES DEVERSOIRS
   !-------------------
   if( size(Deversoir) /= 0 ) then
      do K = 1,size(Deversoir)
         ns_debut    = Deversoir(K)%SectionAm
         ns_fin      = Deversoir(K)%SectionAv
         nb_point    = size(Deversoir(K)%PtZ)
         coeff_debit = Deversoir(K)%CoeffDebit
         cote_crete  = Deversoir(K)%CoteCrete

         ! DEVERSOIR PONCTUEL
         if( ns_debut == ns_fin ) then
            allocate( q_dever(1) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'q_dever' )
               return
            end if

            if( Deversoir(K)%Type == DEVERSOIR_TYPE_CRETE_COEFF ) then
               if( ( Z(ns_debut) - cote_crete ) > 0._DOUBLE ) then
                  q_dever(1)         = coeff_debit * ( 2._DOUBLE * GPES )**W12 * ( Z(ns_debut) - cote_crete )**W32
                  Qdeverse(ns_debut) = Qdeverse(ns_debut) + q_dever(1)
            else
                  q_dever(1)         = 0._DOUBLE
                  Qdeverse(ns_debut) = Qdeverse(ns_debut)
            endif
         else if( Deversoir(K)%Type == DEVERSOIR_TYPE_LOI_Z_Q ) then
            call INTERPOLATION_S    ( &
                  q_dever(1)        , &
                  Z(ns_debut)       , &
                  1                 , &
                  Deversoir(k)%PtZ  , &
                  Deversoir(k)%PtQ  , &
                  nb_point          , &
                  Erreur              &
                                    )

            Qdeverse(ns_debut) = Qdeverse(ns_debut) + q_dever(1)

            if( Erreur%Numero /= 0 ) then
               return
            end if
         end if

         Qinjec(ns_debut) = Qinjec(ns_debut) - q_dever(1)

         deallocate( q_dever , STAT = retour )
         if( RETOUR /= 0 ) then
            Erreur%Numero = 6
            Erreur%ft     = err_6
            Erreur%ft_c   = err_6c
            call TRAITER_ERREUR( Erreur , 'q_dever' )
            return
         end if

      ! DEVERSOIR LINEIQUE
      else

         allocate( q_dever(ns_fin - ns_debut + 1) , STAT = retour )
         if( retour /= 0 ) then
            Erreur%Numero = 5
            Erreur%ft     = err_5
            Erreur%ft_c   = err_5c
            call TRAITER_ERREUR( Erreur , 'q_dever' )
            return
         end if

         do IS = ns_debut , ns_fin - 1

            JJ = IS - ns_debut + 1

            if( Deversoir(K)%Type == DEVERSOIR_TYPE_CRETE_COEFF ) then

               if( ( Z(IS) - cote_crete ) > 0._DOUBLE ) then
                  q_dever(JJ)  = coeff_debit * ( 2._DOUBLE * GPES )**W12 * ( Z(IS) - cote_crete )**W32
                  Qdeverse(IS) = Qdeverse(IS) + q_dever(JJ) * ( X(IS+1) - X(IS) )
               else
                  q_dever(JJ)  = 0._DOUBLE
                  Qdeverse(IS) = Qdeverse(IS)
               endif

            else if( Deversoir(K)%Type == DEVERSOIR_TYPE_LOI_Z_Q ) then
               call INTERPOLATION_S    ( &
                  q_dever(JJ)        , &
                  Z(IS)              , &
                  1                  , &
                  Deversoir(k)%PtZ   , &
                  Deversoir(k)%PtQ   , &
                  nb_point           , &
                  Erreur               &
                                     )
               Qdeverse(IS) = Qdeverse(IS) + q_dever(JJ) * ( X(IS+1) - X(IS) )
               if( Erreur%Numero /= 0 ) then
                  return
               end if
            end if

            long_apport_modele = X(ns_fin) - X(ns_debut)
            if( long_apport_modele > 0._DOUBLE ) then
               Qinjec(IS) = Qinjec(IS) - q_dever(JJ) * ( X(IS+1) - X(IS) )
            else
               Qinjec(IS) = Qinjec(IS) - q_dever(JJ)
            end if

         end do

         if( Deversoir(K)%Type == DEVERSOIR_TYPE_CRETE_COEFF ) then
            if( ( Z(ns_fin) - cote_crete ) > 0._DOUBLE ) then
               q_dever(JJ+1)    = coeff_debit * ( 2._DOUBLE * GPES )**W12 * ( Z(ns_fin) - cote_crete )**W32
               Qdeverse(ns_fin) = Qdeverse(ns_fin) + q_dever(JJ+1) * ( Deversoir(K)%Longueur - long_apport_modele )
            else
               q_dever(JJ+1)    = 0._DOUBLE
               Qdeverse(ns_fin) = Qdeverse(ns_fin)
            endif
         else if( Deversoir(K)%Type == DEVERSOIR_TYPE_LOI_Z_Q ) then
            call INTERPOLATION_S    ( &
                 q_dever(JJ+1)      , &
                 Z(ns_fin)          , &
                 1                  , &
                 Deversoir(k)%PtZ   , &
                 Deversoir(k)%PtQ   , &
                 nb_point           , &
                 Erreur               &
                                     )

            Qdeverse(ns_fin) = Qdeverse(ns_fin) + q_dever(JJ+1) * ( Deversoir(K)%Longueur - long_apport_modele )

            if( Erreur%Numero /= 0 ) then
               return
               end if
            end if

            Qinjec(ns_fin) = Qinjec(ns_fin) - q_dever(JJ+1) * ( Deversoir(K)%Longueur - long_apport_modele )

            deallocate( q_dever , STAT = retour )

            if( RETOUR /= 0 ) then
               Erreur%Numero = 6
               Erreur%ft     = err_6
               Erreur%ft_c   = err_6c
               call TRAITER_ERREUR( Erreur , 'q_dever' )
               return
            end if
         endif
      end do    ! de do k=1,size(Deversoir)
   end if    ! de if(size(Deversoir) /= 0)

   ! Fin des traitements
   !--------------------
   !Erreur%arbredappel = arbredappel_old

   return

end subroutine CQINJ
