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

subroutine  LOILIM      ( &

                R,S,T   , & ! Coefficients de la loi R*delta(Z)+S*delta(Q)=T
                CLZ     , & ! Ordonnee de la loi Z(Q)
                CLQ     , & ! Abscisse de la loi Z(Q)
                NBP     , & ! Nombre de points de la loi Z(Q)
                Z       , & ! Cote de la surface libre
                Q       , & ! Debit
                NumLoi  , & ! Numero de la loi (pour messages d'erreur)
                Erreur    & ! Erreur
                        )

! *********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE
!                             P. CHERUBINI
!                             S. PERON
!
! VERSION : V8P4R0               EDF-CEREMA
! *********************************************************************
!  FONCTION :
!  --------
!
!           CALCUL DES COEFFIENTS R,S,T DE LA CONDITION LIMITE Z(Q)
!
!               R*DELTA(Q) + S*DELTA(Z) = T
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! ._________.___._____._______________________________________________
! !   NOM   !TYPE!MODE!                   ROLE
! !_________!____!____!__________________________________________________
! !   R     ! R  !<-- !   ) COEFFICIENTS DE LA RELATION D'IMPEDANCE
! !   S     ! R  !<-- !   ) A LA LIMITE A TRAITER :
! !   T     ! R  !<-- !   ) R*DELTA(Q) + S*DELTA(Z) = T
! !_________!____!____!______________________________________________
!
!                          VARIABLES LOCALES
! .____________________________________________________________________
! !   DQ    ! R  ! -- ! ECART DE DEBIT DEPUIS LE PAS PRECEDENT .
! !   dqdz  ! R  ! -- ! VALEUR INTERMEDIAIRE D'ECART DE DEBIT .
! !   DZ    ! R  ! -- ! ECART DE COTE DEPUIS LE PAS PRECEDENT .
! !   qm1   ! R  ! -- ! VALEUR DE DEBIT POUR UNE COTE MINOREE,LOI Z(Q)
! !   qp1   ! R  ! -- ! VALEUR DE DEBIT POUR UNE COTE MAJOREE,LOI Z(Q)
! !_________!____!____!______________________________________________
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
!-----------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :
!   ----------------------
!   SOUS PROGRAMMES APPELANTS :  - CCL
!   ---------------------------
!   SOUS PROGRAMMES APPELES :   - INTERPOLATION_S : INTERPOLATION
!   -------------------------     DE LAGRANGE D'ORDRE N
!
!   COMMENTAIRES :
!   ------------
!========================================================================

   !============================ Declarations ==============================
   use M_PRECISION       ! Type DOUBLE
   use M_PARAMETRE_C     ! Parametres de calcul
   use M_MESSAGE_C       ! Liste des messages d'erreur
   use M_INTERPOLATION_S
   use M_ERREUR_T        ! Definition du type ERREUR_T
   use M_TRAITER_ERREUR_I ! Traitement des erreurs

   implicit none

   !.. Arguments ..
   real(DOUBLE)              , intent(  out) :: R , S , T
   real(DOUBLE), dimension(:), intent(in   ) :: CLZ , CLQ
   integer                   , intent(in   ) :: NBP
   real(DOUBLE)              , intent(in   ) :: Z , Q
   integer                   , intent(in   ) :: NumLoi
   type(ERREUR_T)            , intent(inout) :: Erreur
   !.. Local Scalars ..
   real(DOUBLE)   :: dqdz            ! derivee de q/z
   real(DOUBLE)   :: qm1
   real(DOUBLE)   :: qp1
   !character(132) :: arbredappel_old ! arbre d'appel precedent
                                      ! l'entree dans le sous-programme
   real(DOUBLE)   :: dz_1, dz_2      ! delta z

   !.. Intrinsic Functions ..
   intrinsic DSQRT

   !============================= Instructions =============================
   ! INITIALISATIONS
   ! ---------------
   Erreur%Numero = 0
   !arbredappel_old    = trim(Erreur%arbredappel)
   !Erreur%arbredappel = trim(Erreur%arbredappel)//'=>LOILIM'

   dz_1 = EPS2
   dz_2 = EPS2

   !   CONDITION LIMITE LOI Z(Q)
   !   -------------------------
   if( Z < CLZ(1) .or. Z > CLZ(NBP) ) then
      Erreur%numero = 601
      Erreur%ft     = err_601
      Erreur%ft_c   = err_601c
      call TRAITER_ERREUR( Erreur , NumLoi , Z , CLZ(1) , CLZ(NBP) )
      return
   end if

   if( ( Z + dz_1 ) <= CLZ(NBP) ) then

      call INTERPOLATION_S    ( &
              qp1             , &
              Z+dz_1          , &
              1               , &
              CLZ             , &
              CLQ             , &
              NBP             , &
              Erreur            &
                             )

      if( Erreur%Numero /= 0 ) then
         return
      end if

   else

      dz_1 = 0._DOUBLE
      qp1  = Q

   end if

   if( ( Z - dz_2 ) >= CLZ(1) ) then

      call INTERPOLATION_S    ( &
              qm1             , &
              Z-dz_2          , &
              1               , &
              CLZ             , &
              CLQ             , &
              NBP             , &
              Erreur            &
                            )

      if( Erreur%Numero /= 0 ) then
         return
      end if

   else

      dz_2 = 0._DOUBLE
      qm1  = Q

   end if

   dqdz = ( qp1 - qm1 ) / ( dz_1 + dz_2 )

   R = 1._DOUBLE / DSQRT( 1._DOUBLE + dqdz**2 )
   S = - dqdz / DSQRT( 1._DOUBLE + dqdz**2 )
   T = 0._DOUBLE

   ! Fin des traitements
   !--------------------

   !Erreur%arbredappel = arbredappel_old

   return

end subroutine LOILIM
