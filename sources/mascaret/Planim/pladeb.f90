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

subroutine  PLADEB( &
                    ! Resultats
              Section , & ! Section sous la cote de debordement
                    ! Donnees
        LimiteInterne , & ! Limite interieur du lit
        LimiteExterne , & ! Limite exterieur du lit
              NumProf , & ! Numero du profil
             DXP, DYP , & ! Points geometriques du profil
  FrottParoiVerticale , & ! Conservation du frottement sur les parois verticales
      ImpressionPlani , & ! Impression du planimetrage
         UniteListing , & ! Unite logique listing
               Erreur  & ! Erreur
                       )
! *********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE
!                             P. CHERUBINI
!                             S. PERON
!                             S. MANDELKERN
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************
!  FONCTION :
!
!           PLANIMETRAGE DU LIT MAJEUR LORSQUE LA COTE EST EGALE A LA
!           COTE DE DEBORDEMENT , EN RIVE DROITE OU EN RIVE GAUCHE
!
!-----------------------------------------------------------------------
!   FICHIERS  ENTREE/SORTIE :   - UniteListing  : IMPRESSION LISTING
!   -------------------------
!   SOUS-PROGRAMME APPELANT :   - PLANIM
!   -------------------------
!   SOUS-PROGRAMMES APPELES :   - COMPT_CHENAUX
!   -------------------------
!
!   COMMENTAIRES  :
!   ---------------
!***********************************************************************

   !============================= Declarations ===========================
   use M_PRECISION           ! Type DOUBLE
   use M_PARAMETRE_C         ! Parametres de calcul
   use M_MESSAGE_C           ! Liste des messages d'erreur
   use M_ERREUR_T         ! Definition du type ERREUR_T
   use M_TRAITER_ERREUR_I ! Traitement des erreurs
   use M_COMPT_CHENAUX_I  ! Interface de sous-programme

   !.. Implicit Declarations ..
   implicit none

   !.. Parameters ..
   integer, parameter :: NB_MAX_CHENAUX = 100

   !.. Formal Arguments ..
   real(DOUBLE), dimension(2), intent(  out) :: Section
   integer     , dimension(2), intent(in   ) :: LimiteInterne
   integer     , dimension(2), intent(in   ) :: LimiteExterne
   integer                   , intent(in   ) :: NumProf
   real(DOUBLE), dimension(:), intent(inout) :: DXP, DYP
   logical                   , intent(in   ) :: FrottParoiVerticale
   logical                   , intent(in   ) :: ImpressionPlani
   integer                   , intent(in   ) :: UniteListing
   type(ERREUR_T)            , intent(inout) :: Erreur
   !.. Local Scalars ..
   integer :: rive       ! rive gauche ou droite de la riviere
   integer :: irive      ! compteur sur les rives d'un chenal
   integer :: ichenal    ! compteur sur les chenaux
   integer :: ipoint     ! compteur sur les points
   integer :: largeur    ! Largeur, en nombre de points, d'un chenal
   integer :: nb_chenaux ! Nombre de chenaux
   real(DOUBLE) :: YI,YID,YIG,XI,XID
   real(DOUBLE) :: XIG
   real(DOUBLE) :: cote_debord ! Cote de debordement
   !character(132) :: !arbredappel_old
   !.. Local Arrays ..
   integer     , dimension(2)                :: borne              ! Limites de calcul
   integer     , dimension(2,NB_MAX_CHENAUX) :: limite_chenal      ! Limites des chenaux
   real(DOUBLE), dimension(2,NB_MAX_CHENAUX) :: absc_limite_chenal ! Abscisse limite des chenaux
   !.. Intrinsic Functions ..
   intrinsic DABS

   !============================ Instructions ==============================

   ! INITIALISATIONS
   ! ---------------
   Erreur%Numero = 0
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>PLADEB'

   do rive = 1 , 2
      Section(rive) = W0
      if( rive == 1 ) then
         borne(1) = LimiteExterne(1)
         borne(2) = LimiteInterne(1)
      else  if (rive == 2) then
         borne(1) = LimiteInterne(2)
         borne(2) = LimiteExterne(2)
      end if
      cote_debord = DYP(LimiteInterne(rive))

      call COMPT_CHENAUX( &
                 nb_chenaux , &
              limite_chenal , &
         absc_limite_chenal , &
                      borne , &
                cote_debord , &
                   DXP, DYP , &
                     Erreur &
                          )

      ! PLANIMETRAGE
      ! ------------
      if( nb_chenaux > 0 ) then
         do ichenal = 1 , nb_chenaux
            do irive = 1 , 2   ! rives gauche et droite d'un chenal
               ! XI et YI : coordonnees des points <en eau>
               !-------------------------------------------
               XI = DXP(limite_chenal(irive,ichenal) + 2 - irive)
               YI = DYP(limite_chenal(irive,ichenal) + 2 - irive)
               if( ( FrottParoiVerticale ) .or. ( DABS( absc_limite_chenal(irive,ichenal) - XI ) > EPS54 ) ) then
                  ! RIVE GAUCHE D'UN CHENAL
                  if( ( irive == 1 .and. ( absc_limite_chenal(irive,ichenal) >= DXP(borne(1) ) .and. XI <= DXP(borne(2) ) ) ) &
                  ! RIVE DROITE D'UN CHENAL
                  .or. ( irive == 2 .and.( absc_limite_chenal(irive,ichenal) <= DXP(borne(2)) .and. XI >= DXP(borne(1)) ) ) ) then
                     Section(rive) = Section(rive)  +                     &
                                     W12 * DABS( absc_limite_chenal(irive,ichenal) - XI ) * &
                                     ( cote_debord - YI )
                  end if
               endif
            end do ! rives gauche et droite d'un chenal
            ! CENTRE D'UN CHENAL
            largeur = limite_chenal(2,ichenal) - limite_chenal(1,ichenal) - 1
            if( largeur > 0 ) then
               do ipoint = 1 , largeur
                  XIG = DXP( limite_chenal(1,ichenal) + ipoint )
                  YIG = DYP( limite_chenal(1,ichenal) + ipoint )
                  XID = DXP( limite_chenal(1,ichenal) + ipoint + 1 )
                  YID = DYP( limite_chenal(1,ichenal) + ipoint + 1 )
                  if( FrottParoiVerticale .or. DABS( XIG - XID ) > EPS54 ) then
                     if( XIG >= DXP(borne(1) ) .and. XID <= DXP(borne(2)) ) then
                        Section(rive) = Section(rive) + W12 * ( XID - XIG ) * ( W2 * cote_debord - YIG - YID )
                    end if
                  end if
               end do ! sur les points interieurs au chenal
            end if   ! de largeur > 0
         end do     ! sur les chenaux
      end if       ! de nb_chenaux > 0
   end do         ! rive

   ! ECRITURE SUR LE FICHIER UniteListing
   ! ------------------------------
   if(ImpressionPlani) then
      write (UniteListing,1000) NumProf, DYP(LimiteInterne(1)), Section(1), &
                                 DYP(LimiteInterne(2)), Section(2)
   endif

   ! Fin des traitements
   ! -------------------
   !Erreur%arbredappel = !arbredappel_old
   return

! ... Format Declarations ...

1000  format ( &
    'Profil n0 ',i4,'. Debordement progressif.',/, &
    'Rive gauche : cote de debordement = ',f6.2,', section mouillee = ',f8.2,/, &
    'Rive droite : cote de debordement = ',f6.2,', section mouillee = ',f8.2,/)

end subroutine PLADEB
