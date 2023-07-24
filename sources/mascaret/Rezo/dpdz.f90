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

subroutine  DPDZ           ( & ! Resultats
            DPDZ1, DPDZ2   , & ! Gradient de P1 et P2 / Z
            NumSection     , & ! Gradient de P2 / Z
            Z              , & ! Cote de la surface libre
            ZREF           , & ! Cote de reference
            IDT, XDT       , & ! Reperage des sections / profils
            Profil         , & ! Profils geometrique
            ProfilPlan     , & ! Profils planimetrees
            Connect        , & ! Table de connectivite
            X              , & ! Maillage
            Impression     , & ! Flag d'impression
            UniteListing   , & ! Unite logique Fichier listing
            Erreur           & ! Erreur
                           )

! *********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE
!                             P. CHERUBINI
!                             S. MANDELKERN
!
! VERSION : V8P4R0               EDF-CEREMA
! *********************************************************************
!  FONCTION :
!  --------
!           CALCUL DU GRADIENT DP / DZ DANS LA SECTION DE CALCUL NumSection
!           ( UTILE POUR LE SOUS PROGRAMME COEFFS )
!
!-----------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :   UniteListing  : Sortie listing
!   ----------------------
!   SOUS PROGRAMME APPELANT :  REZO
!   -------------------------
!   SOUS PROGRAMMES APPELES :  ---
!   -------------------------
!------------------------------------------------------------------------

   !========================== Declarations ================================
   use M_PRECISION       ! Type DOUBLE
   use M_PARAMETRE_C     ! Parametres de calcul
   use M_MESSAGE_C       ! Liste des messages d'erreur
   use M_PROFIL_T        ! Definition du type PROFIL_T
   use M_PROFIL_PLAN_T   ! Definition du type PROFIL_PLAN_T
   use M_ERREUR_T        ! Definition du type ERREUR_T
   use M_CONNECT_T       ! Definition du type CONNECT_T
   use M_NUM_BIEF_S       ! Calcul du numero de bief d'une section
   use M_TRAITER_ERREUR_I ! Traitement des erreurs

   implicit none

   !.. Arguments ..
   real(DOUBLE)                 , intent(  out) :: DPDZ1, DPDZ2
   type (PROFIL_T), dimension(:), intent(in   ) :: Profil
   type (PROFIL_PLAN_T)         , intent(in   ) :: ProfilPlan
   real(DOUBLE)   , dimension(:), intent(in   ) :: XDT
   integer        , dimension(:), intent(in   ) :: IDT
   real(DOUBLE)                 , intent(in   ) :: Z
   real(DOUBLE)                 , intent(in   ) :: ZREF
   integer                      , intent(in   ) :: NumSection
   type(CONNECT_T)              , intent(in   ) :: Connect
   real(DOUBLE), dimension(:)   , intent(in   ) :: X
   logical                      , intent(in   ) :: Impression
   integer                      , intent(in   ) :: UniteListing
   type(ERREUR_T)               , intent(inout) :: Erreur

   !.. Scalaires locaux ..
   integer        :: I,J,K, IP1
   real(DOUBLE)   :: DDZ,XD
   real(DOUBLE)   :: Y         ! Hauteur d'eau
   integer        :: num_bief  ! numero du bief de la section NumSection
   real(DOUBLE)   :: abs_rel   ! abscisse relative correspondante

   !.. Tableaux locaux ..
   real(DOUBLE), dimension(2) :: FP1,FP2

   !.. Functions intrinseques ..
   intrinsic INT

   !========================== Instructions ================================
   ! INITIALISATIONS
   ! ---------------
   Erreur%Numero = 0
   !arbredappel_old    = trim(Erreur%arbredappel)
   !Erreur%arbredappel = trim(Erreur%arbredappel)//'=>DPDZ'

   Y   = Z - ZREF
   XD  = XDT(NumSection)
   I   = IDT(NumSection)
   IP1 = I + 1

   if( XD <= EPS6 ) then
      IP1 = I
   end if

   DDZ = Profil(I)%Pas + ( Profil(IP1)%Pas - Profil(I)%Pas ) * XD

   if( Y > 0._DOUBLE ) then
      ! NUMERO DE LA TRANCHE DE PROFIL CONTENANT Y
      !-------------------------------------------
      K = INT( Y / DDZ + 1._DOUBLE )
      if( K >= Profil(I)%NbPas ) then
         if( Impression ) then
            write (UniteListing,10000) NumSection, Y, Profil(I)%NbPas, DDZ
         end if
         K = Profil(I)%NbPas - 1
      end if

      do J = 1 , 2
         FP2(J) = ProfilPlan%P2(I,K) + ( ProfilPlan%P2(IP1,K) - ProfilPlan%P2(I,K) ) * XD
         FP1(J) = ProfilPlan%P1(I,K) + ( ProfilPlan%P1(IP1,K) - ProfilPlan%P1(I,K) ) * XD
         K      = K + 1
      end do

      DPDZ1 = ( FP1(2) - FP1(1) ) / DDZ
      DPDZ2 = ( FP2(2) - FP2(1) ) / DDZ

   else
      Erreur%Numero = 600
      Erreur%ft     = err_600
      Erreur%ft_c   = err_600c
      num_bief      = NUM_BIEF_S( Connect , NumSection , Erreur )
      abs_rel       = X(i) - X(Connect%OrigineBief(num_bief))
      call TRAITER_ERREUR( Erreur , num_bief , abs_rel )
      return
   end if

   !Erreur%arbredappel = arbredappel_old

   return

  ! ... Formats ...
10000 format (                                                   &
       '<<ATTENTION>>',/,                                        &
       'Dans la section de calcul n0 ',i4,',',/,                 &
       'Tirant d''eau = ',g15.3,' Depassant le planimetrage.',/, &
       'Augmenter le nombre de pas actuellement egal a ',i4,/,   &
       'ou la valeur du pas : ',g15.3)

end subroutine DPDZ
