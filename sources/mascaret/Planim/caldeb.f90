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

subroutine CALDEB ( &
     DEB          , & ! Debitance
     DEB1         , & ! Debitance mineur
     DEB2         , & ! Debitance majeur
     Section      , & ! Section de calcul
     Pas          , & ! Pas de planimetrage de calcul
     DS1          , & ! Section mineure planimetree
     DS2          , & ! Section majeure planimetree
     DP1          , & ! Perimetre mouillee mineure planimetree
     DP2          , & ! Perimetre mouillee majeure planimetree
     CF1          , & ! Coeff de frottement mineur aux sections
     CF2          , & ! Coeff de frottement majeur aux sections
     ProfGauche   , & ! Profil gauche
     ProfDroit    , & ! Profil droit
     XDELTA       , & ! Position relative de la section / profils
     LoiFrottement, & ! Loi de frottement utilisee
     Erreur         & ! Erreur
     )
! *********************************************************************
! PROGICIEL : MASCARET         N. GOUTAL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************
!   FONCTION : CODE MASCARET : CALCUL DE LA DEBITANCE
!
!-----------------------------------------------------------------------
!
!   FICHIERS  ENTREE/SORTIE : --
!   -------------------------
!
!   SOUS PROGRAMME APPELANT : - PLANMA
!   -------------------------
!   SOUS-PROGRAMMES APPELES : --
!   -------------------------
!***********************************************************************

   !============================= Declarations ===========================
   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_PARAMETRE_C ! W23, W16, W09, PI, SEPS
   use M_DEBITANCE_S ! calcul de la debitance
   use M_ERREUR_T    ! Type ERREUR_T

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   ! 1ere dimension NbSect
   real(double), dimension(:,:)  , intent(  out) :: DEB,DEB1,DEB2
   integer     ,                   intent(in   ) :: Section
   integer     ,                   intent(in   ) :: Pas
   ! 1ere dimension NbProf
   real(double), dimension(:,:)  , intent(in   ) :: DS1,DS2
   real(double), dimension(:,:)  , intent(in   ) :: DP1,DP2
   real(double), dimension(:)    , intent(in   ) :: CF1,CF2
   integer     ,                   intent(in   ) :: ProfGauche
   integer     ,                   intent(in   ) :: ProfDroit
   real(double),                   intent(in   ) :: XDELTA
   integer     ,                   intent(in   ) :: LoiFrottement
   type(ERREUR_T),                 intent(inout) :: Erreur
   !.. Variables locales ..
   !-----------------------
   real(double)   :: S1,S2,P1,P2,R1,R2,RATIOR,A,A0
   real(double)   :: deb_min_g       ! debitance min generalisee
   real(double)   :: deb_maj_g       ! debitance maj generalisee
   real(double)   :: deb_min,deb_maj ! debitances
   real(DOUBLE)   :: st1_temp        ! strickler mineur (ne sert pas)
   !character(132) :: !arbredappel_old

   !============================= Instructions ===========================
   !  Initialisation
   !  --------------
   Erreur%Numero = 0
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>CALDEB'

   S1 = DS1(ProfGauche,Pas) + (DS1(ProfDroit,Pas) - DS1(ProfGauche,Pas)) * XDELTA
   P1 = DP1(ProfGauche,Pas) + (DP1(ProfDroit,Pas) - DP1(ProfGauche,Pas)) * XDELTA
   S2 = DS2(ProfGauche,Pas) + (DS2(ProfDroit,Pas) - DS2(ProfGauche,Pas)) * XDELTA
   P2 = DP2(ProfGauche,Pas) + (DP2(ProfDroit,Pas) - DP2(ProfGauche,Pas)) * XDELTA

   R1 = ( S1 / P1 )

   if( P2 >= SEPS ) then
      R2 = ( S2 / P2 )
   else
      R2 = 0._DOUBLE
   endif

   call DEBITANCE_S( &
      deb_min       , &
      st1_temp      , &
      R1            , &
      S1            , &
      LoiFrottement , &
      CF1(section)  , &
      Erreur          &
                    )
   if( Erreur%Numero /= 0 ) then
      return
   endif

   deb_maj = R2**W23 * S2 * CF2(Section)

   ! CALCUL DE LA DEBITANCE GENERALISEE
   ! ----------------------------------
   if( S2 < SEPS ) then
      ! DEBITANCE MINEURE UNIQUEMENT
      DEB1(Section,Pas) = deb_min
      DEB2(Section,Pas) = 0._DOUBLE
      DEB (Section,Pas) = deb_min
   else
      ! COMPOSITION DES DEBITANCES (HYPOTHESE DEBORD)
      A0 = W09 * ( CF2(Section) / CF1(Section) )**W16
      RATIOR= R2 / R1
      if (RATIOR > 0.3_DOUBLE) then
         A = A0
      else
         A = ( ( 1._DOUBLE - A0 ) * dcos( PI * RATIOR / 0.3_DOUBLE ) + 1._DOUBLE + A0 ) / 2._DOUBLE
      endif

      ! deb_maj_g reformule pour eviter division par 0
      deb_min_g = A * deb_min
      deb_maj_g = dsqrt( S2 * S2 + S1 * S2 * ( 1._DOUBLE - A * A ) ) * CF2(section) * R2**W23
      DEB1(Section,Pas) = deb_min_g
      DEB2(Section,Pas) = deb_maj_g
      DEB(Section,Pas)  = deb_min_g + deb_maj_g
   endif

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine CALDEB
