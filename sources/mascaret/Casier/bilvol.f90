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

subroutine BILVOL( Casier , Liaison , dt , Erreur )

! ******************************************************************
! PROGICIEL : MASCARET                                   C. RISSOAN
!
! VERSION : V8P4R0              EDF-CEREMA
!
! CALCUL DU BILAN ENTREE - SORTIE DANS UN CASIER
!*******************************************************************
!
!   FICHIERS ENTREE/SORTIE :  --
!   ----------------------
!   SOUS PROGRAMMES APPELANTS : - CALCCASIER
!   ---------------------------
!   SOUS PROGRAMMES APPELES :    --
!   -------------------------

!========================== Declarations ==============================
   use M_PRECISION               ! type DOUBLE
   use M_CONSTANTES_CALCUL_C  ! Constantes parametres de calcul (TETA)
   use M_CASIER_T                ! type CASIER
   use M_LIAISON_T               ! type LIAISON
   use M_ERREUR_T                ! type ERREUR
   use M_TRAITER_ERREUR_CASIER_I ! traitement des erreurs
   use M_MESSAGE_CASIER_C        ! messages d erreur propres a CASIER

   implicit none

   !.. Arguments..
   type(CASIER_T)  , intent(inout)                :: Casier
   type(LIAISON_T) , dimension(:) , intent(in   ) :: Liaison
   type(ERREUR_T)  , intent(inout)                :: Erreur
   real(DOUBLE)    , intent(in   )                :: dt

   !.. Variables locales..
   real(DOUBLE) :: debit , debit_precedent
   integer      :: iliaison
   !character(132) :: arbredappel_old ! Arbre d'appel precedent l'entree du sous programme

   !========================== Instructions ==============================
   !
   ! INITIALISATIONS
   ! ---------------
   Erreur%Numero = 0
   !arbredappel_old    = trim(Erreur%arbredappel)
   !Erreur%arbredappel = trim(Erreur%arbredappel)//'=>BILVOL'
   debit           = 0._DOUBLE
   debit_precedent = 0._DOUBLE

   ! CALCUL
   do iliaison = 1 , size( Liaison )
      debit           = debit + Liaison( iliaison )%DebitEchange
      debit_precedent = debit_precedent + Liaison( iliaison )%DebitPrecedent
   enddo

   Casier%Bilan = Casier%Bilan + 0.5_DOUBLE * ( debit + debit_precedent ) * dt
   !MS2019 replacement of .gt. by .lt. (must be confirmed)
   if( Casier%Volume.lt.EPS6 ) then
      Casier%BilanErreur = 0
   else
      Casier%BilanErreur = dabs( 100.D0 - ( 100.D0 * Casier%Bilan ) / Casier%Volume )
   endif

   ! Fin des traitements
   ! -------------------

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine BILVOL
