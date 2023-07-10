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

subroutine SURVOL( Casier , NumeroCasier , Erreur )

! ******************************************************************
! PROGICIEL : MASCARET                  C. RISSOAN     F. ZAOUI
!
! VERSION : V8P4R0                  EDF-CEREMA
!
!CALCUL DE LA SURFACE INONDEE ET DU VOLUME STOCKE DANS UN CASIER
!******************************************************************
!
!   FICHIERS ENTREE/SORTIE :  --
!   ----------------------
!   SOUS PROGRAMMES APPELANTS : - CALCCASIER
!   ---------------------------
!   SOUS PROGRAMMES APPELES :    --
!   -------------------------
!========================== Declarations ==============================

   use M_PRECISION                ! type DOUBLE
   use M_PARAMETRE_C              ! Parametres de calcul
   use M_CASIER_T                 ! type CASIER
   use M_ERREUR_T                 ! type ERREUR
   use M_TRAITER_ERREUR_CASIER_I  ! traitement des erreurs
   use M_MESSAGE_CASIER_C         ! messages d erreur propres a CASIER

   implicit none

!.. Arguments..
   type(CASIER_T), intent(inout) :: Casier
   type(ERREUR_T), intent(inout) :: Erreur
   integer       , intent(in   ) :: NumeroCasier

!.. Variables locales..
   real(DOUBLE) :: hauteur  , &  ! tirant d eau
                   dz       , &  ! difference entre le tirant d eau et le tirant
                                 ! d eau approche par planimetrage
                   surface1 , &  ! surface du casier approchee par planimetrage (valeur inf)
                   surface2 , &  ! surface du casier approchee par planimetrage (valeur sup)
                   volume1       ! volume du casier approche par planimetrage (valeur inf)
   integer      :: nb_pas_planim ! numero du pas inferieur
   !character(132) :: arbredappel_old ! Arbre d'appel precedent l'entree du sous programme

!========================== Instructions ==============================

!
! INITIALISATIONS
! ---------------

   Erreur%Numero = 0
!  arbredappel_old    = trim(!Erreur%arbredappel)
!  Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>SURVOL'

   hauteur = dabs(Casier%Cote-Casier%CoteFond)

   if (hauteur < EPS15) then
      Erreur%Numero = 803
      Erreur%ft     = err_803
      Erreur%ft_c   = err_803c
      call TRAITER_ERREUR_CASIER( Erreur , NumeroCasier , 'SURVOL' )
      return
   end if

   nb_pas_planim = INT( hauteur / Casier%PasPlanim )
   dz            = hauteur - REAL( nb_pas_planim ) * Casier%PasPlanim
   nb_pas_planim = nb_pas_planim + 1

!
! CALCUL
! ------

   if ( nb_pas_planim >= size( Casier%Loi_Z_S(:,2) ) ) then
      Casier%Surface = Casier%Loi_Z_S(size( Casier%Loi_Z_S(:,2) ),2)
      Casier%Volume  = Casier%Loi_Z_V(size( Casier%Loi_Z_S(:,2) ),2) &
                       + (hauteur - REAL(size( Casier%Loi_Z_S(:,2) )-1) * Casier%PasPlanim) &
                       * Casier%Surface
   else
      surface1       = Casier%Loi_Z_S(nb_pas_planim,2)
      surface2       = Casier%Loi_Z_S(nb_pas_planim + 1,2)
      Casier%Surface = surface1 + (dz/Casier%PasPlanim) * (surface2 - surface1)
      volume1        = Casier%Loi_Z_V(nb_pas_planim,2)
      Casier%Volume  = volume1 + ((surface1+Casier%Surface) / 2) * dz
   end if

!
! Fin des traitements
! -------------------

!   Erreur%arbredappel = arbredappel_old

   return

end subroutine SURVOL
