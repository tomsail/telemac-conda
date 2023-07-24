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

module M_PSING_I
!***********************************************************************
! PROGICIEL : MASCARET      A. LEBOSSE
!                           S. PERON
!                           S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine PSING ( &
        ZAm         , & ! Cote de la section amont de la singularite
        Singularite , & ! Singularite
        ZREF        , & ! Cote du fond a la section amont
        ZAV         , & ! Cote de la section aval de la singularite
        QAM         , & ! Debit a la section amont de la singularite
        Profil      , & ! Profils geometriques
        B1Plan      , & ! Largeur au miroir planimetree
        IDT         , & ! numeros des profils amont des sections
        XDT         , & ! Positionnement des sections / profils
        Section     , & ! Numero de la section de la singularite
        Temps       , & ! Temps ou numero de la ligne d'eau a calculer
        Erreur        & ! Erreur
                   )
   !
   ! **********************************************************************
   !  FONCTION :
   !  ----------
   !
   !             CALCUL DE LA COTE A L'AMONT D'UNE SINGULARITE
   !             EN REGIME PERMANENT
   !
   !-----------------------------------------------------------------------
   !
   !  FICHIERS ENTREE/SORTIE :
   !  ----------------------
   !
   !  SOUS PROGRAMME APPELANT :  PERMAT
   !  -------------------------
   !  SOUS PROGRAMMES APPELES :  INTERPOLATION_S
   !  -------------------------
   !
   !  COMMENTAIRES :
   !  ------------
   !
   !  . LES CALCULS DEPENDENT DU TYPE DE LA SINGULARITE
   !  . SI LA SINGULARITE EST DEFINIE AU MOYEN D'UNE FAMILLE DE COURBES
   !    (TYPE 1), LA COTE AMONT EST OBTENUE DIRECTEMENT AU MOYEN
   !    D'INTERPOLATIONS SUR CES COURBES
   !  . SI LA SINGULARITE EST DEFINIE AU MOYEN D'UNE LOI
   !    Q = F ( ZAMONT , ZAVAL) (TYPE 2) , LA COTE AMONT EST
   !    ESTIMEE INITIALEMENT EN SUPPOSANT LE REGIME DENOYE, PUIS ELLE
   !    EST MODIFIEE LE  CAS ECHEANT JUSQU'A OBTENIR LE DEBIT CORRECT
   !  . SI LA SINGULARITE EST DE TYPE 3, LA COTE AMONT EST ESTIMEE
   !    DE MANIERE SEMBLABLE, EN ASSIMILANT LA CHARGE A LA HAUTEUR
   !    AU DESSUS DU SEUIL
   !  . SI LA SINGULARITE EST DE TYPE 4 OU 5 LA SOLUTION EST
   !    IMMEDIATE
   !  . LES TYPES SUIVANTS NE SONT PAS ADMIS EN PERMANENT
   !
   !    EN REGIME NOYE , LA CORRECTION EST DONNEE PAR LE COEFFICIENT C :
   !    RH=(HAVAL-Singularite%CoteCrete)/(HAMONT-Singularite%CoteCrete)
   !       ---          RH < 0.8   C= +1
   !       ---   0.8  < RH < 1.0   C= C1*RH**3 + C2*RH**2 + C3*RH + C4
   !
   ! **********************************************************************

   !============================= Declarations ===========================
   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_PARAMETRE_C      ! EPS3, W23, W32
   use M_MESSAGE_C        ! Messages d'erreur
   use M_ERREUR_T         ! Type ERREUR_T
   use M_PROFIL_T         ! Type PROFIL_T
   use M_SINGULARITE_T    ! Type SINGULARITE_T
   use M_INTERPOLATION_S  ! Sous-programme INTERPOLATION_S
   use M_RHSBP_S          ! Sous programme RHSBP_SECTION_S
   use M_TRAITER_ERREUR_I ! Traitement de l'erreur

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !----------------
   real(DOUBLE)       ,                  intent(inout) :: ZAm
   type(SINGULARITE_T),                  intent(in   ) :: Singularite
   real(DOUBLE)       ,                  intent(in   ) :: ZREF
   real(DOUBLE)       ,                  intent(in   ) :: ZAV
   real(DOUBLE)       ,                  intent(in   ) :: QAM
   type(PROFIL_T)     , dimension(:)  ,  intent(in   ) :: Profil
   real(DOUBLE)       , dimension(:,:),  intent(in   ) :: B1Plan
   integer            , dimension(:)  ,  intent(in   ) :: IDT
   real(DOUBLE)       , dimension(:)  ,  intent(in   ) :: XDT
   integer                            ,  intent(in   ) :: Section
   real(DOUBLE)       ,                  intent(in   ) :: Temps
   type(ERREUR_T)     ,                  intent(inout) :: Erreur

   end subroutine PSING

   end interface

end module M_PSING_I
