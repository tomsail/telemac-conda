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

SUBROUTINE DIRECT( &
              FCOUTI , &
               Z_CAL , &   ! Variables propres au calage
                   Z , &   ! Cote de la surface libre
                   Q , &   ! Debit mineur +majeur
              Qinjec , &   ! Qinjecte
              PCSing , &   ! Pertes de charge singulieres
               ZINIT , &
                  ic   &   ! Numero de la crue
                  )
!
!*********************************************************************
! PROGICIEL : MASCARET         A. LEBOSSE
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************
!
!
   !
   !  VARIABLES LIEES A L'HYDRAULIQUE
   !
   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_SHARE_VAR
   ! Constantes nommees
   use M_PARAMETRE_C    ! GPES
   use M_MESSAGE_C      ! Messages d'erreur
   ! Procedures-module
   use M_RHSBP_S           ! Sous-programme RHSBP_S
   use M_NUM_BIEF_S        ! Numero de bief d'une section
   use M_FROUDE_S          ! Calcul du nombre de Froude
   use M_TRAITER_ERREUR_I  ! Interface generique de traitement des erreurs
   ! Interfaces
   use M_COUT_I
   use M_STRICK_I
   use M_PERMAT_I
   use M_CQINJ_I

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !
   !.. Arguments ..
   !---------------
   !
   integer , intent(in) :: ic
   ! TABLEAU  DIMENSIONNE  A NbSect
   !
   real(DOUBLE)       , dimension(:)  , intent(out)   :: Z
   !
   ! TABLEAU  DIMENSIONNE  A NbSect
   real(DOUBLE)       , dimension(:)  , intent(inout) :: Q
   real(DOUBLE)       ,                 intent(inout) :: ZINIT
   ! TABLEAUX DIMENSIONNES A Nbsect
   real(DOUBLE)       , dimension(:)  , intent(in)    :: PCSing
   real(DOUBLE)       , dimension(:)  , intent(inout) :: Qinjec
   !
   ! VARIABLES CASTOR
   !
   real(DOUBLE)  , dimension(:) ,      intent(inout)  :: Z_cal
   real(DOUBLE)                 ,      intent(inout)  :: FCOUTI


   ! VARIABLES LOCALES
   !
   integer isec , iext , numbief , L , I
   Logical Limite_libre

   ! CALCUL DE LA SOMME DES DEBITS D'APPORT DANS LA RIVIERE
   ! ------------------------------------------------------

   !
   ! VALEUR DE LA FONCTION DE COUT INITIALE
   ! --------------------------------------
   !
   ! Connect%FinBief(noeud_bief) : NUMERO DE LA SECTION EXTREME DU BIEF
   Q(1)  = Extremite(1)%PtQ(1)
   do isec = Connect%OrigineBief(1) + 1 , Connect%FinBief(1)
      Q(isec) = Q(isec - 1) + QInjec(isec)
   end do

   ! On cherche l'extremite libre qui correspond
   ! a la section Connect%FinBief(noeud_bief)
   limite_libre = .false.
   do iext = 1, size( Connect%NumSectionExtLibre(:) )
      if( Connect%NumSectionExtLibre(iext) == Connect%FinBief(1) ) then
         ! iext : NUMERO DE LA LIMITE LIBRE CORRESPONDANTE
         limite_libre = .true.
         exit
      end if
   end do

   if( limite_libre ) then
   ! --  ON PART D'UNE LIMITE LIBRE --
      ZINIT = Extremite(iext)%PtZ(1)
   else
   ! --  ON PART D'UN NOEUD --
      ZINIT = Z(Connect%FinBief(1))
   endif

   Numbief = 1

   !
   ! CALCUL DE LA NOUVELLE LIGNE D'EAU EN PERMANENT
   ! ----------------------------------------------
   !
   CALL PERMAT( &
                Z , &   ! Cotes de la surface libre
                Q , &   ! Debit
            ZINIT , &   ! Cote initiale
                X , &   ! Maillage
             ZREF , &   ! Cote de reference
              CF1 , &   ! Coeff de frottement mineur
              CF2 , &   ! Coeff de frottement majeur
           PCSing , &   ! Pertes de charge singuliere
              IDT , &   ! Positionnnement des sections/profils
              XDT , &   ! idem
           Profil , &   ! Profils geometriques
       ProfilPlan , &   ! Tableaux de planimetrage
               F1 , &   !
          Connect , &   ! Table de connectivite
          NumBief , &   ! Numero du bief
          Nb_sect , &
      Singularite , &   ! Singularites
        ModeleLit , &   ! Modele du lit Debord/Fond/Berge
 ImpressionCalcul , &   ! Flag d'impression
     UniteListing , &   ! Unite logique du fichier listing
            Temps , &   ! Temps
    LoiFrottement , &   ! Loi de frottement
            CQMV  , &   ! Apport de qmv pour les apports
     decentrement , &   ! Option de decentrement
           Erreur )     ! ERREUR
   if(Erreur%Numero.ne.0) return

   !
   ! COTES CALCULEES AUX POINTS DE MESURE
   !
   do L = 1 , calage_crues%NMES(ic)
      I        = I_mesu(ic,L)
      Z_CAL(L) = Z(I)
   enddo

   !
   ! CALCUL DE LA FONCTION DE  COUT
   ! ------------------------------
   !
   CALL COUT( FCOUTI , nb_mes(ic) , Z_MESU(ic,:) , Z_CAL , POND(ic,:) )

   return

END SUBROUTINE DIRECT
