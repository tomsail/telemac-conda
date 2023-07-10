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

module M_DECODER_GEOM_I
!***********************************************************************
! PROGICIEL : MASCARET        S. MANDELKERN         D. POIZAT
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine DECODER_GEOM      ( &
              presence_mot_cle  , &   ! flag de presence du mot cle
              NomBief           , &   ! Nom du bief lu sur la ligne
              NomProf           , &   ! Nom du profil lu sur la ligne
              AbscProf          , &   ! Abscisse du profil lue sur la ligne
              chaine            , &   ! chaine de caracteres a analyser
              MOT_CLE           , &   ! tableau des mot cle a trouver
              iprof             , &   ! compteur du profil
              Erreur            )     ! Variables de controle d'erreur

! .....................................................................
!  FONCTION : DECODAGE D UNE CHAINE DE CARACTERE DANS LE BUT
!  --------   DE DECELER LA PRESENCE DU MOT CLE
!             ET LES VALEURS LUES
!
!----------------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :       --
!   ----------------------
!
!   SOUS-PROGRAMME(S) APPELANT(S) : LEC_GEOM_V3P0
!   -----------------------------
!   SOUS-PROGRAMME(S) APPELE(S)   : ---
!   ---------------------------
!   COMMENTAIRES :
!   ------------
!
!   DOCUMENTATION EXTERNE :
!   ---------------------
!
!***********************************************************************

   !=============================== Declarations ==========================
   use M_PRECISION
   use M_ERREUR_T             ! Definition du type ERREUR_T
   use M_TRAITER_ERREUR_I     ! Interface generique de gestion des erreurs
   use M_MESSAGE_C            ! Messages d'erreur
   use M_CONSTANTES_CALCUL_C  ! Constantes

   implicit none

   ! Arguments
   logical                    , intent(  out) :: presence_mot_cle
   character(*)               , intent(  out) :: NomBief
   character(*)               , intent(  out) :: NomProf
   real(DOUBLE)               , intent(  out) :: AbscProf
   character(80)              , intent(in   ) :: chaine
   character(*), dimension (:), intent(in   ) :: MOT_CLE
   integer                    , intent(in   ) :: iprof
   type(ERREUR_T)             , intent(inout) :: Erreur

   end subroutine DECODER_GEOM

   end interface

end module M_DECODER_GEOM_I
