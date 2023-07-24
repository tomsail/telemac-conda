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

module M_DECODER_GEOM_CASIER_I
!***********************************************************************
! PROGICIEL : MASCARET        A. LEBOSEE    C. RISSOAN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   interface

   subroutine DECODER_GEOM_CASIER ( &
                     presence_mot_cle , &   ! flag de presence du mot cle
                            NomCasier , &   ! Nom du casier lu sur la ligne
                               chaine , &   ! chaine de caracteres a analyser
                              MOT_CLE , &   ! tableau des mot cle a trouver
                              icasier , &   ! compteur du profil
                               Erreur )     ! Variables de controle d'erreur

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
   !   SOUS-PROGRAMME(S) APPELANT(S) : - LEC_GEOM_CASIER
   !   -----------------------------  
   !
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
   character(*)               , intent(  out) :: NomCasier
   character(80)              , intent(in   ) :: chaine
   character(*), dimension (:), intent(in   ) :: MOT_CLE
   integer                    , intent(in   ) :: icasier
   type(ERREUR_T)             , intent(inout) :: Erreur

   ! scalaires locaux
   integer       :: rang
   integer       :: ifor             ! compteur sur les formats
   integer       :: len_mot_cle      ! longueur du mot cle
   !character(132):: !arbredappel_old
   character(80) :: chaine_2         ! chaine sans le mot cle
   integer       :: retour           ! code de retour d'erreur des fonc. d' e/s

   end subroutine DECODER_GEOM_CASIER

   end interface

end module M_DECODER_GEOM_CASIER_I
