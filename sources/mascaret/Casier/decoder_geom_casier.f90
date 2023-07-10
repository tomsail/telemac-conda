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

subroutine DECODER_GEOM_CASIER ( &
                                 presence_mot_cle , &   ! flag de presence du mot cle
                                 NomCasier        , &   ! Nom du casier lu sur la ligne
                                 chaine           , &   ! chaine de caracteres a analyser
                                 MOT_CLE          , &   ! tableau des mot cle a trouver
                                 icasier          , &   ! compteur du profil
                                 Erreur )               ! Variables de controle d'erreur

! .....................................................................
!  PROGICIEL : MASCARET          C. RISSOAN
!
!    VERSION : V8P4R0                     EDF-CEREMA
! .....................................................................
!
!
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
   logical                     , intent(  out) :: presence_mot_cle
   character(*)                , intent(  out) :: NomCasier
   character(80)               , intent(in   ) :: chaine
   character(*), dimension (:) , intent(in   ) :: MOT_CLE
   integer                     , intent(in   ) :: icasier
   type(ERREUR_T)              , intent(inout) :: Erreur

   ! scalaires locaux
   integer       :: rang
   integer       :: ifor         ! compteur sur les formats
   integer       :: len_mot_cle  ! longueur du mot cle
   character(80) :: chaine_2     ! chaine sans le mot cle
   integer       :: retour       ! code de retour d'erreur des fonc. d' e/s
   !character(132):: !arbredappel_old

   ! ================================== instructions ==================================

   ! Initialisation
   len_mot_cle = len( MOT_CLE(1) )
   Erreur%Numero = 0
   !arbredappel_old = trim(Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>DECODER_GEOM_CASIER'

   ! On regarde si le mot-cle MOT_CLE est present sur la ligne
   ! (sous ses trois formes d'ecriture :
   !  majuscule, minuscule ou 1ere lettre en majuscule
   rang = 0
   ifor = 1

   do while( rang == 0 .and. ifor <= size( MOT_CLE ) )
      rang = index( chaine , MOT_CLE(ifor) )
      ifor = ifor + 1
   end do

   !-----------------------------------------------
   ! Si le mot-cle MOT_CLE est absent sur la ligne
   !-----------------------------------------------
   if( rang == 0 ) then
      presence_mot_cle = .false.
      NomCasier  = ''
      !Erreur%arbredappel = !arbredappel_old
      return

      !-----------------------------------------------
      ! Si le mot-cle MOT_CLE est present sur la ligne
      !-----------------------------------------------
   else
      presence_mot_cle = .true.

      ! on lit apres le mot-cle MOT_CLE
      ! les parametres derriere

      chaine_2 = trim( chaine( rang + LEN_MOT_CLE: ) )
      read( chaine_2 , * , iostat = RETOUR ) NomCasier
      if( RETOUR /= 0 ) then
         Erreur%Numero = 82
         Erreur%ft     = err_82
         Erreur%ft_c   = err_82c
         call TRAITER_ERREUR( Erreur , icasier )
         return
      end if
   end if

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine DECODER_GEOM_CASIER
