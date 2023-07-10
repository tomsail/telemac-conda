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

! *********************************************************************
! PROGICIEL : MASCARET       B.MARCHAND (marchand@deltacad.fr)
!
! VERSION : V8P4R0              EDF-CEREMA
! *********************************************************************
   ! .................................................................................................................................
   ! Permet de definir les tailles maximum des valeurs d'une variable
   ! RQ : Depend de l'instance du modele ou de l'etat
   ! .................................................................................................................................
   subroutine SET_TAILLE_VAR_MASCARET(Erreur, Identifiant, NomVar, index1, taille1, taille2, taille3)
     use M_APIMASCARET_STATIC
     use M_MASCARET_T
     implicit none
     integer,                intent(out):: Erreur                         ! different de 0 si erreur
     integer,                intent(in) :: Identifiant                    ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
     character(len= 40),     intent(in) :: NomVar                         ! Nom de la variable du modele ou de l'etat
     integer,                intent(in) :: index1                         ! valeur du 1er indice utilise pour Profils, Lois, Singularites, Deversoirs, Extremites, Casiers et Confluents
     integer,                intent(in):: taille1                        ! valeur max du 1er indice
     integer,                intent(in):: taille2                        ! valeur max du 2e  indice
     integer,                intent(in):: taille3                        ! valeur max du 3e  indice

     ! Variables locales
     character(LEN=256) MessageErreur
     character(LEN=40)  NomVarTrim

     Erreur = TEST_INIT_AND_ID(Identifiant, 'SET_TAILLE_VAR_MASCARET')
     if (Erreur > 0 ) then
        RETURN
     end if


     NomVarTrim = TRIM(NomVar)

     Erreur = SET_TAILLE_VAR_MASC(ptrTabMascaret(Identifiant), NomVarTrim, index1, taille1, taille2, taille3, MessageErreur)

     if (Erreur > 0) then
       ptrMsgsErreurs(Identifiant) = MessageErreur
     end if


   end subroutine SET_TAILLE_VAR_MASCARET
