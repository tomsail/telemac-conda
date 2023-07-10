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
! PROGICIEL : MASCARET       J.-M. LACOMBE
!
! VERSION : V8P4R0              EDF-CEREMA
! *********************************************************************
   ! .................................................................................................................................
   ! Permet d'acceder a la taille maximum des indexes pour acceder a une variable
   ! RQ : Depend de l'instance du modele ou de l'etat
   ! .................................................................................................................................
   subroutine GET_TAILLE_VAR_MASCARET(Erreur, Identifiant, NomVar, index1, taille1, taille2, taille3)
     use M_APIMASCARET_STATIC
     use M_MASCARET_T
     implicit none
     integer,                intent(out):: Erreur                         ! different de 0 si erreur
     integer,                intent(in) :: Identifiant                    ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
     character(len= 40),     intent(in) :: NomVar                         ! Nom de la variable du modele ou de l'etat
     integer,                intent(in) :: index1                         ! valeur du 1er indice utilise pour Profils, Lois, Singularites, Deversoirs, Extremites, Casiers et Confluents
     integer,                intent(out):: taille1                        ! valeur max du 1er indice
     integer,                intent(out):: taille2                        ! valeur max du 2e  indice
     integer,                intent(out):: taille3                        ! valeur max du 3e  indice

     ! Variables locales
     character(LEN=256) MessageErreur
     character(LEN=40)  NomVarTrim
     integer t1, t2, t3

     Erreur = 0
     taille1 = -1
     taille2 = -1
     taille3 = -1

     Erreur = TEST_INIT_AND_ID(Identifiant, 'GET_TAILLE_VAR_MASCARET')
     if (Erreur > 0 ) then
        RETURN
     end if


     NomVarTrim = TRIM(NomVar)

     Erreur = GET_TAILLE_VAR_MASC(ptrTabMascaret(Identifiant), NomVarTrim, index1, t1, t2, t3, MessageErreur)
     taille1 = t1
     taille2 = t2
     taille3 = t3

     if (Erreur > 0) then
       ptrMsgsErreurs(Identifiant) = MessageErreur
     end if


   end subroutine GET_TAILLE_VAR_MASCARET
