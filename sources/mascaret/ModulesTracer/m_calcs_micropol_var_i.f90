!== Copyright (C) 2000-2022 EDF-CEREMA ==
!
!   This file is part of MASCARET-TRACER.
!
!   MASCARET-TRACER is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   MASCARET-TRACER is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with MASCARET-TRACER.  If not, see <http://www.gnu.org/licenses/>
!

module M_CALCS_MICROPOL_VAR_I
!***********************************************************************
! PROGICIEL : TRACER         S.MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

       subroutine GET_TAB_VAR_CALCS_MICROPOL(i, tabNomVar, tabDescriptionVar)
         integer , intent(inout)                                  :: i                 ! indiceTableaux
         character(len= 40), dimension(*)                :: tabNomVar         ! Tableau des noms de variable du modele ou de l'etat
         character(len=110), dimension(*)                :: tabDescriptionVar ! Tableau des description de variable du modele ou de l'etat

      end subroutine GET_TAB_VAR_CALCS_MICROPOL

      function GET_TYPE_VAR_CALCS_MICROPOL(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
         implicit none

         integer                          :: GET_TYPE_VAR_CALCS_MICROPOL ! different de 0 si erreur
         character(LEN=40), intent(in)    :: NomVar                   ! Nom de la variable (notation pointe)
         character(LEN=10), intent(out)   :: TypeVar                  ! "INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
         character(LEN=10), intent(out)   :: Categorie                ! "MODEL" ou "STATE"
         logical          , intent(out)   :: Modifiable               ! Si vrai alors on peut utiliser une fonction SET_XXXX_TRACER sur la variable
         integer          , intent(out)   :: dimVar                   ! dimension (c'est a dire le nombre d'indexe de 0 a 3)
         character(LEN=256), intent(out)  :: MessageErreur            ! Message d'erreur

      end function GET_TYPE_VAR_CALCS_MICROPOL

      function GET_TAILLE_VAR_CALCS_MICROPOL(ParQual_Eau, NomVar, taille1, MessageErreur)
         implicit none

         integer                          :: GET_TAILLE_VAR_CALCS_MICROPOL ! different de 0 si erreur
         real(8), dimension(*), intent(in):: ParQual_Eau
         character(LEN=40), intent(in)    :: NomVar                   ! Nom de la variable (notation pointe)
         integer          , intent(out)   :: taille1                   ! dimension (c'est a dire le nombre d'indexe de 0 a 3)
         character(LEN=256), intent(out)  :: MessageErreur            ! Message d'erreur

      end function GET_TAILLE_VAR_CALCS_MICROPOL

      function GET_DOUBLE_CALCS_MICROPOL(ParQual_Eau, NomVar, index1, valeur, MessageErreur)

         use M_PRECISION

         implicit none
         integer                            :: GET_DOUBLE_CALCS_MICROPOL    ! different de 0 si erreur
         real(DOUBLE), dimension(*), intent(in) :: ParQual_Eau                   ! Instance du type derive dont on souhaite recuperer la valeur
         character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
         integer,                intent(in) :: index1                     ! valeur du 1er indice
         real(DOUBLE),           intent(out):: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
         character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      end function GET_DOUBLE_CALCS_MICROPOL

      function SET_DOUBLE_CALCS_MICROPOL(ParQual_Eau, NomVar, index1, valeur, MessageErreur)

         use M_PRECISION

         implicit none
         integer                            :: SET_DOUBLE_CALCS_MICROPOL    ! different de 0 si erreur
         real(DOUBLE), dimension(*), intent(inout) :: ParQual_Eau                   ! Instance du type derive dont on souhaite recuperer la valeur
         character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
         integer,                intent(in) :: index1                     ! valeur du 1er indice
         real(DOUBLE),           intent(in) :: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
         character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      end function SET_DOUBLE_CALCS_MICROPOL

   end interface

end module M_CALCS_MICROPOL_VAR_I
