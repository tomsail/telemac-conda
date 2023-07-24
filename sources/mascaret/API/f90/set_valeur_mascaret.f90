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
   ! Mutateurs permettant de modifier une valeur d'une variable d'une instance du modele ou de l'etat
   ! .................................................................................................................................
   subroutine SET_DOUBLE_MASCARET(Erreur, Identifiant, NomVar, index1, index2, index3, valeur)
     use M_APIMASCARET_STATIC
     use M_MASCARET_T

      implicit none
      integer,                intent(out):: Erreur                     ! different de 0 si erreur
      integer,                intent(in) :: Identifiant                ! Id mascaret
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(8),                intent(in) :: valeur                     ! valeur du real(8) de l'instance pour les indexes specifies

     ! Variables locales
     character(LEN=256) MessageErreur
     character(LEN=40)  NomVarTrim
     integer ind1,taille1,taille2,taille3

     ind1 = index1

     Erreur = TEST_INIT_AND_ID(Identifiant, 'SET_DOUBLE_MASCARET')
     if (Erreur > 0 ) then
        RETURN
     end if

     CALL GET_TAILLE_VAR_MASCARET(Erreur, Identifiant, NomVar, ind1, taille1, taille2, taille3)
     if(Erreur.ne.0) then
        return
     else
        if(((index1.lt.0).or.(index1.eq.0.and.taille1.ne.0)).or.((index2.lt.0).or.(index2.eq.0.and.taille2.ne.0)).or. &
           ((index3.lt.0).or.(index3.eq.0.and.taille3.ne.0)).or.(index1.gt.taille1).or. &
           (index2.gt.taille2).or.(index3.gt.taille3)) then
              ptrMsgsErreurs(Identifiant) = 'SET_DOUBLE_MASCARET - invalid index'
              Erreur = 1
              return
        endif
     endif
     
     NomVarTrim = TRIM(NomVar)

     ! On conserve l'indication que la geometrie a ete modifiee
     ! dans ce cas on pourra lancer le planimetrage avant le calcul.
     if (INDEX(NomVarTrim,'Model.CrossSection.') > 0) then
        geometrieModifiee(Identifiant) = .TRUE.
     end if

     Erreur = SET_DOUBLE_MASC(ptrTabMascaret(Identifiant), NomVarTrim, index1, index2, index3, valeur, MessageErreur)

     if (Erreur > 0) then
       ptrMsgsErreurs(Identifiant) = MessageErreur
     end if
   end subroutine SET_DOUBLE_MASCARET

   subroutine SET_INT_MASCARET(Erreur, Identifiant, NomVar, index1, index2, index3, valeur)
     use M_APIMASCARET_STATIC
     use M_MASCARET_T
      implicit none
      integer,                intent(out):: Erreur                     ! different de 0 si erreur
      integer,                intent(in) :: Identifiant                ! Id mascaret
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(in) :: valeur                     ! valeur de l'entier de l'instance pour les indexes specifies

     ! Variables locales
     character(LEN=256) MessageErreur
     character(LEN=40)  NomVarTrim
     integer ind1,taille1,taille2,taille3

     ind1 = 0

     Erreur = TEST_INIT_AND_ID(Identifiant, 'SET_INT_MASCARET')
     if (Erreur > 0 ) then
        RETURN
     end if

     CALL GET_TAILLE_VAR_MASCARET(Erreur, Identifiant, NomVar, ind1, taille1, taille2, taille3)
     if(Erreur.ne.0) then
        return
     else
        if(((index1.lt.0).or.(index1.eq.0.and.taille1.ne.0)).or.((index2.lt.0).or.(index2.eq.0.and.taille2.ne.0)).or. &
           ((index3.lt.0).or.(index3.eq.0.and.taille3.ne.0)).or.(index1.gt.taille1).or. &
           (index2.gt.taille2).or.(index3.gt.taille3)) then
              ptrMsgsErreurs(Identifiant) = 'SET_INT_MASCARET - invalid index'
              Erreur = 1
              return
        endif
     endif

     NomVarTrim = TRIM(NomVar)

     Erreur = SET_INT_MASC(ptrTabMascaret(Identifiant), NomVarTrim, index1, index2, index3, valeur, MessageErreur)

     if (Erreur > 0) then
       ptrMsgsErreurs(Identifiant) = MessageErreur
     end if
   end subroutine SET_INT_MASCARET

   subroutine SET_BOOL_MASCARET(Erreur, Identifiant, NomVar, index1, index2, index3, valeur)
     use M_APIMASCARET_STATIC
     use M_MASCARET_T
      implicit none
      integer,                intent(out):: Erreur                     ! different de 0 si erreur
      integer,                intent(in) :: Identifiant                ! Id mascaret
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      logical,                intent(in) :: valeur                     ! valeur du boolean de l'instance pour les indexes specifies

     ! Variables locales
     character(LEN=256) MessageErreur
     character(LEN=40)  NomVarTrim
     integer ind1,taille1,taille2,taille3

     ind1 = 0
     
     Erreur = TEST_INIT_AND_ID(Identifiant, 'SET_BOOL_MASCARET')
     if (Erreur > 0 ) then
        RETURN
     end if

     CALL GET_TAILLE_VAR_MASCARET(Erreur, Identifiant, NomVar, ind1, taille1, taille2, taille3)
     if(Erreur.ne.0) then
        return
     else
        if(((index1.lt.0).or.(index1.eq.0.and.taille1.ne.0)).or.((index2.lt.0).or.(index2.eq.0.and.taille2.ne.0)).or. &
           ((index3.lt.0).or.(index3.eq.0.and.taille3.ne.0)).or.(index1.gt.taille1).or. &
           (index2.gt.taille2).or.(index3.gt.taille3)) then
              ptrMsgsErreurs(Identifiant) = 'SET_BOOL_MASCARET - invalid index'
              Erreur = 1
              return
        endif
     endif

     NomVarTrim = TRIM(NomVar)

     Erreur = SET_BOOL_MASC(ptrTabMascaret(Identifiant), NomVarTrim, index1, index2, index3, valeur, MessageErreur)

     if (Erreur > 0) then
       ptrMsgsErreurs(Identifiant) = MessageErreur
     end if

   end subroutine SET_BOOL_MASCARET

   subroutine SET_STRING_MASCARET(Erreur, Identifiant, NomVar, index1, index2, index3, valeur)
     use M_APIMASCARET_STATIC
     use M_MASCARET_T
      implicit none
      integer,                intent(out):: Erreur                     ! different de 0 si erreur
      integer,                intent(in) :: Identifiant                ! Id mascaret
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      character(LEN=256),     intent(in) :: valeur                     ! valeur de la chaine de caractere de l'instance pour les indexes specifies

     ! Variables locales
     character(LEN=256) MessageErreur
     character(LEN=40)  NomVarTrim
     integer ind1,taille1,taille2,taille3

     ind1 = 0

     Erreur = TEST_INIT_AND_ID(Identifiant, 'SET_STRING_MASCARET')
     if (Erreur > 0 ) then
        RETURN
     end if

     CALL GET_TAILLE_VAR_MASCARET(Erreur, Identifiant, NomVar, ind1, taille1, taille2, taille3)
     if(Erreur.ne.0) then
        return
     else
        if(((index1.lt.0).or.(index1.eq.0.and.taille1.ne.0)).or.((index2.lt.0).or.(index2.eq.0.and.taille2.ne.0)).or. &
           ((index3.lt.0).or.(index3.eq.0.and.taille3.ne.0)).or.(index1.gt.taille1).or. &
           (index2.gt.taille2).or.(index3.gt.taille3)) then
              ptrMsgsErreurs(Identifiant) = 'SET_STRING_MASCARET - invalid index'
              Erreur = 1
              return
        endif
     endif

     NomVarTrim = TRIM(NomVar)

     Erreur = SET_STRING_MASC(ptrTabMascaret(Identifiant), NomVarTrim, index1, index2, index3, valeur, MessageErreur)

     if (Erreur > 0) then
       ptrMsgsErreurs(Identifiant) = MessageErreur
     end if

   end subroutine SET_STRING_MASCARET
