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

module M_PARAMETRES_QUALITE_EAU_T
!***********************************************************************
! PROGICIEL : TRACER         S.MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   !=========================== Declarations ==============================
   use M_PRECISION
   use M_CALCS_O2_VAR_I
   use M_CALCS_BIOMASS_VAR_I
   use M_CALCS_EUTRO_VAR_I
   use M_CALCS_MICROPOL_VAR_I
   use M_CALCS_THERMIC_VAR_I

   type PARAMETRES_QUALITE_EAU_T

      sequence

      real(DOUBLE), dimension(:), pointer   :: ParQual_Eau => null()
   end TYPE PARAMETRES_QUALITE_EAU_T

contains
   ! Retourne les noms des champs du type ainsi qu'une description
   subroutine GET_TAB_VAR_PARAMETRES_QUALITE_EAU(i, tabNomVar, tabDescriptionVar)
      integer , intent(inout)                                  :: i                 ! indiceTableaux
      character(len= 40), dimension(*)                :: tabNomVar         ! Tableau des noms de variable du modele ou de l'etat
      character(len=110), dimension(*)                :: tabDescriptionVar ! Tableau des description de variable du modele ou de l'etat

        tabNomVar(i)         ="Model.Tracer.ParPhy.ParQual_Eau"
        tabDescriptionVar(i) ="Physical parameters of the water quality module"
        i=i+1
        ! Variables schema O2
        call GET_TAB_VAR_CALCS_O2(i, tabNomVar, tabDescriptionVar)
        ! Variables schema BIOMASS
        call GET_TAB_VAR_CALCS_BIOMASS(i, tabNomVar, tabDescriptionVar)
        ! Variables schema EUTRO
        call GET_TAB_VAR_CALCS_EUTRO(i, tabNomVar, tabDescriptionVar)
        ! Variables schema MICROPOL
        call GET_TAB_VAR_CALCS_MICROPOL(i, tabNomVar, tabDescriptionVar)
        ! Variables schema THERMIC
        call GET_TAB_VAR_CALCS_THERMIC(i, tabNomVar, tabDescriptionVar)

        return

     end subroutine GET_TAB_VAR_PARAMETRES_QUALITE_EAU

	! Retourne une description du champ du type au niveau de static (independant de l'instance du modele ou de l'etat)
    function GET_TYPE_VAR_PARAMETRES_QUALITE_EAU(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      implicit none

      integer                          :: GET_TYPE_VAR_PARAMETRES_QUALITE_EAU ! different de 0 si erreur
      character(LEN=40), intent(in)    :: NomVar                   ! Nom de la variable (notation pointe)
      character(LEN=10), intent(out)   :: TypeVar                  ! "INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
      character(LEN=10), intent(out)   :: Categorie                ! "MODEL" ou "STATE"
      logical          , intent(out)   :: Modifiable               ! Si vrai alors on peut utiliser une fonction SET_XXXX_TRACER sur la variable
      integer          , intent(out)   :: dimVar                   ! dimension (c'est a dire le nombre d'indexe de 0 a 3)
      character(LEN=256), intent(out)  :: MessageErreur            ! Message d'erreur

      GET_TYPE_VAR_PARAMETRES_QUALITE_EAU = 0
      TypeVar               = ""
      Categorie             = "MODEL"
      Modifiable            = .TRUE.
      dimVar                = 0
      MessageErreur         = ""

      if ( index(NomVar, 'Model.Tracer.ParPhy.ParQual_Eau') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if (index(NomVar,'Model.Tracer.ParPhy.O2.') > 0) then
         GET_TYPE_VAR_PARAMETRES_QUALITE_EAU = &
            & GET_TYPE_VAR_CALCS_O2(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      else if (index(NomVar,'Model.Tracer.ParPhy.BIOMASS.') > 0) then
         GET_TYPE_VAR_PARAMETRES_QUALITE_EAU = &
            & GET_TYPE_VAR_CALCS_BIOMASS(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      else if (index(NomVar,'Model.Tracer.ParPhy.EUTRO.') > 0) then
         GET_TYPE_VAR_PARAMETRES_QUALITE_EAU = &
            & GET_TYPE_VAR_CALCS_EUTRO(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      else if (index(NomVar,'Model.Tracer.ParPhy.MICROPOL.') > 0) then
         GET_TYPE_VAR_PARAMETRES_QUALITE_EAU = &
            & GET_TYPE_VAR_CALCS_MICROPOL(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      else if (index(NomVar,'Model.Tracer.ParPhy.THERMIC.') > 0) then
         GET_TYPE_VAR_PARAMETRES_QUALITE_EAU = &
            & GET_TYPE_VAR_CALCS_THERMIC(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      else
         GET_TYPE_VAR_PARAMETRES_QUALITE_EAU = 1
         TypeVar = "?"
         Categorie             = "MODEL"
         Modifiable            = .false.
         dimVar                = -1
         MessageErreur         = "GET_TYPE_VAR_PARAMETRES_QUALITE_EAU - Unknown variable name"
      end if

    end function GET_TYPE_VAR_PARAMETRES_QUALITE_EAU

! .................................................................................................................................
! Permet d'acceder a la taille des valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_TAILLE_VAR_PARAMETRES_QUALITE_EAU(Instance, NomVar, taille1, taille2, taille3, MessageErreur)
      implicit none
      integer                            :: GET_TAILLE_VAR_PARAMETRES_QUALITE_EAU     ! different de 0 si erreur
      type(PARAMETRES_QUALITE_EAU_T),    intent(in) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in) :: NomVar                         ! Nom de la variable du source
      integer,                intent(out):: taille1                        ! valeur max du 1er indice
      integer,                intent(out):: taille2                        ! valeur max du 2e  indice
      integer,                intent(out):: taille3                        ! valeur max du 3e  indice
      character(LEN=256),     intent(out):: MessageErreur                  ! Message d'erreur

      GET_TAILLE_VAR_PARAMETRES_QUALITE_EAU = 0
      taille1                = 0
      taille2                = 0
      taille3                = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Tracer.ParPhy.ParQual_Eau') > 0) then
         taille1 = size(Instance%ParQual_Eau)
         taille2 = 0
         taille3 = 0
      else if (index(NomVar,'Model.Tracer.ParPhy.O2.') > 0) then
         GET_TAILLE_VAR_PARAMETRES_QUALITE_EAU = GET_TAILLE_VAR_CALCS_O2(Instance%ParQual_Eau, NomVar, taille1, MessageErreur)
         taille2 = 0
         taille3 = 0
      else if (index(NomVar,'Model.Tracer.ParPhy.BIOMASS.') > 0) then
         GET_TAILLE_VAR_PARAMETRES_QUALITE_EAU = GET_TAILLE_VAR_CALCS_BIOMASS(Instance%ParQual_Eau, NomVar, taille1, MessageErreur)
         taille2 = 0
         taille3 = 0
      else if (index(NomVar,'Model.Tracer.ParPhy.EUTRO.') > 0) then
         GET_TAILLE_VAR_PARAMETRES_QUALITE_EAU = GET_TAILLE_VAR_CALCS_EUTRO(Instance%ParQual_Eau, NomVar, taille1, MessageErreur)
         taille2 = 0
         taille3 = 0
      else if (index(NomVar,'Model.Tracer.ParPhy.MICROPOL.') > 0) then
         GET_TAILLE_VAR_PARAMETRES_QUALITE_EAU = GET_TAILLE_VAR_CALCS_MICROPOL(Instance%ParQual_Eau, NomVar, taille1, MessageErreur)
         taille2 = 0
         taille3 = 0
      else if (index(NomVar,'Model.Tracer.ParPhy.THERMIC.') > 0) then
         GET_TAILLE_VAR_PARAMETRES_QUALITE_EAU = GET_TAILLE_VAR_CALCS_THERMIC(Instance%ParQual_Eau, NomVar, taille1, MessageErreur)
         taille2 = 0
         taille3 = 0
      else
         GET_TAILLE_VAR_PARAMETRES_QUALITE_EAU = 1
         taille1                = -1
         taille2                = -1
         taille3                = -1
         MessageErreur         = "GET_TAILLE_VAR_PARAMETRES_QUALITE_EAU - Unknown variable name"
      end if
   end function GET_TAILLE_VAR_PARAMETRES_QUALITE_EAU

! .................................................................................................................................
! Permet de modifier la taille les variables de type pointeurs fortran
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_TAILLE_VAR_PARAMETRES_QUALITE_EAU(Instance, NomVar, NewT1, NewT2, NewT3, MessageErreur)
      implicit none
      integer                               :: SET_TAILLE_VAR_PARAMETRES_QUALITE_EAU     ! different de 0 si erreur
      type(PARAMETRES_QUALITE_EAU_T),    intent(inout) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in)    :: NomVar                         ! Nom de la variable du modele
      integer,                intent(in)    :: NewT1                          ! Nouvelle valeur max du 1er indice
      integer,                intent(in)    :: NewT2                          ! Nouvelle valeur max du 2e  indice
      integer,                intent(in)    :: NewT3                          ! Nouvelle valeur max du 3e  indice
      character(LEN=256),     intent(out)   :: MessageErreur                  ! Message d'erreur

      integer t1, err

      SET_TAILLE_VAR_PARAMETRES_QUALITE_EAU = 0
      MessageErreur          = ""
      !------------------------------------------------------------------------------
      ! Fin des appels aux fonctions SET_TAILLE_VAR_XXXX des membres de type primitif
      !------------------------------------------------------------------------------
      if ( index(NomVar, 'Model.Tracer.ParPhy.ParQual_Eau') > 0) then
        if (ASSOCIATED(Instance%ParQual_Eau)) then
           t1 = size(Instance%ParQual_Eau)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%ParQual_Eau, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_PARAMETRES_QUALITE_EAU = err
                 MessageErreur = 'SET_TAILLE_VAR_PARAMETRES_QUALITE_EAU : Unable to deallocate ' &
                                 //'MODEL_PARAMETRES_QUALITE_EAU_T.ParQual_Eau'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%ParQual_Eau) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%ParQual_Eau(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_PARAMETRES_QUALITE_EAU = err
              MessageErreur = 'SET_TAILLE_VAR_PARAMETRES_QUALITE_EAU : Unable to allocate ' &
                              //'MODEL_PARAMETRES_QUALITE_EAU_T.ParQual_Eau'
              return
           endif
        endif
      !--------------------------------------------------------------------
      ! Fin de la modification de la taille des pointers de types primitifs
      !--------------------------------------------------------------------
      else
         SET_TAILLE_VAR_PARAMETRES_QUALITE_EAU = 1
         MessageErreur         = "SET_TAILLE_VAR_PARAMETRES_QUALITE_EAU - Unknown variable name"
      end if

    end function SET_TAILLE_VAR_PARAMETRES_QUALITE_EAU

! .................................................................................................................................
! Accesseurs permettant d'acceder aux valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_DOUBLE_PARAMETRES_QUALITE_EAU(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_DOUBLE_PARAMETRES_QUALITE_EAU     ! different de 0 si erreur
      type(PARAMETRES_QUALITE_EAU_T),    intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(out):: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_DOUBLE_PARAMETRES_QUALITE_EAU = 0
      valeur                = -9999999.9999
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Tracer.ParPhy.ParQual_Eau') > 0) then
         valeur = Instance%ParQual_Eau(index1)
      else if (index(NomVar,'Model.Tracer.ParPhy.O2.') > 0) then
         GET_DOUBLE_PARAMETRES_QUALITE_EAU = GET_DOUBLE_CALCS_O2(Instance%ParQual_Eau, NomVar, index1, valeur, MessageErreur)
      else if (index(NomVar,'Model.Tracer.ParPhy.BIOMASS.') > 0) then
         GET_DOUBLE_PARAMETRES_QUALITE_EAU = GET_DOUBLE_CALCS_BIOMASS(Instance%ParQual_Eau, NomVar, index1, valeur, MessageErreur)
      else if (index(NomVar,'Model.Tracer.ParPhy.EUTRO.') > 0) then
         GET_DOUBLE_PARAMETRES_QUALITE_EAU = GET_DOUBLE_CALCS_EUTRO(Instance%ParQual_Eau, NomVar, index1, valeur, MessageErreur)
      else if (index(NomVar,'Model.Tracer.ParPhy.MICROPOL.') > 0) then
         GET_DOUBLE_PARAMETRES_QUALITE_EAU = GET_DOUBLE_CALCS_MICROPOL(Instance%ParQual_Eau, NomVar, index1, valeur, MessageErreur)
      else if (index(NomVar,'Model.Tracer.ParPhy.THERMIC.') > 0) then
         GET_DOUBLE_PARAMETRES_QUALITE_EAU = GET_DOUBLE_CALCS_THERMIC(Instance%ParQual_Eau, NomVar, index1, valeur, MessageErreur)
      else
         GET_DOUBLE_PARAMETRES_QUALITE_EAU = 1
         valeur                = -9999999.9999
         MessageErreur         = "GET_DOUBLE_PARAMETRES_QUALITE_EAU - Unknown variable name"
      end if
   end function GET_DOUBLE_PARAMETRES_QUALITE_EAU

   function GET_INT_PARAMETRES_QUALITE_EAU(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_INT_PARAMETRES_QUALITE_EAU     ! different de 0 si erreur
      type(PARAMETRES_QUALITE_EAU_T),    intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(out):: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_INT_PARAMETRES_QUALITE_EAU = 0
      valeur                = -9999999
      MessageErreur          = ""

      if ( index(NomVar,'Model.Tracer.ParPhy.O2.') > 0) then
         GET_INT_PARAMETRES_QUALITE_EAU = GET_INT_CALCS_O2(Instance%ParQual_Eau, NomVar, index1, valeur, MessageErreur)
      else if ( index(NomVar,'Model.Tracer.ParPhy.EUTRO.') > 0) then
         GET_INT_PARAMETRES_QUALITE_EAU = GET_INT_CALCS_EUTRO(Instance%ParQual_Eau, NomVar, index1, valeur, MessageErreur)
      else
         GET_INT_PARAMETRES_QUALITE_EAU = 1
         valeur                = -9999999
         MessageErreur         = "GET_DOUBLE_PARAMETRES_QUALITE_EAU - Unknown variable name"
      end if
   end function GET_INT_PARAMETRES_QUALITE_EAU

! .................................................................................................................................
! Mutateurs permettant de modifier les differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_DOUBLE_PARAMETRES_QUALITE_EAU(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_DOUBLE_PARAMETRES_QUALITE_EAU     ! different de 0 si erreur
      type(PARAMETRES_QUALITE_EAU_T),    intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(in) :: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_DOUBLE_PARAMETRES_QUALITE_EAU = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Tracer.ParPhy.ParQual_Eau') > 0) then
         Instance%ParQual_Eau(index1) = valeur
      else if ( index(NomVar,'Model.Tracer.ParPhy.O2.') > 0) then
         SET_DOUBLE_PARAMETRES_QUALITE_EAU = SET_DOUBLE_CALCS_O2(Instance%ParQual_Eau, NomVar, index1, valeur, MessageErreur)
      else if ( index(NomVar,'Model.Tracer.ParPhy.BIOMASS.') > 0) then
         SET_DOUBLE_PARAMETRES_QUALITE_EAU = SET_DOUBLE_CALCS_BIOMASS(Instance%ParQual_Eau, NomVar, index1, valeur, MessageErreur)
      else if ( index(NomVar,'Model.Tracer.ParPhy.EUTRO.') > 0) then
         SET_DOUBLE_PARAMETRES_QUALITE_EAU = SET_DOUBLE_CALCS_EUTRO(Instance%ParQual_Eau, NomVar, index1, valeur, MessageErreur)
      else if ( index(NomVar,'Model.Tracer.ParPhy.MICROPOL.') > 0) then
         SET_DOUBLE_PARAMETRES_QUALITE_EAU = SET_DOUBLE_CALCS_MICROPOL(Instance%ParQual_Eau, NomVar, index1, valeur, MessageErreur)
      else if ( index(NomVar,'Model.Tracer.ParPhy.THERMIC.') > 0) then
         SET_DOUBLE_PARAMETRES_QUALITE_EAU = SET_DOUBLE_CALCS_THERMIC(Instance%ParQual_Eau, NomVar, index1, valeur, MessageErreur)
      else
         SET_DOUBLE_PARAMETRES_QUALITE_EAU = 1
         MessageErreur         = "SET_DOUBLE_PARAMETRES_QUALITE_EAU - Unknown variable name"
      end if
   end function SET_DOUBLE_PARAMETRES_QUALITE_EAU

   function SET_INT_PARAMETRES_QUALITE_EAU(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_INT_PARAMETRES_QUALITE_EAU     ! different de 0 si erreur
      type(PARAMETRES_QUALITE_EAU_T),    intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(in) :: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_INT_PARAMETRES_QUALITE_EAU = 0
      MessageErreur          = ""

      if ( index(NomVar,'Model.Tracer.ParPhy.O2.') > 0) then
         SET_INT_PARAMETRES_QUALITE_EAU = SET_INT_CALCS_O2(Instance%ParQual_Eau, NomVar, index1, valeur, MessageErreur)
      else if ( index(NomVar,'Model.Tracer.ParPhy.EUTRO.') > 0) then
         SET_INT_PARAMETRES_QUALITE_EAU = SET_INT_CALCS_EUTRO(Instance%ParQual_Eau, NomVar, index1, valeur, MessageErreur)
      else
         SET_INT_PARAMETRES_QUALITE_EAU = 1
         MessageErreur         = "SET_INT_PARAMETRES_QUALITE_EAU - Unknown variable name"
      end if
   end function SET_INT_PARAMETRES_QUALITE_EAU

! .................................................................................................................................
! Desalloue tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function DESALLOUE_PARAMETRES_QUALITE_EAU(Instance, MessageErreur)
      implicit none
      integer                            :: DESALLOUE_PARAMETRES_QUALITE_EAU      ! different de 0 si erreur
      type(PARAMETRES_QUALITE_EAU_T),    intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      integer                            :: taille
      integer                            :: err
      DESALLOUE_PARAMETRES_QUALITE_EAU = 0
      MessageErreur          = ""

      !----------------------------------------------
      ! Desallocation des pointers de types primitifs
      !----------------------------------------------
      if (ASSOCIATED(Instance%ParQual_Eau)) then
          taille = SIZE(Instance%ParQual_Eau)
          if (taille > 0) then
              DEALLOCATE(Instance%ParQual_Eau, STAT=err)
              if (err /= 0) then
                  DESALLOUE_PARAMETRES_QUALITE_EAU = err
                  MessageErreur = 'Unable to deallocate MODEL_PARAMETRES_QUALITE_EAU_T.ParQual_Eau'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%ParQual_Eau)
      !--------------------------------------------------------
      ! Fin de la desallocation des pointers de types primitifs
      !--------------------------------------------------------

   end function DESALLOUE_PARAMETRES_QUALITE_EAU

! .................................................................................................................................
! Rend null tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function NULLIFIER_PARAMETRES_QUALITE_EAU(Instance, MessageErreur)
      implicit none
      integer                            :: NULLIFIER_PARAMETRES_QUALITE_EAU      ! different de 0 si erreur
      type(PARAMETRES_QUALITE_EAU_T),    intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      NULLIFIER_PARAMETRES_QUALITE_EAU = 0
      MessageErreur          = ""

      !----------------------------------------------
      ! Nullifie des pointers de types primitifs
      !----------------------------------------------
      NULLIFY(Instance%ParQual_Eau)
      !--------------------------------------------------------
      ! Fin de la Nullification des pointers de types primitifs
      !--------------------------------------------------------

   end function NULLIFIER_PARAMETRES_QUALITE_EAU

end module M_PARAMETRES_QUALITE_EAU_T
