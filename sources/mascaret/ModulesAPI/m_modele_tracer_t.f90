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
module M_MODELE_TRACER_T

!=========================== Declarations ==============================

   use M_PRECISION

   use M_FICHIER_T
   use M_PARAMETRES_QUALITE_EAU_T   ! Donnees physiques du modele de QE
   use M_METEO_T                    ! Donnees meteo
   use M_CONSTANTES_TRACER_T        ! Constantes liees au traceur (convection/diffusion)
   use M_SOURCE_TRACER_T
   use M_COND_LIM_TRACER_T
   use M_LOI_TRACER_T
   use M_INDEX_VARIABLE_TRACER_C    ! Index des variables : definit la constante NB_TOT_VARTR
   use M_NODE_TRACER_T              ! Connectivite tracer
   use M_CONSTANTES_CALCUL_TRACER_C

TYPE MODELE_TRACER_T

    sequence

    integer                                          :: Nbtrac !
    integer                                          :: FreqCouplage !
    real(DOUBLE)                                     :: DT_trac  ! pas de temps Tracer
    integer                                          :: Modele_Qual_Eau !
    type(FICHIER_T)                                  :: FichierListingTracer !
    type(FICHIER_T)                                  :: FichierResuTracer    !
    type(FICHIER_T)                                  :: FichierConcInit      !
    logical                                          :: Presence_ConcIni
    integer                                          :: FormatResuTracer     !
    logical, dimension(NB_TOT_VARTR)                 :: VarStoTracer         !
    logical                                          :: ImpressionConcListing  !
    logical                                          :: ImpressionConcIni  !
    logical                                          :: ImpressionBilanTracer  !
    type (PARAMETRES_QUALITE_EAU_T)                  :: ParPhy !
    type (METEO_T)                                   :: Meteo  !
    type (NODE_TRACER_T)                             :: NodeTrac !
    type (CONSTANTES_TRACER_T),dimension(:), pointer :: ConsTrac => null() !
    type (SOURCE_TRACER_T)    ,dimension(:), pointer :: Sources_Tracer => null() !
    type (COND_LIM_TRACER_T)  ,dimension(:), pointer :: CondLimTrac => null() !
    type (LOI_TRACER_T)       ,dimension(:), pointer :: LoiTracer => null() !

END TYPE MODELE_TRACER_T

contains
    ! Retourne les noms des champs du type ainsi qu'une description
    subroutine GET_TAB_VAR_MODELE_TRACER(i, tabNomVar, tabDescriptionVar)
      integer , intent(inout)                                  :: i                 ! indiceTableaux
      character(len= 40), dimension(*)                :: tabNomVar         ! Tableau des noms de variable du modele ou de l'etat
      character(len=110), dimension(*)                :: tabDescriptionVar ! Tableau des description de variable du modele ou de l'etat

        tabNomVar(i)         ="Model.Tracer.Number"
        tabDescriptionVar(i) ="Number of constituants"
        i=i+1
        tabNomVar(i)         ="Model.Tracer.CouplingFreq"
        tabDescriptionVar(i) ="Frequency of coupling between hydro and tracer"
        i=i+1
        tabNomVar(i)         ="Model.Tracer.DT"
        tabDescriptionVar(i) ="Time step (s) for the Tracer module (convection/diffusion equations)"
        i=i+1
        tabNomVar(i)         ="Model.Tracer.ID"
        tabDescriptionVar(i) ="ID number of the water quality module"
        i=i+1
        tabNomVar(i)         ="Model.Tracer.WaterQualMod"
        tabDescriptionVar(i) ="Name of the water quality model"
        i=i+1

        ! --- FICHIER_T FichierListingTracer ---
        call GET_TAB_VAR_FICHIER(i, 'Tracer.Listing',tabNomVar, tabDescriptionVar)

        ! --- FICHIER_T FichierResuTracer ---
        call GET_TAB_VAR_FICHIER(i, 'Tracer.Result',tabNomVar, tabDescriptionVar)

        ! --- FICHIER_T FichierResuTracer ---
        call GET_TAB_VAR_FICHIER(i, 'Tracer.IniConc',tabNomVar, tabDescriptionVar)

        tabNomVar(i)         ="Model.Tracer.IniConcFromFile"
        tabDescriptionVar(i) ="Presence of initial concentrations for Tracer"
        i=i+1
        tabNomVar(i)         ="Model.Tracer.TracerResultFormat"
        tabDescriptionVar(i) ="Format of the output result file for Tracer"
        i=i+1
        tabNomVar(i)         ="Model.Tracer.Var2Save"
        tabDescriptionVar(i) ="Variables to save as results"
        i=i+1
        tabNomVar(i)         ="Model.Tracer.printListing"
        tabDescriptionVar(i) ="Print the variable in the listing file of Tracer"
        i=i+1
        tabNomVar(i)         ="Model.Tracer.printIniConc"
        tabDescriptionVar(i) ="Print the initial concentration in the listing file of Tracer"
        i=i+1
        tabNomVar(i)         ="Model.Tracer.globalResults"
        tabDescriptionVar(i) ="Print the global results"
        i=i+1

        ! --- PARAMETRES_QUALITE_EAU_T ---
        call GET_TAB_VAR_PARAMETRES_QUALITE_EAU(i, tabNomVar, tabDescriptionVar)

        ! --- METEO_T ---
        call GET_TAB_VAR_METEO(i, tabNomVar, tabDescriptionVar)

        ! --- NODE_TRACER_T ---
        call GET_TAB_VAR_NODE_TRACER(i, tabNomVar, tabDescriptionVar)

        ! --- CONSTANTES_TRACER_T ---
        call GET_TAB_VAR_CONSTANTES_TRACER(i, tabNomVar, tabDescriptionVar)

        ! --- SOURCE_TRACER_T ---
        call GET_TAB_VAR_SOURCE_TRACER(i, tabNomVar, tabDescriptionVar)

        ! --- COND_LIM_TRACER_T ---
        call GET_TAB_VAR_COND_LIM_TRACER(i, tabNomVar, tabDescriptionVar)

        ! --- LOI_TRACER_T ---
        call GET_TAB_VAR_LOI_TRACER(i, tabNomVar, tabDescriptionVar)

        return

    end subroutine GET_TAB_VAR_MODELE_TRACER

	! Retourne une description du champ du type au niveau de static (independant de l'instance du modele ou de l'etat)
    function GET_TYPE_VAR_MODELE_TRACER(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      implicit none

      integer                          :: GET_TYPE_VAR_MODELE_TRACER ! different de 0 si erreur
      character(LEN=40), intent(in)    :: NomVar                   ! Nom de la variable (notation pointe)
      character(LEN=10), intent(out)   :: TypeVar                  ! "INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
      character(LEN=10), intent(out)   :: Categorie                ! "MODEL" ou "STATE"
      logical          , intent(out)   :: Modifiable               ! Si vrai alors on peut utiliser une fonction SET_XXXX_TRACER sur la variable
      integer          , intent(out)   :: dimVar                   ! dimension (c'est a dire le nombre d'indexe de 0 a 3)
      character(LEN=256), intent(out)  :: MessageErreur            ! Message d'erreur

      GET_TYPE_VAR_MODELE_TRACER = 0
      TypeVar               = ""
      Categorie             = "MODEL"
      Modifiable            = .TRUE.
      dimVar                = 0
      MessageErreur         = ""

      if ( index(NomVar, 'Model.Tracer.Number') > 0) then
         TypeVar = 'INT'
         dimVar                = 0
      else if ( index(NomVar, 'Model.Tracer.CouplingFreq') > 0) then
         TypeVar = 'INT'
         dimVar                = 0
      else if ( index(NomVar, 'Model.Tracer.DT') > 0) then
         TypeVar = 'DOUBLE'
         dimVar                = 0
      else if ( index(NomVar, 'Model.Tracer.ID') > 0) then
         TypeVar = 'INT'
         dimVar                = 0
      else if ( index(NomVar, 'Model.Tracer.WaterQualMod') > 0) then
         TypeVar = 'STRING'
         dimVar                = 0
      else if ( index(NomVar,'Model.File.Tracer.Listing.') > 0) then
         GET_TYPE_VAR_MODELE_TRACER = GET_TYPE_VAR_FICHIER(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      else if ( index(NomVar,'Model.File.Tracer.Result.') > 0) then
         GET_TYPE_VAR_MODELE_TRACER = GET_TYPE_VAR_FICHIER(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      else if ( index(NomVar, 'Model.Tracer.IniConcFromFile') > 0) then
         TypeVar = 'BOOL'
         dimVar                = 0
      else if ( index(NomVar,'Model.File.Tracer.IniConc.') > 0) then
         GET_TYPE_VAR_MODELE_TRACER = GET_TYPE_VAR_FICHIER(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      else if ( index(NomVar, 'Model.Tracer.TracerResultFormat') > 0) then
         TypeVar = 'INT'
         dimVar                = 0
      else if ( index(NomVar, 'Model.Tracer.Var2Save') > 0) then
         TypeVar = 'TABBOOL'
         dimVar                = 1
      else if ( index(NomVar, 'Model.Tracer.printListing') > 0) then
         TypeVar = 'BOOL'
         dimVar                = 0
      else if ( index(NomVar, 'Model.Tracer.printIniConc') > 0) then
         TypeVar = 'BOOL'
         dimVar                = 0
      else if ( index(NomVar, 'Model.Tracer.globalResults') > 0) then
         TypeVar = 'BOOL'
         dimVar                = 0
      else if ( index(NomVar,'Model.Tracer.ParPhy.') > 0) then
         GET_TYPE_VAR_MODELE_TRACER = GET_TYPE_VAR_PARAMETRES_QUALITE_EAU(NomVar, TypeVar, &
            & Categorie, Modifiable, dimVar, MessageErreur)
      else if ( index(NomVar,'Model.Tracer.Weather.') > 0) then
         GET_TYPE_VAR_MODELE_TRACER = GET_TYPE_VAR_METEO(NomVar, TypeVar, &
            & Categorie, Modifiable, dimVar, MessageErreur)
      else if ( index(NomVar,'Model.Tracer.NodeTrac.') > 0) then
         GET_TYPE_VAR_MODELE_TRACER = GET_TYPE_VAR_NODE_TRACER(NomVar, TypeVar, &
            & Categorie, Modifiable, dimVar, MessageErreur)
      else if ( index(NomVar,'Model.Tracer.Constant.') > 0) then
         GET_TYPE_VAR_MODELE_TRACER = GET_TYPE_VAR_CONSTANTES_TRACER(NomVar, TypeVar, &
            & Categorie, Modifiable, dimVar, MessageErreur)
         dimVar = dimVar + 1
      else if ( index(NomVar,'Model.Tracer.Sources.') > 0) then
         GET_TYPE_VAR_MODELE_TRACER = GET_TYPE_VAR_SOURCE_TRACER(NomVar, TypeVar, &
            & Categorie, Modifiable, dimVar, MessageErreur)
         dimVar = dimVar + 1
      else if ( index(NomVar,'Model.Tracer.BoundaryCond.') > 0) then
         GET_TYPE_VAR_MODELE_TRACER = GET_TYPE_VAR_COND_LIM_TRACER(NomVar, TypeVar, &
            & Categorie, Modifiable, dimVar, MessageErreur)
         dimVar = dimVar + 1
      else if ( index(NomVar,'Model.Tracer.Graph.') > 0) then
         GET_TYPE_VAR_MODELE_TRACER = GET_TYPE_VAR_LOI_TRACER(NomVar, TypeVar, &
            & Categorie, Modifiable, dimVar, MessageErreur)
         dimVar = dimVar + 1
      else
         GET_TYPE_VAR_MODELE_TRACER = 1
         TypeVar = "?"
         Categorie             = "MODEL"
         Modifiable            = .false.
         dimVar                = -1
         MessageErreur         = "GET_TYPE_VAR_MODELE_TRACER - Unknown variable name"
      end if


    end function GET_TYPE_VAR_MODELE_TRACER

! .................................................................................................................................
! Permet d'acceder a la taille des valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_TAILLE_VAR_MODELE_TRACER(Instance, NomVar, index1, taille1, taille2, taille3, MessageErreur)
      implicit none
      integer                            :: GET_TAILLE_VAR_MODELE_TRACER     ! different de 0 si erreur
      type(MODELE_TRACER_T),    intent(in) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in) :: NomVar                         ! Nom de la variable du modele
      integer,                intent(in) :: index1                         ! valeur du 1er indice
      integer,                intent(out):: taille1                        ! valeur max du 1er indice
      integer,                intent(out):: taille2                        ! valeur max du 2e  indice
      integer,                intent(out):: taille3                        ! valeur max du 3e  indice
      character(LEN=256),     intent(out):: MessageErreur                  ! Message d'erreur
      integer                            :: bidon1                         ! variable locale non utilise

      GET_TAILLE_VAR_MODELE_TRACER = 0
      taille1                = 0
      taille2                = 0
      taille3                = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Tracer.Number') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Tracer.CouplingFreq') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Tracer.DT') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Tracer.ID') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Tracer.WaterQualMod') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if (INDEX(NomVar,'Model.File.Tracer.Listing.') > 0) then
         GET_TAILLE_VAR_MODELE_TRACER = GET_TAILLE_VAR_FICHIER(Instance%FichierListingTracer,&
                                               NomVar, taille1, taille2, taille3, MessageErreur)
      else if (INDEX(NomVar,'Model.File.Tracer.Result.') > 0) then
         GET_TAILLE_VAR_MODELE_TRACER = GET_TAILLE_VAR_FICHIER(Instance%FichierResuTracer,&
                                               NomVar, taille1, taille2, taille3, MessageErreur)
      else if ( index(NomVar, 'Model.Tracer.IniConcFromFile') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if (INDEX(NomVar,'Model.File.Tracer.IniConc.') > 0) then
         GET_TAILLE_VAR_MODELE_TRACER = GET_TAILLE_VAR_FICHIER(Instance%FichierConcInit,&
                                               NomVar, taille1, taille2, taille3, MessageErreur)
      else if ( index(NomVar, 'Model.Tracer.TracerResultFormat') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Tracer.Var2Save') > 0) then
         taille1 = size(Instance%VarStoTracer)
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Tracer.printListing') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Tracer.printIniConc') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Tracer.globalResults') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if (INDEX(NomVar,'Model.Tracer.ParPhy.') > 0) then
         GET_TAILLE_VAR_MODELE_TRACER = GET_TAILLE_VAR_PARAMETRES_QUALITE_EAU(Instance%ParPhy,&
                                               NomVar, taille1, taille2, taille3, MessageErreur)
      else if (INDEX(NomVar,'Model.Tracer.Weather.') > 0) then
         GET_TAILLE_VAR_MODELE_TRACER = GET_TAILLE_VAR_METEO(Instance%Meteo,&
                                               NomVar, taille1, taille2, taille3, MessageErreur)
      else if (INDEX(NomVar,'Model.Tracer.NodeTrac.') > 0) then
         GET_TAILLE_VAR_MODELE_TRACER = GET_TAILLE_VAR_NODE_TRACER(Instance%NodeTrac,&
                                               NomVar, taille1, taille2, taille3, MessageErreur)
      else if (INDEX(NomVar,'Model.Tracer.Constant.') > 0) then
         if (ASSOCIATED(Instance%ConsTrac)) then
            taille1 = size(Instance%ConsTrac)
         else
            taille1 = 0
         endif
         if (taille1 > 0) then
             if ((index1 > 1).AND.(index1 <= taille1)) then
                 GET_TAILLE_VAR_MODELE_TRACER = GET_TAILLE_VAR_CONSTANTES_TRACER(Instance%ConsTrac(index1),&
                                                   NomVar, taille2, taille3, bidon1, MessageErreur)
             else
                 GET_TAILLE_VAR_MODELE_TRACER = GET_TAILLE_VAR_CONSTANTES_TRACER(Instance%ConsTrac(1),&
                                                   NomVar, taille2, taille3, bidon1, MessageErreur)
             endif
         else
             taille2 = 0
             taille3 = 0
         end if
      else if (INDEX(NomVar,'Model.Tracer.Sources.') > 0) then
         if (ASSOCIATED(Instance%Sources_Tracer)) then
            taille1 = size(Instance%Sources_Tracer)
         else
            taille1 = 0
         endif
         if (taille1 > 0) then
             if ((index1 > 1).AND.(index1 <= taille1)) then
                 GET_TAILLE_VAR_MODELE_TRACER = GET_TAILLE_VAR_SOURCE_TRACER(Instance%Sources_Tracer(index1),&
                                                   NomVar, taille2, taille3, bidon1, MessageErreur)
             else
                 GET_TAILLE_VAR_MODELE_TRACER = GET_TAILLE_VAR_SOURCE_TRACER(Instance%Sources_Tracer(1),&
                                                   NomVar, taille2, taille3, bidon1, MessageErreur)
             endif
         else
             taille2 = 0
             taille3 = 0
         end if
      else if (INDEX(NomVar,'Model.Tracer.BoundaryCond.') > 0) then
         if (ASSOCIATED(Instance%CondLimTrac)) then
            taille1 = size(Instance%CondLimTrac)
         else
            taille1 = 0
         endif
         if (taille1 > 0) then
             if ((index1 > 1).AND.(index1 <= taille1)) then
                 GET_TAILLE_VAR_MODELE_TRACER = GET_TAILLE_VAR_COND_LIM_TRACER(Instance%CondLimTrac(index1),&
                                                   NomVar, taille2, taille3, bidon1, MessageErreur)
             else
                 GET_TAILLE_VAR_MODELE_TRACER = GET_TAILLE_VAR_COND_LIM_TRACER(Instance%CondLimTrac(1),&
                                                   NomVar, taille2, taille3, bidon1, MessageErreur)
             endif
         else
             taille2 = 0
             taille3 = 0
         end if
      else if (INDEX(NomVar,'Model.Tracer.Graph.') > 0) then
         if (ASSOCIATED(Instance%LoiTracer)) then
            taille1 = size(Instance%LoiTracer)
         else
            taille1 = 0
         endif
         if (taille1 > 0) then
             if ((index1 > 1).AND.(index1 <= taille1)) then
                 GET_TAILLE_VAR_MODELE_TRACER = GET_TAILLE_VAR_LOI_TRACER(Instance%LoiTracer(index1),&
                                                   NomVar, taille2, taille3, bidon1, MessageErreur)
             else
                 GET_TAILLE_VAR_MODELE_TRACER = GET_TAILLE_VAR_LOI_TRACER(Instance%LoiTracer(1),&
                                                   NomVar, taille2, taille3, bidon1, MessageErreur)
             endif
         else
             taille2 = 0
             taille3 = 0
         end if
      else
         GET_TAILLE_VAR_MODELE_TRACER = 1
         taille1                = -1
         taille2                = -1
         taille3                = -1
         MessageErreur         = "GET_TAILLE_VAR_MODELE_TRACER - Unknown variable name"
      end if
   end function GET_TAILLE_VAR_MODELE_TRACER

! .................................................................................................................................
! Permet de modifier la taille les variables de type pointeurs fortran
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_TAILLE_VAR_MODELE_TRACER(Instance, NomVar, NewT1, NewT2, NewT3, MessageErreur)
      implicit none
      integer                               :: SET_TAILLE_VAR_MODELE_TRACER     ! different de 0 si erreur
      type(MODELE_TRACER_T),    intent(inout) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in)    :: NomVar                         ! Nom de la variable du modele
      integer,                intent(in)    :: NewT1                          ! Nouvelle valeur max du 1er indice
      integer,                intent(in)    :: NewT2                          ! Nouvelle valeur max du 2e  indice
      integer,                intent(in)    :: NewT3                          ! Nouvelle valeur max du 3e  indice
      character(LEN=256),     intent(out)   :: MessageErreur                  ! Message d'erreur

      integer t1, err
      integer i, bidon
      character(LEN=256)                 :: MessageErreurType

      SET_TAILLE_VAR_MODELE_TRACER = 0
      MessageErreur          = ""

      !-----------------------------------------------------------------------
      ! Appels aux fonctions SET_TAILLE_VAR_XXXX des membres de type derive
      !-----------------------------------------------------------------------
      if (index(NomVar,'Model.Tracer.ParPhy.') > 0) then
         err = SET_TAILLE_VAR_PARAMETRES_QUALITE_EAU(Instance%ParPhy, &
                                   NomVar, NewT1, NewT2, NewT3, MessageErreurType)
         if (err /= 0) then
            SET_TAILLE_VAR_MODELE_TRACER = err
            MessageErreur = 'Unable to change the size of Instance%ParPhy'
            return
         endif
      else if (index(NomVar,'Model.Tracer.Weather.') > 0) then
         err = SET_TAILLE_VAR_METEO(Instance%Meteo, &
                                   NomVar, NewT1, NewT2, NewT3, MessageErreurType)
         if (err /= 0) then
            SET_TAILLE_VAR_MODELE_TRACER = err
            MessageErreur = 'Unable to change the size of Instance%Meteo'
            return
         endif
      else if (index(NomVar,'Model.Tracer.NodeTrac.') > 0) then
         err = SET_TAILLE_VAR_NODE_TRACER(Instance%NodeTrac, &
                                   NomVar, NewT1, NewT2, NewT3, MessageErreurType)
         if (err /= 0) then
            SET_TAILLE_VAR_MODELE_TRACER = err
            MessageErreur = 'Unable to change the size of Instance%NodeTrac'
            return
         endif
      else if (INDEX(NomVar,'Model.Tracer.Constant.') > 0) then
         if (ASSOCIATED(Instance%ConsTrac)) then
            t1 = SIZE(Instance%ConsTrac)
            if (t1 /= NewT1) then
               DO i=1, t1
                  err = DESALLOUE_CONSTANTES_TRACER(Instance%ConsTrac(i), MessageErreurType)
                  if (err /= 0) then
                     SET_TAILLE_VAR_MODELE_TRACER = err
                     MessageErreur = 'Unable to deallocate MODEL_TRACER_T.CONSTANT(i)'
                     return
                  endif
               enddo
               DEALLOCATE(Instance%ConsTrac, STAT=err)
               if (err /= 0) then
                  SET_TAILLE_VAR_MODELE_TRACER = err
                  MessageErreur = 'Unable to deallocate MODEL_TRACER_T.CONSTANT'
                  return
               endif
               ALLOCATE(Instance%ConsTrac(NewT1), STAT=err)
               DO i=1, NewT1
                  err = NULLIFIER_CONSTANTES_TRACER(Instance%ConsTrac(i), MessageErreurType)
                  if (err /= 0) then
                     SET_TAILLE_VAR_MODELE_TRACER = err
                     MessageErreur = 'Unable to nullify the pointers Instance%ConsTrac'
                     return
                  endif
               enddo
            endif
         else  ! Instance%ConsTrac pas 'associated'
            ALLOCATE(Instance%ConsTrac(NewT1), STAT=err)
            DO i=1, NewT1
              err = NULLIFIER_CONSTANTES_TRACER(Instance%ConsTrac(i), MessageErreurType)
              if (err /= 0) then
                 SET_TAILLE_VAR_MODELE_TRACER = err
                  MessageErreur = 'Unable to nullify the pointers Instance%ConsTrac'
                  return
              endif
            enddo
         endif
         DO i=1, NewT1
            err = SET_TAILLE_VAR_CONSTANTES_TRACER(Instance%ConsTrac(i), &
                                 NomVar, NewT2, NewT3, Bidon, MessageErreurType)
            if (err /= 0) then
               SET_TAILLE_VAR_MODELE_TRACER = err
                MessageErreur = 'Unable to change the size of Instance%ConsTrac'
                return
            endif
         enddo
      else if (INDEX(NomVar,'Model.Tracer.Constant') > 0) then
         if (ASSOCIATED(Instance%ConsTrac)) then
            t1 = SIZE(Instance%ConsTrac)
            if (t1 /= NewT1) then
               DEALLOCATE(Instance%ConsTrac, STAT=err)
               if (err /= 0) then
                  SET_TAILLE_VAR_MODELE_TRACER = err
                  MessageErreur = 'Unable to deallocate MODEL_TRACER_T.CONSTANT'
                  return
               endif
               ALLOCATE(Instance%ConsTrac(NewT1), STAT=err)
            endif
         else  ! Instance%ConsTrac pas 'associated'
            ALLOCATE(Instance%ConsTrac(NewT1), STAT=err)
         endif
      else if (INDEX(NomVar,'Model.Tracer.Sources.') > 0) then
         if (ASSOCIATED(Instance%Sources_Tracer)) then
            t1 = SIZE(Instance%Sources_Tracer)
            if (t1 /= NewT1) then
               DO i=1, t1
                  err = DESALLOUE_SOURCE_TRACER(Instance%Sources_Tracer(i), MessageErreurType)
                  if (err /= 0) then
                     SET_TAILLE_VAR_MODELE_TRACER = err
                     MessageErreur = 'Unable to deallocate MODEL_TRACER_T.SOURCES_TRACER(i)'
                     return
                  endif
               enddo
               DEALLOCATE(Instance%Sources_Tracer, STAT=err)
               if (err /= 0) then
                  SET_TAILLE_VAR_MODELE_TRACER = err
                  MessageErreur = 'Unable to deallocate MODEL_TRACER_T.SOURCES_TRACER'
                  return
               endif
               ALLOCATE(Instance%Sources_Tracer(NewT1), STAT=err)
               DO i=1, NewT1
                  err = NULLIFIER_SOURCE_TRACER(Instance%Sources_Tracer(i), MessageErreurType)
                  if (err /= 0) then
                     SET_TAILLE_VAR_MODELE_TRACER = err
                     MessageErreur = 'Unable to nullify the pointers Instance%Sources_Tracer'
                     return
                  endif
               enddo
            endif
         else  ! Instance%Sources_Tracer pas 'associated'
            ALLOCATE(Instance%Sources_Tracer(NewT1), STAT=err)
            DO i=1, NewT1
              err = NULLIFIER_SOURCE_TRACER(Instance%Sources_Tracer(i), MessageErreurType)
              if (err /= 0) then
                 SET_TAILLE_VAR_MODELE_TRACER = err
                  MessageErreur = 'Unable to nullify the pointers Instance%Sources_Tracer'
                  return
              endif
            enddo
         endif
         DO i=1, NewT1
            err = SET_TAILLE_VAR_SOURCE_TRACER(Instance%Sources_Tracer(i), &
                                 NomVar, NewT2, NewT3, Bidon, MessageErreurType)
            if (err /= 0) then
               SET_TAILLE_VAR_MODELE_TRACER = err
                MessageErreur = 'Unable to change the size of Instance%Sources_Tracer'
                return
            endif
         enddo
      else if (INDEX(NomVar,'Model.Tracer.BoundaryCond.') > 0) then
         if (ASSOCIATED(Instance%CondLimTrac)) then
            t1 = SIZE(Instance%CondLimTrac)
            if (t1 /= NewT1) then
               DO i=1, t1
                  err = DESALLOUE_COND_LIM_TRACER(Instance%CondLimTrac(i), MessageErreurType)
                  if (err /= 0) then
                     SET_TAILLE_VAR_MODELE_TRACER = err
                     MessageErreur = 'Unable to deallocate MODEL_TRACER_T.CONDLIMTRAC(i)'
                     return
                  endif
               enddo
               DEALLOCATE(Instance%CondLimTrac, STAT=err)
               if (err /= 0) then
                  SET_TAILLE_VAR_MODELE_TRACER = err
                  MessageErreur = 'Unable to deallocate MODEL_TRACER_T.CONDLIMTRAC'
                  return
               endif
               ALLOCATE(Instance%CondLimTrac(NewT1), STAT=err)
               DO i=1, NewT1
                  err = NULLIFIER_COND_LIM_TRACER(Instance%CondLimTrac(i), MessageErreurType)
                  if (err /= 0) then
                     SET_TAILLE_VAR_MODELE_TRACER = err
                     MessageErreur = 'Unable to nullify the pointers Instance%CondLimTrac'
                     return
                  endif
               enddo
            endif
         else  ! Instance%CondLimTrac pas 'associated'
            ALLOCATE(Instance%CondLimTrac(NewT1), STAT=err)
            DO i=1, NewT1
              err = NULLIFIER_COND_LIM_TRACER(Instance%CondLimTrac(i), MessageErreurType)
              if (err /= 0) then
                 SET_TAILLE_VAR_MODELE_TRACER = err
                  MessageErreur = 'Unable to nullify the pointers Instance%CondLimTrac'
                  return
              endif
            enddo
         endif
         DO i=1, NewT1
            err = SET_TAILLE_VAR_COND_LIM_TRACER(Instance%CondLimTrac(i), &
                                 NomVar, NewT2, NewT3, Bidon, MessageErreurType)
            if (err /= 0) then
               SET_TAILLE_VAR_MODELE_TRACER = err
                MessageErreur = 'Unable to change the size of Instance%CondLimTrac'
                return
            endif
         enddo
      else if (INDEX(NomVar,'Model.Tracer.Graph.') > 0) then
         if (ASSOCIATED(Instance%LoiTracer)) then
            t1 = SIZE(Instance%LoiTracer)
            if (t1 /= NewT1) then
               DO i=1, t1
                  err = DESALLOUE_LOI_TRACER(Instance%LoiTracer(i), MessageErreurType)
                  if (err /= 0) then
                     SET_TAILLE_VAR_MODELE_TRACER = err
                     MessageErreur = 'Unable to deallocate MODEL_TRACER_T.LOITRACER(i)'
                     return
                  endif
               enddo
               DEALLOCATE(Instance%LoiTracer, STAT=err)
               if (err /= 0) then
                  SET_TAILLE_VAR_MODELE_TRACER = err
                  MessageErreur = 'Unable to deallocate MODEL_TRACER_T.LOITRACER'
                  return
               endif
               ALLOCATE(Instance%LoiTracer(NewT1), STAT=err)
               DO i=1, NewT1
                  err = NULLIFIER_LOI_TRACER(Instance%LoiTracer(i), MessageErreurType)
                  if (err /= 0) then
                     SET_TAILLE_VAR_MODELE_TRACER = err
                     MessageErreur = 'Unable to nullify the pointers Instance%LoiTracer'
                     return
                  endif
               enddo
            endif
         else  ! Instance%LoiTracer pas 'associated'
            ALLOCATE(Instance%LoiTracer(NewT1), STAT=err)
            DO i=1, NewT1
              err = NULLIFIER_LOI_TRACER(Instance%LoiTracer(i), MessageErreurType)
              if (err /= 0) then
                 SET_TAILLE_VAR_MODELE_TRACER = err
                  MessageErreur = 'Unable to nullify the pointers Instance%LoiTracer'
                  return
              endif
            enddo
         endif
         DO i=1, NewT1
            err = SET_TAILLE_VAR_LOI_TRACER(Instance%LoiTracer(i), &
                                 NomVar, NewT2, NewT3, Bidon, MessageErreurType)
            if (err /= 0) then
               SET_TAILLE_VAR_MODELE_TRACER = err
                MessageErreur = 'Unable to change the size of Instance%LoiTracer'
                return
            endif
         enddo
      !--------------------------------------------------------------------------------
      ! Fin des appels aux fonctions SET_TAILLE_VAR_XXXX des membres de type derive
      !--------------------------------------------------------------------------------
      else
         SET_TAILLE_VAR_MODELE_TRACER = 1
         MessageErreur         = "SET_TAILLE_VAR_MODELE_TRACER - Unknown variable name"
      end if

    end function SET_TAILLE_VAR_MODELE_TRACER

! .................................................................................................................................
! Accesseurs permettant d'acceder aux valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_DOUBLE_MODELE_TRACER(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_DOUBLE_MODELE_TRACER     ! different de 0 si erreur
      type(MODELE_TRACER_T),    intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(out):: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur
      integer                            :: bidon1                     ! variable locale non utilise

      GET_DOUBLE_MODELE_TRACER = 0
      valeur                = -9999999.9999
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Tracer.DT') > 0) then
         valeur = Instance%DT_trac
      else if (INDEX(NomVar,'Model.Tracer.ParPhy.') > 0) then
           GET_DOUBLE_MODELE_TRACER = GET_DOUBLE_PARAMETRES_QUALITE_EAU(instance%ParPhy, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Tracer.Weather.') > 0) then
           GET_DOUBLE_MODELE_TRACER = GET_DOUBLE_METEO(instance%Meteo, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Tracer.Constant.') > 0) then
           GET_DOUBLE_MODELE_TRACER = GET_DOUBLE_CONSTANTES_TRACER(instance%ConsTrac(index1), NomVar, index2,&
                index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Tracer.Sources.') > 0) then
           GET_DOUBLE_MODELE_TRACER = GET_DOUBLE_SOURCE_TRACER(instance%Sources_Tracer(index1), NomVar, index2,&
                index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Tracer.BoundaryCond.') > 0) then
           GET_DOUBLE_MODELE_TRACER = GET_DOUBLE_COND_LIM_TRACER(instance%CondLimTrac(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Tracer.Graph.') > 0) then
           GET_DOUBLE_MODELE_TRACER = GET_DOUBLE_LOI_TRACER(instance%LoiTracer(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else
         GET_DOUBLE_MODELE_TRACER = 1
         valeur                = -9999999.9999
         MessageErreur         = "GET_DOUBLE_MODELE_TRACER - Unknown variable name"
      end if
   end function GET_DOUBLE_MODELE_TRACER

   function GET_INT_MODELE_TRACER(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_INT_MODELE_TRACER            ! different de 0 si erreur
      type(MODELE_TRACER_T),        intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(out):: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur
      integer                            :: bidon1                     ! variable locale non utilise

      GET_INT_MODELE_TRACER = 0
      valeur                = -9999
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Tracer.Number') > 0) then
         valeur = Instance%Nbtrac
      else if ( index(NomVar, 'Model.Tracer.CouplingFreq') > 0) then
         valeur = Instance%FreqCouplage
      else if ( index(NomVar, 'Model.Tracer.ID') > 0) then
         valeur = Instance%Modele_Qual_Eau
      else if ( index(NomVar, 'Model.Tracer.TracerResultFormat') > 0) then
         valeur = Instance%FormatResuTracer
      else if (INDEX(NomVar,'Model.File.Tracer.Listing.') > 0) then
           GET_INT_MODELE_TRACER = GET_INT_FICHIER(instance%FichierListingTracer, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.Tracer.Result.') > 0) then
           GET_INT_MODELE_TRACER = GET_INT_FICHIER(instance%FichierResuTracer, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.Tracer.IniConc.') > 0) then
           GET_INT_MODELE_TRACER = GET_INT_FICHIER(instance%FichierConcInit, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Tracer.NodeTrac.') > 0) then
           GET_INT_MODELE_TRACER = GET_INT_NODE_TRACER(instance%NodeTrac, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Tracer.Constant.') > 0) then
           GET_INT_MODELE_TRACER = GET_INT_CONSTANTES_TRACER(instance%ConsTrac(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Tracer.Sources.') > 0) then
           GET_INT_MODELE_TRACER = GET_INT_SOURCE_TRACER(instance%Sources_Tracer(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (index(NomVar,'Model.Tracer.BoundaryCond.') > 0) then
           GET_INT_MODELE_TRACER = GET_INT_COND_LIM_TRACER(instance%CondLimTrac(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Tracer.ParPhy.') > 0) then
           GET_INT_MODELE_TRACER = GET_INT_PARAMETRES_QUALITE_EAU(instance%ParPhy, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else
         GET_INT_MODELE_TRACER = 1
         valeur                = -9999
         MessageErreur         = "GET_INT_MODELE_TRACER - Unknown variable name"
      end if
   end function GET_INT_MODELE_TRACER

   function GET_BOOL_MODELE_TRACER(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_BOOL_MODELE_TRACER       ! different de 0 si erreur
      type(MODELE_TRACER_T),    intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      logical,                intent(out):: valeur                     ! valeur du logical de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur
      integer                            :: bidon1                     ! variable locale non utilise

      GET_BOOL_MODELE_TRACER = 0
      valeur                = .FALSE.
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Tracer.Var2Save') > 0) then
         valeur = Instance%VarStoTracer(index1)
      else if ( index(NomVar, 'Model.Tracer.printListing') > 0) then
         valeur = Instance%ImpressionConcListing
      else if ( index(NomVar, 'Model.Tracer.printIniConc') > 0) then
         valeur = Instance%ImpressionConcIni
      else if ( index(NomVar, 'Model.Tracer.globalResults') > 0) then
         valeur = Instance%ImpressionBilanTracer
      else if ( index(NomVar, 'Model.Tracer.IniConcFromFile') > 0) then
         valeur = Instance%Presence_ConcIni
      else if (INDEX(NomVar,'Model.Tracer.Constant.') > 0) then
           GET_BOOL_MODELE_TRACER = GET_BOOL_CONSTANTES_TRACER(instance%ConsTrac(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Tracer.Sources.') > 0) then
           GET_BOOL_MODELE_TRACER = GET_BOOL_SOURCE_TRACER(instance%Sources_Tracer(index1), NomVar, index2,&
                index3, bidon1, valeur, MessageErreur)
      else
         GET_BOOL_MODELE_TRACER = 1
         valeur                = .FALSE.
         MessageErreur         = "GET_BOOL_MODELE_TRACER - Unknown variable name"
      end if
   end function GET_BOOL_MODELE_TRACER

   function GET_STRING_MODELE_TRACER(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_STRING_MODELE_TRACER ! different de 0 si erreur
      type(MODELE_TRACER_T),intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      character(LEN=256),     intent(out):: valeur                     ! valeur du character(LEN=256) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur
      integer                            :: bidon1                         ! variable locale non utilise

      GET_STRING_MODELE_TRACER = 0
      valeur                = ""
      MessageErreur          = ""

      if (index(NomVar, 'Model.Tracer.WaterQualMod') > 0) then
         valeur = TRIM(NOM_MODELE_QE(Instance%Modele_Qual_Eau))
      else if (INDEX(NomVar,'Model.File.Tracer.Listing.') > 0) then
           GET_STRING_MODELE_TRACER = GET_STRING_FICHIER(instance%FichierListingTracer, NomVar, index1,&
                index2, index3, valeur, MessageErreur)
        else if (INDEX(NomVar,'Model.File.Tracer.Result.') > 0) then
           GET_STRING_MODELE_TRACER = GET_STRING_FICHIER(instance%FichierResuTracer, NomVar, index1,&
                index2, index3, valeur, MessageErreur)
        else if (INDEX(NomVar,'Model.File.Tracer.IniConc.') > 0) then
           GET_STRING_MODELE_TRACER = GET_STRING_FICHIER(instance%FichierConcInit, NomVar, index1,&
                index2, index3, valeur, MessageErreur)
        else if (INDEX(NomVar,'Model.Tracer.Weather.') > 0) then
           GET_STRING_MODELE_TRACER = GET_STRING_METEO(instance%Meteo, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
        else if (INDEX(NomVar,'Model.Tracer.Graph.') > 0) then
           GET_STRING_MODELE_TRACER = GET_STRING_LOI_TRACER(instance%LoiTracer(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
        else if (INDEX(NomVar,'Model.Tracer.Sources.') > 0) then
           GET_STRING_MODELE_TRACER = GET_STRING_SOURCE_TRACER(instance%Sources_Tracer(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
        else
         GET_STRING_MODELE_TRACER = 1
         valeur                = ""
         MessageErreur         = "GET_STRING_MODELE_TRACER - Unknown variable name"
      end if
   end function GET_STRING_MODELE_TRACER


! .................................................................................................................................
! Mutateurs permettant de modifier les differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_DOUBLE_MODELE_TRACER(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_DOUBLE_MODELE_TRACER     ! different de 0 si erreur
      type(MODELE_TRACER_T),    intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(in) :: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur
      integer                            :: bidon1                     ! variable locale non utilise

      SET_DOUBLE_MODELE_TRACER = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Tracer.DT') > 0) then
         Instance%DT_trac = valeur
      else if (INDEX(NomVar,'Model.Tracer.ParPhy.') > 0) then
           SET_DOUBLE_MODELE_TRACER = SET_DOUBLE_PARAMETRES_QUALITE_EAU(instance%ParPhy, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Tracer.Weather.') > 0) then
           SET_DOUBLE_MODELE_TRACER = SET_DOUBLE_METEO(instance%Meteo, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Tracer.Constant.') > 0) then
           SET_DOUBLE_MODELE_TRACER = SET_DOUBLE_CONSTANTES_TRACER(instance%ConsTrac(index1), NomVar, index2,&
                index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Tracer.Sources.') > 0) then
           SET_DOUBLE_MODELE_TRACER = SET_DOUBLE_SOURCE_TRACER(instance%Sources_Tracer(index1), NomVar, index2,&
                index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Tracer.BoundaryCond.') > 0) then
           SET_DOUBLE_MODELE_TRACER = SET_DOUBLE_COND_LIM_TRACER(instance%CondLimTrac(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Tracer.Graph.') > 0) then
           SET_DOUBLE_MODELE_TRACER = SET_DOUBLE_LOI_TRACER(instance%LoiTracer(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else
         SET_DOUBLE_MODELE_TRACER = 1
         MessageErreur         = "SET_DOUBLE_MODELE_TRACER - Unknown variable name"
      end if
   end function SET_DOUBLE_MODELE_TRACER

   function SET_INT_MODELE_TRACER(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_INT_MODELE_TRACER            ! different de 0 si erreur
      type(MODELE_TRACER_T),        intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(in) :: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur
      integer                            :: bidon1                     ! variable locale non utilise

      SET_INT_MODELE_TRACER = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Tracer.Number') > 0) then
         Instance%Nbtrac = valeur
      else if ( index(NomVar, 'Model.Tracer.CouplingFreq') > 0) then
         Instance%FreqCouplage = valeur
      else if ( index(NomVar, 'Model.Tracer.ID') > 0) then
         Instance%Modele_Qual_Eau = valeur
      else if ( index(NomVar, 'Model.Tracer.TracerResultFormat') > 0) then
         Instance%FormatResuTracer = valeur
      else if (INDEX(NomVar,'Model.File.Tracer.Listing.') > 0) then
           SET_INT_MODELE_TRACER = SET_INT_FICHIER(instance%FichierListingTracer, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.Tracer.Result.') > 0) then
           SET_INT_MODELE_TRACER = SET_INT_FICHIER(instance%FichierResuTracer, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.Tracer.IniConc.') > 0) then
           SET_INT_MODELE_TRACER = SET_INT_FICHIER(instance%FichierConcInit, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Tracer.NodeTrac.') > 0) then
           SET_INT_MODELE_TRACER = SET_INT_NODE_TRACER(instance%NodeTrac, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Tracer.Constant.') > 0) then
           SET_INT_MODELE_TRACER = SET_INT_CONSTANTES_TRACER(instance%ConsTrac(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Tracer.Sources.') > 0) then
           SET_INT_MODELE_TRACER = SET_INT_SOURCE_TRACER(instance%Sources_Tracer(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Tracer.BoundaryCond.') > 0) then
           SET_INT_MODELE_TRACER = SET_INT_COND_LIM_TRACER(instance%CondLimTrac(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Tracer.ParPhy.') > 0) then
           SET_INT_MODELE_TRACER = SET_INT_PARAMETRES_QUALITE_EAU(instance%ParPhy, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else
         SET_INT_MODELE_TRACER = 1
         MessageErreur         = "SET_INT_MODELE_TRACER - Unknown variable name"
      end if
   end function SET_INT_MODELE_TRACER

   function SET_BOOL_MODELE_TRACER(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_BOOL_MODELE_TRACER       ! different de 0 si erreur
      type(MODELE_TRACER_T),    intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      logical,                intent(in) :: valeur                     ! valeur du logical de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur
      integer                            :: bidon1                     ! variable locale non utilise

      SET_BOOL_MODELE_TRACER = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Tracer.Var2Save') > 0) then
         Instance%VarStoTracer(index1) = valeur
      else if ( index(NomVar, 'Model.Tracer.printListing') > 0) then
         Instance%ImpressionConcListing = valeur
      else if ( index(NomVar, 'Model.Tracer.printIniConc') > 0) then
         Instance%ImpressionConcIni = valeur
      else if ( index(NomVar, 'Model.Tracer.globalResults') > 0) then
         Instance%ImpressionBilanTracer = valeur
      else if ( index(NomVar, 'Model.Tracer.IniConcFromFile') > 0) then
         Instance%Presence_ConcIni = valeur
      else if (INDEX(NomVar,'Model.Tracer.Constant.') > 0) then
           SET_BOOL_MODELE_TRACER = SET_BOOL_CONSTANTES_TRACER(instance%ConsTrac(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Tracer.Sources.') > 0) then
           SET_BOOL_MODELE_TRACER = SET_BOOL_SOURCE_TRACER(instance%Sources_Tracer(index1), NomVar, index2,&
                index3, bidon1, valeur, MessageErreur)
      else
         SET_BOOL_MODELE_TRACER = 1
         MessageErreur         = "SET_BOOL_MODELE_TRACER - Unknown variable name"
      end if
   end function SET_BOOL_MODELE_TRACER

   function SET_STRING_MODELE_TRACER(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_STRING_MODELE_TRACER ! different de 0 si erreur
      type(MODELE_TRACER_T),intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      character(LEN=256),     intent(in):: valeur                     ! valeur du character(LEN=256) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur
      integer                            :: bidon1                         ! variable locale non utilise
      logical                            :: noqual

      SET_STRING_MODELE_TRACER = 0
      MessageErreur          = ""

      if (index(NomVar, 'Model.Tracer.WaterQualMod') > 0) then
         noqual = .true.
         do bidon1 = 1, NB_MODELE
            if (index(trim(valeur), trim(NOM_MODELE_QE(bidon1))) > 0) then
               Instance%Modele_Qual_Eau = bidon1
               noqual = .false.
               exit
            end if
         end do
         if (noqual) then
            SET_STRING_MODELE_TRACER = 1
            MessageErreur         = "SET_STRING_MODELE_TRACER - Unknown water quality name"
         end if
      else if (INDEX(NomVar,'Model.File.Tracer.Listing.') > 0) then
           SET_STRING_MODELE_TRACER = SET_STRING_FICHIER(instance%FichierListingTracer, NomVar, index1,&
                index2, index3, valeur, MessageErreur)
        else if (INDEX(NomVar,'Model.File.Tracer.Result.') > 0) then
           SET_STRING_MODELE_TRACER = SET_STRING_FICHIER(instance%FichierResuTracer, NomVar, index1,&
                index2, index3, valeur, MessageErreur)
        else if (INDEX(NomVar,'Model.File.Tracer.Ini.Conc.') > 0) then
           SET_STRING_MODELE_TRACER = SET_STRING_FICHIER(instance%FichierConcInit, NomVar, index1,&
                index2, index3, valeur, MessageErreur)
        else if (INDEX(NomVar,'Model.Tracer.Weather.') > 0) then
           SET_STRING_MODELE_TRACER = SET_STRING_METEO(instance%Meteo, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
        else if (INDEX(NomVar,'Model.Tracer.Graph.') > 0) then
           SET_STRING_MODELE_TRACER = SET_STRING_LOI_TRACER(instance%LoiTracer(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
        else if (INDEX(NomVar,'Model.Tracer.Sources.') > 0) then
           SET_STRING_MODELE_TRACER = SET_STRING_SOURCE_TRACER(instance%Sources_Tracer(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
        else
         SET_STRING_MODELE_TRACER = 1
         MessageErreur         = "SET_STRING_MODELE_TRACER - Unknown variable name"
      end if
   end function SET_STRING_MODELE_TRACER

! .................................................................................................................................
! Desalloue tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function DESALLOUE_MODELE_TRACER(Instance, MessageErreur)
      implicit none
      integer                            :: DESALLOUE_MODELE_TRACER      ! different de 0 si erreur
      type(MODELE_TRACER_T),    intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      integer                            :: taille
      integer                            :: err
      integer                            :: i
      character(LEN=256)                 :: MessageErreurType
      DESALLOUE_MODELE_TRACER = 0
      MessageErreur          = ""

      !-----------------------------------------------------------------------
      ! Appels aux fonctions desalloue des membres de type derive
      !-----------------------------------------------------------------------
      err = DESALLOUE_PARAMETRES_QUALITE_EAU(Instance%ParPhy, MessageErreurType)
      if (err /= 0) then
          DESALLOUE_MODELE_TRACER = err
          MessageErreur = 'Unable to deallocate MODEL_TRACER_T.PARPHY'
      endif
      err = DESALLOUE_METEO(Instance%Meteo, MessageErreurType)
      if (err /= 0) then
          DESALLOUE_MODELE_TRACER = err
          MessageErreur = 'Unable to deallocate MODEL_TRACER_T.METEO'
      endif
      err = DESALLOUE_NODE_TRACER(Instance%NodeTrac, MessageErreurType)
      if (err /= 0) then
          DESALLOUE_MODELE_TRACER = err
          MessageErreur = 'Unable to deallocate MODEL_TRACER_T.NODETRAC'
      endif
      if (ASSOCIATED(Instance%ConsTrac)) then
          taille = SIZE(Instance%ConsTrac)
          DO i=1, taille
              err = DESALLOUE_CONSTANTES_TRACER(Instance%ConsTrac(i), MessageErreurType)
              if (err /= 0) then
                  DESALLOUE_MODELE_TRACER = err
                  MessageErreur = 'Unable to deallocate MODEL_TRACER_T.CONSTANT(i)'
                  return
              endif
          enddo
          DEALLOCATE(Instance%ConsTrac, STAT=err)
          if (err /= 0) then
              DESALLOUE_MODELE_TRACER = err
              MessageErreur = 'Unable to deallocate MODEL_TRACER_T.CONSTANT'
              return
          endif
          NULLIFY(Instance%ConsTrac)
      endif
      if (ASSOCIATED(Instance%Sources_Tracer)) then
          taille = SIZE(Instance%Sources_Tracer)
          DO i=1, taille
              err = DESALLOUE_SOURCE_TRACER(Instance%Sources_Tracer(i), MessageErreurType)
              if (err /= 0) then
                  DESALLOUE_MODELE_TRACER = err
                  MessageErreur = 'Unable to deallocate MODEL_TRACER_T.SOURCES_TRACER(i)'
                  return
              endif
          enddo
          DEALLOCATE(Instance%Sources_Tracer, STAT=err)
          if (err /= 0) then
              DESALLOUE_MODELE_TRACER = err
              MessageErreur = 'Unable to deallocate MODEL_TRACER_T.SOURCES_TRACER'
              return
          endif
          NULLIFY(Instance%Sources_Tracer)
      endif
      if (ASSOCIATED(Instance%CondLimTrac)) then
          taille = SIZE(Instance%CondLimTrac)
          DO i=1, taille
              err = DESALLOUE_COND_LIM_TRACER(Instance%CondLimTrac(i), MessageErreurType)
              if (err /= 0) then
                  DESALLOUE_MODELE_TRACER = err
                  MessageErreur = 'Unable to deallocate MODEL_TRACER_T.CONDLIMTRAC(i)'
                  return
              endif
          enddo
          DEALLOCATE(Instance%CondLimTrac, STAT=err)
          if (err /= 0) then
              DESALLOUE_MODELE_TRACER = err
              MessageErreur = 'Unable to deallocate MODEL_TRACER_T.CONDLIMTRAC'
              return
          endif
          NULLIFY(Instance%CondLimTrac)
      endif
      if (ASSOCIATED(Instance%LoiTracer)) then
          taille = SIZE(Instance%LoiTracer)
          DO i=1, taille
              err = DESALLOUE_LOI_TRACER(Instance%LoiTracer(i), MessageErreurType)
              if (err /= 0) then
                  DESALLOUE_MODELE_TRACER = err
                  MessageErreur = 'Unable to deallocate MODEL_TRACER_T.LOITRACER(i)'
                  return
              endif
          enddo
          DEALLOCATE(Instance%LoiTracer, STAT=err)
          if (err /= 0) then
              DESALLOUE_MODELE_TRACER = err
              MessageErreur = 'Unable to deallocate MODEL_TRACER_T.LOITRACER'
              return
          endif
          NULLIFY(Instance%LoiTracer)
      endif
      !--------------------------------------------------------------------------------
      ! Fin des appels aux fonctions desalloue des membres de type derive
      !--------------------------------------------------------------------------------
   end function DESALLOUE_MODELE_TRACER

! .................................................................................................................................
! Rend null tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function NULLIFIER_MODELE_TRACER(Instance, MessageErreur)
      implicit none
      integer                            :: NULLIFIER_MODELE_TRACER      ! different de 0 si erreur
      type(MODELE_TRACER_T),    intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      integer                            :: err
      character(LEN=256)                 :: MessageErreurType
      NULLIFIER_MODELE_TRACER = 0
      MessageErreur          = ""

      !-----------------------------------------------------------------------
      ! Appels aux fonctions nullifier des membres de type derive
      !-----------------------------------------------------------------------
      err = NULLIFIER_FICHIER(Instance%FichierListingTracer, MessageErreurType)
      if (err /= 0) then
          NULLIFIER_MODELE_TRACER = err
          MessageErreur = 'Unable to nullify MODEL_TRACER_T.FILE.LISTINGTRACERNAME'
          return
      endif
      err = NULLIFIER_FICHIER(Instance%FichierResuTracer, MessageErreurType)
      if (err /= 0) then
          NULLIFIER_MODELE_TRACER = err
          MessageErreur = 'Unable to nullify MODEL_TRACER_T.FILE.RESUTRACERNAME'
          return
      endif
      err = NULLIFIER_FICHIER(Instance%FichierConcInit, MessageErreurType)
      if (err /= 0) then
          NULLIFIER_MODELE_TRACER = err
          MessageErreur = 'Unable to nullify MODEL_TRACER_T.FILE.CONCINITNAME'
          return
      endif
      err = NULLIFIER_PARAMETRES_QUALITE_EAU(Instance%ParPhy, MessageErreurType)
      if (err /= 0) then
          NULLIFIER_MODELE_TRACER = err
          MessageErreur = 'Unable to nullify MODEL_TRACER_T.PARPHY'
          return
      endif
      err = NULLIFIER_METEO(Instance%Meteo, MessageErreurType)
      if (err /= 0) then
          NULLIFIER_MODELE_TRACER = err
          MessageErreur = 'Unable to nullify MODEL_TRACER_T.METEO'
          return
      endif
      err = NULLIFIER_NODE_TRACER(Instance%NodeTrac, MessageErreurType)
      if (err /= 0) then
          NULLIFIER_MODELE_TRACER = err
          MessageErreur = 'Unable to nullify MODEL_TRACER_T.NODETRAC'
          return
      endif
      NULLIFY(Instance%ConsTrac)
      NULLIFY(Instance%Sources_Tracer)
      NULLIFY(Instance%CondLimTrac)
      NULLIFY(Instance%LoiTracer)
      !--------------------------------------------------------------------------------
      ! Fin des appels aux fonctions nullifier des membres de type derive
      !--------------------------------------------------------------------------------
   end function NULLIFIER_MODELE_TRACER

end module M_MODELE_TRACER_T
