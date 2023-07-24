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

module M_CASIER_T
!***********************************************************************
! PROGICIEL : MASCARET        A. LEBOSEE    C. RISSOAN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   !=========================== Declarations ==============================
   use M_PRECISION

   TYPE CASIER_T
      sequence
      real(DOUBLE) :: CoteFond        ! Cote du fond du casier
      real(DOUBLE) :: Cote            ! Cote de la surface libre :
                                      ! en entree ---> au debut du pas de temps traite
                                      ! en sortie ---> a la fin du pas de temps traite
      real(DOUBLE) :: Surface         ! Surface du casier
                                      ! en entree ---> au debut du pas de temps traite
                                      ! en sortie ---> a la fin du pas de temps traite
      real(DOUBLE) :: Volume          ! Volume du Casier
                                      ! en entree ---> au debut du pas de temps traite
                                      ! en sortie ---> a la fin du pas de temps traite
      real(DOUBLE) :: VolumeIni       ! volume initial
      real(DOUBLE) :: Bilan
      real(DOUBLE) :: BilanErreur
      real(DOUBLE) :: DzCas           ! Variation de cote dans le casier au cours
                                      ! du pas de temps traite
      real(DOUBLE) :: CoteMax         ! Cote max observee au cours du calcul
      real(DOUBLE) :: TempsMax        ! Temps correspondant a CoteMax
      integer, dimension(:,:), pointer :: LiaisonCC => null()   ! LiaisonCc(:,1) ---> numero de la liaison
                                                       ! LiaisonCc(:,2) ---> numero du casier associe
      integer, dimension(:,:), pointer :: LiaisonRC => null()  ! LiaisonRc(:,1) ---> numero de la liaison
                                                       ! LiaisonRc(:,2) ---> numero de la section associee
      real(DOUBLE), dimension(:,:), pointer :: Loi_Z_S => null()! Surface du casier a une cote donnee
                                                       ! resultats du planimetrage ou donnee utilis.
      real(DOUBLE), dimension(:,:), pointer :: Loi_Z_V => null()! Volume du casier a une cote donnee
                                                       ! resultats du planimetrage ou donnee utilis.
      real(DOUBLE) :: PasPlanim       ! Valeur du pas de planimetrage
                                      ! dans le cas ou le planimetrage est automatique
      integer      :: NbCotePlanim
      real(DOUBLE), dimension(:,:), pointer :: PointFrontiere => null()! matrice des coordonnees des points frontiere
      real(DOUBLE), dimension(:,:), pointer :: PointInterieur => null()! matrice des coordonnees des points interieur

   END TYPE CASIER_T

   contains
    ! Retourne les noms des champs du type ainsi qu'une description
    subroutine GET_TAB_VAR_CASIER(i, tabNomVar, tabDescriptionVar)
      integer , intent(inout)                                  :: i                 ! indiceTableaux
      character(len= 40), dimension(*)                :: tabNomVar         ! Tableau des noms de variable du modele
      character(len=110), dimension(*)                :: tabDescriptionVar ! Tableau des description de variable du modele

        tabNomVar(i)         ="Model.StorageArea.BottomLevel"
        tabDescriptionVar(i) ="Bottom level of a storage area (m)"
        i=i+1
        tabNomVar(i)         ="Model.StorageArea.Level"
        tabDescriptionVar(i) ="Water level of a storage area (m)"
        i=i+1
        tabNomVar(i)         ="Model.StorageArea.Surface"
        tabDescriptionVar(i) ="Surface of a storage area (m2)"
        i=i+1
        tabNomVar(i)         ="Model.StorageArea.Volume"
        tabDescriptionVar(i) ="Volume of a storage area (m3)"
        i=i+1
        tabNomVar(i)         ="Model.StorageArea.InitVolume"
        tabDescriptionVar(i) ="Initial volume of a storage area (m3)"
        i=i+1
        tabNomVar(i)         ="Model.StorageArea.Statement"
        tabDescriptionVar(i) ="Statement of a storage area"
        i=i+1
        tabNomVar(i)         ="Model.StorageArea.ErrorStatement"
        tabDescriptionVar(i) ="Error statement of a storage area"
        i=i+1
        tabNomVar(i)         ="Model.StorageArea.DZ"
        tabDescriptionVar(i) ="Water level elevation of a storage area for one time step"
        i=i+1
        tabNomVar(i)         ="Model.StorageArea.MaxLevel"
        tabDescriptionVar(i) ="Maximal water level of a storage area"
        i=i+1
        tabNomVar(i)         ="Model.StorageArea.TimeMaxLevel"
        tabDescriptionVar(i) ="Time for the maximal water level"
        i=i+1
        tabNomVar(i)         ="Model.StorageArea.SSLink"
        tabDescriptionVar(i) ="SSLink(:,1) : number of the link, SSLink(:,2) : number of the corresponding storage area"
        i=i+1
        tabNomVar(i)         ="Model.StorageArea.RSLink"
        tabDescriptionVar(i) ="RSLink(:,1) : number of the link, RSLink(:,2) : number of the corresponding node"
        i=i+1
        tabNomVar(i)         ="Model.StorageArea.Graph_Z_S"
        tabDescriptionVar(i) ="Surface of the storage area as a funtion of the elevation"
        i=i+1
        tabNomVar(i)         ="Model.StorageArea.Graph_Z_V"
        tabDescriptionVar(i) ="Volume of the storage area as a funtion of the elevation"
        i=i+1
        tabNomVar(i)         ="Model.StorageArea.VertDisc"
        tabDescriptionVar(i) ="Vertical discretisation step for a storage area"
        i=i+1
        tabNomVar(i)         ="Model.StorageArea.NbVertDisc"
        tabDescriptionVar(i) ="Number of data for the vertical discretisation"
        i=i+1
        tabNomVar(i)         ="Model.StorageArea.BoundPts"
        tabDescriptionVar(i) ="Coordinates of the boundary points"
        i=i+1
        tabNomVar(i)         ="Model.StorageArea.InternalPts"
        tabDescriptionVar(i) ="Coordinates of the internal points"
        i=i+1

      return

    end subroutine GET_TAB_VAR_CASIER

	! Retourne une description du champ du type au niveau de static (independant de l'instance du modele ou de l'etat)
    function GET_TYPE_VAR_CASIER(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      implicit none

      integer                          :: GET_TYPE_VAR_CASIER      ! different de 0 si erreur
      character(LEN=40), intent(in)    :: NomVar                   ! Nom de la variable (notation pointe)
      character(LEN=10), intent(out)   :: TypeVar                  ! "INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
      character(LEN=10), intent(out)   :: Categorie                ! "MODEL" ou "STATE"
      logical          , intent(out)   :: Modifiable               ! Si vrai alors on peut utiliser une fonction SET_XXXX_MASCARET sur la variable
      integer          , intent(out)   :: dimVar                   ! dimension (c'est a dire le nombre d'indexe de 0 a 3)
      character(LEN=256), intent(out)  :: MessageErreur            ! Message d'erreur

      GET_TYPE_VAR_CASIER   = 0
      TypeVar               = ""
      Categorie             = "MODEL"
      Modifiable            = .FALSE.
      dimVar                = 0
      MessageErreur         = ""


       if ( index(NomVar, 'Model.StorageArea.BottomLevel') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.StorageArea.Level') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.StorageArea.Surface') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.StorageArea.Volume') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.StorageArea.InitVolume') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.StorageArea.Statement') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.StorageArea.ErrorStatement') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.StorageArea.DZ') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.StorageArea.MaxLevel') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.StorageArea.TimeMaxLevel') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.StorageArea.SSLink') > 0) then
          TypeVar = 'TABINT'
          dimVar                = 2
       else if ( index(NomVar, 'Model.StorageArea.RSLink') > 0) then
          TypeVar = 'TABINT'
          dimVar                = 2
       else if ( index(NomVar, 'Model.StorageArea.Graph_Z_S') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.StorageArea.Graph_Z_V') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.StorageArea.VertDisc') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.StorageArea.NbVertDisc') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( index(NomVar, 'Model.StorageArea.BoundPts') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.StorageArea.InternalPts') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
      else
        GET_TYPE_VAR_CASIER   = 1
        TypeVar = "?"
        Categorie             = "MODEL"
        Modifiable            = .FALSE.
        dimVar                = -1
        MessageErreur         = "GET_TYPE_VAR_CASIER - Unknown variable name"
      end if


    end function GET_TYPE_VAR_CASIER

! .................................................................................................................................
! Permet d'acceder a la taille des valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_TAILLE_VAR_CASIER(Instance, NomVar, taille1, taille2, taille3, MessageErreur)
      implicit none
      integer                            :: GET_TAILLE_VAR_CASIER          ! different de 0 si erreur
      type(CASIER_T),         intent(in) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in) :: NomVar                         ! Nom de la variable du modele
      integer,                intent(out):: taille1                        ! valeur max du 1er indice
      integer,                intent(out):: taille2                        ! valeur max du 2e  indice
      integer,                intent(out):: taille3                        ! valeur max du 3e  indice
      character(LEN=256),     intent(out):: MessageErreur                  ! Message d'erreur

      GET_TAILLE_VAR_CASIER = 0
      taille1                = 0
      taille2                = 0
      taille3                = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.StorageArea.BottomLevel') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.StorageArea.Level') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.StorageArea.Surface') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.StorageArea.Volume') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.StorageArea.InitVolume') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.StorageArea.Statement') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.StorageArea.ErrorStatement') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.StorageArea.DZ') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.StorageArea.MaxLevel') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.StorageArea.TimeMaxLevel') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.StorageArea.SSLink') > 0) then
         if (ASSOCIATED(Instance%LiaisonCC)) then
            taille1 = size(Instance%LiaisonCC, 1)
            taille2 = size(Instance%LiaisonCC, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.StorageArea.RSLink') > 0) then
         if (ASSOCIATED(Instance%LiaisonRC)) then
            taille1 = size(Instance%LiaisonRC, 1)
            taille2 = size(Instance%LiaisonRC, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.StorageArea.Graph_Z_S') > 0) then
         if (ASSOCIATED(Instance%Loi_Z_S)) then
            taille1 = size(Instance%Loi_Z_S, 1)
            taille2 = size(Instance%Loi_Z_S, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.StorageArea.Graph_Z_V') > 0) then
         if (ASSOCIATED(Instance%Loi_Z_V)) then
            taille1 = size(Instance%Loi_Z_V, 1)
            taille2 = size(Instance%Loi_Z_V, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.StorageArea.VertDisc') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.StorageArea.NbVertDisc') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.StorageArea.BoundPts') > 0) then
         if (ASSOCIATED(Instance%PointFrontiere)) then
            taille1 = size(Instance%PointFrontiere, 1)
            taille2 = size(Instance%PointFrontiere, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.StorageArea.InternalPts') > 0) then
         if (ASSOCIATED(Instance%PointInterieur)) then
            taille1 = size(Instance%PointInterieur, 1)
            taille2 = size(Instance%PointInterieur, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else
         GET_TAILLE_VAR_CASIER = 1
         taille1                = -1
         taille2                = -1
         taille3                = -1
         MessageErreur         = "GET_TAILLE_VAR_CASIER - Unknown variable name"
      end if
   end function GET_TAILLE_VAR_CASIER

! .................................................................................................................................
! Permet de modifier la taille les variables de type pointeurs fortran
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_TAILLE_VAR_CASIER(Instance, NomVar, NewT1, NewT2, NewT3, MessageErreur)
      implicit none
      integer                               :: SET_TAILLE_VAR_CASIER          ! different de 0 si erreur
      type(CASIER_T),         intent(inout) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in)    :: NomVar                         ! Nom de la variable du modele
      integer,                intent(in)    :: NewT1                          ! Nouvelle valeur max du 1er indice
      integer,                intent(in)    :: NewT2                          ! Nouvelle valeur max du 2e  indice
      integer,                intent(in)    :: NewT3                          ! Nouvelle valeur max du 3e  indice
      character(LEN=256),     intent(out)   :: MessageErreur                  ! Message d'erreur

      integer t1, t2, t3, err

      SET_TAILLE_VAR_CASIER = 0
      t1                     = -1
      t2                     = -1
      t3                     = -1
      MessageErreur          = ""
      err                    = 0

      !----------------------------------------------------------
      ! Modification de la taille des pointers de types primitifs
      !----------------------------------------------------------
      if ( index(NomVar, 'Model.StorageArea.SSLink') > 0) then
        if (ASSOCIATED(Instance%LiaisonCC)) then
           t1 = size(Instance%LiaisonCC, 1)
           t2 = size(Instance%LiaisonCC, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%LiaisonCC, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_CASIER = err
                 MessageErreur = 'SET_TAILLE_VAR_CASIER : Unable to deallocate CASIER_T.LiaisonCC'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%LiaisonCC).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%LiaisonCC(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_CASIER = err
              MessageErreur = 'SET_TAILLE_VAR_CASIER : Unable to allocate CASIER_T.LiaisonCC'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.StorageArea.RSLink') > 0) then
        if (ASSOCIATED(Instance%LiaisonRC)) then
           t1 = size(Instance%LiaisonRC, 1)
           t2 = size(Instance%LiaisonRC, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%LiaisonRC, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_CASIER = err
                 MessageErreur = 'SET_TAILLE_VAR_CASIER : Unable to deallocate CASIER_T.LiaisonRC'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%LiaisonRC).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%LiaisonRC(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_CASIER = err
              MessageErreur = 'SET_TAILLE_VAR_CASIER : Unable to allocate CASIER_T.LiaisonRC'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.StorageArea.Graph_Z_S') > 0) then
        if (ASSOCIATED(Instance%Loi_Z_S)) then
           t1 = size(Instance%Loi_Z_S, 1)
           t2 = size(Instance%Loi_Z_S, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%Loi_Z_S, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_CASIER = err
                 MessageErreur = 'SET_TAILLE_VAR_CASIER : Unable to deallocate CASIER_T.Loi_Z_S'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%Loi_Z_S).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%Loi_Z_S(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_CASIER = err
              MessageErreur = 'SET_TAILLE_VAR_CASIER : Unable to allocate CASIER_T.Loi_Z_S'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.StorageArea.Graph_Z_V') > 0) then
        if (ASSOCIATED(Instance%Loi_Z_V)) then
           t1 = size(Instance%Loi_Z_V, 1)
           t2 = size(Instance%Loi_Z_V, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%Loi_Z_V, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_CASIER = err
                 MessageErreur = 'SET_TAILLE_VAR_CASIER : Unable to deallocate CASIER_T.Loi_Z_V'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%Loi_Z_V).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%Loi_Z_V(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_CASIER = err
              MessageErreur = 'SET_TAILLE_VAR_CASIER : Unable to allocate CASIER_T.Loi_Z_V'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.StorageArea.BoundPts') > 0) then
        if (ASSOCIATED(Instance%PointFrontiere)) then
           t1 = size(Instance%PointFrontiere, 1)
           t2 = size(Instance%PointFrontiere, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%PointFrontiere, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_CASIER = err
                 MessageErreur = 'SET_TAILLE_VAR_CASIER : Unable to deallocate CASIER_T.PointFrontiere'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%PointFrontiere).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%PointFrontiere(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_CASIER = err
              MessageErreur = 'SET_TAILLE_VAR_CASIER : Unable to allocate CASIER_T.PointFrontiere'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.StorageArea.InternalPts') > 0) then
        if (ASSOCIATED(Instance%PointInterieur)) then
           t1 = size(Instance%PointInterieur, 1)
           t2 = size(Instance%PointInterieur, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%PointInterieur, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_CASIER = err
                 MessageErreur = 'SET_TAILLE_VAR_CASIER : Unable to deallocate CASIER_T.PointInterieur'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%PointInterieur).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%PointInterieur(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_CASIER = err
              MessageErreur = 'SET_TAILLE_VAR_CASIER : Unable to allocate CASIER_T.PointInterieur'
              return
           endif
        endif
      !--------------------------------------------------------------------
      ! Fin de la modification de la taille des pointers de types primitifs
      !--------------------------------------------------------------------

      else
         SET_TAILLE_VAR_CASIER = 1
         MessageErreur         = "SET_TAILLE_VAR_CASIER - Unknown variable name"
      end if
   end function SET_TAILLE_VAR_CASIER

! .................................................................................................................................
! Accesseurs permettant d'acceder aux valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_DOUBLE_CASIER(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_DOUBLE_CASIER          ! different de 0 si erreur
      type(CASIER_T),         intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(out):: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_DOUBLE_CASIER = 0
      valeur                = -9999999.9999
      MessageErreur          = ""

      if ( index(NomVar, 'Model.StorageArea.BottomLevel') > 0) then
         valeur = Instance%CoteFond
      else if ( index(NomVar, 'Model.StorageArea.Level') > 0) then
         valeur = Instance%Cote
      else if ( index(NomVar, 'Model.StorageArea.Surface') > 0) then
         valeur = Instance%Surface
      else if ( index(NomVar, 'Model.StorageArea.Volume') > 0) then
         valeur = Instance%Volume
      else if ( index(NomVar, 'Model.StorageArea.InitVolume') > 0) then
         valeur = Instance%VolumeIni
      else if ( index(NomVar, 'Model.StorageArea.Statement') > 0) then
         valeur = Instance%Bilan
      else if ( index(NomVar, 'Model.StorageArea.ErrorStatement') > 0) then
         valeur = Instance%BilanErreur
      else if ( index(NomVar, 'Model.StorageArea.DZ') > 0) then
         valeur = Instance%DzCas
      else if ( index(NomVar, 'Model.StorageArea.MaxLevel') > 0) then
         valeur = Instance%CoteMax
      else if ( index(NomVar, 'Model.StorageArea.TimeMaxLevel') > 0) then
         valeur = Instance%TempsMax
      else if ( index(NomVar, 'Model.StorageArea.Graph_Z_S') > 0) then
         valeur = Instance%Loi_Z_S(index1, index2)
      else if ( index(NomVar, 'Model.StorageArea.Graph_Z_V') > 0) then
         valeur = Instance%Loi_Z_V(index1, index2)
      else if ( index(NomVar, 'Model.StorageArea.VertDisc') > 0) then
         valeur = Instance%PasPlanim
      else if ( index(NomVar, 'Model.StorageArea.BoundPts') > 0) then
         valeur = Instance%PointFrontiere(index1, index2)
      else if ( index(NomVar, 'Model.StorageArea.InternalPts') > 0) then
         valeur = Instance%PointInterieur(index1, index2)
      else
         GET_DOUBLE_CASIER = 1
         valeur                = -9999999.9999
         MessageErreur         = "GET_DOUBLE_CASIER - Unknown variable name"
      end if
   end function GET_DOUBLE_CASIER


   function GET_INT_CASIER(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_INT_CASIER             ! different de 0 si erreur
      type(CASIER_T),         intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(out):: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_INT_CASIER = 0
      valeur                = -9999
      MessageErreur          = ""

      if ( index(NomVar, 'Model.StorageArea.SSLink') > 0) then
         valeur = Instance%LiaisonCC(index1, index2)
      else if ( index(NomVar, 'Model.StorageArea.RSLink') > 0) then
         valeur = Instance%LiaisonRC(index1, index2)
      else if ( index(NomVar, 'Model.StorageArea.NbVertDisc') > 0) then
         valeur = Instance%NbCotePlanim
      else
         GET_INT_CASIER = 1
         valeur                = -9999
         MessageErreur         = "GET_INT_CASIER - Unknown variable name"
      end if
   end function GET_INT_CASIER



! .................................................................................................................................
! Mutateurs permettant de modifier les differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_DOUBLE_CASIER(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_DOUBLE_CASIER          ! different de 0 si erreur
      type(CASIER_T),         intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(in) :: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_DOUBLE_CASIER = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.StorageArea.BottomLevel') > 0) then
         Instance%CoteFond = valeur
      else if ( index(NomVar, 'Model.StorageArea.Level') > 0) then
         Instance%Cote = valeur
      else if ( index(NomVar, 'Model.StorageArea.Surface') > 0) then
         Instance%Surface = valeur
      else if ( index(NomVar, 'Model.StorageArea.Volume') > 0) then
         Instance%Volume = valeur
      else if ( index(NomVar, 'Model.StorageArea.InitVolume') > 0) then
         Instance%VolumeIni = valeur
      else if ( index(NomVar, 'Model.StorageArea.Statement') > 0) then
         Instance%Bilan = valeur
      else if ( index(NomVar, 'Model.StorageArea.ErrorStatement') > 0) then
         Instance%BilanErreur = valeur
      else if ( index(NomVar, 'Model.StorageArea.DZ') > 0) then
         Instance%DzCas = valeur
      else if ( index(NomVar, 'Model.StorageArea.MaxLevel') > 0) then
         Instance%CoteMax = valeur
      else if ( index(NomVar, 'Model.StorageArea.TimeMaxLevel') > 0) then
         Instance%TempsMax = valeur
      else if ( index(NomVar, 'Model.StorageArea.Graph_Z_S') > 0) then
         Instance%Loi_Z_S(index1, index2) = valeur
      else if ( index(NomVar, 'Model.StorageArea.Graph_Z_V') > 0) then
         Instance%Loi_Z_V(index1, index2) = valeur
      else if ( index(NomVar, 'Model.StorageArea.VertDisc') > 0) then
         Instance%PasPlanim = valeur
      else if ( index(NomVar, 'Model.StorageArea.BoundPts') > 0) then
         Instance%PointFrontiere(index1, index2) = valeur
      else if ( index(NomVar, 'Model.StorageArea.InternalPts') > 0) then
         Instance%PointInterieur(index1, index2) = valeur
      else
         SET_DOUBLE_CASIER = 1
         MessageErreur         = "SET_DOUBLE_CASIER - Unknown variable name"
      end if
   end function SET_DOUBLE_CASIER


   function SET_INT_CASIER(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_INT_CASIER             ! different de 0 si erreur
      type(CASIER_T),         intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(in) :: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_INT_CASIER = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.StorageArea.SSLink') > 0) then
         Instance%LiaisonCC(index1, index2) = valeur
      elseif ( index(NomVar, 'Model.StorageArea.RSLink') > 0) then
         Instance%LiaisonRC(index1, index2) = valeur
      else if ( index(NomVar, 'Model.StorageArea.NbVertDisc') > 0) then
         Instance%NbCotePlanim = valeur
      else
         SET_INT_CASIER = 1
         MessageErreur         = "SET_INT_CASIER - Unknown variable name"
      end if
   end function SET_INT_CASIER



! .................................................................................................................................
! Desalloue tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function DESALLOUE_CASIER(Instance, MessageErreur)
      implicit none
      integer                            :: DESALLOUE_CASIER           ! different de 0 si erreur
      type(CASIER_T),         intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      integer                            :: taille
      integer                            :: err
      DESALLOUE_CASIER = 0
      MessageErreur    = ""
      err              = 0

      !----------------------------------------------
      ! Desallocation des pointers de types primitifs
      !----------------------------------------------
      if (ASSOCIATED(Instance%LiaisonCC)) then
          taille = SIZE(Instance%LiaisonCC, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%LiaisonCC, STAT=err)
              if (err /= 0) then
                  DESALLOUE_CASIER = err
                  MessageErreur = 'Unable to deallocate CASIER_T.LiaisonCC'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%LiaisonCC)
      if (ASSOCIATED(Instance%LiaisonRC)) then
          taille = SIZE(Instance%LiaisonRC, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%LiaisonRC, STAT=err)
              if (err /= 0) then
                  DESALLOUE_CASIER = err
                  MessageErreur = 'Unable to deallocate CASIER_T.LiaisonRC'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%LiaisonRC)
      if (ASSOCIATED(Instance%Loi_Z_S)) then
          taille = SIZE(Instance%Loi_Z_S, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%Loi_Z_S, STAT=err)
              if (err /= 0) then
                  DESALLOUE_CASIER = err
                  MessageErreur = 'Unable to deallocate CASIER_T.Loi_Z_S'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%Loi_Z_S)
      if (ASSOCIATED(Instance%Loi_Z_V)) then
          taille = SIZE(Instance%Loi_Z_V, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%Loi_Z_V, STAT=err)
              if (err /= 0) then
                  DESALLOUE_CASIER = err
                  MessageErreur = 'Unable to deallocate CASIER_T.Loi_Z_V'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%Loi_Z_V)
      if (ASSOCIATED(Instance%PointFrontiere)) then
          taille = SIZE(Instance%PointFrontiere, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%PointFrontiere, STAT=err)
              if (err /= 0) then
                  DESALLOUE_CASIER = err
                  MessageErreur = 'Unable to deallocate CASIER_T.PointFrontiere'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%PointFrontiere)
      if (ASSOCIATED(Instance%PointInterieur)) then
          taille = SIZE(Instance%PointInterieur, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%PointInterieur, STAT=err)
              if (err /= 0) then
                  DESALLOUE_CASIER = err
                  MessageErreur = 'Unable to deallocate CASIER_T.PointInterieur'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%PointInterieur)
      !--------------------------------------------------------
      ! Fin de la desallocation des pointers de types primitifs
      !--------------------------------------------------------

   end function DESALLOUE_CASIER

! .................................................................................................................................
! Rend null tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function NULLIFIER_CASIER(Instance, MessageErreur)
      implicit none
      integer                            :: NULLIFIER_CASIER           ! different de 0 si erreur
      type(CASIER_T),         intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      NULLIFIER_CASIER = 0
      MessageErreur          = ""

      !----------------------------------------------
      ! Nullifie des pointers de types primitifs
      !----------------------------------------------
      NULLIFY(Instance%LiaisonCC)
      NULLIFY(Instance%LiaisonRC)
      NULLIFY(Instance%Loi_Z_S)
      NULLIFY(Instance%Loi_Z_V)
      NULLIFY(Instance%PointFrontiere)
      NULLIFY(Instance%PointInterieur)
      !--------------------------------------------------------
      ! Fin de la Nullification des pointers de types primitifs
      !--------------------------------------------------------

   end function NULLIFIER_CASIER

end module M_CASIER_T
