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

module M_SAUVE_T
!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   !=========================== Declarations ==============================
use M_PRECISION
TYPE SAUVE_T
  sequence
  Real(DOUBLE) , dimension(:) , pointer :: H2OIB  => null() ! MASSE D'EAU INITIALE BIEF
  Real(DOUBLE) , dimension(:) , pointer :: H2OTB  => null() ! MASSE D'EAU TOTALE BIEF
  Real(DOUBLE) , dimension(:) , pointer :: H2OEB  => null() ! MASSE D'EAU ENTREE BIEF
  Real(DOUBLE) , dimension(:) , pointer :: H2OSB  => null() ! MASSE D'EAU SORTIE BIEF
  Real(DOUBLE) , dimension(:) , pointer :: H2OIC  => null() ! MASSE D'EAU INITIALE CONFLUENT
  Real(DOUBLE) , dimension(:) , pointer :: H2OTC  => null() ! MASSE D'EAU TOTALE CONFLUENT
  Real(DOUBLE) , dimension(:) , pointer :: H2OEC  => null() ! MASSE D'EAU ENTREE CONFLUENT
  Real(DOUBLE) , dimension(:) , pointer :: H2OSC  => null() ! MASSE D'EAU SORTIE CONFLUENT
  Real(DOUBLE) , dimension(:) , pointer :: H2OTBS => null() ! MASSE D'EAU TOTALE BIEF SORTIE
  Real(DOUBLE) , dimension(:) , pointer :: H2OIBS => null() ! MASSE D'EAU INITIALE BIEF SORTIE
  Real(DOUBLE) , dimension(:) , pointer :: SPREC  => null() ! SURFACE MOUILLEE PRECEDENTE
  Real(DOUBLE) , dimension(:) , pointer :: QPREC  => null() ! DEBIT PRECEDENT

  Real(DOUBLE)                          :: H2OIG  ! MASSE D'EAU INITIALE GLOBAL
  Real(DOUBLE)                          :: H2OIGS ! MASSE D'EAU INITIALE GLOBAL SORTIE
  Real(DOUBLE)                          :: H2OTG  ! MASSE D'EAU TOTALE GLOBAL
  Real(DOUBLE)                          :: H2OTGS ! MASSE D'EAU TOTALE GLOBAL SORTIE
  Real(DOUBLE)                          :: H2OEG  ! MASSE D'EAU ENTREE GLOBAL
  Real(DOUBLE)                          :: H2OSG  ! MASSE D'EAU SORTIE GLOBAL

END TYPE SAUVE_T

contains
    ! Retourne les noms des champs du type ainsi qu'une description
    subroutine GET_TAB_VAR_SAUVE(i, tabNomVar, tabDescriptionVar)
      integer , intent(inout)                         :: i                 ! indiceTableaux
      character(len= 40), dimension(*)                :: tabNomVar         ! Tableau des noms de variable du modele ou de l'?tat
      character(len=110), dimension(*)                :: tabDescriptionVar ! Tableau des description de variable du modele ou de l'?tat

        tabNomVar(i)         ="State.Save.H2OIB"
        tabDescriptionVar(i) ="Initial water mass - REACH"
        i=i+1
        tabNomVar(i)         ="State.Save.H2OTB"
        tabDescriptionVar(i) ="Total water mass - REACH"
        i=i+1
        tabNomVar(i)         ="State.Save.H2OEB"
        tabDescriptionVar(i) ="Incoming water mass - REACH"
        i=i+1
        tabNomVar(i)         ="State.Save.H2OSB"
        tabDescriptionVar(i) ="Outgoing water mass - REACH"
        i=i+1
        tabNomVar(i)         ="State.Save.H2OIC"
        tabDescriptionVar(i) ="Initial water mass - JUNCTION"
        i=i+1
        tabNomVar(i)         ="State.Save.H2OTC"
        tabDescriptionVar(i) ="Total water mass - JUNCTION"
        i=i+1
        tabNomVar(i)         ="State.Save.H2OEC"
        tabDescriptionVar(i) ="Incoming water mass - JUNCTION"
        i=i+1
        tabNomVar(i)         ="State.Save.H2OSC"
        tabDescriptionVar(i) ="Outgoing water mass - JUNCTION"
        i=i+1
        tabNomVar(i)         ="State.Save.H2OTBS"
        tabDescriptionVar(i) ="Total outgoing water mass - REACH"
        i=i+1
        tabNomVar(i)         ="State.Save.H2OIBS"
        tabDescriptionVar(i) ="Initial outgoing water mass - REACH"
        i=i+1
        tabNomVar(i)         ="State.Save.SPREC"
        tabDescriptionVar(i) ="Previous wetted area"
        i=i+1
        tabNomVar(i)         ="State.Save.QPREC"
        tabDescriptionVar(i) ="Previous discharge"
        i=i+1
        tabNomVar(i)         ="State.Save.H2OIG"
        tabDescriptionVar(i) ="Global initial water mass"
        i=i+1
        tabNomVar(i)         ="State.Save.H2OIGS"
        tabDescriptionVar(i) ="Outgoing global initial water mass"
        i=i+1
        tabNomVar(i)         ="State.Save.H2OTG"
        tabDescriptionVar(i) ="Global total water mass"
        i=i+1
        tabNomVar(i)         ="State.Save.H2OTGS"
        tabDescriptionVar(i) ="Outgoing global total water mass"
        i=i+1
        tabNomVar(i)         ="State.Save.H2OEG"
        tabDescriptionVar(i) ="Global incoming water mass"
        i=i+1
        tabNomVar(i)         ="State.Save.H2OSG"
        tabDescriptionVar(i) ="Global outgoing water mass"
        i=i+1

        return

    end subroutine GET_TAB_VAR_SAUVE

	! Retourne une description du champ du type au niveau de static (independant de l'instance du modele ou de l'etat)
    function GET_TYPE_VAR_SAUVE(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      implicit none

      integer                          :: GET_TYPE_VAR_SAUVE ! different de 0 si erreur
      character(LEN=40), intent(in)    :: NomVar                   ! Nom de la variable (notation point?)
      character(LEN=10), intent(out)   :: TypeVar                  ! "INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
      character(LEN=10), intent(out)   :: Categorie                ! "MODEL" ou "STATE"
      logical          , intent(out)   :: Modifiable               ! Si vrai alors on peut utiliser une fonction SET_XXXX_MASCARET sur la variable
      integer          , intent(out)   :: dimVar                   ! dimension (c'est ? dire le nombre d'indexe de 0 ? 3)
      character(LEN=256), intent(out)  :: MessageErreur            ! Message d'erreur

      GET_TYPE_VAR_SAUVE = 0
      TypeVar               = ""
      Categorie             = "STATE"
      Modifiable            = .FALSE.
      dimVar                = 0
      MessageErreur         = ""

      if ( index(NomVar, 'State.Save.H2OTBS') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar, 'State.Save.H2OIBS') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar, 'State.Save.H2OIB') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar, 'State.Save.H2OTB') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar, 'State.Save.H2OEB') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar, 'State.Save.H2OSB') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar, 'State.Save.H2OIC') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar, 'State.Save.H2OTC') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar, 'State.Save.H2OEC') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar, 'State.Save.H2OSC') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar, 'State.Save.SPREC') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar, 'State.Save.QPREC') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar, 'State.Save.H2OIGS') > 0) then
         TypeVar = 'DOUBLE'
         dimVar                = 0
      else if ( index(NomVar, 'State.Save.H2OIG') > 0) then
         TypeVar = 'DOUBLE'
         dimVar                = 0
      else if ( index(NomVar, 'State.Save.H2OTGS') > 0) then
         TypeVar = 'DOUBLE'
         dimVar                = 0
      else if ( index(NomVar, 'State.Save.H2OTG') > 0) then
         TypeVar = 'DOUBLE'
         dimVar                = 0
      else if ( index(NomVar, 'State.Save.H2OEG') > 0) then
         TypeVar = 'DOUBLE'
         dimVar                = 0
      else if ( index(NomVar, 'State.Save.H2OSG') > 0) then
         TypeVar = 'DOUBLE'
         dimVar                = 0
      else
        GET_TYPE_VAR_SAUVE = 1
        TypeVar = "?"
        Categorie             = "STATE"
        Modifiable            = .FALSE.
        dimVar                = -1
        MessageErreur         = "GET_TYPE_VAR_SAUVE - Unknown variable name"
      end if

    end function GET_TYPE_VAR_SAUVE

! .................................................................................................................................
! Permet d'acceder a la taille des valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_TAILLE_VAR_SAUVE(Instance, NomVar, taille1, taille2, taille3, MessageErreur)
      implicit none
      integer                            :: GET_TAILLE_VAR_SAUVE           ! different de 0 si erreur
      type(SAUVE_T),          intent(in) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in) :: NomVar                         ! Nom de la variable du modele
      integer,                intent(out):: taille1                        ! valeur max du 1er indice
      integer,                intent(out):: taille2                        ! valeur max du 2e  indice
      integer,                intent(out):: taille3                        ! valeur max du 3e  indice
      character(LEN=256),     intent(out):: MessageErreur                  ! Message d'erreur

      GET_TAILLE_VAR_SAUVE = 0
      taille1                = 0
      taille2                = 0
      taille3                = 0
      MessageErreur          = ""

      if ( index(NomVar, 'State.Save.H2OTBS') > 0) then
         if (ASSOCIATED(Instance%H2OTBS)) then
            taille1 = size(Instance%H2OTBS)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Save.H2OIBS') > 0) then
         if (ASSOCIATED(Instance%H2OIBS)) then
            taille1 = size(Instance%H2OIBS)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Save.H2OIB') > 0) then
         if (ASSOCIATED(Instance%H2OIB)) then
            taille1 = size(Instance%H2OIB)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Save.H2OTB') > 0) then
         if (ASSOCIATED(Instance%H2OTB)) then
            taille1 = size(Instance%H2OTB)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Save.H2OEB') > 0) then
         if (ASSOCIATED(Instance%H2OEB)) then
            taille1 = size(Instance%H2OEB)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Save.H2OSB') > 0) then
         if (ASSOCIATED(Instance%H2OSB)) then
            taille1 = size(Instance%H2OSB)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Save.H2OIC') > 0) then
         if (ASSOCIATED(Instance%H2OIC)) then
            taille1 = size(Instance%H2OIC)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Save.H2OTC') > 0) then
         if (ASSOCIATED(Instance%H2OTC)) then
            taille1 = size(Instance%H2OTC)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Save.H2OEC') > 0) then
         if (ASSOCIATED(Instance%H2OEC)) then
            taille1 = size(Instance%H2OEC)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Save.H2OSC') > 0) then
         if (ASSOCIATED(Instance%H2OSC)) then
            taille1 = size(Instance%H2OSC)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Save.SPREC') > 0) then
         if (ASSOCIATED(Instance%SPREC)) then
            taille1 = size(Instance%SPREC)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Save.QPREC') > 0) then
         if (ASSOCIATED(Instance%QPREC)) then
            taille1 = size(Instance%QPREC)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Save.H2OIGS') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Save.H2OIG') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Save.H2OTGS') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Save.H2OTG') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Save.H2OEG') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Save.H2OSG') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else
         GET_TAILLE_VAR_SAUVE = 1
         taille1                = -1
         taille2                = -1
         taille3                = -1
         MessageErreur         = "GET_TAILLE_VAR_SAUVE - Unknown variable name"
      end if
   end function GET_TAILLE_VAR_SAUVE

! .................................................................................................................................
! Permet de modifier la taille les variables de type pointeurs fortran
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_TAILLE_VAR_SAUVE(Instance, NomVar, NewT1, NewT2, NewT3, MessageErreur)
      implicit none
      integer                               :: SET_TAILLE_VAR_SAUVE           ! different de 0 si erreur
      type(SAUVE_T),          intent(inout) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in)    :: NomVar                         ! Nom de la variable du modele
      integer,                intent(in)    :: NewT1                          ! Nouvelle valeur max du 1er indice
      integer,                intent(in)    :: NewT2                          ! Nouvelle valeur max du 2e  indice
      integer,                intent(in)    :: NewT3                          ! Nouvelle valeur max du 3e  indice
      character(LEN=256),     intent(out)   :: MessageErreur                  ! Message d'erreur

      integer t1, t2, t3, err

      SET_TAILLE_VAR_SAUVE = 0
      t1                     = -1
      t2                     = -1
      t3                     = -1
      MessageErreur          = ""
      err                    = 0

      !----------------------------------------------------------
      ! Modification de la taille des pointers de types primitifs
      !----------------------------------------------------------
      if ( index(NomVar, 'State.Save.H2OTBS') > 0) then
        if (ASSOCIATED(Instance%H2OTBS)) then
           t1 = size(Instance%H2OTBS)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%H2OTBS, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_SAUVE = err
                 MessageErreur = 'SET_TAILLE_VAR_SAUVE : Unable to deallocate SAUVE_T.H2OTBS'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%H2OTBS) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%H2OTBS(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_SAUVE = err
              MessageErreur = 'SET_TAILLE_VAR_SAUVE : Unable to allocate SAUVE_T.H2OTBS'
              return
           endif
        endif
      else if ( index(NomVar, 'State.Save.H2OIBS') > 0) then
        if (ASSOCIATED(Instance%H2OIBS)) then
           t1 = size(Instance%H2OIBS)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%H2OIBS, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_SAUVE = err
                 MessageErreur = 'SET_TAILLE_VAR_SAUVE : Unable to deallocate SAUVE_T.H2OIBS'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%H2OIBS) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%H2OIBS(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_SAUVE = err
              MessageErreur = 'SET_TAILLE_VAR_SAUVE : Unable to allocate SAUVE_T.H2OIBS'
              return
           endif
        endif
      else if ( index(NomVar, 'State.Save.H2OIB') > 0) then
        if (ASSOCIATED(Instance%H2OIB)) then
           t1 = size(Instance%H2OIB)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%H2OIB, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_SAUVE = err
                 MessageErreur = 'SET_TAILLE_VAR_SAUVE : Unable to deallocate SAUVE_T.H2OIB'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%H2OIB) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%H2OIB(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_SAUVE = err
              MessageErreur = 'SET_TAILLE_VAR_SAUVE : Unable to allocate SAUVE_T.H2OIB'
              return
           endif
        endif
      else if ( index(NomVar, 'State.Save.H2OTB') > 0) then
        if (ASSOCIATED(Instance%H2OTB)) then
           t1 = size(Instance%H2OTB)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%H2OTB, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_SAUVE = err
                 MessageErreur = 'SET_TAILLE_VAR_SAUVE : Unable to deallocate SAUVE_T.H2OTB'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%H2OTB) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%H2OTB(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_SAUVE = err
              MessageErreur = 'SET_TAILLE_VAR_SAUVE : Unable to allocate SAUVE_T.H2OTB'
              return
           endif
        endif
      else if ( index(NomVar, 'State.Save.H2OEB') > 0) then
        if (ASSOCIATED(Instance%H2OEB)) then
           t1 = size(Instance%H2OEB)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%H2OEB, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_SAUVE = err
                 MessageErreur = 'SET_TAILLE_VAR_SAUVE : Unable to deallocate SAUVE_T.H2OEB'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%H2OEB) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%H2OEB(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_SAUVE = err
              MessageErreur = 'SET_TAILLE_VAR_SAUVE : Unable to allocate SAUVE_T.H2OEB'
              return
           endif
        endif
      else if ( index(NomVar, 'State.Save.H2OSB') > 0) then
        if (ASSOCIATED(Instance%H2OSB)) then
           t1 = size(Instance%H2OSB)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%H2OSB, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_SAUVE = err
                 MessageErreur = 'SET_TAILLE_VAR_SAUVE : Unable to deallocate SAUVE_T.H2OSB'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%H2OSB) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%H2OSB(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_SAUVE = err
              MessageErreur = 'SET_TAILLE_VAR_SAUVE : Unable to allocate SAUVE_T.H2OSB'
              return
           endif
        endif
      else if ( index(NomVar, 'State.Save.H2OIC') > 0) then
        if (ASSOCIATED(Instance%H2OIC)) then
           t1 = size(Instance%H2OIC)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%H2OIC, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_SAUVE = err
                 MessageErreur = 'SET_TAILLE_VAR_SAUVE : Unable to deallocate SAUVE_T.H2OIC'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%H2OIC) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%H2OIC(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_SAUVE = err
              MessageErreur = 'SET_TAILLE_VAR_SAUVE : Unable to allocate SAUVE_T.H2OIC'
              return
           endif
        endif
      else if ( index(NomVar, 'State.Save.H2OTC') > 0) then
        if (ASSOCIATED(Instance%H2OTC)) then
           t1 = size(Instance%H2OTC)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%H2OTC, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_SAUVE = err
                 MessageErreur = 'SET_TAILLE_VAR_SAUVE : Unable to deallocate SAUVE_T.H2OTC'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%H2OTC) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%H2OTC(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_SAUVE = err
              MessageErreur = 'SET_TAILLE_VAR_SAUVE : Unable to allocate SAUVE_T.H2OTC'
              return
           endif
        endif
      else if ( index(NomVar, 'State.Save.H2OEC') > 0) then
        if (ASSOCIATED(Instance%H2OEC)) then
           t1 = size(Instance%H2OEC)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%H2OEC, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_SAUVE = err
                 MessageErreur = 'SET_TAILLE_VAR_SAUVE : Unable to deallocate SAUVE_T.H2OEC'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%H2OEC) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%H2OEC(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_SAUVE = err
              MessageErreur = 'SET_TAILLE_VAR_SAUVE : Unable to allocate SAUVE_T.H2OEC'
              return
           endif
        endif
      else if ( index(NomVar, 'State.Save.H2OSC') > 0) then
        if (ASSOCIATED(Instance%H2OSC)) then
           t1 = size(Instance%H2OSC)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%H2OSC, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_SAUVE = err
                 MessageErreur = 'SET_TAILLE_VAR_SAUVE : Unable to deallocate SAUVE_T.H2OSC'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%H2OSC) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%H2OSC(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_SAUVE = err
              MessageErreur = 'SET_TAILLE_VAR_SAUVE : Unable to allocate SAUVE_T.H2OSC'
              return
           endif
        endif
      else if ( index(NomVar, 'State.Save.SPREC') > 0) then
        if (ASSOCIATED(Instance%SPREC)) then
           t1 = size(Instance%SPREC)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%SPREC, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_SAUVE = err
                 MessageErreur = 'SET_TAILLE_VAR_SAUVE : Unable to deallocate SAUVE_T.SPREC'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%SPREC) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%SPREC(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_SAUVE = err
              MessageErreur = 'SET_TAILLE_VAR_SAUVE : Unable to allocate SAUVE_T.SPREC'
              return
           endif
        endif
      else if ( index(NomVar, 'State.Save.QPREC') > 0) then
        if (ASSOCIATED(Instance%QPREC)) then
           t1 = size(Instance%QPREC)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%QPREC, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_SAUVE = err
                 MessageErreur = 'SET_TAILLE_VAR_SAUVE : Unable to deallocate SAUVE_T.QPREC'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%QPREC) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%QPREC(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_SAUVE = err
              MessageErreur = 'SET_TAILLE_VAR_SAUVE : Unable to allocate SAUVE_T.QPREC'
              return
           endif
        endif
      !--------------------------------------------------------------------
      ! Fin de la modification de la taille des pointers de types primitifs
      !--------------------------------------------------------------------

      else
         SET_TAILLE_VAR_SAUVE = 1
         MessageErreur         = "SET_TAILLE_VAR_SAUVE - Unknown variable name"
      end if
   end function SET_TAILLE_VAR_SAUVE

! .................................................................................................................................
! Accesseurs permettant d'acceder aux valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_DOUBLE_SAUVE(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_DOUBLE_SAUVE           ! different de 0 si erreur
      type(SAUVE_T),          intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(out):: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_DOUBLE_SAUVE = 0
      valeur                = -9999999.9999
      MessageErreur          = ""

      if ( index(NomVar, 'State.Save.H2OTBS') > 0) then
         valeur = Instance%H2OTBS(index1)
      else if ( index(NomVar, 'State.Save.H2OIBS') > 0) then
         valeur = Instance%H2OIBS(index1)
      else if ( index(NomVar, 'State.Save.H2OIB') > 0) then
         valeur = Instance%H2OIB(index1)
      else if ( index(NomVar, 'State.Save.H2OTB') > 0) then
         valeur = Instance%H2OTB(index1)
      else if ( index(NomVar, 'State.Save.H2OEB') > 0) then
         valeur = Instance%H2OEB(index1)
      else if ( index(NomVar, 'State.Save.H2OSB') > 0) then
         valeur = Instance%H2OSB(index1)
      else if ( index(NomVar, 'State.Save.H2OIC') > 0) then
         valeur = Instance%H2OIC(index1)
      else if ( index(NomVar, 'State.Save.H2OTC') > 0) then
         valeur = Instance%H2OTC(index1)
      else if ( index(NomVar, 'State.Save.H2OEC') > 0) then
         valeur = Instance%H2OEC(index1)
      else if ( index(NomVar, 'State.Save.H2OSC') > 0) then
         valeur = Instance%H2OSC(index1)
      else if ( index(NomVar, 'State.Save.SPREC') > 0) then
         valeur = Instance%SPREC(index1)
      else if ( index(NomVar, 'State.Save.QPREC') > 0) then
         valeur = Instance%QPREC(index1)
      else if ( index(NomVar, 'State.Save.H2OIGS') > 0) then
         valeur = Instance%H2OIGS
      else if ( index(NomVar, 'State.Save.H2OIG') > 0) then
         valeur = Instance%H2OIG
      else if ( index(NomVar, 'State.Save.H2OTGS') > 0) then
         valeur = Instance%H2OTGS
      else if ( index(NomVar, 'State.Save.H2OTG') > 0) then
         valeur = Instance%H2OTG
      else if ( index(NomVar, 'State.Save.H2OEG') > 0) then
         valeur = Instance%H2OEG
      else if ( index(NomVar, 'State.Save.H2OSG') > 0) then
         valeur = Instance%H2OSG
      else
         GET_DOUBLE_SAUVE = 1
         valeur                = -9999999.9999
         MessageErreur         = "GET_DOUBLE_SAUVE - Unknown variable name"
      end if
   end function GET_DOUBLE_SAUVE



! .................................................................................................................................
! Mutateurs permettant de modifier les differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_DOUBLE_SAUVE(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_DOUBLE_SAUVE           ! different de 0 si erreur
      type(SAUVE_T),          intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(in) :: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_DOUBLE_SAUVE = 0
      MessageErreur          = ""

      if ( index(NomVar, 'State.Save.H2OTBS') > 0) then
         Instance%H2OTBS(index1) = valeur
      else if ( index(NomVar, 'State.Save.H2OIBS') > 0) then
         Instance%H2OIBS(index1) = valeur
      else if ( index(NomVar, 'State.Save.H2OIB') > 0) then
         Instance%H2OIB(index1) = valeur
      else if ( index(NomVar, 'State.Save.H2OTB') > 0) then
         Instance%H2OTB(index1) = valeur
      else if ( index(NomVar, 'State.Save.H2OEB') > 0) then
         Instance%H2OEB(index1) = valeur
      else if ( index(NomVar, 'State.Save.H2OSB') > 0) then
         Instance%H2OSB(index1) = valeur
      else if ( index(NomVar, 'State.Save.H2OIC') > 0) then
         Instance%H2OIC(index1) = valeur
      else if ( index(NomVar, 'State.Save.H2OTC') > 0) then
         Instance%H2OTC(index1) = valeur
      else if ( index(NomVar, 'State.Save.H2OEC') > 0) then
         Instance%H2OEC(index1) = valeur
      else if ( index(NomVar, 'State.Save.H2OSC') > 0) then
         Instance%H2OSC(index1) = valeur
      else if ( index(NomVar, 'State.Save.SPREC') > 0) then
         Instance%SPREC(index1) = valeur
      else if ( index(NomVar, 'State.Save.QPREC') > 0) then
         Instance%QPREC(index1) = valeur
      else if ( index(NomVar, 'State.Save.H2OIGS') > 0) then
         Instance%H2OIGS = valeur
      else if ( index(NomVar, 'State.Save.H2OIG') > 0) then
         Instance%H2OIG = valeur
      else if ( index(NomVar, 'State.Save.H2OTGS') > 0) then
         Instance%H2OTGS = valeur
      else if ( index(NomVar, 'State.Save.H2OTG') > 0) then
         Instance%H2OTG = valeur
      else if ( index(NomVar, 'State.Save.H2OEG') > 0) then
         Instance%H2OEG = valeur
      else if ( index(NomVar, 'State.Save.H2OSG') > 0) then
         Instance%H2OSG = valeur
      else
         SET_DOUBLE_SAUVE = 1
         MessageErreur         = "SET_DOUBLE_SAUVE - Unknown variable name"
      end if
   end function SET_DOUBLE_SAUVE



! .................................................................................................................................
! Desalloue tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function DESALLOUE_SAUVE(Instance, MessageErreur)
      implicit none
      integer                            :: DESALLOUE_SAUVE            ! different de 0 si erreur
      type(SAUVE_T),          intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      integer                            :: taille
      integer                            :: err
      DESALLOUE_SAUVE = 0
      MessageErreur          = ""
      err             = 0

      !----------------------------------------------
      ! Desallocation des pointers de types primitifs
      !----------------------------------------------
      if (ASSOCIATED(Instance%H2OIB)) then
          taille = SIZE(Instance%H2OIB)
          if (taille > 0) then
              DEALLOCATE(Instance%H2OIB, STAT=err)
              if (err /= 0) then
                  DESALLOUE_SAUVE = err
                  MessageErreur = 'Unable to deallocate SAUVE_T.H2OIB'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%H2OIB)
      if (ASSOCIATED(Instance%H2OTB)) then
          taille = SIZE(Instance%H2OTB)
          if (taille > 0) then
              DEALLOCATE(Instance%H2OTB, STAT=err)
              if (err /= 0) then
                  DESALLOUE_SAUVE = err
                  MessageErreur = 'Unable to deallocate SAUVE_T.H2OTB'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%H2OTB)
      if (ASSOCIATED(Instance%H2OEB)) then
          taille = SIZE(Instance%H2OEB)
          if (taille > 0) then
              DEALLOCATE(Instance%H2OEB, STAT=err)
              if (err /= 0) then
                  DESALLOUE_SAUVE = err
                  MessageErreur = 'Unable to deallocate SAUVE_T.H2OEB'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%H2OEB)
      if (ASSOCIATED(Instance%H2OSB)) then
          taille = SIZE(Instance%H2OSB)
          if (taille > 0) then
              DEALLOCATE(Instance%H2OSB, STAT=err)
              if (err /= 0) then
                  DESALLOUE_SAUVE = err
                  MessageErreur = 'Unable to deallocate SAUVE_T.H2OSB'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%H2OSB)
      if (ASSOCIATED(Instance%H2OIC)) then
          taille = SIZE(Instance%H2OIC)
          if (taille > 0) then
              DEALLOCATE(Instance%H2OIC, STAT=err)
              if (err /= 0) then
                  DESALLOUE_SAUVE = err
                  MessageErreur = 'Unable to deallocate SAUVE_T.H2OIC'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%H2OIC)
      if (ASSOCIATED(Instance%H2OTC)) then
          taille = SIZE(Instance%H2OTC)
          if (taille > 0) then
              DEALLOCATE(Instance%H2OTC, STAT=err)
              if (err /= 0) then
                  DESALLOUE_SAUVE = err
                  MessageErreur = 'Unable to deallocate SAUVE_T.H2OTC'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%H2OTC)
      if (ASSOCIATED(Instance%H2OEC)) then
          taille = SIZE(Instance%H2OEC)
          if (taille > 0) then
              DEALLOCATE(Instance%H2OEC, STAT=err)
              if (err /= 0) then
                  DESALLOUE_SAUVE = err
                  MessageErreur = 'Unable to deallocate SAUVE_T.H2OEC'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%H2OEC)
      if (ASSOCIATED(Instance%H2OSC)) then
          taille = SIZE(Instance%H2OSC)
          if (taille > 0) then
              DEALLOCATE(Instance%H2OSC, STAT=err)
              if (err /= 0) then
                  DESALLOUE_SAUVE = err
                  MessageErreur = 'Unable to deallocate SAUVE_T.H2OSC'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%H2OSC)
      if (ASSOCIATED(Instance%H2OTBS)) then
          taille = SIZE(Instance%H2OTBS)
          if (taille > 0) then
              DEALLOCATE(Instance%H2OTBS, STAT=err)
              if (err /= 0) then
                  DESALLOUE_SAUVE = err
                  MessageErreur = 'Unable to deallocate SAUVE_T.H2OTBS'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%H2OTBS)
      if (ASSOCIATED(Instance%H2OIBS)) then
          taille = SIZE(Instance%H2OIBS)
          if (taille > 0) then
              DEALLOCATE(Instance%H2OIBS, STAT=err)
              if (err /= 0) then
                  DESALLOUE_SAUVE = err
                  MessageErreur = 'Unable to deallocate SAUVE_T.H2OIBS'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%H2OIBS)
      if (ASSOCIATED(Instance%SPREC)) then
          taille = SIZE(Instance%SPREC)
          if (taille > 0) then
              DEALLOCATE(Instance%SPREC, STAT=err)
              if (err /= 0) then
                  DESALLOUE_SAUVE = err
                  MessageErreur = 'Unable to deallocate SAUVE_T.SPREC'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%SPREC)
      if (ASSOCIATED(Instance%QPREC)) then
          taille = SIZE(Instance%QPREC)
          if (taille > 0) then
              DEALLOCATE(Instance%QPREC, STAT=err)
              if (err /= 0) then
                  DESALLOUE_SAUVE = err
                  MessageErreur = 'Unable to deallocate SAUVE_T.QPREC'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%QPREC)
      !--------------------------------------------------------
      ! Fin de la desallocation des pointers de types primitifs
      !--------------------------------------------------------

   end function DESALLOUE_SAUVE

! .................................................................................................................................
! Rend null tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function NULLIFIER_SAUVE(Instance, MessageErreur)
      implicit none
      integer                            :: NULLIFIER_SAUVE            ! different de 0 si erreur
      type(SAUVE_T),          intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      NULLIFIER_SAUVE = 0
      MessageErreur          = ""

      !----------------------------------------------
      ! Nullifie des pointers de types primitifs
      !----------------------------------------------
      NULLIFY(Instance%H2OIB)
      NULLIFY(Instance%H2OTB)
      NULLIFY(Instance%H2OEB)
      NULLIFY(Instance%H2OSB)
      NULLIFY(Instance%H2OIC)
      NULLIFY(Instance%H2OTC)
      NULLIFY(Instance%H2OEC)
      NULLIFY(Instance%H2OSC)
      NULLIFY(Instance%H2OTBS)
      NULLIFY(Instance%H2OIBS)
      NULLIFY(Instance%SPREC)
      NULLIFY(Instance%QPREC)
      !--------------------------------------------------------
      ! Fin de la Nullification des pointers de types primitifs
      !--------------------------------------------------------

   end function NULLIFIER_SAUVE

end module M_SAUVE_T
