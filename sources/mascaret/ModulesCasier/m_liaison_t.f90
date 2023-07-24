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

module M_LIAISON_T
!***********************************************************************
! PROGICIEL : MASCARET       C. RISSOAN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   !=========================== Declarations ==============================
   use M_PRECISION            ! type DOUBLE
   use M_CONSTANTES_CASIER_C  ! constantes de calcul propres a CASIER
   use M_LIAISONRC_T          ! type liaison riviere-casier
   use M_LIAISONCC_T          ! type liaison casier-casier

   TYPE LIAISON_T
      sequence
      integer           :: TypeLiaison       ! Type de liaison (seuil,chenal, canal ou siphon)
      integer           :: NatureLiaison     ! riviere-casier ou casier-casier
      integer           :: TypeOrifice       ! indique le sens de l'ecoulement impose pour les orifices
                                             ! (orifice ou clapet)
      type(LIAISONRC_T) :: CaracRC           ! caracteristiques propres aux liaisons riviere-casier
      type(LIAISONCC_T) :: CaracCC           ! caracteristiques propres aux liaisons casier-casier
      real(DOUBLE)      :: Largeur           ! largeur de la liaison
      real(DOUBLE)      :: Longueur          ! longueur de la liaison
      real(DOUBLE)      :: Cote              ! cote de la liaison
      real(DOUBLE)      :: CoteMoyenne       ! cote d'eau moyenne dans la section
      real(DOUBLE)      :: CoefDebitOrifice  ! niveau limite a l'amont physique du clapet en dessous
                                             ! duquel ne passe aucun debit (quelque soit Zaval)
      real(DOUBLE)      :: Section           ! section du siphon ou de l'orifice rectangulaire
      real(DOUBLE)      :: Rugosite          ! rugosite de la liaison
      real(DOUBLE)      :: CoefDebitSeuil    ! coefficient de debit pour une liaison seuil
                                             ! coefficient de contraction horizontale pour un orifice
      real(DOUBLE)      :: CoefPerteCharge   ! coefficient de perte de charge pour un siphon ou un orifice
      real(DOUBLE)      :: CoefNoye          ! coefficient noye/denoye pour un seuil
      real(DOUBLE)      :: DebitEchange      ! Debit de la liaison au debut du pas de temps
      real(DOUBLE)      :: DebitPrecedent
      real(DOUBLE)      :: DebitMax          ! Debit max au cours du calcul
      real(DOUBLE)      :: TempsDebitMax     ! Temps correspondant a Debit Max
      real(DOUBLE)      :: VitesseEchange    ! Vitesse de la liaison au debut du pas de temps
      real(DOUBLE)      :: VitesseMax        ! Vitesse max au cours du calcul
      real(DOUBLE)      :: TempsVitesseMax   ! Temps correspondant a Vitesse Max

   END TYPE LIAISON_T

   contains
    ! Retourne les noms des champs du type ainsi qu'une description
    subroutine GET_TAB_VAR_LIAISON(i, tabNomVar, tabDescriptionVar)
      integer , intent(inout)                                  :: i                 ! indiceTableaux
      character(len= 40), dimension(*)                :: tabNomVar         ! Tableau des noms de variable du modele
      character(len=110), dimension(*)                :: tabDescriptionVar ! Tableau des description de variable du modele

        tabNomVar(i)         ="Model.Link.Type"
        tabDescriptionVar(i) ="Type of the link (Weir, channel, syphon or culvert)"
        i=i+1
        tabNomVar(i)         ="Model.Link.Kind"
        tabDescriptionVar(i) ="River-TO-Storage Area or Storage Area-TO-Storage Area"
        i=i+1
        tabNomVar(i)         ="Model.Link.CulverFlowDir"
        tabDescriptionVar(i) ="Flow direction for the culvert"
        i=i+1
        ! --- LIAISON_T LIAISONRC_T ---
        call GET_TAB_VAR_LIAISONRC(i, tabNomVar, tabDescriptionVar)

        ! --- LIAISON_T LIAISONCC_T ---
        call GET_TAB_VAR_LIAISONCC(i, tabNomVar, tabDescriptionVar)

        ! --- LIAISON_T (suite) ---
        tabNomVar(i)         ="Model.Link.Width"
        tabDescriptionVar(i) ="Width of the link (m)"
        i=i+1
        tabNomVar(i)         ="Model.Link.Length"
        tabDescriptionVar(i) ="Length of the link (m)"
        i=i+1
        tabNomVar(i)         ="Model.Link.Level"
        tabDescriptionVar(i) ="Level of the link (m)"
        i=i+1
        tabNomVar(i)         ="Model.Link.MeanLevel"
        tabDescriptionVar(i) ="Mean level of the link (m)"
        i=i+1
        tabNomVar(i)         ="Model.Link.CoefCulvertDischarge"
        tabDescriptionVar(i) = "Coefficient of the culvert discharge"
        i=i+1
        tabNomVar(i)         ="Model.Link.CSection"
        tabDescriptionVar(i) ="Cross section of the culvert or the syphon (m2)"
        i=i+1
        tabNomVar(i)         ="Model.Link.Roughness"
        tabDescriptionVar(i) ="Roughness (Strickler) of the link"
        i=i+1
        tabNomVar(i)         ="Model.Link.CoefWeirDischarge"
        tabDescriptionVar(i) ="Weir type discharge coefficient (weir or culvert)"
        i=i+1
        tabNomVar(i)         ="Model.Link.HeadLossCoef"
        tabDescriptionVar(i) ="Head loss coefficient (syphon or culvert)"
        i=i+1
        tabNomVar(i)         ="Model.Link.ActivationCoef"
        tabDescriptionVar(i) ="Activation coefficient for a weir"
        i=i+1
        tabNomVar(i)         ="Model.Link.FlowExchange"
        tabDescriptionVar(i) ="Flow exchanged (m3/s)"
        i=i+1
        tabNomVar(i)         ="Model.Link.PrevDischarge"
        tabDescriptionVar(i) ="Discharge value at the previous time step (m3/s)"
        i=i+1
        tabNomVar(i)         ="Model.Link.MaxDischarge"
        tabDescriptionVar(i) ="Maximal discharge value (m3/s)"
        i=i+1
        tabNomVar(i)         ="Model.Link.TimeMaxDischarge"
        tabDescriptionVar(i) ="Time corresponding to the maximal discharge value (s)"
        i=i+1
        tabNomVar(i)         ="Model.Link.ExchangeVelocity"
        tabDescriptionVar(i) ="Exchange velocity"
        i=i+1
        tabNomVar(i)         ="Model.Link.MaxVelocity"
        tabDescriptionVar(i) ="Maximal velocity value"
        i=i+1
        tabNomVar(i)         ="Model.Link.TimeMaxVelocity"
        tabDescriptionVar(i) ="Time corresponding to the maximal velocity value"
        i=i+1

      return

    end subroutine GET_TAB_VAR_LIAISON

 ! Retourne une description du champ du type au niveau de static (independant de l'instance du modele ou de l'etat)
    function GET_TYPE_VAR_LIAISON(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      implicit none

      integer                          :: GET_TYPE_VAR_LIAISON     ! different de 0 si erreur
      character(LEN=40), intent(in)    :: NomVar                   ! Nom de la variable (notation point)
      character(LEN=10), intent(out)   :: TypeVar                  ! "INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "DOUBLE" ou "TABBOOL"
      character(LEN=10), intent(out)   :: Categorie                ! "MODEL" ou "STATE"
      logical          , intent(out)   :: Modifiable               ! Si vrai alors on peut utiliser une fonction SET_XXXX_MASCARET sur la variable
      integer          , intent(out)   :: dimVar                   ! dimension (c'est a dire le nombre d'indexe de 0 a 3)
      character(LEN=256), intent(out)  :: MessageErreur            ! Message d'erreur

      GET_TYPE_VAR_LIAISON = 0
      TypeVar               = ""
      Categorie             = "MODEL"
      Modifiable            = .FALSE.
      dimVar                = 0
      MessageErreur         = ""


       if ( index(NomVar, 'Model.Link.Type') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Link.Kind') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Link.CulverFlowDir') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if (INDEX(NomVar,'Model.Link.StoR') > 0) then
          GET_TYPE_VAR_LIAISON =  GET_TYPE_VAR_LIAISONRC(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
       else if (INDEX(NomVar,'Model.Link.StoS') > 0) then
          GET_TYPE_VAR_LIAISON =  GET_TYPE_VAR_LIAISONCC(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
       else if ( index(NomVar, 'Model.Link.Width') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Link.Length') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Link.Level') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Link.MeanLevel') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Link.CoefCulvertDischarge') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Link.CSection') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Link.Roughness') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Link.CoefWeirDischarge') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Link.HeadLossCoef') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Link.ActivationCoef') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Link.FlowExchange') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Link.PrevDischarge') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Link.MaxDischarge') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Link.TimeMaxDischarge') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Link.ExchangeVelocity') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Link.MaxVelocity') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Link.TimeMaxVelocity') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
      else
        GET_TYPE_VAR_LIAISON = 1
        TypeVar = "?"
        Categorie             = "MODEL"
        Modifiable            = .FALSE.
        dimVar                = -1
        MessageErreur         = "GET_TYPE_VAR_LIAISON - Unknown variable name"
      end if


    end function GET_TYPE_VAR_LIAISON

! .................................................................................................................................
! Permet d'acceder a la taille des valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_TAILLE_VAR_LIAISON(Instance, NomVar, taille1, taille2, taille3, MessageErreur)
      implicit none
      integer                            :: GET_TAILLE_VAR_LIAISON         ! different de 0 si erreur
      type(LIAISON_T),        intent(in) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in) :: NomVar                         ! Nom de la variable du modele
      integer,                intent(out):: taille1                        ! valeur max du 1er indice
      integer,                intent(out):: taille2                        ! valeur max du 2e  indice
      integer,                intent(out):: taille3                        ! valeur max du 3e  indice
      character(LEN=256),     intent(out):: MessageErreur                  ! Message d'erreur

      GET_TAILLE_VAR_LIAISON = 0
      taille1                = 0
      taille2                = 0
      taille3                = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Link.Type') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Link.Kind') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Link.CulverFlowDir') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Link.Width') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Link.Length') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Link.Level') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Link.MeanLevel') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Link.CoefCulvertDischarge') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Link.CSection') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Link.Roughness') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Link.CoefWeirDischarge') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Link.HeadLossCoef') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Link.ActivationCoef') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Link.FlowExchange') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Link.PrevDischarge') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Link.MaxDischarge') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Link.TimeMaxDischarge') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Link.ExchangeVelocity') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Link.MaxVelocity') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Link.TimeMaxVelocity') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if (INDEX(NomVar,'Model.Link.StoR.') > 0) then
         GET_TAILLE_VAR_LIAISON = GET_TAILLE_VAR_LIAISONRC(Instance%CaracRC,&
                                               NomVar, taille1, taille2, taille3, MessageErreur)
      else if (INDEX(NomVar,'Model.Link.StoS.') > 0) then
         GET_TAILLE_VAR_LIAISON = GET_TAILLE_VAR_LIAISONCC(Instance%CaracCC,&
                                               NomVar, taille1, taille2, taille3, MessageErreur)
      else
         GET_TAILLE_VAR_LIAISON = 1
         taille1                = -1
         taille2                = -1
         taille3                = -1
         MessageErreur         = "GET_TAILLE_VAR_LIAISON - Unknown variable name"
      end if
   end function GET_TAILLE_VAR_LIAISON

! .................................................................................................................................
! Permet de modifier la taille les variables de type pointeurs fortran
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_TAILLE_VAR_LIAISON(Instance, NomVar, NewT1, NewT2, NewT3, MessageErreur)
      implicit none
      integer                               :: SET_TAILLE_VAR_LIAISON         ! different de 0 si erreur
      type(LIAISON_T),        intent(inout) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in)    :: NomVar                         ! Nom de la variable du modele
      integer,                intent(in)    :: NewT1                          ! Nouvelle valeur max du 1er indice
      integer,                intent(in)    :: NewT2                          ! Nouvelle valeur max du 2e  indice
      integer,                intent(in)    :: NewT3                          ! Nouvelle valeur max du 3e  indice
      character(LEN=256),     intent(out)   :: MessageErreur                  ! Message d'erreur

      integer err
      character(LEN=256)                 :: MessageErreurType

      SET_TAILLE_VAR_LIAISON = 0
      MessageErreur          = ""

      !-----------------------------------------------------------------------
      ! Appels aux fonctions SET_TAILLE_VAR_XXXX des membres de type derive
      !-----------------------------------------------------------------------
      if (INDEX(NomVar,'Model.Link.StoR.') > 0) then
         err = SET_TAILLE_VAR_LIAISONRC(Instance%CaracRC, &
                                   NomVar, NewT1, NewT2, NewT3, MessageErreurType)
         if (err /= 0) then
            SET_TAILLE_VAR_LIAISON = err
            MessageErreur = 'Unable to change the size of Instance%CaracRC'
            return
         endif
      else if (INDEX(NomVar,'Model.Link.StoS.') > 0) then
         err = SET_TAILLE_VAR_LIAISONCC(Instance%CaracCC, &
                                   NomVar, NewT1, NewT2, NewT3, MessageErreurType)
         if (err /= 0) then
            SET_TAILLE_VAR_LIAISON = err
            MessageErreur = 'Unable to change the size of Instance%CaracCC'
            return
         endif
      !--------------------------------------------------------------------------------
      ! Fin des appels aux fonctions SET_TAILLE_VAR_XXXX des membres de type derive
      !--------------------------------------------------------------------------------
      else
         SET_TAILLE_VAR_LIAISON = 1
         MessageErreur         = "SET_TAILLE_VAR_LIAISON - Unknown variable name"
      end if
   end function SET_TAILLE_VAR_LIAISON

! .................................................................................................................................
! Accesseurs permettant d'acceder aux valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_DOUBLE_LIAISON(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_DOUBLE_LIAISON         ! different de 0 si erreur
      type(LIAISON_T),        intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(out):: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_DOUBLE_LIAISON = 0
      valeur                = -9999999.9999
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Link.Width') > 0) then
         valeur = Instance%Largeur
      else if ( index(NomVar, 'Model.Link.Length') > 0) then
         valeur = Instance%Longueur
      else if ( index(NomVar, 'Model.Link.Level') > 0) then
         valeur = Instance%Cote
      else if ( index(NomVar, 'Model.Link.MeanLevel') > 0) then
         valeur = Instance%CoteMoyenne
      else if ( index(NomVar, 'Model.Link.CoefCulvertDischarge') > 0) then
         valeur = Instance%CoefDebitOrifice
      else if ( index(NomVar, 'Model.Link.CSection') > 0) then
         valeur = Instance%Section
      else if ( index(NomVar, 'Model.Link.Roughness') > 0) then
         valeur = Instance%Rugosite
      else if ( index(NomVar, 'Model.Link.CoefWeirDischarge') > 0) then
         valeur = Instance%CoefDebitSeuil
      else if ( index(NomVar, 'Model.Link.HeadLossCoef') > 0) then
         valeur = Instance%CoefPerteCharge
      else if ( index(NomVar, 'Model.Link.ActivationCoef') > 0) then
         valeur = Instance%CoefNoye
      else if ( index(NomVar, 'Model.Link.FlowExchange') > 0) then
         valeur = Instance%DebitEchange
      else if ( index(NomVar, 'Model.Link.PrevDischarge') > 0) then
         valeur = Instance%DebitPrecedent
      else if ( index(NomVar, 'Model.Link.MaxDischarge') > 0) then
         valeur = Instance%DebitMax
      else if ( index(NomVar, 'Model.Link.TimeMaxDischarge') > 0) then
         valeur = Instance%TempsDebitMax
      else if ( index(NomVar, 'Model.Link.ExchangeVelocity') > 0) then
         valeur = Instance%VitesseEchange
      else if ( index(NomVar, 'Model.Link.MaxVelocity') > 0) then
         valeur = Instance%VitesseMax
      else if ( index(NomVar, 'Model.Link.TimeMaxVelocity') > 0) then
         valeur = Instance%TempsVitesseMax
      else if (INDEX(NomVar,'Model.Link.StoR.') > 0) then
      GET_DOUBLE_LIAISON = GET_DOUBLE_LIAISONRC(instance%CaracRC, NomVar, index1,&
                                    index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Link.StoS.') > 0) then
      GET_DOUBLE_LIAISON = GET_DOUBLE_LIAISONCC(instance%CaracCC, NomVar, index1,&
                                    index2, index3, valeur, MessageErreur)
      else
         GET_DOUBLE_LIAISON = 1
         valeur                = -9999999.9999
         MessageErreur         = "GET_DOUBLE_LIAISON - Unknown variable name"
      end if
   end function GET_DOUBLE_LIAISON


   function GET_INT_LIAISON(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_INT_LIAISON            ! different de 0 si erreur
      type(LIAISON_T),        intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(out):: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_INT_LIAISON = 0
      valeur                = -9999
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Link.Type') > 0) then
         valeur = Instance%TypeLiaison
      else if ( index(NomVar, 'Model.Link.Kind') > 0) then
         valeur = Instance%NatureLiaison
      else if ( index(NomVar, 'Model.Link.CulverFlowDir') > 0) then
         valeur = Instance%TypeOrifice
      else if (INDEX(NomVar,'Model.Link.StoR.') > 0) then
      GET_INT_LIAISON = GET_INT_LIAISONRC(instance%CaracRC, NomVar, index1,&
                                    index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Link.StoS.') > 0) then
      GET_INT_LIAISON = GET_INT_LIAISONCC(instance%CaracCC, NomVar, index1,&
                                    index2, index3, valeur, MessageErreur)
      else
         GET_INT_LIAISON = 1
         valeur                = -9999
         MessageErreur         = "GET_INT_LIAISON - Unknown variable name"
      end if
   end function GET_INT_LIAISON



! .................................................................................................................................
! Mutateurs permettant de modifier les differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_DOUBLE_LIAISON(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_DOUBLE_LIAISON         ! different de 0 si erreur
      type(LIAISON_T),        intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(in) :: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_DOUBLE_LIAISON = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Link.Width') > 0) then
         Instance%Largeur = valeur
      else if ( index(NomVar, 'Model.Link.Length') > 0) then
         Instance%Longueur = valeur
      else if ( index(NomVar, 'Model.Link.Level') > 0) then
         Instance%Cote = valeur
      else if ( index(NomVar, 'Model.Link.MeanLevel') > 0) then
         Instance%CoteMoyenne = valeur
      else if ( index(NomVar, 'Model.Link.CoefCulvertDischarge') > 0) then
         Instance%CoefDebitOrifice = valeur
      else if ( index(NomVar, 'Model.Link.CSection') > 0) then
         Instance%Section = valeur
      else if ( index(NomVar, 'Model.Link.Roughness') > 0) then
         Instance%Rugosite = valeur
      else if ( index(NomVar, 'Model.Link.CoefWeirDischarge') > 0) then
         Instance%CoefDebitSeuil = valeur
      else if ( index(NomVar, 'Model.Link.HeadLossCoef') > 0) then
         Instance%CoefPerteCharge = valeur
      else if ( index(NomVar, 'Model.Link.ActivationCoef') > 0) then
         Instance%CoefNoye = valeur
      else if ( index(NomVar, 'Model.Link.FlowExchange') > 0) then
         Instance%DebitEchange = valeur
      else if ( index(NomVar, 'Model.Link.PrevDischarge') > 0) then
         Instance%DebitPrecedent = valeur
      else if ( index(NomVar, 'Model.Link.MaxDischarge') > 0) then
         Instance%DebitMax = valeur
      else if ( index(NomVar, 'Model.Link.TimeMaxDischarge') > 0) then
         Instance%TempsDebitMax = valeur
      else if ( index(NomVar, 'Model.Link.ExchangeVelocity') > 0) then
         Instance%VitesseEchange = valeur
      else if ( index(NomVar, 'Model.Link.MaxVelocity') > 0) then
         Instance%VitesseMax = valeur
      else if ( index(NomVar, 'Model.Link.TimeMaxVelocity') > 0) then
         Instance%TempsVitesseMax = valeur
      else if (INDEX(NomVar,'Model.Link.StoR.') > 0) then
      SET_DOUBLE_LIAISON = SET_DOUBLE_LIAISONRC(instance%CaracRC, NomVar, index1,&
                                    index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Link.StoS.') > 0) then
      SET_DOUBLE_LIAISON = SET_DOUBLE_LIAISONCC(instance%CaracCC, NomVar, index1,&
                                    index2, index3, valeur, MessageErreur)
      else
         SET_DOUBLE_LIAISON = 1
         MessageErreur         = "SET_DOUBLE_LIAISON - Unknown variable name"
      end if
   end function SET_DOUBLE_LIAISON


   function SET_INT_LIAISON(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_INT_LIAISON            ! different de 0 si erreur
      type(LIAISON_T),        intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(in) :: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_INT_LIAISON = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Link.Type') > 0) then
         Instance%TypeLiaison = valeur
      else if ( index(NomVar, 'Model.Link.Kind') > 0) then
         Instance%NatureLiaison = valeur
      else if ( index(NomVar, 'Model.Link.CulverFlowDir') > 0) then
         Instance%TypeOrifice = valeur
      else if (INDEX(NomVar,'Model.Link.StoR.') > 0) then
      SET_INT_LIAISON = SET_INT_LIAISONRC(instance%CaracRC, NomVar, index1,&
                                    index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Link.StoS.') > 0) then
      SET_INT_LIAISON = SET_INT_LIAISONCC(instance%CaracCC, NomVar, index1,&
                                    index2, index3, valeur, MessageErreur)
      else
         SET_INT_LIAISON = 1
         MessageErreur         = "SET_INT_LIAISON - Unknown variable name"
      end if
   end function SET_INT_LIAISON



! .................................................................................................................................
! Desalloue tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function DESALLOUE_LIAISON(Instance, MessageErreur)
      implicit none
      integer                            :: DESALLOUE_LIAISON          ! different de 0 si erreur
      type(LIAISON_T),        intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      integer                            :: err
      character(LEN=256)                 :: MessageErreurType
      DESALLOUE_LIAISON = 0
      MessageErreur          = ""

      !-----------------------------------------------------------------------
      ! Appels aux fonctions desalloue des membres de type derive
      !-----------------------------------------------------------------------
      err = DESALLOUE_LIAISONRC(Instance%CaracRC, MessageErreurType)
      if (err /= 0) then
          DESALLOUE_LIAISON = err
          MessageErreur = 'Unable to deallocate LIAISON_T.CaracRC'
      endif
      err = DESALLOUE_LIAISONCC(Instance%CaracCC, MessageErreurType)
      if (err /= 0) then
          DESALLOUE_LIAISON = err
          MessageErreur = 'Unable to deallocate LIAISON_T.CaracCC'
      endif
      !--------------------------------------------------------------------------------
      ! Fin des appels aux fonctions desalloue des membres de type derive
      !--------------------------------------------------------------------------------
   end function DESALLOUE_LIAISON

! .................................................................................................................................
! Rend null tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function NULLIFIER_LIAISON(Instance, MessageErreur)
      implicit none
      integer                            :: NULLIFIER_LIAISON          ! different de 0 si erreur
      type(LIAISON_T),        intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      integer                            :: err
      character(LEN=256)                 :: MessageErreurType
      NULLIFIER_LIAISON = 0
      MessageErreur          = ""

      !-----------------------------------------------------------------------
      ! Appels aux fonctions nullifier des membres de type derive
      !-----------------------------------------------------------------------
      err = NULLIFIER_LIAISONRC(Instance%CaracRC, MessageErreurType)
      if (err /= 0) then
          NULLIFIER_LIAISON = err
          MessageErreur = 'Unable to nullify LIAISON_T.CaracRC'
          return
      endif
      err = NULLIFIER_LIAISONCC(Instance%CaracCC, MessageErreurType)
      if (err /= 0) then
          NULLIFIER_LIAISON = err
          MessageErreur = 'Unable to nullify LIAISON_T.CaracCC'
          return
      endif
      !--------------------------------------------------------------------------------
      ! Fin des appels aux fonctions nullifier des membres de type derive
      !--------------------------------------------------------------------------------
   end function NULLIFIER_LIAISON

end module M_LIAISON_T
