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
   !.................................................................................................................................
   ! Exportation d'un modele Mascaret en XML
   ! .................................................................................................................................
subroutine EXPORT_XML(RetourErreur, Identifiant, NomFichier, avecDesc, exportModele)
   use M_APIMASCARET_STATIC
   use M_MASCARET_T
   use M_MODELE_MASCARET_T   ! Type MODELE_MASCARET_T

   implicit none

   integer, intent(out)                        :: RetourErreur        ! different de 0 si erreur
   integer, intent(in )                        :: Identifiant         ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   character(len=255), intent(in )             :: NomFichier          ! Nom du fichier XML cree contenant le Modele Mascaret
   logical, intent(in)                         :: avecDesc            ! si vrai, on ajoute la description de la variable
   logical, intent(in)                         :: exportModele        ! si vrai export du modele, sinon export de l'etat

   integer erreur, uniteLogique
   integer i
   character(len=40) , dimension(NB_VAR_MASCARET) :: TabNom
   character(len=110), dimension(NB_VAR_MASCARET) :: TabDesc
   character(len=40)  nomVar
   character(LEN=10) TypeVar                  ! "INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
   character(LEN=10) Categorie                ! "MODEL" ou "STATE"
   logical           Modifiable               ! Si vrai alors on peut utiliser une fonction SET_XXXX_MASCARET sur la variable
   integer           dimVar                   ! dimension (c'est a dire le nombre d'indexe de 0 a 3)
   character(LEN=256) MessageErreur           ! Message d'erreur
   logical tracerOption
   character(len=255) baliseModeleEtat

   if (exportModele) then
       baliseModeleEtat="MASCARET_Model"
   else
       baliseModeleEtat="MASCARET_State"
   endif
   RetourErreur = 0
   uniteLogique = 37

   RetourErreur = TEST_INIT_AND_ID(Identifiant, 'EXPORT_XML')
   if (RetourErreur > 0 ) then
      RETURN
   end if

   ! Test for Tracer option
   call GET_BOOL_MASCARET(RetourErreur, Identifiant, 'Model.TracerOn                          ', 0, 0, 0, tracerOption)

   call OUVERTURE_BALISE_XML(RetourErreur, Identifiant, NomFichier, uniteLogique, baliseModeleEtat)

   ! Initialisation des noms et des descriptions des variables MASCARET
   call GET_DESC_VAR_MASCARET(erreur, Identifiant, TabNom, TabDesc, NB_VAR_MASCARET)
   if (erreur /= 0) then
      RetourErreur = erreur
      RETURN
   end if

   do i=1, NB_VAR_MASCARET
      nomVar = TabNom(i)
     if(.not.((index(nomVar, '.Tracer').gt.0.or.tracerOption).and.&
        (.not.(index(nomVar, '.Tracer').gt.0.and.tracerOption)))) then
        erreur = GET_TYPE_VAR_MASC(nomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
        if (erreur /= 0) then
           RetourErreur = erreur
           RETURN
        end if

        if ((exportModele       .AND. (Categorie=='MODEL')).OR. &
           ((.NOT.exportModele) .AND. (Categorie/='MODEL'))    ) then

           call EXPORT_VAR_XML(erreur, Identifiant, uniteLogique, nomVar, avecDesc)
           if (erreur /= 0) then
              RetourErreur = erreur
              RETURN
           end if
        end if
      endif
   end do

   ! Fermeture de l'entete XML
   call FERMETURE_BALISE_XML(erreur, Identifiant, uniteLogique, baliseModeleEtat)


end subroutine EXPORT_XML

  !.................................................................................................................................
   ! Exportation d'un modele Mascaret en XML au format utilise par Saint Venant
   ! .................................................................................................................................
subroutine EXPORT_XML_SAINT_VENANT(RetourErreur, Identifiant, NomFichier)
   use M_APIMASCARET_STATIC
   use M_MASCARET_T
   use M_MODELE_MASCARET_T   ! Type MODELE_MASCARET_T
   use M_CONSTANTES_CALCUL_C ! Recuperation de la constante TETA

   implicit none

   integer, intent(out)               :: RetourErreur        ! different de 0 si erreur
   integer, intent(in )               :: Identifiant         ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   character(len=255), intent(in )    :: NomFichier          ! Nom du fichier XML cree contenant le Modele Mascaret

   integer erreur, uniteLogique
   integer i
   integer date_time(8)
   character(len=10) a, b, c
   character(len=110) descrip
   character(len=40)  nomVar
   character(len=40)  nomBalise
   character(len=10) TypeVar               ! "INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
   character(len=10) Categorie             ! "MODEL" ou "STATE"
   logical           Modifiable            ! Si vrai alors on peut utiliser une fonction SET_XXXX_MASCARET sur la variable
   logical           bDesc                 ! si vrai, on ajoute la description de la variable
   integer           dimVar                ! dimension (c'est a dire le nombre d'indexe de 0 a 3)
   character(LEN=256) MessageErreur        ! Message d'erreur
   integer index1, valInt
   character(LEN=20) tailleChar
   logical valBool
   character(LEN=256) valString
   integer taille1, taille2, taille3
   integer nbVar, nbTaille
   character(len=255) baliseModeleEtat
   ! Tableau des infos a exporter
   ! Les tailles sont initialisees a une taille max (50) et
   ! la taille reelle est conservee dans une variable
   character(LEN=40), dimension(50)  :: TabVar         ! Tableau des noms des variables a exporter
   character(LEN=40), dimension(50)  :: TabTailleVar   ! Tableau des tailles de variables a exporter
   character(LEN=40), dimension(50)  :: TabNomTaille   ! Tableau des balises pour les tailles
   character(LEN=110), dimension(50) :: TabDescTaille  ! Tableau des description des variables a exporter

   ! Liste des variables a exporter pour Saint Venant
   ! Noms des differents elements (choisir ceux  exporter)
   i = 1
   ! TabVar(i) = 'Model.File.Listing.Name'
   ! i = i + 1
   TabVar(i) = 'Model.Graph.Name'
   i = i + 1
   ! TabVar(i) = 'Model.Boundary.Name'
   ! i = i + 1
   ! TabVar(i) = 'Model.CrossSection.Name'
   ! i = i + 1
   TabVar(i) = 'Model.Inflow.Name'
   i = i + 1
   ! TabVar(i) = 'Model.LateralWeir.Name'
   ! i = i + 1
   ! TabVar(i) = 'Model.Weir.Name'
   ! i = i + 1

   ! Variables
   TabVar(i) = 'Model.Connect.FirstNdNum'
   i = i + 1
   TabVar(i) = 'Model.Connect.LastNdNum'
   i = i + 1
   TabVar(i) = 'Model.Boundary.Type'
   i = i + 1
   TabVar(i) = 'Model.Boundary.GraphNum'
   i = i + 1
   TabVar(i) = 'Model.Connect.ReachNumFreeOutflow'
   i = i + 1
   TabVar(i) = 'Model.Connect.NodeNumFreeOutflow'
   i = i + 1
   TabVar(i) = 'Model.Connect.NumReachJunction'
   i = i + 1
   TabVar(i) = 'Model.Connect.NodeNum'
   i = i + 1
   TabVar(i) = 'Model.CrossSection.Step'
   i = i + 1
   TabVar(i) = 'Model.Inflow.ReachNum'
   i = i + 1
   TabVar(i) = 'Model.Inflow.UpNode'
   i = i + 1
   TabVar(i) = 'Model.Inflow.GraphNum'
   i = i + 1
   TabVar(i) = 'Model.Weir.Node'
   i = i + 1
   TabVar(i) = 'Model.Weir.CrestLevel'
   i = i + 1
   TabVar(i) = 'Model.Weir.BrkLevel'
   i = i + 1
   TabVar(i) = 'Model.Weir.PtZ'
   i = i + 1
   TabVar(i) = 'Model.Weir.PtQ'
   i = i + 1
   TabVar(i) = 'Model.LateralWeir.UpNode'
   i = i + 1
   TabVar(i) = 'Model.LateralWeir.DownNode'
   i = i + 1
   TabVar(i) = 'Model.LateralWeir.Length'
   i = i + 1
   TabVar(i) = 'Model.LateralWeir.DischCoef'
   i = i + 1
   TabVar(i) = 'Model.LateralWeir.CrestLevel'
   i = i + 1
   TabVar(i) = 'Model.FricCoefMainCh'
   i = i + 1
   TabVar(i) = 'Model.FricCoefFP'
   i = i + 1
   TabVar(i) = 'Model.Zbot'
   i = i + 1
   TabVar(i) = 'Model.X'
   i = i + 1
   TabVar(i) = 'Model.XDT'
   i = i + 1
   TabVar(i) = 'Model.IDT'
   i = i + 1
   TabVar(i) = 'Model.VDCrossSection.P1'
   i = i + 1
   TabVar(i) = 'Model.VDCrossSection.S1'
   i = i + 1
   TabVar(i) = 'Model.VDCrossSection.B1'
   i = i + 1
   TabVar(i) = 'Model.VDCrossSection.P2'
   i = i + 1
   TabVar(i) = 'Model.VDCrossSection.S2'
   i = i + 1
   TabVar(i) = 'Model.VDCrossSection.B2'
   i = i + 1
   TabVar(i) = 'Model.VDCrossSection.BS'
   i = i + 1
   TabVar(i) = 'Model.LocalHeadLoss'
   i = i + 1
   TabVar(i) = 'Model.FirstCSReach'
   i = i + 1
   TabVar(i) = 'Model.LastCSReach'
   nbVar = i

   ! Liste et description des tailles de variables a exporter pour Saint Venant
   i = 1
   TabTailleVar(i) = 'Model.FirstCSReach'
   TabNomTaille(i) = 'nbBiefs'
   TabDescTaille(i) = 'Nombre de biefs'
   i = i + 1
   TabTailleVar(i) = 'Model.Connect.NumReachJunction'
   TabNomTaille(i) = 'nbConfluences'
   TabDescTaille(i) = 'Nombre de confluences'
   i = i + 1
   TabTailleVar(i) = 'Model.X'
   TabNomTaille(i) = 'nbSections'
   TabDescTaille(i) = 'Nombre de sections de calcul'
   i = i + 1
   TabTailleVar(i) = 'Model.CrossSection.Name'
   TabNomTaille(i) = 'nbProfils'
   TabDescTaille(i) = 'Nombre de profils'
   i = i + 1
   TabTailleVar(i) = 'Model.VDCrossSection.B1'
   TabNomTaille(i) = 'nbPasPlanim'
   TabDescTaille(i) = 'Nombre de pas de planimetrage'
   i = i + 1
   TabTailleVar(i) = 'Model.LateralWeir.Name'
   TabNomTaille(i) = 'nbDeversoirs'
   TabDescTaille(i) = 'Nombre de deversoirs'
   i = i + 1
   TabTailleVar(i) = 'Model.Connect.NodeNumFreeOutflow'
   TabNomTaille(i) = 'nbCLim'
   TabDescTaille(i) = 'Nombre de conditions au limites'
   i = i + 1
   TabTailleVar(i) = 'Model.Inflow.Name'
   TabNomTaille(i) = 'nbApports'
   TabDescTaille(i) = 'Nombre d apports'
   i = i + 1
   TabTailleVar(i) = 'Model.Weir.Name'
   TabNomTaille(i) = 'nbSing'
   TabDescTaille(i) = 'Nombre de singularites'
   i = i + 1
   TabTailleVar(i) = 'Model.Weir.PtZ'
   TabNomTaille(i) = 'nbPtsSing'
   TabDescTaille(i) = 'Nombre de points decrivant les lois de singularite'
   i = i + 1
   TabTailleVar(i) = 'Model.Inflow.GraphNum'
   TabNomTaille(i) = 'nbPtsApport'
   TabDescTaille(i) = 'Nombre de points decrivant les lois d apport'

   ! Sauvegarde de la taille du tableau
   nbTaille = i

   baliseModeleEtat='ModeleMascaretSVT'
   RetourErreur = 0
   uniteLogique = 37
   bDesc = .TRUE.
   Categorie = 'MODEL'

   if (Identifiant < 0) then
     MsgErreur = 'EXPORT_XML - Identifiant negatif : pas autorise'
     RetourErreur = 2
     RETURN
   end if
   if (Identifiant > NB_MAX_MASCARET) then
     MsgErreur = 'EXPORT_XML - Identifiant trop grand : pas autorise'
     RetourErreur = 2
     RETURN
   end if

   if (mascaretCree(Identifiant) == 0) then
     ptrMsgsErreurs(Identifiant) = 'EXPORT_XML - Mascaret n''est pas cree'
     RetourErreur = 2
     RETURN
   end if

   ! Ouverture fichier XML premiere balise <ModeleMascaretSVT>
   call OUVERTURE_BALISE_XML(RetourErreur, Identifiant, NomFichier, uniteLogique, baliseModeleEtat)

   ! Ecriture des differentes tailles de variables pour le format SVT
   !
   ! Initialisation des noms et des descriptions des variables MASCARET
   ! all GET_DESC_VAR_MASCARET(erreur, Identifiant, TabNom, TabDesc, NB_VAR_MASCARET)

   ! Ecriture du nom du modele
   nomVar = 'Model.Title'
   nomBalise = 'NomModele'
   descrip = 'Nom du modele'
   TypeVar = 'STRING'
   call GET_STRING_MASCARET(erreur, Identifiant, nomVar, 1, 1, 1, valString)
   if(len(trim(valString)).eq.0) then
      call date_and_time(a, b, c, date_time)
      b(1:2) = a(7:8)
      b(3:3) = '_'
      b(4:5) = a(5:6)
      b(6:6) = '_'
      b(7:10) = a(1:4)
      valString = 'SVT_' // b
   endif
   call EXPORT_USERVAR_XML(erreur, Identifiant, uniteLogique, nomBalise, TypeVar, descrip, valString)

   ! Ecriture des balises avec les tailles des variables pour allocation
   ! Les tailles a ecrires sont donnees dans le tableau TabTailleVar
   do i=1, nbTaille
       TypeVar = 'INT'
       nomVar = TabTailleVar(i)
       nomBalise = TabNomTaille(i)
       descrip = TabDescTaille(i)
       taille1 = 1
       taille2 = 1
       taille3 = 1
       call GET_TAILLE_VAR_MASCARET(erreur, Identifiant, nomVar, index1, taille1, taille2, taille3)
       if (erreur /= 0) then
         RetourErreur = erreur
         RETURN
       end if

       ! write(*,*) TRIM(nomVar), taille1, taille2, taille3
       if ( (nomBalise == 'nbPasPlanim').or.(nomBalise == 'nbPtsSing') ) then
         write (tailleChar,'(I10)') taille2
       else
         write (tailleChar,'(I10)') taille1
       end if

       call EXPORT_USERVAR_XML(erreur, Identifiant, uniteLogique, nomBalise, TypeVar, descrip, tailleChar)

   end do

   ! Ecriture du nombre de Pts de conditions aux limites fixe a 2
   nomBalise = 'nbPtsCLim'
   TypeVar = 'INT'
   descrip = 'nombre de points de conditions aux limites'
   tailleChar = '2'
   call EXPORT_USERVAR_XML(erreur, Identifiant, uniteLogique, nomBalise, TypeVar, descrip, tailleChar)

   ! Ecriture du Flag de convection
   nomBalise = 'FlagConvection'
   descrip = 'Flag de convection'
   TypeVar = 'BOOL'
   nomVar = 'Model.NoConvection'
   call GET_BOOL_MASCARET(Erreur, Identifiant, nomVar, 0, 0, 0, valBool)
   if(valBool.eqv..true.) then
       valInt = 1
   else
       valInt = 0
   endif
   write (tailleChar,'(I10)') valInt
   call EXPORT_USERVAR_XML(erreur, Identifiant, uniteLogique, nomBalise, TypeVar, descrip, tailleChar)

   ! Ecriture du parametre TETA
   nomBalise = 'Theta'
   descrip = 'Constante coefficient d implicitation pour la discretisation'
   TypeVar = 'DOUBLE'
   write (tailleChar,'(F6.3)') TETA
   call EXPORT_USERVAR_XML(erreur, Identifiant, uniteLogique, nomBalise, TypeVar, descrip, tailleChar)

   ! Ecriture des variables du modele demandees (voir tableau TabVar)
   do i=1, nbVar
      nomVar = TabVar(i)
      erreur = GET_TYPE_VAR_MASC(nomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      if (erreur /= 0) then
         RetourErreur = erreur
         RETURN
      end if

      call EXPORT_VAR_XML(erreur, Identifiant, uniteLogique, nomVar, bDesc)

   end do

   ! Fermeture de l'entete XML
   call FERMETURE_BALISE_XML(erreur, Identifiant, uniteLogique, baliseModeleEtat)


end subroutine EXPORT_XML_SAINT_VENANT


!-----------------------------------------------------------------------------------
! Exportation d'une variable Mascaret dans un fichier XML
! Avant d'utiliser cette procedure, il faut avoir deja execute OUVERTURE_BALISE_XML
! et avoir initialiser le modele avec IMPORT_MODELE_MASCARET.
! On peut ensuite utiliser plusieurs fois cette procedure
!-----------------------------------------------------------------------------------
subroutine EXPORT_VAR_XML(RetourErreur, Identifiant, uniteLogique, nomVar, avecDesc)
   use M_MASCARET_T
   implicit none

   integer, intent(out)          :: RetourErreur   ! different de zero en cas d'erreur
   integer, intent(in)           :: Identifiant    ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   integer, intent(in)           :: uniteLogique   ! Unite logique utilise par le fichier XML : doit egale a celle de l'ouverture
   character(len=40), intent(in) :: nomVar         ! Nom de la variable Mascaret e exporter
   logical, intent(in)           :: avecDesc       ! Si vrai, on ajoute une description en francais de la variable

   character(len=256) valString, MessageErreur
   character(len=110) description
   character(LEN=10) TypeVar                  ! "INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
   character(LEN=10) Categorie                ! "MODEL" ou "STATE"
   logical           Modifiable               ! Si vrai alors on peut utiliser une fonction SET_XXXX_MASCARET sur la variable
   integer           dimVar                   ! dimension (c'est a dire le nombre d'indexe de 0 a 3)
   integer           erreur


   RetourErreur = 0
   erreur = GET_TYPE_VAR_MASC(nomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
   RetourErreur = erreur
   ! Ecriture de la variable nomVar dans le fichier XML
   if (dimVar /=0) then
      if (avecDesc) then
        call GET_DESCRIPTION_VAR(nomVar, description)
        write(uniteLogique,"(1x,'<',A,' type=""',A,'"" description=""',A,'"" dimension=""',I1,'"">')")&
                           TRIM(nomVar),TRIM(TypeVar),TRIM(description),dimVar
      else
         write(uniteLogique,"(1x,'<',A,' type=""',A,'"" dimension=""',I1,'"">')")&
                            TRIM(nomVar),TRIM(TypeVar),dimVar
      end if
      if (dimVar==1) then
         call EXPORT_DIM1(erreur, Identifiant, nomVar, TypeVar, uniteLogique)
      elseif (dimVar==2) then
         call EXPORT_DIM2(erreur, Identifiant, nomVar, TypeVar, uniteLogique)
      elseif (dimVar==3) then
         call EXPORT_DIM3(erreur, Identifiant, nomVar, TypeVar, uniteLogique)
      endif

      write(uniteLogique,"(1x,'</',A,'>')") TRIM(nomVar)
      if (erreur /= 0) then
         RetourErreur = erreur
         RETURN
      end if


   else
      call getValeurString(erreur, Identifiant, nomVar, TypeVar, 0, 0, 0, valString)
      if (erreur /= 0) then
         RetourErreur = erreur
         RETURN
      end if

      if (avecDesc) then
         call GET_DESCRIPTION_VAR(nomVar, description)
         write(uniteLogique,&
              "(1x,'<',A,' type=""',A,'"" description=""',A,'"" dimension=""',I1,'"">',A,'</',A,'>')") &
              TRIM(nomVar),TRIM(TypeVar),TRIM(description),dimVar,TRIM(valString),TRIM(nomVar)
      else
         write(uniteLogique,"(1x,'<',A,' type=""',A,'"" dimension=""',I1,'"">',A,'</',A,'>')") &
                            TRIM(nomVar),TRIM(TypeVar),dimVar,TRIM(valString),TRIM(nomVar)
      end if
   end if
end subroutine EXPORT_VAR_XML

!-----------------------------------------------------------------------------------
! Exportation d'une variable scalaire utilisateur dans un fichier XML
! Avant d'utiliser cette procedure, il faut avoir deja execute OUVERTURE_BALISE_XML
! et avoir initialiser le modele avec IMPORT_MODELE_MASCARET.
! On peut ensuite utiliser plusieurs fois cette procedure
!-----------------------------------------------------------------------------------
subroutine EXPORT_USERVAR_XML(RetourErreur, Identifiant, uniteLogique, nomUserVar, typeVar, descVar, valeurVar)
   use M_MASCARET_T
   implicit none

   integer, intent(out)           :: RetourErreur  ! different de zero en cas d'erreur
   integer, intent(in)            :: Identifiant   ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   integer, intent(in)            :: uniteLogique  ! Unite logique utilise par le fichier XML : doit egale a celle de l'ouverture
   character(len=40), intent(in)  :: nomUserVar    ! Nom de la variable utilisateur a exporter
   character(LEN=10)              :: typeVar       ! "INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
   character(len=110), intent(in) :: descVar       ! texte de la description en francais de la variable
   character(len=20), intent(in)  :: valeurVar     ! valeur de la variable au format texte

   !character(LEN=10) Categorie                ! "MODEL" ou "STATE"
   integer           erreur


   RetourErreur = 0
   ! write(*,*) "avecDesc=",avecDesc
   !erreur = GET_TYPE_VAR_MASC(nomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
   !RetourErreur = erreur
   ! Ecriture de la variable nomVar dans le fichier XML
   ! par ex: <maVariable type="DOUBLE" description="c'est ma variable">4.88</maVariable>
   write(uniteLogique,"(1x,'<',A,' type=""',A,'"" description=""',A,'"">',A,'</',A,'>')")&
                     TRIM(nomUserVar),TRIM(typeVar),TRIM(descVar),TRIM(ADJUSTL(valeurVar)),TRIM(nomUserVar)
   if (erreur /= 0) then
      RetourErreur = erreur
      RETURN
   end if

end subroutine EXPORT_USERVAR_XML


!------------------------------------------------------------------------------------------
!  Retourne la valeur primitive d'une variable sous la forme d'une chaine de caractere
!  Fonctionne de la meme facon quelquesoit le type de la variable
!------------------------------------------------------------------------------------------
subroutine getValeurString(erreur, Identifiant, nomVar, TypeVar, index1, index2, index3, valeur)
   use M_APIMASCARET_I

   implicit none
   integer,                intent(out):: erreur                     ! erreur si different de zero
   integer,                intent(in) :: Identifiant                ! Id mascaret
   character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable
   character(len= 10),     intent(in) :: TypeVar                    ! Type de la variable
   integer,                intent(in) :: index1                     ! valeur du 1er indice
   integer,                intent(in) :: index2                     ! valeur du 2e  indice
   integer,                intent(in) :: index3                     ! valeur du 3e  indice
   character(LEN=256),     intent(out):: valeur                     ! valeur de la chaine de caractere de l'instance pour les indexes specifies

   integer valInt
   real(8) valDouble
   logical valBool
   character(LEN=256) valString
   character(LEN=12) intString
   character(LEN=35) doubleString
   character(LEN=4) vrai, faux

   vrai="VRAI"
   faux="FAUX"
   valeur = ""
   if ((TypeVar == 'INT').OR.(TypeVar == 'TABINT')) then
     call GET_INT_MASCARET(erreur, Identifiant, nomVar, index1, index2, index3, valInt)
     if (erreur /= 0) then
       return
     end if

     ! conversion de valInt en chaine de caractere (valeur) justifie a gauche
     write(intString,*) valInt
     valeur = adjustl(intString)

   elseif ((TypeVar == 'DOUBLE').OR.(TypeVar == 'TABDOUBLE')) then
     call GET_DOUBLE_MASCARET(erreur, Identifiant, nomVar, index1, index2, index3, valDouble)
     if (erreur /= 0) then
       return
     end if

    ! conversion de valDouble en chaine de caractere (valeur) justifie a gauche
     write(doubleString, *) valDouble

     valeur = adjustl(doubleString)

   elseif ((TypeVar == 'BOOL').OR.(TypeVar == 'TABBOOL')) then
     call GET_BOOL_MASCARET(erreur, Identifiant, nomVar, index1, index2, index3, valBool)
     if (erreur /= 0) then
       return
     end if
     if (valBool) then
       valeur= vrai
     else
       valeur=faux
     endif

   elseif ((TypeVar == 'STRING').OR.(TypeVar == 'TABSTRING')) then
     call GET_STRING_MASCARET(erreur, Identifiant, nomVar, index1, index2, index3, valString)
     if (erreur /= 0) then
       return
     end if
     valeur=valString
   end if

end subroutine getValeurString
!-------------------------------------------------------------------------------
!  Ecriture dans le fichier XML deja cree d'une variable Mascaret de dimension 1
!  Exemple pour un tableau de booleens :
!      <TABBOOL dim="1" taille="3">
!         <v>VRAI</v>
!         <v>VRAI</v>
!         <v>FAUX</v>
!      </TABBOOL>
!-------------------------------------------------------------------------------
subroutine EXPORT_DIM1(Erreur, Identifiant, NomVar, TypeVar, UniteLogique)
   use M_APIMASCARET_I

   implicit none
   integer,                intent(out):: Erreur                     ! erreur si different de zero
   integer,                intent(in) :: Identifiant                ! Id mascaret
   character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable
   character(len= 10),     intent(in) :: TypeVar                    ! Type de la variable
   integer,                intent(in) :: UniteLogique               ! Unite Logique du fichier

   integer i, taille1, taille2, taille3
   character(len= 256)     valeur
   character(len=12) tailleString

   taille1 = 1
   taille2 = 1
   taille3 = 1
   call GET_TAILLE_VAR_MASCARET(Erreur, Identifiant, NomVar, 1, taille1, taille2, taille3)
   if (Erreur /= 0) then
      return
   end if

   ! conversion de taille1 en chaine de caractere (tailleString)
   write(tailleString,*) taille1

   write(UniteLogique,"(2x,'<',A,' dim=""1"" taille=""',A,'"">')") TRIM(TypeVar), TRIM(adjustl(tailleString))

   do i=1,taille1
      call getValeurString(Erreur, Identifiant, NomVar, TypeVar, i, 0, 0, valeur)
      if (Erreur /= 0) then
        return
      end if
      write(uniteLogique,"(3x,'<v>',A,'</v>')") TRIM(valeur)
   end do

   write(UniteLogique,"(2x,'</',A,'>')") TRIM(TypeVar)

end subroutine EXPORT_DIM1

!-------------------------------------------------------------------------------
!  Ecriture dans le fichier XML deja cree d'une variable Mascaret de dimension 2
!  Exemple pour un tableau de reels double precision :
!      <TABDOUBLE dim="2" taille="2">
!         <TABDOUBLE dim="1" taille="1">
!            <v>0.200000000000000000E+02</v>
!         </TABDOUBLE>
!         <TABDOUBLE dim="1" taille="3">
!            <v>0.210000000000000000E+02</v>
!            <v>0.100000000000000000E+02</v>
!            <v>0.000000000000000000E+00</v>
!         </TABDOUBLE>
!      </TABDOUBLE>
!-------------------------------------------------------------------------------
subroutine EXPORT_DIM2(Erreur, Identifiant, NomVar, TypeVar, UniteLogique)
   use M_APIMASCARET_I

   implicit none
   integer,                intent(out):: Erreur                     ! erreur si different de zero
   integer,                intent(in) :: Identifiant                ! Id mascaret
   character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable
   character(len= 10),     intent(in) :: TypeVar                    ! Type de la variable
   integer,                intent(in) :: UniteLogique               ! Unite Logique du fichier

   integer i, taille1, taille2, taille3
   character(len= 256)     valeur
   character(len=12) tailleString

   integer j, t

   taille1 = 1
   taille2 = 1
   taille3 = 1
   call GET_TAILLE_VAR_MASCARET(Erreur, Identifiant, NomVar, 1, taille1, taille2, taille3)
   if (Erreur /= 0) then
      return
   end if

   ! conversion de taille1 en chaine de caractere (tailleString)
   write(tailleString,*) taille1

   write(UniteLogique,"(2x,'<',A,' dim=""2"" taille=""',A,'"">')") TRIM(TypeVar), TRIM(adjustl(tailleString))

   do i=1,taille1
      call GET_TAILLE_VAR_MASCARET(Erreur, Identifiant, NomVar, i, t, taille2, taille3)

      ! conversion de taille2 en chaine de caractere (tailleString)
      write(tailleString,*) taille2

      write(UniteLogique,"(3x,'<',A,' dim=""1"" taille=""',A,'"">')") TRIM(TypeVar), TRIM(adjustl(tailleString))
      do j=1, taille2
         call getValeurString(Erreur, Identifiant, NomVar, TypeVar, i, j, 0, valeur)
         if (Erreur /= 0) then
           return
         end if
         write(UniteLogique,"(4x,'<v>',A,'</v>')") TRIM(valeur)
      end do
      write(UniteLogique,"(3x,'</',A,'>')") TRIM(TypeVar)

   end do

   write(UniteLogique,"(2x,'</',A,'>')") TRIM(TypeVar)

end subroutine EXPORT_DIM2

!-------------------------------------------------------------------------------
!  Ecriture dans le fichier XML deja cree d'une variable Mascaret de dimension 3
!  Exemple pour un tableau d'entiers :
!      <TABINT dim="3" taille="2">
!         <TABINT dim="2" taille="1">
!            <TABINT dim="1" taille="2">
!               <v>2</v>
!               <v>2</v>
!            </TABINT>
!         </TABINT>
!         <TABINT dim="2" taille="1">
!            <TABINT dim="1" taille="2">
!               <v>2</v>
!               <v>1</v>
!            </TABINT>
!         </TABINT>
!      </TABINT>
!-------------------------------------------------------------------------------
subroutine EXPORT_DIM3(Erreur, Identifiant, NomVar, TypeVar, UniteLogique)
   use M_APIMASCARET_I

   implicit none
   integer,                intent(out):: Erreur                     ! erreur si different de zero
   integer,                intent(in) :: Identifiant                ! Id mascaret
   character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable
   character(len= 10),     intent(in) :: TypeVar                    ! Type de la variable
   integer,                intent(in) :: UniteLogique               ! Unite Logique du fichier

   integer i, taille1, taille2, taille3
   character(len= 256)     valeur
   character(len=12) tailleString

   integer j, k, t1,t2

   taille1 = 1
   taille2 = 1
   taille3 = 1
   call GET_TAILLE_VAR_MASCARET(Erreur, Identifiant, NomVar, 1, taille1, taille2, taille3)
   if (Erreur /= 0) then
       return
   end if

   ! conversion de taille1 en chaine de caractere (tailleString)
   write(tailleString,*) taille1

   write(UniteLogique,"(2x,'<',A,' dim=""3"" taille=""',A,'"">')") TRIM(TypeVar), TRIM(adjustl(tailleString))

   do i=1,taille1
      call GET_TAILLE_VAR_MASCARET(Erreur, Identifiant, NomVar, i, t1, taille2, taille3)

      ! conversion de taille2 en chaine de caractere (tailleString)
      write(tailleString,*) taille2

      write(UniteLogique,"(3x,'<',A,' dim=""2"" taille=""',A,'"">')") TRIM(TypeVar), TRIM(adjustl(tailleString))
      do j=1, taille2
         call GET_TAILLE_VAR_MASCARET(Erreur, Identifiant, NomVar, i, t1, t2, taille3)

         ! conversion de taille2 en chaine de caractere (tailleString)
         write(tailleString,*) taille3

         write(UniteLogique,"(4x,'<',A,' dim=""1"" taille=""',A,'"">')") TRIM(TypeVar), TRIM(adjustl(tailleString))
         do k=1, taille3
            call getValeurString(Erreur, Identifiant, NomVar, TypeVar, i, j, k, valeur)
            if (Erreur /= 0) then
              return
            end if
            write(UniteLogique,"(5x,'<v>',A,'</v>')") TRIM(valeur)
         end do
         write(UniteLogique,"(4x,'</',A,'>')") TRIM(TypeVar)
      end do
      write(UniteLogique,"(3x,'</',A,'>')") TRIM(TypeVar)

   end do

   write(UniteLogique,"(2x,'</',A,'>')") TRIM(TypeVar)

end subroutine EXPORT_DIM3
!-------------------------------------------------------------------------------
! Ouverture du fichier XML suivi de l'ouverture de la balise XML racine
!-------------------------------------------------------------------------------
subroutine OUVERTURE_BALISE_XML(erreur, Identifiant, NomFichier, uniteLogique, balise)
   use M_APIMASCARET_STATIC

   implicit none

   integer, intent(out)            :: erreur        ! different de zero en cas d'erreur
   integer, intent(in )            :: Identifiant   ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   character(len=255), intent(in ) :: NomFichier    ! Nom du fichier XML cree contenant le Modele Mascaret
   integer, intent(in)             :: uniteLogique  ! unite logique utilise par le fichier XML : doit etre strictement superieur a 36
   character(len=255), intent(in)  :: balise        ! balise racine du fichier XML

   character(len=10) version
   integer VersionMaj, VersionMin, VersionMic

   erreur = 0
   if (uniteLogique < 37) then
     ptrMsgsErreurs(Identifiant) = 'OUVERTURE_BALISE_XML - Unite logique incorrecte car inferieur a 37'
     erreur = 1
     RETURN
   end if


   open(unit=uniteLogique, file=NomFichier, access='SEQUENTIAL', action='WRITE', &
        form='FORMATTED', iostat=erreur, position='rewind', status='REPLACE')
   if (erreur /=0) then
     ptrMsgsErreurs(Identifiant) = 'OUVERTURE_BALISE_XML - Impossible de creer le fichier XML'
     RETURN
   end if

   ! Recuperation de la version courante de Mascaret et conversion en chaine
   ! (ex : "8.1.0")
   call VERSION_MASCARET(VersionMaj, VersionMin, VersionMic)
   write(version,'(I1,".",I1,".",I1)') VersionMaj, VersionMin, VersionMic

   ! Ecriture de l'entete XML
   write(uniteLogique,"(A)") '<?xml version="1.0" encoding="ISO-8859-1"?>'
   write(uniteLogique,"('<',A,'  version=""',A,'"">')") TRIM(balise), TRIM(version)

end subroutine OUVERTURE_BALISE_XML

!-----------------------------------------------------------------------------
! Fermeture du la balise XML racine puis fermeture du fichier
! Doit etre utiliser apres OUVERTURE_BALISE_XML et normalement apres EXPORT_VAR_XML
!-----------------------------------------------------------------------------
subroutine FERMETURE_BALISE_XML(erreur, Identifiant, uniteLogique, balise)
   use M_APIMASCARET_STATIC

   implicit none

   integer, intent(out)           :: erreur       ! different de zero en cas d'erreur
   integer, intent(in )           :: Identifiant  ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   integer, intent(in)            :: uniteLogique ! unite logique utilise par le fichier XML : doit egale a celle de l'ouverture
   character(len=255), intent(in) :: balise       ! balise racine du fichier XML

   erreur = 0
   if (uniteLogique < 37) then
     ptrMsgsErreurs(Identifiant) = 'OUVERTURE_BALISE_XML - Unite logique incorrecte car inferieur a 37'
     erreur = 1
     RETURN
   end if

   ! Fermeture de l'entete XML
   write(uniteLogique,"('</',A,'>')") TRIM(balise)
   close(uniteLogique)
end subroutine FERMETURE_BALISE_XML
