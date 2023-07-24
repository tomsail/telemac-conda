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

MODULE M_APIMASCARET_I
!***********************************************************************
! PROGICIEL : MASCARET        J.-M. LACOMBE
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
  implicit none
  interface

   !.................................................................................................................................
   ! Initialise les ressources associees a une instance de Mascaret (Modele et Etat)
   ! Retourne l'identifiant de l'instance
   ! .................................................................................................................................
    subroutine CREATE_MASCARET(Erreur, Identifiant)
        implicit none
        integer, intent(out) :: Erreur                  ! different de 0 si erreur
        integer, intent(out) :: Identifiant             ! Identifiant de l'instance Mascaret utilise en argument des autres fonctions de l'API
    end subroutine CREATE_MASCARET

   !.................................................................................................................................
   ! Libere les ressources associees a une instance de Mascaret (Modele et Etat)
   ! RQ : Depend de l'instance du modele ou de l'etat
   ! .................................................................................................................................
    subroutine DELETE_MASCARET(Erreur, Identifiant)
        implicit none
        integer, intent(out) :: Erreur                  ! different de 0 si erreur
        integer, intent(in ) :: Identifiant             ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
    end subroutine DELETE_MASCARET

   ! .................................................................................................................................
   ! Retourne le message d'une erreur
   ! RQ : Depend de l'instance du modele ou de l'etat
   ! .................................................................................................................................
    subroutine GET_ERREUR_MASCARET(Erreur, Identifiant, Message)
        implicit none
        integer, intent(out)                         :: Erreur                ! different de 0 si erreur
        integer, intent(in )                         :: Identifiant           ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
        character(LEN=256), intent(out)              :: Message               ! Message d'erreur
    end subroutine GET_ERREUR_MASCARET

   !.................................................................................................................................
   ! Sauvegarde en memoire de l'etat courant d'une instance de Mascaret pour un usage ulterieur
   !.................................................................................................................................
    subroutine SAVE_ETAT_MASCARET(Erreur, Identifiant, IdentifiantEtat)
        implicit none
        integer, intent(out) :: Erreur                ! different de 0 si erreur
        integer, intent(in ) :: Identifiant           ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
        integer, intent(out) :: IdentifiantEtat       ! Identifiant de l'etat Mascaret sauvegarde
    end subroutine SAVE_ETAT_MASCARET

   !.................................................................................................................................
   ! Supprime l'etat courant d'une instance de Mascaret et l'initialise a partir d'un etat prealablement sauvegarde par SAVE_ETAT_MASCARET
   ! Supprime la sauvegarde de l'State.
   !.................................................................................................................................
    subroutine SET_ETAT_MASCARET(Erreur, Identifiant, IdentifiantEtat, AndDelete)
        implicit none
        integer, intent(out) :: Erreur                ! different de 0 si erreur
        integer, intent(in ) :: Identifiant           ! Identifiant de l'instance de Mascaret dont l'etat est modifie
        integer, intent(in ) :: IdentifiantEtat       ! Identifiant de l'etat Mascaret sauvegarde
        integer, intent(in ) :: AndDelete             ! Efface l'etat sauvegarde si different de zero
    end subroutine SET_ETAT_MASCARET
    
   !.................................................................................................................................
   ! DESALLOCATION de tous les etats sauvegardes concernant un identifiant Mascaret
   !.................................................................................................................................
    subroutine FREE_ALL_SAVE_ETAT_MASCARET(Erreur, Identifiant)
        implicit none
        integer, intent(out) :: Erreur                ! different de 0 si erreur
        integer, intent(in ) :: Identifiant           ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
    end subroutine FREE_ALL_SAVE_ETAT_MASCARET

   !.................................................................................................................................
   ! DESALLOCATION d'un etat sauvegarde
   !.................................................................................................................................
    subroutine FREE_SAVE_ETAT_MASCARET(Erreur, IdentifiantEtat)
        implicit none
        integer, intent(out) :: Erreur                ! different de 0 si erreur
        integer, intent(in ) :: IdentifiantEtat       ! Identifiant de l'etat Mascaret sauvegarde retourne par "SAVE_ETAT_MASCARET"
    end subroutine FREE_SAVE_ETAT_MASCARET

   !.................................................................................................................................
   ! Importation d'un modele mascaret a partir des fichiers natifs de Mascaret
   ! .................................................................................................................................
   subroutine IMPORT_MODELE_MASCARET(Erreur, Identifiant, TabNomFichier, TypeNomFichier, Taille, Impression)
       implicit none
       integer, intent(out)                         :: Erreur         ! different de 0 si erreur
       integer, intent(in )                         :: Identifiant    ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
       character(LEN=255), dimension(*), intent(in) :: TabNomFichier  ! Tableau des noms des fichiers natifs Mascaret a importer
       character(LEN=40),  dimension(*), intent(in) :: TypeNomFichier ! Tableau des type des fichiers natifs Mascaret a importer:
                                                                      ! "dico", "casier", "geo", "loi", "cas","listing","damocle",
                                                                      ! "res", "listing_casier", "listing_liaison", "res_casier", "res_liaison"
       integer, intent(in )                         :: Taille         ! Taille des 2 tableaux TabNomFichier et TypeNomFichier
       integer, intent(in )                         :: Impression     ! impression sur les fichiers listing (1-> Vrai 0-> Faux)
   end subroutine IMPORT_MODELE_MASCARET


   !.................................................................................................................................
   ! Importation de l'etat Mascaret a partir du fichier natif contenant la ligne d'eau initiale
   ! .................................................................................................................................
   subroutine INIT_ETAT_MASCARET(Erreur, Identifiant, NomFichier, Impression)
       implicit none

       integer, intent(out)                        :: Erreur         ! different de 0 si erreur
       integer, intent(in )                        :: Identifiant    ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
       character(LEN=255), intent(in)              :: NomFichier     ! Nom du fichier natif contenant la ligne d'eau initiale
       integer, intent(in )                        :: Impression     ! impression sur les fichiers listing (1-> Vrai 0-> Faux)
   end subroutine INIT_ETAT_MASCARET

   !.................................................................................................................................
   ! Importation de l'etat Mascaret a partir de la ligne d'eau initiale (debit, cote) passee en argument
   ! .................................................................................................................................
   subroutine INIT_LIGNE_MASCARET(Erreur, Identifiant, Q, Z, Taille)
       implicit none

       integer, intent(out)                        :: Erreur         ! different de 0 si erreur
       integer                                     :: Identifiant          ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
       real(8), dimension(*), intent(in)           :: Q                    ! Tableau des debits de la ligne d'eau initiale
       real(8), dimension(*), intent(in)           :: Z                    ! Tableau des cotes de la ligne d'eau initiale
       integer, intent(in )                        :: Taille               ! Taille des 2 tableaux Q et Z
   end subroutine INIT_LIGNE_MASCARET

   !.................................................................................................................................
   ! Importation de l'etat Tracer a partir du fichier natif contenant la concentration initiale ou a zero
   ! .................................................................................................................................
   subroutine INIT_ETAT_TRACER(Erreur, Identifiant, Impression)
       implicit none

       integer, intent(out)                        :: Erreur         ! different de 0 si erreur
       integer, intent(in )                        :: Identifiant    ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
       integer, intent(in )                        :: Impression     ! impression sur les fichiers listing (1-> Vrai 0-> Faux)
   end subroutine INIT_ETAT_TRACER

   !.................................................................................................................................
   ! Calcul d'un nouvel etat au "TpsFinal" en utilisant le modele courant et l'etat precedent
   ! .................................................................................................................................
   subroutine CALCUL_MASCARET(Erreur, Identifiant, TpsInitial, TpsFinal, PasTps, Impression)
       implicit none

       integer, intent(out)                        :: Erreur              ! different de 0 si erreur
       integer, intent(in )                        :: Identifiant         ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
       real(8), intent(in )                        :: TpsInitial          ! Temps initial du calcul
       real(8), intent(in )                        :: TpsFinal            ! Temps final du calcul
       real(8), intent(in )                        :: PasTps              ! Pas de temps interne du calcul
       integer, intent(in )                        :: Impression          ! impression sur les fichiers listing (1-> Vrai 0-> Faux)
   end subroutine CALCUL_MASCARET

   !.................................................................................................................................
   ! Recupere le nombre de condition limite dans le modele
   !.................................................................................................................................
   subroutine GET_NB_CONDITION_LIMITE_MASCARET(Erreur, Identifiant, NbCL)
       implicit none
       integer, intent(out)                         :: Erreur             ! different de 0 si erreur
       integer , intent(in)                         :: Identifiant        ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
       integer , intent(out)                        :: NbCL               ! Nombre de condition limite dans le modele
   end subroutine GET_NB_CONDITION_LIMITE_MASCARET

   !.................................................................................................................................
   ! Recupere le nom de la condition limite dans le modele
   !.................................................................................................................................
   subroutine GET_NOM_CONDITION_LIMITE_MASCARET(Erreur, Identifiant, NumCL, NomCL, NumLoi)
       implicit none
       integer, intent(out)                         :: Erreur             ! different de 0 si erreur
       integer , intent(in)                         :: Identifiant        ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
       integer , intent(in)                         :: NumCL              ! Numero de la condition limite dans le modele
       character(30), intent(out)                   :: NomCL              ! Nom de la condition limite identifie par son numero
       integer , intent(out)                        :: NumLoi             ! Numero de la Loi correspondant a la condition limite dans le modele
   end subroutine GET_NOM_CONDITION_LIMITE_MASCARET

   !.................................................................................................................................
   ! Calcul d'un nouvel etat au "TpsFinal" en utilisant le modele courant et l'etat precedent et les nouvelles conditions limites
   ! .................................................................................................................................
   subroutine CALCUL_MASCARET_CONDITION_LIMITE(Erreur, Identifiant, TpsInitial, TpsFinal, PasTps, &
                                               TpsCl, TailleTpsCL, Cl1, CL2, Impression)
       implicit none

       integer, intent(out)                        :: Erreur              ! different de 0 si erreur
       integer, intent(in )                        :: Identifiant         ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
       real(8), intent(in )                        :: TpsInitial          ! Temps initial du calcul
       real(8), intent(in )                        :: TpsFinal            ! Temps final du calcul
       real(8), intent(in )                        :: PasTps              ! Pas de temps interne du calcul
       real(8), dimension(*),   intent(in)         :: TpsCl               ! Le vecteur temps commun aux conditons limites - Dimension : TailleTpsCL
       integer, intent(in )                        :: TailleTpsCL         ! Nombre de pas de temps pour les conditions limites
       real(8), dimension(TailleTpsCL,*), intent(in)           :: Cl1                 ! Composante 1 de la condition limite (type1->Q, type2->Z, type3->Q, type7->Zinf
                                                                          ! Dimension : (NbCL x TailleTpsCL)
       real(8), dimension(TailleTpsCL,*), intent(in)           :: Cl2                 ! Composante 2 de la condition limite (type1 et type2->Pas utilise, type3->Z, type7->Zsup
                                                                          ! Dimension : (NbCL x TailleTpsCL)
       integer, intent(in )                        :: Impression          ! impression sur les fichiers listing (1-> Vrai 0-> Faux)
   end subroutine CALCUL_MASCARET_CONDITION_LIMITE
   !.................................................................................................................................
   ! Recupere la liste des variables de Mascaret accompagne d'une description
   ! RQ : Ne depend pas de l'instance du modele ou de l'etat
   !.................................................................................................................................
   subroutine GET_DESC_VAR_MASCARET(Erreur, Identifiant, TabNom, TabDesc, Taille)
       implicit none
       integer, intent(out)                         :: Erreur             ! different de 0 si erreur
       integer , intent(in)                         :: Identifiant        ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
       character(len= 40), dimension(*), intent(out):: TabNom             ! Tableau des noms de variable du modele ou de l'etat
       character(len=110), dimension(*), intent(out):: TabDesc            ! Tableau des descriptions de variable du modele ou de l'etat
       integer, intent(in)                          :: Taille             ! Taille des tableaux des noms et des descriptions de variable : doit etre egale a "NbVarMascaret"
    end subroutine GET_DESC_VAR_MASCARET

   !.................................................................................................................................
   ! Retourne de nombre de variable dans Mascaret
   ! C'est a dire la taille des tableaux TabNom et TabDesc
   ! .................................................................................................................................
    subroutine GET_NB_VAR_MASCARET(NbVarMascaret)
        implicit none
        integer, intent(out) :: NbVarMascaret ! different de 0 si erreur
    end subroutine GET_NB_VAR_MASCARET

   ! .................................................................................................................................
   ! Recupere des informations sur une variable (identifie par NomVar) :
   !   * le type (reel, entier, booleen, chaine de caractere),
   !   * la categorie (modele ou etat)
   !   * si la variable est modifiable dans l'API avec SET_XXXX_MASCARET
   !   * le nombre d'indexe pour acceder a la variable
   ! RQ : Ne depend pas de l'instance du modele ou de l'etat
   ! .................................................................................................................................
    subroutine GET_TYPE_VAR_MASCARET(Erreur, Identifiant, NomVar, TypeVar, Categorie, Modifiable, dimVar)
        implicit none
        integer          , intent(out)   :: Erreur                   ! different de 0 si erreur
        integer          , intent(in)    :: Identifiant              ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
        character(LEN=40), intent(in)    :: NomVar                   ! Nom de la variable (notation pointe)
        character(LEN=10), intent(out)   :: TypeVar                  ! "INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
        character(LEN=10), intent(out)   :: Categorie                ! "MODEL" ou "STATE"
        integer          , intent(out)   :: Modifiable               ! Si vrai alors on peut utiliser une fonction SET_XXXX_MASCARET sur la variable
        integer          , intent(out)   :: dimVar                   ! dimension (c'est a dire le nombre d'indexe de 0 a 3)
    end subroutine GET_TYPE_VAR_MASCARET

   ! .................................................................................................................................
   ! Accede a la taille maximum des indexes pour acceder a une variable
   ! RQ : Depend de l'instance du modele ou de l'etat
   ! .................................................................................................................................
   subroutine GET_TAILLE_VAR_MASCARET(Erreur, Identifiant, NomVar, index1, taille1, taille2, taille3)
       implicit none
       integer,                intent(out):: Erreur                         ! different de 0 si erreur
       integer,                intent(in) :: Identifiant                    ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
       character(len= 40),     intent(in) :: NomVar                         ! Nom de la variable du modele ou de l'etat
       integer,                intent(in) :: index1                         ! valeur du 1er indice utilise pour Profils, Lois, Singularites, Deversoirs, Extremites, Casiers et Confluents
       integer,                intent(out):: taille1                        ! valeur max du 1er indice
       integer,                intent(out):: taille2                        ! valeur max du 2e  indice
       integer,                intent(out):: taille3                        ! valeur max du 3e  indice
   end subroutine GET_TAILLE_VAR_MASCARET

   ! .................................................................................................................................
   ! Permet de definir les tailles maximum des valeurs d'une variable
   ! RQ : Depend de l'instance du modele ou de l'etat
   ! .................................................................................................................................
   subroutine SET_TAILLE_VAR_MASCARET(Erreur, Identifiant, NomVar, index1, taille1, taille2, taille3)
       implicit none
       integer,                intent(out):: Erreur                         ! different de 0 si erreur
       integer,                intent(in) :: Identifiant                    ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
       character(len= 40),     intent(in) :: NomVar                         ! Nom de la variable du modele ou de l'etat
       integer,                intent(in) :: index1                         ! valeur du 1er indice utilise pour Profils, Lois, Singularites, Deversoirs, Extremites, Casiers et Confluents
       integer,                intent(in) :: taille1                        ! valeur max du 1er indice
       integer,                intent(in) :: taille2                        ! valeur max du 2e  indice
       integer,                intent(in) :: taille3                        ! valeur max du 3e  indice
   end subroutine SET_TAILLE_VAR_MASCARET 

   ! .................................................................................................................................
   ! Accesseurs permettant d'acceder aux valeurs d'une instance du modele ou de l'etat
   ! RQ : Depend de l'instance du modele ou de l'etat
   ! .................................................................................................................................
   subroutine GET_DOUBLE_MASCARET(Erreur, Identifiant, NomVar, index1, index2, index3, valeur)
      implicit none
      integer,                intent(out):: Erreur                     ! different de 0 si erreur
      integer,                intent(in) :: Identifiant                ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(8),                intent(out):: valeur                     ! valeur du real(8) de l'instance pour les indexes specifies
   end subroutine GET_DOUBLE_MASCARET

   subroutine GET_INT_MASCARET(Erreur, Identifiant, NomVar, index1, index2, index3, valeur)
      implicit none
      integer,                intent(out):: Erreur                     ! different de 0 si erreur
      integer,                intent(in) :: Identifiant                ! Id mascaret
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(out):: valeur                     ! valeur de l'entier de l'instance pour les indexes specifies
   end subroutine GET_INT_MASCARET

   subroutine GET_BOOL_MASCARET(Erreur, Identifiant, NomVar, index1, index2, index3, valeur)
      implicit none
      integer,                intent(out):: Erreur                     ! different de 0 si erreur
      integer,                intent(in) :: Identifiant                ! Id mascaret
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      logical,                intent(out):: valeur                     ! valeur du boolean de l'instance pour les indexes specifies
   end subroutine GET_BOOL_MASCARET

   subroutine GET_STRING_MASCARET(Erreur, Identifiant, NomVar, index1, index2, index3, valeur)
      implicit none
      integer,                intent(out):: Erreur                     ! different de 0 si erreur
      integer,                intent(in) :: Identifiant                ! Id mascaret
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      character(LEN=256),     intent(out):: valeur                     ! valeur de la chaine de caractere de l'instance pour les indexes specifies
   end subroutine GET_STRING_MASCARET

   ! .................................................................................................................................
   ! Mutateurs permettant de modifier une valeur d'une variable d'une instance du modele ou de l'etat
   ! RQ : Depend de l'instance du modele ou de l'etat
   ! .................................................................................................................................
   subroutine SET_DOUBLE_MASCARET(Erreur, Identifiant, NomVar, index1, index2, index3, valeur)
      implicit none
      integer,                intent(out):: Erreur                     ! different de 0 si erreur
      integer,                intent(in) :: Identifiant                ! Id mascaret
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(8),                intent(in) :: valeur                     ! valeur du real(8) de l'instance pour les indexes specifies
   end subroutine SET_DOUBLE_MASCARET

   subroutine SET_INT_MASCARET(Erreur, Identifiant, NomVar, index1, index2, index3, valeur)
      implicit none
      integer,                intent(out):: Erreur                     ! different de 0 si erreur
      integer,                intent(in) :: Identifiant                ! Id mascaret
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(in) :: valeur                     ! valeur de l'entier de l'instance pour les indexes specifies
   end subroutine SET_INT_MASCARET

   subroutine SET_BOOL_MASCARET(Erreur, Identifiant, NomVar, index1, index2, index3, valeur)
      implicit none
      integer,                intent(out):: Erreur                     ! different de 0 si erreur
      integer,                intent(in) :: Identifiant                ! Id mascaret
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      logical,                intent(in) :: valeur                     ! valeur du boolean de l'instance pour les indexes specifies
   end subroutine SET_BOOL_MASCARET

   subroutine SET_STRING_MASCARET(Erreur, Identifiant, NomVar, index1, index2, index3, valeur)
      implicit none
      integer,                intent(out):: Erreur                     ! different de 0 si erreur
      integer,                intent(in) :: Identifiant                ! Id mascaret
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      character(LEN=256),     intent(in) :: valeur                     ! valeur de la chaine de caractere de l'instance pour les indexes specifies
   end subroutine SET_STRING_MASCARET

   !.................................................................................................................................
   ! Retourne la version courante de Mascaret
   ! .................................................................................................................................
    subroutine VERSION_MASCARET(Majeur, Mineur, Micro)
        implicit none
        integer, intent(out) :: Majeur  ! Numero de la version Majeur de Mascaret
        integer, intent(out) :: Mineur  ! Numero de la version Mineur de Mascaret
        integer, intent(out) :: Micro   ! Numero de la version Micro de Mascaret
    end subroutine VERSION_MASCARET

    !.................................................................................................................................
    !  Import d'un modele Mascaret depuis un fichier XML
    !.................................................................................................................................
    subroutine IMPORT_XML(Erreur, Identifiant, NomFichier, importModele)
        implicit none

        integer, intent(out)             :: Erreur        ! different de 0 si erreur
        integer, intent(in )             :: Identifiant   ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
        character(len=255), intent(in )  :: NomFichier    ! Nom du fichier XML contenant le Modele Mascaret
        logical, intent(in)              :: importModele  ! si vrai import du modele, sinon import de l'etat
    end subroutine IMPORT_XML

    !.................................................................................................................................
    !  Export d'un modele Mascaret dans un fichier XML
    !.................................................................................................................................
    subroutine EXPORT_XML(Erreur, Identifiant, NomFichier, AvecDesc, exportModele)
        implicit none

        integer, intent(out)             :: Erreur        ! different de 0 si erreur
        integer, intent(in )             :: Identifiant   ! Identifiant de l'instance Mascaret a exporter retourne par "CREATE_MASCARET"
        character(len=255), intent(in )  :: NomFichier    ! Nom du fichier XML cree contenant le Modele Mascaret
        logical, intent(in)              :: AvecDesc      ! si vrai, on ajoute la description de la variable
        logical, intent(in)              :: exportModele  ! si vrai export du modele, sinon export de l'etat
   end subroutine EXPORT_XML

   !.................................................................................................................................
   !  Export d'un modele Mascaret dans un fichier XML au format Saint Venant
   !.................................................................................................................................
   subroutine EXPORT_XML_SAINT_VENANT(Erreur, Identifiant, NomFichier)
        implicit none

        integer, intent(out)             :: Erreur        ! different de 0 si erreur
        integer, intent(in )             :: Identifiant   ! Identifiant de l'instance Mascaret a exporter retourne par "CREATE_MASCARET"
        character(len=255), intent(in )  :: NomFichier    ! Nom du fichier XML cree contenant le Modele Mascaret
   end subroutine EXPORT_XML_SAINT_VENANT

   
   !-------------------------------------------------------------------------------
   ! Ouverture du fichier XML suivi de l'ouverture de la balise XML racine
   !-------------------------------------------------------------------------------
   subroutine OUVERTURE_BALISE_XML(erreur, Identifiant, NomFichier, uniteLogique, balise)
      implicit none

      integer, intent(out)            :: erreur        ! different de zero en cas d'erreur
      integer, intent(in )            :: Identifiant   ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
      character(len=255), intent(in ) :: NomFichier    ! Nom du fichier XML cree contenant le Modele Mascaret
      integer, intent(in)             :: uniteLogique  ! unite logique utilise par le fichier XML : doit etre strictement superieur a 36
      character(len=255), intent(in)  :: balise        ! balise racine du fichier XML
   end subroutine OUVERTURE_BALISE_XML

   !-----------------------------------------------------------------------------------
   ! Exportation d'une variable Mascaret dans un fichier XML
   ! Avant d'utiliser cette procedure, il faut avoir deja execute OUVERTURE_BALISE_XML
   ! et avoir initialiser le modele avec IMPORT_MODELE_MASCARET.
   ! On peut ensuite utiliser plusieurs fois cette procedure
   !-----------------------------------------------------------------------------------
   subroutine EXPORT_VAR_XML(RetourErreur, Identifiant, uniteLogique, nomVar, avecDesc)
      implicit none

      integer, intent(out)          :: RetourErreur   ! different de zero en cas d'erreur
      integer, intent(in)           :: Identifiant    ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
      integer, intent(in)           :: uniteLogique   ! Unite logique utilise par le fichier XML : doit egale a celle de l'ouverture
      character(len=40), intent(in) :: nomVar         ! Nom de la variable Mascaret e exporter
      logical, intent(in)           :: avecDesc       ! Si vrai, on ajoute une description en francais de la variable
   end subroutine EXPORT_VAR_XML

   !-----------------------------------------------------------------------------------
   ! Exportation d'une variable scalaire utilisateur dans un fichier XML
   ! Avant d'utiliser cette procedure, il faut avoir deja execute OUVERTURE_BALISE_XML
   ! et avoir initialiser le modele avec IMPORT_MODELE_MASCARET.
   ! On peut ensuite utiliser plusieurs fois cette procedure
   !-----------------------------------------------------------------------------------
   subroutine EXPORT_USERVAR_XML(RetourErreur, Identifiant, uniteLogique, nomUserVar, typeVar, descVar, valeurVar)
      implicit none
      
      integer, intent(out)           :: RetourErreur  ! different de zero en cas d'erreur
      integer, intent(in)            :: Identifiant   ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
      integer, intent(in)            :: uniteLogique  ! Unite logique utilise par le fichier XML : doit egale a celle de l'ouverture
      character(len=40), intent(in)  :: nomUserVar    ! Nom de la variable utilisateur a exporter
      character(LEN=10)              :: typeVar       ! "INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
      character(len=110), intent(in) :: descVar       ! texte de la description en français de la variable
      character(len=20), intent(in)  :: valeurVar     ! valeur de la variable au format texte
   end subroutine EXPORT_USERVAR_XML

   
   !...............................................................................
   ! Fermeture du la balise XML racine puis fermeture du fichier
   ! Doit etre utiliser apres OUVERTURE_BALISE_XML et normalement apres EXPORT_VAR_XML
   !-----------------------------------------------------------------------------
   subroutine FERMETURE_BALISE_XML(erreur, Identifiant, uniteLogique, balise)
      implicit none

      integer, intent(out)     :: erreur       ! different de zero en cas d'erreur
      integer, intent(in )     :: Identifiant  ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
      integer, intent(in)      :: uniteLogique ! unite logique utilise par le fichier XML : doit egale a celle de l'ouverture
      character(len=255), intent(in) :: balise ! balise racine du fichier XML
   end subroutine FERMETURE_BALISE_XML
  end interface
end module M_APIMASCARET_I
