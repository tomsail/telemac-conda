//== Copyright (C) 2000-2022 EDF-CEREMA ==
//
//   This file is part of MASCARET.
//
//   MASCARET is free software: you can redistribute it and/or modify
//   it under the terms of the GNU General Public License as published by
//   the Free Software Foundation, either version 3 of the License, or
//   (at your option) any later version.
//
//   MASCARET is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//   GNU General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with MASCARET.  If not, see <http://www.gnu.org/licenses/>
//

//***********************************************************************
// PROGICIEL : MASCARET        J.-M. LACOMBE
//
// VERSION : V8P4R0               EDF-CEREMA
//***********************************************************************
/************************************************************************
 *
 * DESCRIPTION DE L'API utilise par du langage C (ou compatible)
 *
 ************************************************************************/
 #ifdef __cplusplus
 #define EXTC extern "C"
 #else
 #define EXTC
 #endif
    //.................................................................................................................................
   // Initialise les ressources associees a une instance de Mascaret (Modele et Etat)
   // Retourne l'identifiant de l'instance
   // .................................................................................................................................
   EXTC int C_CREATE_MASCARET(int *);
   // Argument en sortie :
   //      Identifiant de l'instance Mascaret utilise en argument des autres fonctions de l'API

   //.................................................................................................................................
   // Libere les ressources associees a une instance de Mascaret (Modele et Etat)
   // RQ : Depend de l'instance du modele ou de l'etat
   // .................................................................................................................................
   EXTC int C_DELETE_MASCARET(int );
   // Argument en entree :
   //      Identifiant :  Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"

   // .................................................................................................................................
   // Retourne le message d'une erreur
   // RQ : Depend de l'instance du modele ou de l'etat
   // .................................................................................................................................
   EXTC int C_GET_ERREUR_MASCARET(int *, char **);
   // Argument en entree :
   //      Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   // Argument en sortie :
   //      Message : Message d'erreur en francais de la derniere fonction appelee (max 256 caracteres)

   //.................................................................................................................................
   // Sauvegarde en memoire de l'etat courant d'une instance de Mascaret pour un usage ulterieur
   //.................................................................................................................................
   EXTC int C_SAVE_ETAT_MASCARET(int , int *);
   // Argument en entree :
   //      Identifiant :  Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   // Argument en sortie :
   //      IdentifiantEtat : identifiant de l'etat de l'instance Mascaret sauvegarde

   //.................................................................................................................................
   // Initialise l'etat courant d'une instance de Mascaret a partir d'un etat prealablement sauvegarde par SAVE_ETAT_MASCARET
   // Supprime la sauvegarde de l'etat.
   //.................................................................................................................................
   EXTC int C_SET_ETAT_MASCARET(int , int , int );
   // Arguments en entree :
   //      Identifiant     :  Identifiant de l'instance de Mascaret dont l'etat va etre modifie
   //      IdentifiantEtat :  Identifiant de l'etat Mascaret sauvegarde par SAVE_ETAT_MASCARET
   //      AndDelete       :  Efface (si !=0) ou non (si =0) l'etat sauvegarde apres restauration

   //.................................................................................................................................
   // Desallocation de tous les etats sauvegardes concernant un identifiant Mascaret
   //.................................................................................................................................
   EXTC int C_FREE_ALL_SAVE_ETAT_MASCARET(int );
   // Arguments en entree :
   //      Identifiant     :  Identifiant de l'instance de Mascaret dont les etats sauvegardes vont etre supprimes

   //.................................................................................................................................
   // Desallocation d'un etat sauvegarde
   //.................................................................................................................................
   EXTC int C_FREE_SAVE_ETAT_MASCARET(int );
   // Arguments en entree :
   //      IdentifiantEtat     :  Identifiant de l'etat Mascaret sauvegarde par SAVE_ETAT_MASCARET qui va etre supprime

   //.................................................................................................................................
   // Importation d'un modele mascaret a partir des fichiers natifs de Mascaret
   // .................................................................................................................................
   EXTC int C_IMPORT_MODELE_MASCARET(int , char *[], char *[], int , int);
   // Arguments en entree :
   //      Identifiant    : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //      TabNomFichier  : Tableau des noms des fichiers natifs Mascaret a importer (un nom de fichier ne doit pas depasser 255 caracteres)
   //      TypeNomFichier : Tableau des type des fichiers natifs Mascaret a importer, valeurs acceptees :
   //                       "dico", "casier", "geo", "loi", "cas","listing","damocle", "res", "listing_casier", "listing_liaison",
   //                       "res_casier", "res_liaison"
   //      Taille         : Taille des 2 tableaux TabNomFichier et TypeNomFichier
   //      Impression     : impression sur les fichiers listing (1-> Vrai 0-> Faux)

   //.................................................................................................................................
   // Importation d'un modele mascaret a partir d'un fichier listant les fichiers natifs de Mascaret
   // .................................................................................................................................
   EXTC int C_IMPORT_MODELE_MASCARET_ONEFILE(int *, int *, char **);
   // Arguments en entree :
   //      Identifiant    : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //      Impression     : impression sur les fichiers listing (1-> Vrai 0-> Faux)
   //      MascFile       : Fichier listant les fichiers Mascaret (.xcas, .geo, .loi, etc...)

   //.................................................................................................................................
   // Importation de l'etat Mascaret a partir du fichier natif contenant la ligne d'eau initiale
   // .................................................................................................................................
   EXTC int C_INIT_ETAT_MASCARET(int *, char **, int *);
   // Arguments en entree :
   //      Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //      NomFichier  : Nom du fichier natif contenant la ligne d'eau initiale (max 255 carateres)
   //      Impression  : impression sur les fichiers listing (1-> Vrai 0-> Faux)

   //.................................................................................................................................
   // Importation de l'etat Mascaret a partir de la ligne d'eau initiale (debit, cote) passee en argument
   // .................................................................................................................................
   EXTC int C_INIT_LIGNE_MASCARET(int , double [], double [], int );
   // Arguments en entree :
   //      Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //      Q           : Tableau des debits de la ligne d'eau initiale
   //      Z           : Tableau des cotes de la ligne d'eau initiale
   //      Taille      : Taille des 2 tableaux Q et Z

   //.................................................................................................................................
   // Renvoie la ligne d'eau (debit, cote) du modele a l'instant courant
   // .................................................................................................................................
   EXTC int C_GET_LIGNE(int *, double [], double []);
   // Arguments en entree :
   //      Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //      Q           : Tableau des debits de la ligne d'eau
   //      Z           : Tableau des cotes de la ligne d'eau

   //.................................................................................................................................
   // Importation de l'etat Tracer a partir des concentrations initiales passees en argument
   // .................................................................................................................................
   EXTC int C_INIT_LIGNE_TRACER(int , double [], int , int , int );
   // Arguments en entree :
   //      Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //      C           : Tableau 2D des concentrations initiales
   //      Taille      : Taille des 2 tableaux Q et Z
   //      NbTrac      : Nombre de traceurs
   //      Impression  : impression sur les fichiers listing (1-> Vrai 0-> Faux)

   //.................................................................................................................................
   // Renvoie les concentrations des traceurs a l'instant courant
   // .................................................................................................................................
   EXTC int C_GET_LIGNE_TRACER(int , double []);
   // Arguments en entree :
   //      Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //      C           : Tableau des concentrations des traceurs

   //.................................................................................................................................
   // Importation de l'etat Tracer a partir du fichier natif contenant la concentration initiale ou a zero
   // .................................................................................................................................
   EXTC int C_INIT_ETAT_TRACER(int , int );
   // Arguments en entree :
   //      Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //      Impression  : impression sur les fichiers listing (1-> Vrai 0-> Faux)

   //.................................................................................................................................
   // Calcul d'un nouvel etat au "TpsFinal" en utilisant le modele courant et l'etat precedent
   // .................................................................................................................................
   EXTC int C_CALCUL_MASCARET(int , double , double , double , int );
   // Arguments en entree :
   //      Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //      TpsInitial  : Temps initiale du calcul
   //      TpsFinal    : Temps final du calcul
   //      PasTps      : Pas de temps interne du calcul, si negatif ou egale a zero, le pas de temps est recupere du modele
   //      Impression  : impression sur les fichiers listing (1-> Vrai 0-> Faux)

   //.................................................................................................................................
   // Recupere le nombre de condition limite dans le modele
   // Argument en entree :
   //        Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   // Arguments en sortie :
   //        NbCL : le nombre de condition limite dans le modele
   //.................................................................................................................................
   EXTC int C_GET_NB_CONDITION_LIMITE_MASCARET(int *, int *);

   //.................................................................................................................................
   // Recupere le nom d'une condition limite du modele ainsi que le numero de la loi correspondante
   // Argument en entree :
   //        Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //        NumCL       : Le numero de la condition limite dont on veut connaitre le nom
   // Arguments en sortie :
   //        NomCL  : Le nom de la condition limite dans le modele
   //        NumLoi : Numero de la Loi correspondant a la condition limite dans le modele
   //.................................................................................................................................
   EXTC int C_GET_NOM_CONDITION_LIMITE_MASCARET(int *, int *, char **, int *);

   //.................................................................................................................................
   // Calcul d'un nouvel etat au "TpsFinal" en utilisant le modele courant, les nouvelles contions limites et l'etat precedent
   //.................................................................................................................................
   EXTC int C_CALCUL_MASCARET_CONDITION_LIMITE(int , double , double , double , double [], int , double **, double **, int );
   // Arguments en entree :
   //       Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //       TpsInitial  : Temps initiale du calcul
   //       TpsFinal    : Temps final du calcul
   //       PasTps      : Pas de temps interne du calcul
   //       TpsCl       : Vecteur temps des nouvelles conditions limites
   //       TailleTpsCL : Nombre d'element du vecteur temps des nouvelles conditions limites
   //       Cl1         : Matrice de la premiere composante des nouvelles conditions limites
   //       Cl2         : Matrice de la deuxieme composante des nouvelles conditions limites
   //       Impression  : impression sur les fichiers listing (1-> Vrai 0-> Faux)
   // .................................................................................................................................

   //.................................................................................................................................
   // Recupere la liste des variables de Mascaret accompagne d'une description
   // RQ : Ne depend pas de l'instance du modele ou de l'etat
   //.................................................................................................................................
   EXTC int C_GET_DESC_VAR_MASCARET(int , char **[], char **[], int *);
   // Argument en entree :
   //      Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   // Arguments en sortie :
   //      TabNom      : Tableau des noms de variable du modele ou de l'etat
   //      TabDesc     : Tableau des descriptions de variable du modele ou de l'etat
   //      Taille      : Taille des tableaux des noms et des descriptions de variable


   // .................................................................................................................................
   // Recupere des informations sur une variable (identifie par NomVar) :
   //   * le type (reel, entier, booleen, chaine de caractere),
   //   * la categorie (modele ou etat)
   //   * si la variable est modifiable dans l'API avec SET_XXXX_MASCARET
   //   * le nombre d'indexe pour acceder a la variable
   // RQ : Ne depend pas de l'instance du modele ou de l'etat
   // .................................................................................................................................
   EXTC int C_GET_TYPE_VAR_MASCARET(int *, char **, char **, char **, int *, int *);
   // Arguments en entree :
   //      Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //      NomVar      : Nom de la variable (notation pointe), la liste des variables est retournee par C_GET_DESC_VAR_MASCARET
   // Arguments en sortie :
   //      TypeVar     : valeurs possibles :"INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
   //      Categorie   : valeurs possibles : "MODEL" ou "STATE"
   //      Modifiable  : Si vrai alors on peut utiliser une fonction SET_XXXX_MASCARET sur la variable
   //      dimVar      : dimension (c'est a dire le nombre d'indexe de 0 a 3)

   // .................................................................................................................................
   // Accede a la taille maximum des indexes pour acceder a une variable
   // RQ : Depend de l'instance du modele ou de l'etat
   // .................................................................................................................................
   EXTC int C_GET_TAILLE_VAR_MASCARET(int *, char **, int *, int *, int *, int *);
   // Arguments en entree :
   //      Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //      NomVar      : Nom de la variable (notation pointe), la liste des variables est retournee par C_GET_DESC_VAR_MASCARET
   //      index1      : valeur du 1er indice utilise pour Profils, Lois, Singularites, Deversoirs, Extremites, Casiers et Confluents
   // Arguments en sortie :
   //      taille1     : valeur max du 1er indice
   //      taille2     : valeur max du 2e  indice
   //      taille3     : valeur max du 3e  indice

   // .................................................................................................................................
   // Definit la taille maximum des indexes pour acceder a une variable
   // RQ : Depend de l'instance du modele ou de l'etat
   // .................................................................................................................................
   EXTC int C_SET_TAILLE_VAR_MASCARET(int , char *, int ,int , int , int );
   // Arguments en entree :
   //        Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //        NomVar      : Nom de la variable (notation pointe), la liste des variables est retournee par C_GET_DESC_VAR_MASCARET
   //        index1      : valeur du 1er indice utilise pour Profils, Lois, Singularites, Deversoirs, Extremites, Casiers et Confluents
   //        taille1     : valeur max du 1er indice
   //        taille2     : valeur max du 2e  indice
   //        taille3     : valeur max du 3e  indice

   /*********************************************************************************************************************************
    *
    *  Accesseurs permettant d'acceder aux valeurs des variables d'une instance du modele ou de l'etat
    * RQ : Depend de l'instance du modele ou de l'etat
    *
    *********************************************************************************************************************************/
   EXTC int C_GET_DOUBLE_MASCARET(int *, char **, int *, int *, int *, double *);
   // Arguments en entree :
   //      Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //      NomVar      : Nom de la variable (notation pointe), la liste des variables est retournee par C_GET_DESC_VAR_MASCARET
   //      index1      : valeur du 1er indice
   //      index2      : valeur du 2e indice
   //      index3      : valeur du 3e indice
   // Argument en sortie :
   //      valeur       : valeur du double de la variable pour les indexes specifies

   int C_GET_INT_MASCARET(int *, char **, int *, int *, int *, int *);
   // Arguments en entree :
   //      Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //      NomVar      : Nom de la variable (notation pointe), la liste des variables est retournee par C_GET_DESC_VAR_MASCARET
   //      index1      : valeur du 1er indice
   //      index2      : valeur du 2e indice
   //      index3      : valeur du 3e indice
   // Argument en sortie :
   //      valeur       : valeur de l'entier de la variable pour les indexes specifies

   EXTC int C_GET_BOOL_MASCARET(int *, char **, int *, int *, int *, int *);
   // Arguments en entree :
   //      Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //      NomVar      : Nom de la variable (notation pointe), la liste des variables est retournee par C_GET_DESC_VAR_MASCARET
   //      index1      : valeur du 1er indice
   //      index2      : valeur du 2e indice
   //      index3      : valeur du 3e indice
   // Argument en sortie :
   //      valeur       : valeur du boolean (sous la forme d'un entier : 1 ->VRAI, 0 ->FAUX) de la variable pour les indexes specifies
   //      trueref      : valeur du boolean pour true sous la forme d'un entier

   EXTC int C_GET_STRING_MASCARET(int *, char **, int *, int *, int *, char **);
   // Arguments en entree :
   //      Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //      NomVar      : Nom de la variable (notation pointe), la liste des variables est retournee par C_GET_DESC_VAR_MASCARET
   //      index1      : valeur du 1er indice
   //      index2      : valeur du 2e indice
   //      index3      : valeur du 3e indice
   // Argument en sortie :
   //      valeur       : valeur de la chaine de caractere (max 256 caracteres de longueur) de la variable pour les indexes specifies

   /*********************************************************************************************************************************
    *
    * Mutateurs permettant de modifier une valeur d'une variable d'une instance du modele ou de l'etat
    * RQ : Depend de l'instance du modele ou de l'etat
    *
    *********************************************************************************************************************************/
   EXTC int C_SET_DOUBLE_MASCARET(int *, char **, int *, int *, int *, double *);
   // Arguments en entree :
   //      Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //      NomVar      : Nom de la variable (notation pointe), la liste des variables est retournee par C_GET_DESC_VAR_MASCARET
   //      index1      : valeur du 1er indice
   //      index2      : valeur du 2e indice
   //      index3      : valeur du 3e indice
   //      valeur      : nouvelle valeur de la variable pour les indexes specifies

   EXTC int C_SET_INT_MASCARET(int *, char **, int *, int *, int *, int *);
   // Arguments en entree :
   //      Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //      NomVar      : Nom de la variable (notation pointe), la liste des variables est retournee par C_GET_DESC_VAR_MASCARET
   //      index1      : valeur du 1er indice
   //      index2      : valeur du 2e indice
   //      index3      : valeur du 3e indice
   //      valeur      : nouvelle valeur de la variable pour les indexes specifies

   EXTC int C_SET_BOOL_MASCARET(int *, char **, int *, int *, int *, int *);
   // Arguments en entree :
   //      Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //      NomVar      : Nom de la variable (notation pointe), la liste des variables est retournee par C_GET_DESC_VAR_MASCARET
   //      index1      : valeur du 1er indice
   //      index2      : valeur du 2e indice
   //      index3      : valeur du 3e indice
   //      valeur      : nouvelle valeur de la variable pour les indexes specifies (sous la forme d'un entier : 1 ->VRAI, 0 ->FAUX)

   EXTC int C_SET_STRING_MASCARET(int *, char **, int *, int *, int *, char **);
   // Arguments en entree :
   //      Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //      NomVar      : Nom de la variable (notation pointe), la liste des variables est retournee par C_GET_DESC_VAR_MASCARET
   //      index1      : valeur du 1er indice
   //      index2      : valeur du 2e indice
   //      index3      : valeur du 3e indice
   //      valeur      : nouvelle valeur de la variable pour les indexes specifies

/*************************************************************************************************************************************
 *
 *  Une fonction utilitaire
 *
 *************************************************************************************************************************************/
   //.................................................................................................................................
   // Retourne la version courante de Mascaret
   // .................................................................................................................................
   EXTC int C_VERSION_MASCARET(int *, int *, int *);
   // Arguments en sortie :
   //      Majeur  : Numero de la version Majeur de Mascaret
   //      Mineur  : Numero de la version Mineur de Mascaret
   //      Micro   : Numero de la version Micro de Mascaret


/*************************************************************************************************************************************
 *
 *  Quelques fonctions utiles pour importer/exporter en XML des variables de Mascaret
 *
 *************************************************************************************************************************************/
   //.................................................................................................................................
   //  Import d'un modele ou d'un etat Mascaret depuis un fichier XML
   //.................................................................................................................................
   EXTC int C_IMPORT_XML(int , char *, int );
   // Arguments en entree :
   //      Identifiant  : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //      NomFichier   : Nom du fichier XML contenant le Modele ou l'etat Mascaret (max 132 caracteres)
   //      importModele : si vrai (valeur 1), import du modele, sinon import de l'etat

   //.................................................................................................................................
   //  Export d'un modele ou d'un etat Mascaret dans un fichier XML
   //.................................................................................................................................
   EXTC int C_EXPORT_XML(int , char *, int , int );
   // Arguments en entree :
   //      Identifiant  : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //      NomFichier   : Nom du fichier XML cree contenant le Modele ou l'etat Mascaret (max 132 caracteres)
   //      AvecDesc     : si vrai (valeur 1), ajoute la description de la variable
   //      exportModele : si vrai (valeur 1), exportation du modele, sinon export de l'etat

   //.................................................................................................................................
   //  Export d'un modele Saint Venant dans un fichier XML
   //.................................................................................................................................
   EXTC int C_EXPORT_XML_SAINT_VENANT(int , char *);
   // Arguments en entree :
   //      Identifiant  : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //      NomFichier   : Nom du fichier XML cree contenant le Modele ou l'etat Mascaret (max 132 caracteres)


   //-------------------------------------------------------------------------------
   // Ouverture du fichier XML suivi de l'ouverture de la balise XML racine
   // Ce fichier est prevu pour contenir des variables au choix de Mascaret via l'utilisation de C_EXPORT_VAR_XML
   //-------------------------------------------------------------------------------
   EXTC int C_OUVERTURE_BALISE_XML(int , char *, int , char *);
   // Arguments en entree :
   //      Identifiant  : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //      NomFichier   : Nom du fichier XML cree contenant l'entete du fichier XML (max 132 caracteres)
   //      uniteLogique : unite logique utilise par le fichier XML : doit etre strictement superieur a 36
   //      balise       : balise racine du fichier XML

   //-----------------------------------------------------------------------------------
   // Exportation d'une variable Mascaret dans un fichier XML
   // Avant d'utiliser cette procedure, il faut avoir deja execute OUVERTURE_BALISE_XML
   // et avoir initialiser le modele avec IMPORT_MODELE_MASCARET.
   // On peut ensuite utiliser plusieurs fois cette procedure
   //-----------------------------------------------------------------------------------
   EXTC int C_EXPORT_VAR_XML(int , int , char *, int );
   // Arguments en entree :
   //      Identifiant  : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //      uniteLogique : Unite logique utilise par le fichier XML - Doit egale a celle de l'ouverture (utilisation de C_OUVERTURE_BALISE_XML)
   //      nomVar       : Nom de la variable Mascaret a exporter
   //      avecDesc     : Si vrai (valeur 1), on ajoute une description en francais de la variable

    //-----------------------------------------------------------------------------------
   // Exportation d'une variable utilisateur dans un fichier XML
   // par ex: <nomUserVar type="typeVar" description="descVar">valeurVar</nomUserVar>
   // Avant d'utiliser cette procedure, il faut avoir deja execute OUVERTURE_BALISE_XML
   // et avoir initialiser le modele avec IMPORT_MODELE_MASCARET.
   // On peut ensuite utiliser plusieurs fois cette procedure
   //-----------------------------------------------------------------------------------
   EXTC int C_EXPORT_USERVAR_XML(int , int , char *, char *, char *, char *);
   // Arguments en entree :
   //      Identifiant  : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //      uniteLogique : Unite logique utilise par le fichier XML - Doit egale a celle de l'ouverture (utilisation de C_OUVERTURE_BALISE_XML)
   //      nomUserVar   : Nom de la variable Utilisateur a exporter
   //      typeVar      : chaine du type de la variable "INT", "DOUBLE", "STRING", ...
   //      descVar      : chaine de description de la variable
   //      valeurVar    : chaine de la valeur a ecrire dans la balise XML


   //------------------------------------------------------------------------------------
   // Fermeture du la balise XML racine puis fermeture du fichier
   // Doit etre utiliser apres OUVERTURE_BALISE_XML et normalement apres C_EXPORT_VAR_XML
   //------------------------------------------------------------------------------------
   EXTC int C_FERMETURE_BALISE_XML(int , int , char *);
   // Arguments en entree :
   //      Identifiant  : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //      uniteLogique : Unite logique utilise par le fichier XML - Doit egale a celle de l'ouverture (utilisation de C_OUVERTURE_BALISE_XML)
   //      balise       : balise racine du fichier XML - Doit egale a celle de l'ouverture (utilisation de C_OUVERTURE_BALISE_XML)
