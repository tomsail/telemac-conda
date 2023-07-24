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
// VERSION : V8P4R0              EDF-CEREMA
//***********************************************************************
/************************************************************************
 *
 * IMPLEMENTAION DE L'API
 *
 ************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "apimascaret.h"

// Signatures des subroutines fortrans utilisees
// Elles peuvent dependre legerement du compilateur utilise
// Dans le cas de gfortran : le nom est en minuscule et un underscore a la fin du nom.
void create_mascaret_(int *, int *);
void delete_mascaret_(int *, int *);
void save_etat_mascaret_(int *, int *, int *);
void set_etat_mascaret_(int *, int *, int *, int *);
void free_all_save_etat_mascaret_(int *, int *);
void free_save_etat_mascaret_(int *, int *);
void get_erreur_mascaret_(int *, int *, char *, int);
void import_modele_mascaret_(int *, int *, char[], char[], int *, int *, char *, int, int, int);
void init_etat_mascaret_(int *, int *, char *, int *, int);
void init_ligne_mascaret_(int *, int *, double[], double[], int *);
void init_ligne_tracer_(int *, int *, double[], int *, int *, int *);
void init_etat_tracer_(int *, int *, int *);
void get_ligne_(int *, int *, double[], double[]);
void get_ligne_tracer_(int *, int *, double[]);
void calcul_mascaret_(int *, int *, double *, double *, double *, int *);
void get_nb_condition_limite_mascaret_(int *, int *, int *);
void get_nom_condition_limite_mascaret_(int *, int *, int *, char *, int *, int);
void calcul_mascaret_condition_limite_(int *, int *, double *, double *, double *, double[], int *, double *, double *, int *);
void get_nb_var_mascaret_(int *);
void get_desc_var_mascaret_(int *, int *, char[], char[], int *, int, int);
void get_type_var_mascaret_(int *, int *, char *, char *, char *, int *, int *, int, int, int);
void get_taille_var_mascaret_(int *, int *, char *, int *, int *, int *, int *, int);
void set_taille_var_mascaret_(int *, int *, char *, int *, int *, int *, int *, int);
void get_double_mascaret_(int *, int *, char *, int *, int *, int *, double *, int);
void get_int_mascaret_(int *, int *, char *, int *, int *, int *, int *, int);
void get_bool_mascaret_(int *, int *, char *, int *, int *, int *, int *, int);
void get_string_mascaret_(int *, int *, char *, int *, int *, int *, char *, int, int);
void set_double_mascaret_(int *, int *, char *, int *, int *, int *, double *, int);
void set_int_mascaret_(int *, int *, char *, int *, int *, int *, int *, int);
void set_bool_mascaret_(int *, int *, char *, int *, int *, int *, int *, int);
void set_string_mascaret_(int *, int *, char *, int *, int *, int *, char *, int, int);
void version_mascaret_(int *, int *, int *);
void import_xml_(int *, int *, char *, int *, int);
void export_xml_(int *, int *, char *, int *, int *, int);
void export_xml_saint_venant_(int *, int *, char *, int);
void ouverture_balise_xml_(int *, int *, char *, int *, char *, int, int);
void export_var_xml_(int *, int *, int *, char *, int *, int);
void export_uservar_xml_(int *, int *, int *, char *, char *, char *, char *, int, int, int, int);
void fermeture_balise_xml_(int *, int *, int *, char *, int);

//.................................................................................................................................
// Initialise les ressources associees a une instance de Mascaret (Modele et Etat)
// Retourne l'identifiant de l'instance
// Argument en sortie :
//      Identifiant de l'instance Mascaret utilise en argument des autres fonctions de l'API
// .................................................................................................................................
int C_CREATE_MASCARET(int *Identifiant)
{
   int erreurFortran;
   int idFortran;
   // Appel API fortran
   create_mascaret_(&erreurFortran, &idFortran);
   *Identifiant = idFortran;
   return erreurFortran;
}

//.................................................................................................................................
// Libere les ressources associees a une instance de Mascaret (Modele et Etat)
// RQ : Depend de l'instance du modele ou de l'etat
// Argument en entree :
//     Identifiant :  Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
// .................................................................................................................................
int C_DELETE_MASCARET(int Identifiant)
{
   int erreurFortran;
   delete_mascaret_(&erreurFortran, &Identifiant);

   return erreurFortran;
}

//.................................................................................................................................
// Sauvegarde en memoire de l'etat courant d'une instance de Mascaret pour un usage ulterieur
// Argument en entree :
//      Identifiant :  Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
// Argument en sortie :
//      IdentifiantEtat : identifiant de l'etat de l'instance Mascaret sauvegarde
//.................................................................................................................................
int C_SAVE_ETAT_MASCARET(int Identifiant, int *IdentifiantEtat)
{
   int erreurFortran;
   int idFortranEtat;
   save_etat_mascaret_(&erreurFortran, &Identifiant, &idFortranEtat);
   *IdentifiantEtat = idFortranEtat;
   return erreurFortran;
}

//.................................................................................................................................
// Initialise l'etat courant d'une instance de Mascaret a partir d'un etat prealablement sauvegarde par SAVE_ETAT_MASCARET
// Supprime la sauvegarde de l'etat.
// Arguments en entree :
//      Identifiant     :  Identifiant de l'instance de Mascaret dont l'etat va etre modifie
//      IdentifiantEtat :  Identifiant de l'etat Mascaret sauvegarde par SAVE_ETAT_MASCARET
//      AndDelete       :  Efface (si !=0) ou non (si =0) l'etat sauvegarde apres restauration
//.................................................................................................................................
int C_SET_ETAT_MASCARET(int Identifiant, int IdentifiantEtat, int AndDelete)
{
   int erreurFortran;
   set_etat_mascaret_(&erreurFortran, &Identifiant, &IdentifiantEtat, &AndDelete);

   return erreurFortran;
}
//.................................................................................................................................
// Desallocation de tous les etats sauvegardes concernant un identifiant Mascaret
// Arguments en entree :
//      Identifiant     :  Identifiant de l'instance de Mascaret dont les etats sauvegardes vont etre supprimes
//.................................................................................................................................
int C_FREE_ALL_SAVE_ETAT_MASCARET(int Identifiant)
{
   int erreurFortran;
   free_all_save_etat_mascaret_(&erreurFortran, &Identifiant);

   return erreurFortran;
}

//.................................................................................................................................
// Desallocation d'un etat sauvegardes
// Arguments en entree :
//      IdentifiantEtat     :  Identifiant de l'etat Mascaret sauvegarde par SAVE_ETAT_MASCARET qui va etre supprime
//.................................................................................................................................
int C_FREE_SAVE_ETAT_MASCARET(int IdentifiantEtat)
{
   int erreurFortran;
   free_save_etat_mascaret_(&erreurFortran, &IdentifiantEtat);

   return erreurFortran;
}

// .................................................................................................................................
// Retourne le message d'une erreur
// RQ : Depend de l'instance du modele ou de l'etat
// Argument en entree :
//    Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
// Argument en sortie :
//    Message : Message d'erreur en francais de la derniere fonction appelee (max 256 caracteres)
// .................................................................................................................................
int C_GET_ERREUR_MASCARET(int *Identifiant, char **Message)
{
   char messageFortran[256];
   int erreurFortran, taille;

   get_erreur_mascaret_(&erreurFortran, Identifiant, messageFortran, 256);

   memcpy(Message[0], messageFortran, 256 * sizeof(char));
   taille = 256;
   while (Message[0][taille - 1] == ' ')
      Message[0][--taille] = 0;

   return erreurFortran;
}

//.................................................................................................................................
// Importation d'un modele mascaret a partir des fichiers natifs de Mascaret
// Arguments en entree :
//       Identifiant    : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
//       TabNomFichier  : Tableau des noms des fichiers natifs Mascaret a importer (un nom de fichier ne doit pas depasser 255 caracteres)
//       TypeNomFichier : Tableau des type des fichiers natifs Mascaret a importer, valeurs acceptees :
//                        "abaques", "dico", "casier", "geo", "loi", "cas","listing","damocle", "res", "listing_casier", "listing_liaison",
//                        "res_casier", "res_liaison"
//       Taille         : Taille des 2 tableaux TabNomFichier et TypeNomFichier
//       Impression     : impression sur les fichiers listing (1-> Vrai 0-> Faux)
// .................................................................................................................................
int C_IMPORT_MODELE_MASCARET(int Identifiant, char *TabNomFichier[], char *TypeNomFichier[], int Taille, int Impression)
{
   int erreurFortran;
   int i, strsize;
   char *tabNomFichierFortran = NULL;
   char *typeNomFichierFortran = NULL;

   if (Taille > 0)
   {
      tabNomFichierFortran = (char *)malloc((Taille * 255) * sizeof(char));
      for (i = 0; i < Taille; i++)
      {
         strsize = strlen(TabNomFichier[i]);
         strcpy(&tabNomFichierFortran[i * 255], TabNomFichier[i]);
         memset(&tabNomFichierFortran[i * 255] + strsize, ' ', 255 - strsize);
      }

      typeNomFichierFortran = (char *)malloc((Taille * 40) * sizeof(char));
      for (i = 0; i < Taille; i++)
      {
         strsize = strlen(TypeNomFichier[i]);
         strcpy(&typeNomFichierFortran[i * 40], TypeNomFichier[i]);
         memset(&typeNomFichierFortran[i * 40] + strsize, ' ', 40 - strsize);
      }

      import_modele_mascaret_(&erreurFortran, &Identifiant, tabNomFichierFortran, typeNomFichierFortran, &Taille, &Impression, NULL, 255, 40, 255);
   }
   else
   {
      return -1;
   }
   free(tabNomFichierFortran);
   free(typeNomFichierFortran);

   return erreurFortran;
}

int C_IMPORT_MODELE_MASCARET_ONEFILE(int *Identifiant, int *Impression, char **MascFile)
{
   int erreurFortran;
   int Taille = 0;
   int strsize;
   char *tabNomFichierFortran = NULL;
   char *typeNomFichierFortran = NULL;
   char FichierMascaret[255];

   strsize = strlen(MascFile[0]);
   strcpy(FichierMascaret, MascFile[0]);
   memset(FichierMascaret + strsize, ' ', 255 - strsize);

   import_modele_mascaret_(&erreurFortran, Identifiant, tabNomFichierFortran, typeNomFichierFortran, &Taille, Impression, FichierMascaret, 255, 40, 255);

   return erreurFortran;
}
//.................................................................................................................................
// Importation de l'etat Mascaret a partir du fichier natif contenant la ligne d'eau initiale
// Arguments en entree :
//       Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
//       NomFichier  : Nom du fichier natif contenant la ligne d'eau initiale (max 255 carateres)
//       Impression  : impression sur les fichiers listing (1-> Vrai 0-> Faux)
// .................................................................................................................................
int C_INIT_ETAT_MASCARET(int *Identifiant, char **NomFichier, int *Impression)
{
   int erreurFortran, taille;
   char nomFichierFortran[255];

   taille = strlen(NomFichier[0]);
   strcpy(nomFichierFortran, NomFichier[0]);
   memset(nomFichierFortran + taille, ' ', 255 - taille);

   init_etat_mascaret_(&erreurFortran, Identifiant, nomFichierFortran, Impression, 255);
   return erreurFortran;
}

//.................................................................................................................................
// Importation de l'etat Mascaret a partir de la ligne d'eau initiale (debit, cote) passee en argument
// Arguments en entree :
//       Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
//       Q           : Tableau des debits de la ligne d'eau initiale
//       Z           : Tableau des cotes de la ligne d'eau initiale
//       Taille      : Taille des 2 tableaux Q et Z
// .................................................................................................................................
int C_INIT_LIGNE_MASCARET(int Identifiant, double Q[], double Z[], int Taille)
{
   int erreurFortran;

   init_ligne_mascaret_(&erreurFortran, &Identifiant, Q, Z, &Taille);

   return erreurFortran;
}

//.................................................................................................................................
// Renvoie la ligne d'eau (debit, cote) du modele a l'instant courant
// Arguments en entree :
//       Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
//       Q           : Tableau des debits de la ligne d'eau
//       Z           : Tableau des cotes de la ligne d'eau
// .................................................................................................................................
int C_GET_LIGNE(int *Identifiant, double Q[], double Z[])
{
   int erreurFortran;

   get_ligne_(&erreurFortran, Identifiant, Q, Z);

   return erreurFortran;
}

//.................................................................................................................................
// Importation de l'etat Tracer a partir des concentrations initiales passees en argument
// Arguments en entree :
//       Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
//       C           : Tableau 2D des concentrations initiales
//       Taille      : Taille de la ligne d'eau
//       NbTrac      : Nombre de traceurs
//       Impression  : impression sur les fichiers listing (1-> Vrai 0-> Faux)
// .................................................................................................................................
int C_INIT_LIGNE_TRACER(int Identifiant, double C[], int Taille, int NbTrac, int Impression)
{
   int erreurFortran;

   init_ligne_tracer_(&erreurFortran, &Identifiant, C, &Taille, &NbTrac, &Impression);

   return erreurFortran;
}

//.................................................................................................................................
// Renvoie les concentrations des traceurs a l'instant courant
// Arguments en entree :
// Arguments en entree :
//      Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
//      C           : Tableau des concentrations des traceurs
// .................................................................................................................................
int C_GET_LIGNE_TRACER(int Identifiant, double C[])
{
   int erreurFortran;

   get_ligne_tracer_(&erreurFortran, &Identifiant, C);

   return erreurFortran;
}

//.................................................................................................................................
// Importation de l'etat Tracer a partir du fichier natif contenant la concentration initiale ou a zero
// Arguments en entree :
//       Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
//       Impression  : impression sur les fichiers listing (1-> Vrai 0-> Faux)
// .................................................................................................................................
int C_INIT_ETAT_TRACER(int Identifiant, int Impression)
{
   int erreurFortran;

   init_etat_tracer_(&erreurFortran, &Identifiant, &Impression);
   return erreurFortran;
}

//.................................................................................................................................
// Calcul d'un nouvel etat au "TpsFinal" en utilisant le modele courant et l'etat precedent
// Arguments en entree :
//       Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
//       TpsInitial  : Temps initiale du calcul
//       TpsFinal    : Temps final du calcul
//       PasTps      : Pas de temps interne du calcul
//       Impression  : impression sur les fichiers listing (1-> Vrai 0-> Faux)
// .................................................................................................................................
int C_CALCUL_MASCARET(int Identifiant, double TpsInitial, double TpsFinal, double PasTps, int Impression)
{
   int erreurFortran;

   calcul_mascaret_(&erreurFortran, &Identifiant, &TpsInitial, &TpsFinal, &PasTps, &Impression);

   return erreurFortran;
}

//.................................................................................................................................
// Recupere le nombre de condition limite dans le modele
// Argument en entree :
//        Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
// Arguments en sortie :
//        NbCL : le nombre de condition limite dans le modele
//.................................................................................................................................
int C_GET_NB_CONDITION_LIMITE_MASCARET(int *Identifiant, int *NbCL)
{
   int erreurFortran;

   get_nb_condition_limite_mascaret_(&erreurFortran, Identifiant, NbCL);

   return erreurFortran;
}

//.................................................................................................................................
// Recupere le nom d'une condition limite du modele
// Argument en entree :
//        Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
//        NumCL       : Le numero de la condition limite dont on veut connaitre le nom
// Arguments en sortie :
//        NbCL   : le nombre de condition limite dans le modele
//        NumLoi : Numero de la Loi correspondant a la condition limite dans le modele
//.................................................................................................................................
int C_GET_NOM_CONDITION_LIMITE_MASCARET(int *Identifiant, int *NumCL, char **NomCL, int *NumLoi)
{
   int erreurFortran, taille;
   char messageFortran[30];

   get_nom_condition_limite_mascaret_(&erreurFortran, Identifiant, NumCL, messageFortran, NumLoi, 30);

   memcpy(NomCL[0], messageFortran, 30 * sizeof(char));
   taille = 30;
   while (NomCL[0][taille - 1] == ' ')
      NomCL[0][--taille] = 0;

   return erreurFortran;
}

//.................................................................................................................................
// Calcul d'un nouvel etat au "TpsFinal" en utilisant le modele courant, les nouvelles contions limites et l'etat precedent
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
int C_CALCUL_MASCARET_CONDITION_LIMITE(int Identifiant, double TpsInitial, double TpsFinal, double PasTps, double TpsCl[], int TailleTpsCL, double **Cl1, double **Cl2, int Impression)
{
   int erreurFortran;
   double *Cl1Fortran, *Cl2Fortran;
   int NbCL, i, j, indexFortran;

   get_nb_condition_limite_mascaret_(&erreurFortran, &Identifiant, &NbCL);

   Cl1Fortran = (double *)malloc((NbCL * TailleTpsCL) * sizeof(double));
   Cl2Fortran = (double *)malloc((NbCL * TailleTpsCL) * sizeof(double));
   indexFortran = 0;
   for (i = 0; i < NbCL; i++)
   {
      for (j = 0; j < TailleTpsCL; j++)
      {
         Cl1Fortran[indexFortran] = Cl1[i][j];
         Cl2Fortran[indexFortran] = Cl2[i][j];
         indexFortran++;
      }
   }

   calcul_mascaret_condition_limite_(&erreurFortran, &Identifiant, &TpsInitial, &TpsFinal, &PasTps,
                                     TpsCl, &TailleTpsCL, Cl1Fortran, Cl2Fortran, &Impression);

   free(Cl1Fortran);
   free(Cl2Fortran);

   return erreurFortran;
}

//.................................................................................................................................
// Recupere la liste des variables de Mascaret accompagnee d'une description
// RQ : Ne depend pas de l'instance du modele ou de l'etat
// Argument en entree :
//        Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
// Arguments en sortie :
//        TabNom      : Tableau des noms de variable du modele ou de l'etat
//        TabDesc     : Tableau des descriptions de variable du modele ou de l'etat
//        Taille      : Taille des tableaux des noms et des descriptions de variable
//.................................................................................................................................
int C_GET_DESC_VAR_MASCARET(int Identifiant, char **TabNom[], char **TabDesc[], int *Taille)
{
   int NbVarMascaret;
   int erreurFortran;
   int i, j;
   char *tabNomFortran;
   char *tabDescFortran;
   char *chaineC1, *chaineC2;
   char tmpVarFortran[40];
   char tmpDescFortran[110];

   get_nb_var_mascaret_(&NbVarMascaret);

   tabNomFortran = (char *)malloc((NbVarMascaret * 40) * sizeof(char));
   tabDescFortran = (char *)malloc((NbVarMascaret * 110) * sizeof(char));

   *TabNom = (char **)malloc(NbVarMascaret * sizeof(char *));
   *TabDesc = (char **)malloc(NbVarMascaret * sizeof(char *));

   get_desc_var_mascaret_(&erreurFortran, &Identifiant, tabNomFortran, tabDescFortran, &NbVarMascaret, 40, 110);

   for (i = 0; i < NbVarMascaret; i++)
   {
      for (j = 0; j < 40; j++)
      {
         tmpVarFortran[j] = tabNomFortran[(40 * i) + j];
      }
      chaineC1 = (char *)malloc(40 * sizeof(char));
      memcpy(chaineC1, tmpVarFortran, 40 * sizeof(char));
      memset(chaineC1 + 40 - 1, '\0', 1);
      (*TabNom)[i] = chaineC1;

      for (j = 0; j < 110; j++)
      {
         tmpDescFortran[j] = tabDescFortran[(110 * i) + j];
      }
      chaineC2 = (char *)malloc(110 * sizeof(char));
      memcpy(chaineC2, tmpDescFortran, 110 * sizeof(char));
      memset(chaineC2 + 110 - 1, '\0', 1);
      (*TabDesc)[i] = chaineC2;
   }

   free(tabNomFortran);
   free(tabDescFortran);
   *Taille = NbVarMascaret;
   return erreurFortran;
}

// .................................................................................................................................
// Recupere des informations sur une variable (identifie par NomVar) :
//   * le type (reel, entier, booleen, chaine de caractere),
//   * la categorie (modele ou etat)
//   * si la variable est modifiable dans l'API avec SET_XXXX_MASCARET
//   * le nombre d'indexe pour acceder a la variable
// RQ : Ne depend pas de l'instance du modele ou de l'etat
// Arguments en entree :
//        Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
//        NomVar      : Nom de la variable (notation pointe), la liste des variables est retournee par C_GET_DESC_VAR_MASCARET
// Arguments en sortie :
//        TypeVar     : valeurs possibles :"INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
//        Categorie   : valeurs possibles : "MODEL" ou "STATE"
//        Modifiable  : Si vrai alors on peut utiliser une fonction SET_XXXX_MASCARET sur la variable
//        dimVar      : dimension (c'est a dire le nombre d'indexe de 0 a 3)
// .................................................................................................................................
int C_GET_TYPE_VAR_MASCARET(int *Identifiant, char **NomVar, char **TypeVar, char **Categorie, int *Modifiable, int *dimVar)
{
   int erreurFortran, taille;
   char nomVarFortran[40];
   char typeVarFortran[10], categorieFortran[10];

   taille = strlen(NomVar[0]);
   strcpy(nomVarFortran, NomVar[0]);
   memset(nomVarFortran + taille, ' ', 40 - taille);

   get_type_var_mascaret_(&erreurFortran, Identifiant, nomVarFortran, typeVarFortran, categorieFortran, Modifiable, dimVar, 40, 10, 10);

   memcpy(TypeVar[0], typeVarFortran, 10 * sizeof(char));
   taille = 10;
   while (TypeVar[0][taille - 1] == ' ')
      TypeVar[0][--taille] = 0;

   memcpy(Categorie[0], categorieFortran, 10 * sizeof(char));
   taille = 10;
   while (Categorie[0][taille - 1] == ' ')
      Categorie[0][--taille] = 0;

   return erreurFortran;
}

// .................................................................................................................................
// Accede a la taille maximum des indexes pour acceder a une variable
// RQ : Depend de l'instance du modele ou de l'etat
// Arguments en entree :
//        Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
//        NomVar      : Nom de la variable (notation pointe), la liste des variables est retournee par C_GET_DESC_VAR_MASCARET
//        index1      : valeur du 1er indice utilise pour Profils, Lois, Singularites, Deversoirs, Extremites, Casiers et Confluents
// Arguments en sortie :
//        taille1     : valeur max du 1er indice
//        taille2     : valeur max du 2e  indice
//        taille3     : valeur max du 3e  indice
// .................................................................................................................................
int C_GET_TAILLE_VAR_MASCARET(int *Identifiant, char **NomVar, int *index1, int *taille1, int *taille2, int *taille3)
{
   int erreurFortran, taille;
   char nomVarFortran[40];

   taille = strlen(NomVar[0]);
   strcpy(nomVarFortran, NomVar[0]);
   memset(nomVarFortran + taille, ' ', 40 - taille);

   get_taille_var_mascaret_(&erreurFortran, Identifiant, nomVarFortran, index1, taille1, taille2, taille3, 40);

   return erreurFortran;
}

// .................................................................................................................................
// Definit la taille maximum des indexes pour acceder a une variable
// RQ : Depend de l'instance du modele ou de l'etat
// Arguments en entree :
//        Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
//        NomVar      : Nom de la variable (notation pointe), la liste des variables est retournee par C_GET_DESC_VAR_MASCARET
//        index1      : valeur du 1er indice utilise pour Profils, Lois, Singularites, Deversoirs, Extremites, Casiers et Confluents
//        taille1     : valeur max du 1er indice
//        taille2     : valeur max du 2e  indice
//        taille3     : valeur max du 3e  indice
// .................................................................................................................................
int C_SET_TAILLE_VAR_MASCARET(int Identifiant, char *NomVar, int index1,
                              int taille1, int taille2, int taille3)
{
   int erreurFortran, taille;
   char nomVarFortran[40];

   taille = strlen(NomVar);
   strcpy(nomVarFortran, NomVar);
   memset(nomVarFortran + taille, ' ', 40 - taille);

   set_taille_var_mascaret_(&erreurFortran, &Identifiant, nomVarFortran,
                            &index1, &taille1, &taille2, &taille3, 40);

   return erreurFortran;
}

/*********************************************************************************************************************************
    *
    *  Accesseurs permettant d'acceder aux valeurs des variables d'une instance du modele ou de l'etat
    * RQ : Depend de l'instance du modele ou de l'etat
    *
    *********************************************************************************************************************************/
// Lecture d'une variable de type reel
// Arguments en entree :
//        Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
//        NomVar      : Nom de la variable (notation pointe), la liste des variables est retournee par C_GET_DESC_VAR_MASCARET
//        index1      : valeur du 1er indice
//        index2      : valeur du 2e indice
//        index3      : valeur du 3e indice
//        valeur      : valeur de la variable pour les indexes specifies
int C_GET_DOUBLE_MASCARET(int *Identifiant, char **NomVar, int *index1, int *index2, int *index3, double *valeur)
{
   int erreurFortran, taille;
   char nomVarFortran[40];

   taille = strlen(NomVar[0]);
   strcpy(nomVarFortran, NomVar[0]);
   memset(nomVarFortran + taille, ' ', 40 - taille);

   get_double_mascaret_(&erreurFortran, Identifiant, nomVarFortran, index1, index2, index3, valeur, 40);

   return erreurFortran;
}

// Lecture d'une variable de type entier
// Arguments en entree :
//        Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
//        NomVar      : Nom de la variable (notation pointe), la liste des variables est retournee par C_GET_DESC_VAR_MASCARET
//        index1      : valeur du 1er indice
//        index2      : valeur du 2e indice
//        index3      : valeur du 3e indice
//        valeur      : valeur de la variable pour les indexes specifies
int C_GET_INT_MASCARET(int *Identifiant, char **NomVar, int *index1, int *index2, int *index3, int *valeur)
{
   int erreurFortran, taille;
   char nomVarFortran[40];

   taille = strlen(NomVar[0]);
   strcpy(nomVarFortran, NomVar[0]);
   memset(nomVarFortran + taille, ' ', 40 - taille);

   get_int_mascaret_(&erreurFortran, Identifiant, nomVarFortran, index1, index2, index3, valeur, 40);

   return erreurFortran;
}
// Lecture d'une variable de type booleen
// Arguments en entree :
//        Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
//        NomVar      : Nom de la variable (notation pointe), la liste des variables est retournee par C_GET_DESC_VAR_MASCARET
//        index1      : valeur du 1er indice
//        index2      : valeur du 2e indice
//        index3      : valeur du 3e indice
//        valeur      : valeur de la variable pour les indexes specifies
int C_GET_BOOL_MASCARET(int *Identifiant, char **NomVar, int *index1, int *index2, int *index3, int *valeur)
{
   int erreurFortran, taille;
   char nomVarFortran[40];

   taille = strlen(NomVar[0]);
   strcpy(nomVarFortran, NomVar[0]);
   memset(nomVarFortran + taille, ' ', 40 - taille);

   get_bool_mascaret_(&erreurFortran, Identifiant, nomVarFortran, index1, index2, index3, valeur, 40);

   return erreurFortran;
}

// Lecture d'une variable de type chaine de caractere
// Arguments en entree :
//        Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
//        NomVar      : Nom de la variable (notation pointe), la liste des variables est retournee par C_GET_DESC_VAR_MASCARET
//        index1      : valeur du 1er indice
//        index2      : valeur du 2e indice
//        index3      : valeur du 3e indice
//        valeur      : valeur de la variable pour les indexes specifies
int C_GET_STRING_MASCARET(int *Identifiant, char **NomVar, int *index1, int *index2, int *index3, char **valeur)
{
   int erreurFortran, taille;
   char nomVarFortran[40];
   char valeurFortran[256];

   taille = strlen(NomVar[0]);
   strcpy(nomVarFortran, NomVar[0]);
   memset(nomVarFortran + taille, ' ', 40 - taille);

   get_string_mascaret_(&erreurFortran, Identifiant, nomVarFortran, index1, index2, index3, valeurFortran, 40, 256);

   memcpy(valeur[0], valeurFortran, 256 * sizeof(char));
   taille = 256;
   while (valeur[0][taille - 1] == ' ')
      valeur[0][--taille] = 0;

   return erreurFortran;
}

/*********************************************************************************************************************************
    *
    * Mutateurs permettant de modifier une valeur d'une variable d'une instance du modele ou de l'etat
    * RQ : Depend de l'instance du modele ou de l'etat
    *
    *********************************************************************************************************************************/
// Modification d'une variable de type reel
// Arguments en entree :
//        Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
//        NomVar      : Nom de la variable (notation pointe), la liste des variables est retournee par C_GET_DESC_VAR_MASCARET
//        index1      : valeur du 1er indice
//        index2      : valeur du 2e indice
//        index3      : valeur du 3e indice
//        valeur      : nouvelle valeur de la variable pour les indexes specifies
int C_SET_DOUBLE_MASCARET(int *Identifiant, char **NomVar, int *index1, int *index2, int *index3, double *valeur)
{
   int erreurFortran, taille;
   char nomVarFortran[40];

   taille = strlen(NomVar[0]);
   strcpy(nomVarFortran, NomVar[0]);
   memset(nomVarFortran + taille, ' ', 40 - taille);

   set_double_mascaret_(&erreurFortran, Identifiant, nomVarFortran, index1, index2, index3, valeur, 40);

   return erreurFortran;
}
// Modification d'une variable de type entier
// Arguments en entree :
//        Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
//        NomVar      : Nom de la variable (notation pointe), la liste des variables est retournee par C_GET_DESC_VAR_MASCARET
//        index1      : valeur du 1er indice
//        index2      : valeur du 2e indice
//        index3      : valeur du 3e indice
//        valeur      : nouvelle valeur de la variable pour les indexes specifies
int C_SET_INT_MASCARET(int *Identifiant, char **NomVar, int *index1, int *index2, int *index3, int *valeur)
{
   int erreurFortran, taille;
   char nomVarFortran[40];

   taille = strlen(NomVar[0]);
   strcpy(nomVarFortran, NomVar[0]);
   memset(nomVarFortran + taille, ' ', 40 - taille);

   set_int_mascaret_(&erreurFortran, Identifiant, nomVarFortran, index1, index2, index3, valeur, 40);

   return erreurFortran;
}
// Modification d'une variable de type booleen
// Arguments en entree :
//        Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
//        NomVar      : Nom de la variable (notation pointe), la liste des variables est retournee par C_GET_DESC_VAR_MASCARET
//        index1      : valeur du 1er indice
//        index2      : valeur du 2e indice
//        index3      : valeur du 3e indice
//        valeur      : nouvelle valeur de la variable pour les indexes specifies
int C_SET_BOOL_MASCARET(int *Identifiant, char **NomVar, int *index1, int *index2, int *index3, int *valeur)
{
   int erreurFortran, taille;
   char nomVarFortran[40];

   taille = strlen(NomVar[0]);
   strcpy(nomVarFortran, NomVar[0]);
   memset(nomVarFortran + taille, ' ', 40 - taille);

   set_bool_mascaret_(&erreurFortran, Identifiant, nomVarFortran, index1, index2, index3, valeur, 40);

   return erreurFortran;
}

// Modification d'une variable de type chaine de caractere
// Arguments en entree :
//        Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
//        NomVar      : Nom de la variable (notation pointe), la liste des variables est retournee par C_GET_DESC_VAR_MASCARET
//        index1      : valeur du 1er indice
//        index2      : valeur du 2e indice
//        index3      : valeur du 3e indice
//        valeur      : nouvelle valeur de la variable pour les indexes specifies
int C_SET_STRING_MASCARET(int *Identifiant, char **NomVar, int *index1, int *index2, int *index3, char **valeur)
{
   int erreurFortran, taille;
   char nomVarFortran[40];
   char valeurFortran[256];

   taille = strlen(NomVar[0]);
   strcpy(nomVarFortran, NomVar[0]);
   memset(nomVarFortran + taille, ' ', 40 - taille);

   taille = strlen(valeur[0]);
   strcpy(valeurFortran, valeur[0]);
   memset(valeurFortran + taille, ' ', 256 - taille);

   set_string_mascaret_(&erreurFortran, Identifiant, nomVarFortran, index1, index2, index3, valeurFortran, 40, 256);

   return erreurFortran;
}

/*************************************************************************************************************************************
 *
 *  Une fonction utilitaire
 *
 *************************************************************************************************************************************/
//.................................................................................................................................
// Retourne la version courante de Mascaret
// Arguments en sortie :
//        Majeur  : Numero de la version Majeur de Mascaret
//        Mineur  : Numero de la version Mineur de Mascaret
//        Micro   : Numero de la version Micro de Mascaret
// .................................................................................................................................
int C_VERSION_MASCARET(int *Majeur, int *Mineur, int *Micro)
{
   version_mascaret_(Majeur, Mineur, Micro);
   return 0;
}

/*************************************************************************************************************************************
 *
 *  Quelques fonctions utiles pour importer/exporter en XML des variables de Mascaret
 *
 *************************************************************************************************************************************/
//.................................................................................................................................
//  Import d'un modele Mascaret depuis un fichier XML
//  Arguments en entree :
//        Identifiant  : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
//        NomFichier   : Nom du fichier XML cree contenant le Modele ou l'etat Mascaret (max 255 caracteres)
//        importModele : si vrai (valeur 1), import du modele, sinon import de l'etat
//.................................................................................................................................
int C_IMPORT_XML(int Identifiant, char *NomFichier, int importModele)
{
   int erreurFortran, taille;
   char nomFichierFortran[255];

   taille = strlen(NomFichier);
   strcpy(nomFichierFortran, NomFichier);
   memset(nomFichierFortran + taille, ' ', 255 - taille);

   import_xml_(&erreurFortran, &Identifiant, nomFichierFortran, &importModele, 255);

   return erreurFortran;
}

//.................................................................................................................................
//  Export d'un modele Mascaret dans un fichier XML
//  Arguments en entree :
//        Identifiant  : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
//        NomFichier   : Nom du fichier XML cree contenant le Modele ou l'etat Mascaret (max 255 caracteres)
//        AvecDesc     : si vrai (valeur 1), ajoute la description de la variable
//        exportModele : si vrai (valeur 1), exportation du modele, sinon export de l'etat
//.................................................................................................................................
int C_EXPORT_XML(int Identifiant, char *NomFichier, int AvecDesc, int exportModele)
{
   int erreurFortran, taille;
   char nomFichierFortran[255];

   taille = strlen(NomFichier);
   strcpy(nomFichierFortran, NomFichier);
   memset(nomFichierFortran + taille, ' ', 255 - taille);

   export_xml_(&erreurFortran, &Identifiant, nomFichierFortran, &AvecDesc, &exportModele, 255);

   return erreurFortran;
}

//.................................................................................................................................
//  Export d'un modele Mascaret dans un fichier XML
//  Arguments en entree :
//        Identifiant  : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
//        NomFichier   : Nom du fichier XML cree contenant le Modele ou l'etat Mascaret (max 255 caracteres)
//        AvecDesc     : si vrai (valeur 1), ajoute la description de la variable
//        exportModele : si vrai (valeur 1), exportation du modele, sinon export de l'etat
//.................................................................................................................................
int C_EXPORT_XML_SAINT_VENANT(int Identifiant, char *NomFichier)
{
   int erreurFortran, taille;
   char nomFichierFortran[255];

   taille = strlen(NomFichier);
   strcpy(nomFichierFortran, NomFichier);
   memset(nomFichierFortran + taille, ' ', 255 - taille);

   export_xml_saint_venant_(&erreurFortran, &Identifiant, nomFichierFortran, 255);

   return erreurFortran;
}

//-------------------------------------------------------------------------------
// Ouverture du fichier XML suivi de l'ouverture de la balise XML racine
// Ce fichier est prevu pour contenir des variables au choix de Mascaret via l'utilisation de C_EXPORT_VAR_XML
// Arguments en entree :
//        Identifiant  : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
//        NomFichier   : Nom du fichier XML cree contenant l'entete du fichier XML (max 255 caracteres)
//        uniteLogique : unite logique utilise par le fichier XML : doit etre strictement superieur a 36
//        balise       : balise racine du fichier XML
//-------------------------------------------------------------------------------
int C_OUVERTURE_BALISE_XML(int Identifiant, char *NomFichier, int uniteLogique, char *balise)
{
   int erreurFortran, taille;
   char nomFichierFortran[255];
   char baliseFortran[255];

   taille = strlen(NomFichier);
   strcpy(nomFichierFortran, NomFichier);
   memset(nomFichierFortran + taille, ' ', 255 - taille);

   taille = strlen(balise);
   strcpy(baliseFortran, balise);
   memset(baliseFortran + taille, ' ', 255 - taille);

   ouverture_balise_xml_(&erreurFortran, &Identifiant, nomFichierFortran, &uniteLogique, baliseFortran, 255, 255);

   return erreurFortran;
}

//-----------------------------------------------------------------------------------
// Exportation d'une variable Mascaret dans un fichier XML
// Avant d'utiliser cette procedure, il faut avoir deja execute OUVERTURE_BALISE_XML
// et avoir initialiser le modele avec IMPORT_MODELE_MASCARET.
// On peut ensuite utiliser plusieurs fois cette procedure
// Arguments en entree :
//        Identifiant  : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
//        uniteLogique : Unite logique utilise par le fichier XML - Doit egale a celle de l'ouverture (utilisation de C_OUVERTURE_BALISE_XML)
//        nomVar       : Nom de la variable Mascaret a exporter
//        avecDesc     : Si vrai (valeur 1), on ajoute une description en francais de la variable
//-----------------------------------------------------------------------------------
int C_EXPORT_VAR_XML(int Identifiant, int uniteLogique, char *nomVar, int avecDesc)
{
   int erreurFortran, taille;
   char nomVarFortran[40];

   taille = strlen(nomVar);
   strcpy(nomVarFortran, nomVar);
   memset(nomVarFortran + taille, ' ', 40 - taille);

   export_var_xml_(&erreurFortran, &Identifiant, &uniteLogique, nomVarFortran, &avecDesc, 40);

   return erreurFortran;
}

//-----------------------------------------------------------------------------------
// Exportation d'une variable utilisateur dans un fichier XML
// par ex: <nomUserVar type="typeVar" description="descVar">valeurVar</nomUserVar>
// Avant d'utiliser cette procedure, il faut avoir deja execute OUVERTURE_BALISE_XML
// et avoir initialiser le modele avec IMPORT_MODELE_MASCARET.
// On peut ensuite utiliser plusieurs fois cette procedure
// Arguments en entree :
//      Identifiant  : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
//      uniteLogique : Unite logique utilise par le fichier XML - Doit egale a celle de l'ouverture (utilisation de C_OUVERTURE_BALISE_XML)
//      nomUserVar   : Nom de la variable Utilisateur a exporter
//      typeVar      : chaine du type de la variable "INT", ou "DOUBLE"
//      descVar      : chaine de description de la variable
//      valeurVar    : chaine de la valeur a ecrire dans la balise XML
//-----------------------------------------------------------------------------------
int C_EXPORT_USERVAR_XML(int Identifiant, int uniteLogique, char *nomUserVar, char *typeVar, char *descVar, char *valeurVar)
{
   int erreurFortran, taille;
   char nomVarFortran[40];
   char typeVarFortran[20];
   char descVarFortran[110];
   char valeurVarFortran[20];

   taille = strlen(nomUserVar);
   strcpy(nomVarFortran, nomUserVar);
   memset(nomVarFortran + taille, ' ', 40 - taille);

   taille = strlen(typeVar);
   strcpy(typeVarFortran, typeVar);
   memset(typeVarFortran + taille, ' ', 20 - taille);

   taille = strlen(descVar);
   strcpy(descVarFortran, descVar);
   memset(descVarFortran + taille, ' ', 110 - taille);

   taille = strlen(valeurVar);
   strcpy(valeurVarFortran, valeurVar);
   memset(valeurVarFortran + taille, ' ', 20 - taille);

   export_uservar_xml_(&erreurFortran, &Identifiant, &uniteLogique, nomVarFortran, typeVarFortran, descVarFortran, valeurVarFortran, 40, 20, 110, 20);

   return erreurFortran;
}

//------------------------------------------------------------------------------------
// Fermeture du la balise XML racine puis fermeture du fichier
// Doit etre utiliser apres OUVERTURE_BALISE_XML et normalement apres C_EXPORT_VAR_XML
// Arguments en entree :
//        Identifiant  : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
//        uniteLogique : Unite logique utilise par le fichier XML - Doit egale a celle de l'ouverture (utilisation de C_OUVERTURE_BALISE_XML)
//        balise       : balise racine du fichier XML - Doit egale a celle de l'ouverture (utilisation de C_OUVERTURE_BALISE_XML)
//-----------------------------------------------------------------------------------
int C_FERMETURE_BALISE_XML(int Identifiant, int uniteLogique, char *balise)
{
   int erreurFortran, taille;
   char baliseFortran[255];

   taille = strlen(balise);
   strcpy(baliseFortran, balise);
   memset(baliseFortran + taille, ' ', 255 - taille);

   fermeture_balise_xml_(&erreurFortran, &Identifiant, &uniteLogique, baliseFortran, 255);
   return erreurFortran;
}
