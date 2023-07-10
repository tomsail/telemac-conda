/*  This file is part of MED.
 *
 *  COPYRIGHT (C) 1999 - 2019  EDF R&D, CEA/DEN
 *  MED is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  MED is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with MED.  If not, see <http://www.gnu.org/licenses/>.
 */



#include "med_config.h"
#include "med_outils.h"
#include <string.h>

/* #ifdef __cplusplus */
/* } */
/* #endif */

#include <hdf5.h>
#include "med21.h"
#include "med_hdfi21.h"
#include "MAJ_21_22.h"

void MAJ_21_22_profils(med_idt fid,med_int nprofil)
{
  med_err ret;
  med_int i;
  char nom[MED_TAILLE_NOM+1];
  char chemin[MED_TAILLE_DESC+1];
  med_idt gid;
  med_int att;

  for (i=0;i<nprofil;i++)
    {    
      /* on recupere le nom du profil */
      ret = _MEDobjetIdentifier(fid,(char *) MED_PROFILS,i,nom);
      EXIT_IF(ret < 0,"Identification d'un profil",NULL);
      fprintf(stdout,"  >>> Normalisation du profil [%s] \n",nom);

      /* on accede au profil */
      strcpy(chemin,MED_PROFILS);
      strcat(chemin,nom);
      gid = _MEDdatagroupOuvrir(fid,chemin); 
      EXIT_IF(gid < 0,"Accès au profil",nom);

      /* On change l'attribut MED_NOM_N => MED_NOM_NBR */
      ret = _MEDattrEntierLire(gid,(char *) "N",&att);
      EXIT_IF(ret < 0,"Lecture de l'attribut MED_NOM_N",NULL);
      ret = H5Adelete(gid,"N");
      EXIT_IF(ret < 0,"Destruction de l'attribut MED_NOM_N",NULL);
      ret = _MEDattrEntierEcrire(gid,(char *)(MED_NOM_NBR),&att);
      EXIT_IF(ret < 0,"Ecriture de l'attribut MED_NOM_NBR dans le profil ",nom);

      /* on ferme le profil */
      ret = _MEDdatagroupFermer(gid);
      EXIT_IF(ret < 0,"Fermeture de l'acces au profil",nom);

      fprintf(stdout,"  >>> Normalisation du profil [%s] effectuée \n",nom);
    }
}


