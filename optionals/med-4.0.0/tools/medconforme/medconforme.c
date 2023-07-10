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


#include <med.h>
#include <med_config.h>
#include <med_utils.h>
#include <stdlib.h>



int main(int argc, char *argv[]) {
  med_idt  fid    =0;
  med_int  majeur =0, mineur=0, release=0;
  med_err  ret    =-1;
  med_bool hdfok     =MED_FALSE;
  med_bool medok     =MED_FALSE;
  med_bool fileexist =MED_FALSE;
  med_bool accessok  =MED_FALSE;

  if (argc != 2) {
    fprintf(stdout,">> Utilisation : medconforme <nom_de_fichier_med> \n");
    return 0;
  }

  /*
   * Quelle version de la bibliotheque MED est utilisee ?
   */
  ret=MEDlibraryNumVersion(&majeur, &mineur, &release);
  EXIT_IF( ret<0 , "Erreur d'appel de la routine MEDlibraryNumVersion.", NULL);
  fprintf(stdout,"- Version de MED-fichier utilisée par medconforme : "IFORMAT"."IFORMAT"."IFORMAT" \n",majeur,mineur,release); 

  /*
   * Le fichier à lire est-il accessible ?
   */
  ret = MEDfileExist(argv[1],MED_ACC_RDONLY,&fileexist,&accessok );
  MED_ERR_EXIT_IF(ret < 0 , MED_ERR_CALL,MED_ERR_API,"MEDfileExist");
  if ( !fileexist ) {  fprintf(stdout,"- Le fichier [%s] n'existe pas \n",argv[1]); goto SORTIE; }
  if ( !accessok  ) {  fprintf(stdout,"- Le fichier [%s] n'est pas accessible en lecture \n",argv[1]); goto SORTIE; }
  
  /*
   * Le fichier à lire est-il au bon format de fichier HDF ?
   */
  ret=MEDfileCompatibility(argv[1],&hdfok,&medok);
  MED_ERR_EXIT_IF(ret < 0 , MED_ERR_CALL,MED_ERR_API,"MEDfileCompatibility");
  if ( hdfok ) fprintf(stdout,"- Format HDF du fichier MED [%s] conforme au format HDF utilisé par la bibliothèque \n",argv[1]);
  else       { fprintf(stdout,"- Format HDF du fichier MED [%s] non conforme au format HDF utilisé par la bibliothèque \n",argv[1]); goto SORTIE; }

  /*
   * Le fichier à lire a-t-il été créé avec une version de la bilbiothèque MED conforme avec celle utilisée ?
   * (Numéros majeur identique et mineur de la bibliothèque supérieur à celui du fichier).
   */
  if ( medok ) {
    fprintf(stdout,"- Version MED du fichier [%s] conforme a la bibliothèque MED utilisée \n",argv[1]);

    if ((fid = MEDfileOpen(argv[1],MED_ACC_RDONLY)) < 0) {
      MED_ERR_(ret,MED_ERR_OPEN,MED_ERR_FILE,argv[1]);
      goto ERROR;
    }
    
    /*
     * Une fois le fichier ouvert on peut avoir acces au numero de version complet
     */
    if ( MEDfileNumVersionRd(fid, &majeur, &mineur, &release) < 0 ) {
      MED_ERR_(ret,MED_ERR_CALL,MED_ERR_API,"MEDfileNumVersionRd");
      goto ERROR;
    }
    fprintf(stdout,"- Ce fichier a ete créé avec MED-fichier V"IFORMAT"."IFORMAT"."IFORMAT" \n",majeur,mineur,release);

  }
  else
    fprintf(stdout,"- Version MED du fichier [%s] non conforme avec celle de la bibliothèque utilisée \n",argv[1]); 

 SORTIE:
  ret = 0;
 ERROR:
  
  if (fid > 0)
    if (MEDfileClose(fid) < 0) {
      MED_ERR_(ret,MED_ERR_CLOSE,MED_ERR_FILE,argv[1]);
      ret = -1;
    }

  return ret;
}
