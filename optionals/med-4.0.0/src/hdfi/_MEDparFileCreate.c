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
#include <med_outils.h>

#include <mpi.h>

/*
 * - Nom de la fonction : _MEDfichierCreer
 * - Description : creation d'un fichier HDF
 * - Parametres :
 *     - nom (IN) : le nom du fichier
 * - Resultat : ID du fichier en cas de succes, -1 sinon
 */
med_idt _MEDparFileCreate(const char * const filename, const med_access_mode accessmode,
			  const MPI_Comm comm, const MPI_Info info)
{
  med_idt _fid=-1,_gid=-1;
  hid_t   _fapl    = H5P_DEFAULT;
  med_int _major   = MED_NUM_MAJEUR;
  med_int _minor   = MED_NUM_MINEUR;
  med_int _release = MED_NUM_RELEASE;

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();

  /*
    TODO : Créer une version de parFileOpen avec gestion des versions de fichier créer
  */

  if ( (_fapl = H5Pcreate (H5P_FILE_ACCESS)) < 0 ) {
    MED_ERR_(_fid,MED_ERR_CREATE,MED_ERR_PROPERTY,MED_ERR_PARALLEL_MSG);
    goto ERROR;
  }

  if ( H5Pset_fapl_mpio(_fapl, comm, info) < 0 ) {
    MED_ERR_(_fid,MED_ERR_INIT,MED_ERR_PROPERTY,MED_ERR_PARALLEL_MSG);
/*     ISCRUTE_int(comm); */
    goto ERROR;
  }

  /*
   * Cette ligne est censée obliger HDF à ne pas utiliser un modèle interne supérieur à 1.8.z
   * En HDF5-1.10.0p1 cela n'a aucun effet ! 
   * Un test autoconf permet de fixer un intervalle de version HDF à MED.
   */
#if H5_VERS_MINOR > 10
#error "Don't forget to change the compatibility version of the library !"
#endif
   
  if ( H5Pset_libver_bounds( _fapl, H5F_LIBVER_18, H5F_LIBVER_18 ) ) {
    MED_ERR_(_fid,MED_ERR_INIT,MED_ERR_PROPERTY,MED_ERR_FILEVERSION_MSG);
    goto ERROR;
  }

  if ((_fid = H5Fcreate(filename,H5F_ACC_TRUNC,
			H5P_DEFAULT,_fapl)) < 0) {
    MED_ERR_(_fid,MED_ERR_CREATE,MED_ERR_FILE,filename);
    goto ERROR;
  }

  if ( H5Pclose(_fapl) < 0 ) {
    MED_ERR_(_fid,MED_ERR_CLOSE,MED_ERR_PROPERTY,"");
    goto ERROR;
  }

  _MEDsetModeAcces(_fid,MED_ACC_RDWR);

  if ((_gid = _MEDdatagroupCreer(_fid,MED_INFOS)) < 0) {
    MED_ERR_(_fid,MED_ERR_CREATE,MED_ERR_FILE,MED_INFOS);
    goto ERROR;
  }

  /* Numero de versions de MED */
  if ( _MEDattributeIntWr(_gid,MED_NOM_MAJEUR,&_major)) {
    MED_ERR_(_fid,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_NOM_MAJEUR);
    goto ERROR;
  }

  if ( _MEDattributeIntWr(_gid,MED_NOM_MINEUR,&_minor) < 0) {
    MED_ERR_(_fid,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_NOM_MINEUR);
    goto ERROR;
  }

  if (_MEDattributeIntWr(_gid,MED_NOM_RELEASE,&_release) < 0) {
    MED_ERR_(_fid,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_NOM_RELEASE);
    goto ERROR;
  }

  /* On ferme tout */
  if ( _MEDdatagroupFermer(_gid) < 0) {
    MED_ERR_(_fid,MED_ERR_CLOSE,MED_ERR_DATAGROUP,MED_INFOS);
    goto ERROR;
  }

  _MEDsetModeAcces(_fid,accessmode);

 ERROR:
  return _fid;

}
