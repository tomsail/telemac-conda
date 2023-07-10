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

#ifdef PPRO_NT
#define F_OK 0
#else
#include <unistd.h>
#endif


/**\ingroup MEDfile
  \brief \MEDfileVersionOpenBrief
  \param filename \filename
  \param accessmode \accessmode
  \param major \major
  \param minor \minor
  \param release \release
  \retval med_idt  \fidDes
  \details \MEDfileVersionOpenDetails
  \par Remarques
  \MEDfileVersionOpenNote
*/

med_idt
MEDfileVersionOpen(const char* const filename,
		   const med_access_mode accessmode,
		   const med_int major,
		   const med_int minor,
		   const med_int release)

{
  med_idt _fid = 0;
  med_err _ret = -1;

  /*
   * On inhibe le gestionnaire d'erreur HDF
   */
  _MEDmodeErreurVerrouiller();

  /*
   * On ouvre le fichier MED sous HDF
   */
  switch(accessmode)
    {
    case MED_ACC_RDONLY :
      if (access(filename,F_OK)) {
	MED_ERR_(_ret,MED_ERR_EXIST,MED_ERR_FILE,filename);
	goto ERROR;

      } else {
	if ((_fid = _MEDfileOpen((char*) filename,accessmode)) < 0) {
	  MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_FILE,filename);
	  goto ERROR;
	}
      };
      break;

    case MED_ACC_RDWR :
      if (access(filename,F_OK)) {
	if ((_fid = _MEDfileCreate((char*) filename,accessmode,major,minor,release)) < 0) {
	  MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_FILE,filename);
	  goto ERROR;
	}
      } else
	if ((_fid = _MEDfileOpen((char*) filename,accessmode)) < 0) {
	  MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_FILE,filename);
	  goto ERROR;
	}
      break;

    case MED_ACC_RDEXT :
      if (access(filename,F_OK))
	{
	  if ((_fid = _MEDfileCreate((char*) filename,accessmode,major,minor,release)) < 0) {
	    MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_FILE,filename);
	    goto ERROR;
	  }
	}
      else
	if ((_fid = _MEDfileOpen((char *) filename,accessmode)) < 0) {
	  MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_FILE,filename);
	  goto ERROR;
	}
      break;

    case MED_ACC_CREAT :
      if ((_fid = _MEDfileCreate((char *) filename,MED_ACC_RDWR,major,minor,release)) < 0) {
	MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_FILE,filename);
	goto ERROR;
      }
      break;

    default :
      MED_ERR_(_ret,MED_ERR_INIT,MED_ERR_FILE,filename);
      goto ERROR;
    }

  _ret=0;
 ERROR:

  if (_ret < 0)
    _fid = (med_idt) _ret;

  return _fid;
}

