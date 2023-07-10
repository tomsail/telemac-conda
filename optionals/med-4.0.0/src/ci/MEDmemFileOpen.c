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

#ifdef PPRO_NT_CALL
#define F_OK 0
#else
#include <unistd.h>
#endif

/**\ingroup MEDfile
  \brief \MEDmemFileOpenBrief
  \param filename \filename
  \param memfile \memfile
  \param filesync \filesync
  \param accessmode \accessmode
  \retval med_idt  \fid
  \details \MEDmemFileOpenDetails
  \par Remarques
  \MEDmemFileOpenNote1
  \MEDmemFileOpenNote2
*/

med_idt
MEDmemFileOpen(const char* const filename, med_memfile * const memfile, const med_bool filesync, const med_access_mode accessmode)
{
  med_idt _fid = 0;
  med_err _ret = -1;
  /* med_bool file_exist = MED_FALSE; */

  /*
   * ON inhibe le gestionnaire d'erreur HDF
   */
  _MEDmodeErreurVerrouiller();

  /*
   * On ouvre le fichier MED sous HDF
   */
  switch(accessmode)
    {
    case MED_ACC_RDONLY :
    case MED_ACC_CREAT :
    case MED_ACC_RDWR :
	if ((_fid = _MEDmemFileOpen((char *) filename, memfile, filesync,  accessmode)) < 0) { 
	  MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_MEMFILE,filename);
	  goto ERROR;
	}
	break;

    case MED_ACC_RDEXT :
	  MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_MEMFILE,filename);
      break;

    default :
      MED_ERR_(_ret,MED_ERR_INIT,MED_ERR_MEMFILE,filename);
      goto ERROR;
    }

  _ret=0;
 ERROR:

  if (_ret < 0)
    _fid = (med_idt) _ret;

  return _fid;
}

