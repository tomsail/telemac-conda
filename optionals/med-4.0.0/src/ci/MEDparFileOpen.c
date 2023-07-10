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

#ifdef PPRO_NT_CALL
#define F_OK 0
#else
#include <unistd.h>
#endif


med_access_mode MED_ACCESS_MODE;

/**\ingroup MEDfile
  \brief \MEDparfileOpenBrief
  \param filename \filename
  \param accessmode \accessmode
  \param comm \comm
  \param info \info
  \retval med_idt  \fidDes
  \details \MEDparfileOpenDetails
  \par Remarques
  \MEDparfileOpenNote
*/

/*#define true 1*/

med_idt
MEDparFileOpen(const char* const filename,
	       const med_access_mode accessmode,
	       const MPI_Comm comm, const MPI_Info info)
{
  med_idt _fid = -1;

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
	MED_ERR_(_fid,MED_ERR_DOESNTEXIST,MED_ERR_FILE,filename);
	goto ERROR;

      } else {
	if ((_fid = _MEDparFileOpen((char*) filename, accessmode, comm, info)) < 0) {
	  MED_ERR_(_fid,MED_ERR_OPEN,MED_ERR_FILE,filename);
	  goto ERROR;
	}
      };
      break;

    case MED_ACC_RDWR :
      if (access(filename,F_OK)) {
	if ((_fid = _MEDparFileCreate((char*) filename,accessmode, comm, info)) < 0) {
	  MED_ERR_(_fid,MED_ERR_CREATE,MED_ERR_FILE,filename);
	  goto ERROR;
	}
      } else
	if ((_fid = _MEDparFileOpen((char*) filename,accessmode, comm, info)) < 0) {
	  MED_ERR_(_fid,MED_ERR_OPEN,MED_ERR_FILE,filename);
	  goto ERROR;
	}
      break;

    case MED_ACC_RDEXT :
      if (access(filename,F_OK))
	{
	  if ((_fid = _MEDparFileCreate((char*) filename,accessmode, comm, info)) < 0) {
	    MED_ERR_(_fid,MED_ERR_CREATE,MED_ERR_FILE,filename);
	    goto ERROR;
	  }
	} else

	if ((_fid = _MEDparFileOpen((char *) filename, accessmode, comm, info)) < 0) {
	  MED_ERR_(_fid,MED_ERR_OPEN,MED_ERR_FILE,filename);
	  goto ERROR;
	}
      break;

    case MED_ACC_CREAT :
      if ((_fid = _MEDparFileCreate((char *) filename,MED_ACC_RDWR, comm, info)) < 0) {
	MED_ERR_(_fid,MED_ERR_CREATE,MED_ERR_FILE,filename);
	goto ERROR;
      }
      break;

    default :
      MED_ERR_(_fid,MED_ERR_RANGE,MED_ERR_PARAMETER,"");
      ISCRUTE_int(accessmode);
      goto ERROR;
    }

  if (_MEDcheckVersion30(_fid) < 0) {
    MEDfileClose(_fid);
    _fid=-1;goto ERROR;
  }

 ERROR:

  return _fid;
}
