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

#include <string.h>

/**\ingroup MEDfile
  \brief \MEDfileCommentWrBrief
  \param fid \fid
  \param comment \comment
  \retval med_err \error
  \details \MEDfileCommentWrDetails
 */

med_err
MEDfileCommentWr(const med_idt fid,
		 const char* const comment)
{
  med_idt _rootId=0;
  med_err _ret=-1;

  _MEDmodeErreurVerrouiller();
 if (_MEDcheckVersion30(fid) < 0) goto ERROR;

  /* the root data group is open */
  if ((_rootId = _MEDdatagroupOuvrir(fid,"/")) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP," : '/'");
    goto ERROR;
  }

  /* The comment is stored in a HDF attribute */
  if (_MEDattributeStringWr(_rootId,MED_COMMENT_NAME,MED_COMMENT_SIZE,(char*)comment) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_COMMENT_NAME);
    SSCRUTE(comment);
    goto ERROR;
  }

  _ret = 0;
 ERROR:

  /* the "/" group has to be closed */
  if (_rootId > 0)
    if (_MEDdatagroupFermer(_rootId) < 0) {
      MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP," : '/'");
    }
  return _ret;
}
