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

void _MEDfileCommentRd30(int dummy, ...) {

  med_idt _rootId=0;
  med_err _ret=-1;
  med_bool attributeexist=MED_FALSE;

  MED_VARGS_DECL(const, med_idt , , fid     );
  MED_VARGS_DECL(, char*, const   , comment );
  MED_VARGS_DECL(, med_err *     ,, fret    );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt , , fid     );
  MED_VARGS_DEF(, char*, const   , comment );
  MED_VARGS_DEF(, med_err *     ,, fret    );


  _MEDmodeErreurVerrouiller();

  if ( _MEDattributeExist(fid, "/",MED_COMMENT_NAME, &attributeexist ) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDattributeExist");
  }

  if ( !attributeexist ) { _ret=MED_ERR_DOESNTEXIST;goto ERROR;}

  /* The file descriptor is read
     and returned in "comment". */
  if ((_rootId = _MEDdatagroupOuvrir(fid,"/")) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP," : '/'");
    goto ERROR;
  }

  if (_MEDattrStringLire(_rootId,MED_COMMENT_NAME,
			 MED_COMMENT_SIZE,(char*)comment) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE," : '/'");
    goto ERROR;
  }

  _ret=0;
 ERROR :
  if (_rootId > 0)
    if (_MEDdatagroupFermer(_rootId) < 0) {
      MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP," : '/'");
      _ret = -1;
    }

  va_end(params);
  *fret = _ret;
  return;
}
