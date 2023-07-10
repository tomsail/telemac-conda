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

/**\ingroup MEDfilter
   \brief \MEDfilterCloseBrief
   \param filter \filter
   \return \error

   \details
   \MEDfilterCloseDetails

   \par Remarques
   \MEDfilterCloseNote
*/

med_err MEDfilterClose(  med_filter * const filter ) {

  med_err _ret =-1;
  int     _i=0;
  med_idt _memspace[MED_MAX_FILTER_SPACES]=MED_MAX_FILTER_SPACES_INIT;
  med_idt _diskspace[MED_MAX_FILTER_SPACES]=MED_MAX_FILTER_SPACES_INIT;

  for (_i=0; _i < (*filter).nspaces; ++_i) {
    if ( H5Sclose((*filter).memspace[_i]) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATASPACE, MED_ERR_ID_MSG );
      ISCRUTE_id((*filter).memspace[_i]);
      ISCRUTE_int(_i);
      ISCRUTE((*filter).nspaces);
      goto ERROR;
    } else
      (*filter).memspace[_i]=0;

    if ( H5Sclose((*filter).diskspace[_i]) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATASPACE, MED_ERR_ID_MSG );
      ISCRUTE_id((*filter).diskspace[_i]);
      ISCRUTE_int(_i);
      ISCRUTE((*filter).nspaces);
      goto ERROR;
    } else
      (*filter).diskspace[_i]=0;
  }


  (*filter).nspaces=0;
  /*Le champ filterarray23v30 est uniquement utilisé dans _MEDfilterEntityCr236
    pour les besoins de _MEDmeshAdvancedRd236, il n'est donc pas intégré
    à la signature des MED*Filter. */
  if ((*filter).filterarray23v30) {
    free((*filter).filterarray23v30);
    (*filter).filterarray23v30 = NULL;
  }

  if ( _MEDsetFilter(MED_MAX_FILTER_SPACES,_memspace, _diskspace,
		     0, 0, 0, 0, MED_UNDEF_INTERLACE,
		     MED_NO_FILTER_SIZE, MED_NO_PROFILE_SIZE,
		     MED_UNDEF_STMODE, MED_NO_PROFILE, filter ) <0) {
    MED_ERR_(_ret,MED_ERR_INIT,MED_ERR_FILTER,"");
    goto ERROR;
  }
  (*filter).nspaces=0;

  _ret = 0;
 ERROR:
  return _ret;
}
