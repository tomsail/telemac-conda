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

/**\ingroup MEDfilter
 */

med_err  _MEDsetFilter(const med_int   nspaces,
		       const med_idt*  const memspace,
		       const med_idt*  const diskspace,
		       const med_int   nentity,
		       const med_int   nvaluesperentity,
		       const med_int   nconstituentpervalue,
		       const med_int   constituentselect,
		       const med_switch_mode switchmode,
		       const med_int   filterarraysize,
		       const med_int   profilearraysize,
		       const med_storage_mode storagemode,
		       const char*     const profilename,
		       med_filter*     const filter) {

  med_err    _ret=-1;
  int        _i=0;

  if ( (*filter).nspaces > 0 ) {
    MED_ERR_(_ret,MED_ERR_NOTNULL,MED_ERR_FILTER,"");
    ISCRUTE((*filter).nspaces);
    goto ERROR;
  }

  if ( (*filter).memspace[0] > 0 ) {
    MED_ERR_(_ret,MED_ERR_NOTNULL,MED_ERR_FILTER,"");
    ISCRUTE_id((*filter).memspace[0]);
    goto ERROR;
  }

  if ( (*filter).diskspace[0] > 0 ) {
    MED_ERR_(_ret,MED_ERR_NOTNULL,MED_ERR_FILTER,"");
    ISCRUTE_id((*filter).diskspace[0]);
    goto ERROR;
  }

  if ( nspaces > MED_MAX_FILTER_SPACES ) {
    MESSAGE("Impossible de créer un filtre avec un nombre de memspace supérieur à : ");
    ISCRUTE_int(MED_MAX_FILTER_SPACES);
    goto ERROR;
  }

  /*Le champ filterarray23v30 est uniquement utilisé dans _MEDfilterEntityCr236
    pour les besoins de _MEDmeshAdvancedRd236, il n'est donc pas intégré
    à la signature de MEDsetFilter.*/
  if ( (*filter).filterarray23v30 != NULL ) {
    MESSAGE("Impossible de créer un filtre avec un tableau filterarray23v30 non desalloué : ");
    XSCRUTE((void*) (*filter).filterarray23v30);
    goto ERROR;
  }


  (*filter).nspaces              = nspaces;
  for (_i=0; _i < nspaces; ++_i) {
    (*filter).memspace[_i]          = memspace[_i];
    (*filter).diskspace[_i]         = diskspace[_i];
  }
  (*filter).nentity              = nentity;
  (*filter).nvaluesperentity     = nvaluesperentity;
  (*filter).nconstituentpervalue = nconstituentpervalue;
  (*filter).constituentselect       = constituentselect;
  (*filter).switchmode              = switchmode;
  (*filter).filterarraysize         = filterarraysize;
  /*Le champ filterarray23v30 est uniquement utilisé dans _MEDfilterEntityCr236
    pour les besoins de _MEDmeshAdvancedRd236, il n'est donc pas intégré
    à la signature de MEDsetFilter.*/
  (*filter).filterarray23v30        = NULL;
  (*filter).profilearraysize        = profilearraysize;
  (*filter).storagemode             = storagemode;
  strcpy((*filter).profilename,profilename);


/* ISCRUTE((*filter).nspaces              ); */
/*   ISCRUTE((*filter).nentity              ); */
/*   ISCRUTE((*filter).nvaluesperentity     ); */
/*   ISCRUTE((*filter).nconstituentpervalue ); */
/*   ISCRUTE((*filter).constituentselect       ); */
/*   ISCRUTE((*filter).switchmode              ); */
/*   ISCRUTE((*filter).filterarraysize         ); */
/*   ISCRUTE((*filter).profilearraysize        ); */
/*   ISCRUTE((*filter).storagemode             ); */
/*   SSCRUTE((*filter).profilename             ); */
  
  _ret = 0;

 ERROR:

  return _ret;
}
