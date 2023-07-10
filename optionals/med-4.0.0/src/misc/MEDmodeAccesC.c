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


#include <stdio.h>
#include <med_config.h>
#include <med_misc.h>
#include <med_hdfi.h>
#include <med_utils.h>

typedef unsigned long   keyType;
typedef keyType         fileNo;
typedef med_access_mode  valueType;

extern valueType getModeAcces(keyType key);
extern med_err   setModeAcces(keyType key,valueType v);

/*
 * - Nom de la fonction : _MEDmodeAcces
 * - Description : Renvoi le mode d'accès du fichier contenant
                   l'objet identifié par oid.
		   Ce mode d'accès est positionné par MEDouvrir.
 * - Parametres :
 *     - id1    (IN)  : identificateur de l'objet
 * - Resultat : mode d'accès au fichier eventuellement
 *               MED_ACC_UNDEF
 */ 

med_access_mode _MEDmodeAcces(med_idt oid) {

  fileNo fileno=0;
  
  if ( _MEDfichierNo(oid,&fileno) < 0 ) {
    ISCRUTE_id(oid);
    ISCRUTE_int((int)(fileno));
    return MED_ACC_UNDEF;
  }  else {
/*     ISCRUTE_id(oid); */
/*     ISCRUTE(fileno); */
/*     ISCRUTE( getModeAcces(fileno)); */
    return getModeAcces(fileno);
  }
}

med_err _MEDsetModeAcces(med_idt fid, med_access_mode mode) {

  fileNo fileno;
  med_err ret;

  if ( _MEDfichierNo(fid,&fileno) < 0 ) {
    MESSAGE("Impossible de positioner le mode d'accès au fichier.");
    ISCRUTE_id(fid);
    ISCRUTE_int((int)(fileno));
    return -1;
  }

  ret = setModeAcces(fileno,mode);
/*   ISCRUTE(setModeAcces(fileno,mode)); */

  return ret;
}
