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
#include <stdlib.h>

void
_MEDfamily23Info30(int dummy,...)
{

  MED_VARGS_DECL(const, med_idt       , , fid             );
  MED_VARGS_DECL(const, char*   , const , meshname        );
  MED_VARGS_DECL(const, int           , , famit           );
  MED_VARGS_DECL(, char*        , const , familyname      );
  MED_VARGS_DECL(, med_int*     , const , attributenumber );
  MED_VARGS_DECL(, med_int*     , const , attributevalue  );
  MED_VARGS_DECL(, char*        , const , attributedes    );
  MED_VARGS_DECL(, med_int*     , const , familynumber    );
  MED_VARGS_DECL(, char*        , const , groupname       );
  MED_VARGS_DECL(, med_err*            ,, fret            );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt       , , fid             );
  MED_VARGS_DEF(const, char*   , const , meshname        );
  MED_VARGS_DEF(const, int           , , famit           );
  MED_VARGS_DEF(, char*        , const , familyname      );
  MED_VARGS_DEF(, med_int*     , const , attributenumber );
  MED_VARGS_DEF(, med_int*     , const , attributevalue  );
  MED_VARGS_DEF(, char*        , const , attributedes    );
  MED_VARGS_DEF(, med_int*     , const , familynumber    );
  MED_VARGS_DEF(, char*        , const , groupname       );
  MED_VARGS_DEF(, med_err*            ,, fret            );
/*   ISCRUTE(                   fid); */
/*   SSCRUTE(		     meshname); */
/*   ISCRUTE_int(		     famit); */
/*   SSCRUTE(		     familyname); */

  *fret = MEDfamilyInfo(fid,
			meshname,
			famit,
			familyname,
			familynumber,
			groupname);
/*   ISCRUTE(		     *familynumber); */
/*   ISCRUTE(		     *attributenumber); */
/*   XSCRUTE(		     attributevalue); */
/*   SSCRUTE(		     attributedes); */
/*   SSCRUTE(		     groupname); */

  va_end(params);

  return;
}
