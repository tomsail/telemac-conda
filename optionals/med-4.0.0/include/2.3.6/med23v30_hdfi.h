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

#ifndef MED23v30_HDFI_H
#define MED23v30_HDFI_H

#ifdef __cplusplus
extern "C" {
#endif

extern MEDC_EXPORT
med_err _MEDdatasetNumLire(med_idt pere,char *nom,med_type_champ type,
                           med_mode_switch interlace, med_size nbdim, med_size fixdim,
                           med_size psize, med_mode_profil pflmod, med_int pflcmp, med_size * pfltab,
                           med_int ngauss, med_int nbelem, unsigned char *val);

extern MEDC_EXPORT
med_err _MEDdatasetStringLire(med_idt pere,char *nom,char *val);

#ifdef __cplusplus
}
#endif

#endif /* MED23V30_HDFI_H */

