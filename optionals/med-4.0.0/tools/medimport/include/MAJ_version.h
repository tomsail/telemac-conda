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

#ifndef MAJ_VERSION_H
#define MAJ_VERSION_H


#ifdef __cplusplus
# include "MEDerreur.hxx"
#endif

extern void MAJ_version_num(med_idt fid, const int majeur, const int mineur, const int release );
extern void MAJ_write_version_num(med_idt fid, const int majeur, const int mineur, const int release );
extern void MAJ_version(med_idt fid);

#endif
