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

#ifndef MED23V30_PROTO_H
#define MED23V30_PROTO_H

#include "medC_win_dll.h"
#include "med_proto.h"

#ifdef __cplusplus
extern "C" {
#endif

MEDC_EXPORT med_err
MED23v30axesInfo(med_idt fid, char *maa, med_repere *type_rep,
		 char *nom, char *unit);

#ifdef __cplusplus
}
#endif

#endif /* MED23V30_PROTO_H */




