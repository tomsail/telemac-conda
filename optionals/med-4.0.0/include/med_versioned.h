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

#ifndef MED_VERSIONED_H
#define MED_VERSIONED_H

#ifdef __cplusplus
extern "C" {
#endif

#include "med_versioned_proto_en.h"

#include "med_versioned_proto.h"
#include "med_versioned_hdfi.h"
#include "med_versioned_misc.h"

#if ! defined(MED_HAVE_FORTRAN)
#error "There must be a configuration error, fortran should be either enabled or diasabled, hence MED_HAVE_FORTRAN  macro must be defined."
#error "Verify that you include med_versioned.h after med_config.h, thanks."
#endif

#if MED_HAVE_FORTRAN == 1
#include "med_versioned_cfi.h"
#endif

#ifdef __cplusplus
}
#endif


#endif
