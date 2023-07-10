/* -*- mode:C; coding:utf-8 -*- */
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

#ifndef MED_VARGS
#define MED_VARGS

#include "med_config.h"

#ifdef PPRO_NT

#define MED_VARGS_DECL(cst1,type,cst2,varname) type varname
#define MED_VARGS_DEF(cst1,type,cst2,varname) varname = va_arg(params, cst1 type cst2)

#elif HAVE_CC_C99 || defined(__STDC__)

#define MED_VARGS_DECL(cst1,type,cst2,varname)
#define MED_VARGS_DEF(cst1,type,cst2,varname) cst1 type cst2 varname = va_arg(params, cst1 type cst2)

#endif

#endif

