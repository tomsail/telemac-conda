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


#ifndef MEDC_WIN_DLL_H
#define MEDC_WIN_DLL_H

#if !defined(MED_API_23)
#error "MED_API_23 must be defined before including medC_win_dll.h"
#endif

#if !defined(MED3_USESTATIC)
# ifdef PPRO_NT
#  ifdef medC_EXPORTS
#   define MEDC_EXPORT __declspec( dllexport )
#  else
#   define MEDC_EXPORT __declspec( dllimport )
#  endif
# else
#  if (MED_API_23 == 0) && (__GNUC__ >= 4)
#   define MEDC_EXPORT __attribute__ ((visibility ("hidden")))
#  else
#   define MEDC_EXPORT
#  endif
# endif
#else
# define MEDC_EXPORT
#endif

#endif
