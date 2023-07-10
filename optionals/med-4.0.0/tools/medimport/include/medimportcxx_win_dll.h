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

#ifndef MEDIMPORTCXX_WIN_DLL_H
#define MEDIMPORTCXX_WIN_DLL_H

#if !defined(MED3_USESTATIC) && defined(PPRO_NT)
# ifdef MEDIMPORTCXX_DLL_EXPORTS
#  define MEDIMPORTCXX_EXPORT __declspec( dllexport )
# else
#  define MEDIMPORTCXX_EXPORT __declspec( dllimport )
# endif
#else
# define MEDIMPORTCXX_EXPORT
#endif

#endif
