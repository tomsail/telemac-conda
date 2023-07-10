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


#ifndef MED_ERREUR_HXX
#define MED_ERREUR_HXX

#include <exception>
#include <string>

struct MEDerreur : public std::exception {

  MEDerreur( const char *fichier="",
	     const unsigned int ligneNo=0,
	     const char * message="",
	     const char * arg="" );

  virtual const char* what( void ) const throw ();
  //opérateur de recopie

  virtual ~MEDerreur(void) throw() {};

protected:

  std::string _what;
};

#endif
