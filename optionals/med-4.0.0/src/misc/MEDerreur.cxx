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
#include "MEDerreur.hxx"

#include <iostream>
#include <sstream>
//#include <memory>

#include <cstdlib>
#include <cstring>

/**
 * Class OSS is useful when streaming data through a function
 * that expect a string as parameter
 */
class OSS
{
private:
  std::ostringstream oss_;

public:
  explicit OSS() : oss_() {}

  template <class T>
  OSS & operator<<(T obj)
  {
    oss_ << obj;
    return *this;
  }

  operator std::string()
  {
    return oss_.str();
  }

  // Surtout ne pas écrire le code suivant:
  // car oss_.str() renvoie une string temporaire
  //   operator const char*()
  //   {
  //     return oss_.str().c_str();
  //   }


}; /* end class OSS */


MEDerreur::~MEDerreur() throw() {};

MEDerreur::MEDerreur( const char *fichier,
		      const unsigned int ligneNo,
		      const char * message,
		      const char * arg )
{
  OSS oss ;
  oss << "MEDerreur" ;

  if ( strcmp(fichier,"") ) {
    oss << " dans le fichier " << fichier;
    
    if ( ligneNo) oss << "[" << ligneNo << "]";
  }
    
  oss << " : " <<  message;

  _what = oss;
}

const char* MEDerreur::what( void ) const throw ()
{
  return _what.c_str();
}


