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


#include "MEDimport.h"
#include <stdio.h>

int main(int argc, char *argv[]) {

  char * fileOut;
  int    ret;

  if ( (argc > 3) || (argc < 2) ) {
    fprintf(stderr,"Nombre de parametre incorrect : medimport filein [fileout]\n");
    return -1;
  }
  
  if (argc == 2 ) fileOut=""; else fileOut=argv[2];
  ret = MEDimport(argv[1], fileOut) ;
 
 return ret;
 
}

