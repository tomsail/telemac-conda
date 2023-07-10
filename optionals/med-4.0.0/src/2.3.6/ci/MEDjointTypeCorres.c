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

#include <med.h>
#include <med_config.h>
#include <med_outils.h>

#include <string.h>
#include <stdlib.h>

extern int mode_interlace; 

med_err
MEDjointTypeCorres(med_idt fid, char *maa, char *jn,int ind,
		    med_entite_maillage *typ_ent_local,   med_geometrie_element *typ_geo_local,
		    med_entite_maillage *typ_ent_distant, med_geometrie_element *typ_geo_distant)
{
  med_int nent;

  static med_int geo_ent_local=0,geo_ent_distant=0;
  static int ind_type_courrant=0;
  int ind_type_a_trouver=ind;

  const med_int nb_geo_ent=1+MED_NBR_GEOMETRIE_MAILLE+MED_NBR_GEOMETRIE_ARETE+MED_NBR_GEOMETRIE_FACE;

  med_geometrie_element typ_geo_ent[1+MED_NBR_GEOMETRIE_MAILLE+MED_NBR_GEOMETRIE_ARETE+MED_NBR_GEOMETRIE_FACE][2]=
    {
      {MED_NOEUD,0},
      {MED_MAILLE,MED_POINT1   },
      {MED_MAILLE,MED_SEG2     },
      {MED_MAILLE,MED_SEG3     },
      {MED_MAILLE,MED_TRIA3    },
      {MED_MAILLE,MED_TRIA6    },
      {MED_MAILLE,MED_QUAD4    },
      {MED_MAILLE,MED_QUAD8    },
      {MED_MAILLE,MED_TETRA4   },
      {MED_MAILLE,MED_TETRA10  },
      {MED_MAILLE,MED_HEXA8    },
      {MED_MAILLE,MED_HEXA20   },
      {MED_MAILLE,MED_PENTA6   },
      {MED_MAILLE,MED_PENTA15  },
      {MED_MAILLE,MED_PYRA5    },
      {MED_MAILLE,MED_PYRA13   },
      {MED_ARETE ,MED_SEG2     },
      {MED_ARETE ,MED_SEG3     },
      {MED_FACE  ,MED_TRIA3    },
      {MED_FACE  ,MED_TRIA6    },
      {MED_FACE  ,MED_QUAD4    },
      {MED_FACE  ,MED_QUAD8    }
    };

  if (ind != -1) {
    geo_ent_local=0;geo_ent_distant=0;
    ind_type_courrant=0;
    ind_type_a_trouver=ind;
  }
  else {
    ind_type_a_trouver=ind_type_courrant+1;
  }


  /* recherche du type des entites en regard... passage par toutes les combinaisons */


  for (;ind_type_courrant!=ind_type_a_trouver && geo_ent_local<nb_geo_ent;geo_ent_local++) {
    *typ_ent_local = typ_geo_ent[geo_ent_local][0];
    *typ_geo_local = typ_geo_ent[geo_ent_local][1];
    geo_ent_distant=0;
    for (;ind_type_courrant!=ind_type_a_trouver && geo_ent_distant<nb_geo_ent;geo_ent_distant++) {
      *typ_ent_distant = typ_geo_ent[geo_ent_distant][0];
      *typ_geo_distant = typ_geo_ent[geo_ent_distant][1];

      if ( (nent=MEDjointnCorres(fid,maa,jn,*typ_ent_local,*typ_geo_local,
				*typ_ent_distant,*typ_geo_distant)
	    ) >0 ) {
	ind_type_courrant ++;
      }
    }
  }


  if (ind_type_courrant!=ind_type_a_trouver) {
    MESSAGE("Can't find attended corresponding type for the given correspondence number.");
    ISCRUTE_int(ind_type_courrant);ISCRUTE_int(ind_type_a_trouver);
    return -1;
  }

  return 0;

}



