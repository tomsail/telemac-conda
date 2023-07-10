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
#include <hdf5.h>

/*
 * - Nom de la fonction : _MEDdatasetNumLire
 * - Description : lecture d'un dataset tableau numerique
 * - Parametres :
 *     - pere (IN)     : l'ID de l'objet HDF pere ou placer l'attribut
 *     - nom  (IN)     : le nom du dataset
 *     - type (IN)     : type numerique MED
 *     - interlace (IN) : Choix du type d'entrelacement demandé par l'appelant { MED_FULL_INTERLACE(x1,y1,z1,x2,...)) , MED_NO_INTERLACE(x1,x2,y1,y2,z1,z2) }
 *       - nbdim   (IN) : Dimension des éléments
 *       - fixdim  (IN) : MED_ALL ou n° de la dimension a enregistrer à partir de 1..oo
 *     - psize     (IN) : Taille du profil à utiliser, MED_NOPF si pas de profil
 *       - pflmod  (IN) : Indique comment lire les informations en mémoire { MED_COMPACT, MED_GLOBAL }. 
 *       - pfltab  (IN) : Tableau contenant les n° déléments à traiter (1....oo)
 *       - ngauss  (IN) : Nombre de points de GAUSS par élément
 *     - val  (OUT)    : valeurs du tableau
 * - Resultat : 0 en cas de succes, -1 sinon
 *  Equivalent à l'ancienne routine si .....,MED_NO_INTERLACE,1,MED_ALL,MED_NOPF,0,1 (peu importe),....
 */ 


med_err _MEDdatasetNumLire(med_idt pere,char *nom,med_type_champ type,
			   med_mode_switch interlace, med_size nbdim, med_size fixdim, 
			   med_size psize, med_mode_profil pflmod, med_int pflcmp, med_size * pfltab,
               med_int ngauss, med_int nbelem, unsigned char *val)
{
  char *  name = "_MEDdatasetNumLire";
  int     dummy=0;
  med_err fret=-1;
  med_int majeur, mineur, release;
  MedFuncType func;

  MEDversionLire(pere, &majeur, &mineur, &release);

  func = _MEDversionedApi(name,majeur,mineur,release);
  if ( func != (MedFuncType) NULL )
    func (dummy, pere, nom,  type,
	  interlace, nbdim, fixdim,
	  psize, pflmod, pflcmp, pfltab,
	  ngauss, nbelem, val, &fret);

  return fret;
}
