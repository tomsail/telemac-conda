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
 * - Nom de la fonction : _MEDdatasetNumEcrire
 * - Description : ecriture d'un dataset tableau numerique
 * - Parametres :
 *     - pere (IN)      : l'ID de l'objet HDF pere ou placer l'attribut
 *     - nom  (IN)      : le nom du dataset
 *     - type (IN)      : type numerique MED { MED_FLOAT64 , MED_INT32 , MED_INT64 }
 *     - interlace (IN) : Choix du type d'entrelacement utilisé par l'appelant 
 *                        { MED_FULL_INTERLACE(x1,y1,z1,x2,...)) , MED_NO_INTERLACE(x1,x2,y1,y2,z1,z2) }
 *       - nbdim   (IN) : Dimension des éléments
 *       - fixdim  (IN) : MED_ALL ou n° de la dimension a enregistrer
 *     - psize     (IN) : Taille du profil à utiliser, MED_NOPF si pas de profil 
 *                        (référence les élements, cette taille ne prend pas en compte le nombre de pts de gauss ni la dimension )  
 *       - pflmod  (IN) : Indique comment lire les informations en mémoire { MED_COMPACT, MED_GLOBALE }. 
 *       - modifpfl(IN) : Si psize != MED_NOPF :
 *                         - modifpfl == 1   indique que la taille ou le contenu du profil a changé
 *                         - modifpfl == 0   indique que la taille ou le contenu du profil n'a pas changé
 *       - pfltab  (IN) : Tableau contenant les n° déléments à traiter (1....oo)
 *       - ngauss  (IN) : Nombre de points de GAUSS par élément
 *     - size (IN)     : Taille du tableau de valeurs
 *                        (référence tous les élements, cette taille  prend en compte le nombre de pts de gauss et la dimension )  
 *     - val  (IN)     : valeurs du tableau
 * - Resultat : 0 en cas de succes, -1 sinon
 */ 

med_err _MEDdatasetNumEcrire(med_idt pere,char *nom, med_type_champ type,
			     med_mode_switch interlace, med_size nbdim, med_size fixdim, 
			     med_size psize, med_mode_profil pflmod, med_int modifpfl, med_size * pfltab,
			     med_int ngauss, med_size *size,  unsigned char *val)
{
  char *  name = "_MEDdatasetNumEcrire";
  int     dummy=0;
  med_err fret=-1;
  med_int majeur, mineur, release;
  MedFuncType func;

  MEDversionLire(pere, &majeur, &mineur, &release);

  func = _MEDversionedApi(name,majeur,mineur,release);
  if ( func != (MedFuncType) NULL )
    func  (dummy, pere, nom,  type,
	    interlace, nbdim, fixdim,
	    psize, pflmod, modifpfl, pfltab,
	    ngauss, size,val, &fret);

  return fret;
}
