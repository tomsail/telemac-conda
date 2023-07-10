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

/*
 * - Nom de la fonction : MEDchampEcr
 * - Description : ecriture d'un Champ Résultat
 * - Parametres :
 *     - fid      (IN)  : ID du fichier HDF courant
 *     - maa      (IN)  : le nom du maillage sur lequel s'applique le champ (eventuellement distant)
 *     - cha      (IN)  : le nom du champ
 *     - val      (IN)  : valeurs du champ à stocker
 *     - interlace(IN)  : entrelacement utilisé en mémoire {MED_FULL_INTERLACE,MED_NO_INTERLACE} 
 *     - nbelem   (IN)  : nombre d'éléments (prend en compte le nbre
 *                         de points de Gauss (c'est demandé à l'utilisateur ds la doc)
                           mais pas le nbre de composantes)
 *     - locname  (IN)  : clé utilisée pour la définition de la localisation
                          des points de GAUSS (MED_NOGAUSS si aucun, MED_GAUSS_ELNO si les points de Gauss
 *                        portent sur les noeuds de l'element). La localisation doit exister 
 *                        avant l'appel à MEDchampEcr.
 *     - numco    (IN)  : n° de la composante à stocker (MED_ALL si toutes)
 *     - profil   (IN)  : nom du profil utilisé (MED_NOPFL si inutilisé)
 *     - pflmod   (IN)  : Indique comment lire les informations en mémoire { MED_COMPACT, MED_GLOBAL }. 
 *     - type_ent (IN)  : entité concerné par le champ {MED_NOEUD,MED_ARETE,MED_FACE,MED_MAILLE}
 *     - type_geo (IN)  : type géométrique de l'entité concerné {MED_POINT,MED_SEG2 ......}
 *     - numdt    (IN)  : n° du pas de temps (MED_NOPDT si aucun)
 *     - dt_unit  (IN)  : chaine de taille MED_NOMP indiquant l'unité du champ
 *     - dt       (IN)  : valeur du pas de temps
 *     - numo     (IN)  : n° d'ordre utilisé MED_NONOR si inutile
 * - Resultat : 0 en cas de succes, -1 sinon
 */

med_err MEDchampEcr(med_idt fid, char *maa, char *cha,
		    unsigned char *val, med_mode_switch interlace, med_int nbelem, char * locname, 
		    med_int numco, char * profil, med_mode_profil pflmod,
		    med_entite_maillage type_ent, med_geometrie_element type_geo,
		    med_int numdt,char * dt_unit, med_float dt, med_int numo)
{
  char *  name = "MEDchampEcr";
  int     dummy=0;
  med_err fret=-1;
  med_int majeur, mineur, release;
  MedFuncType func;

  MEDversionLire(fid, &majeur, &mineur, &release);

  func = _MEDversionedApi(name,majeur,mineur,release);
  if ( func != (MedFuncType) NULL )
    func (dummy, fid, maa, cha,
	  val, interlace, nbelem, locname,
	  numco, profil, pflmod,
	  type_ent, type_geo,
	  numdt, dt_unit, dt, numo , &fret);


  return fret;
}
