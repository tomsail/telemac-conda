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


med_err
MED23v30axesInfo(med_idt fid, char *maa, med_repere *type_rep,
		 char *nom, char *unit)
{
  med_err      _ret=-1;
  med_idt      maaid=0, noeid=0;
  med_idt      dataset[3]= {0} ;
  char         *griddatasetname[3]={MED_NOM_IN1,MED_NOM_IN2,MED_NOM_IN3};
  med_int      _ndataset=1;
  char         chemin[MED_TAILLE_MAA+MED_TAILLE_NOM+1];
  med_int      type_rep_int;
  med_int      _spacedim=0,_meshdim=0,_nsubstring=0;
  med_int      att=0;
  med_maillage maillage_type;
  med_type_grille type;
  int          _i=0,len=0,_p=0;
  char*        _ptmp=NULL;
  med_booleen  _nomlenwas0=MED_FAUX,_unitlenwas0=MED_FAUX;
  /*
   * On inhibe le gestionnaire d'erreur
   */
  _MEDmodeErreurVerrouiller();
  if (MEDcheckVersion(fid) < 0) return -1;


  /*
   * Si le maillage n'existe pas => erreur
   * Sinon on recupere sa dimension au passage
   */
  strcpy(chemin,MED_MAA);
  strcat(chemin,maa);
  if ((maaid = _MEDdatagroupOuvrir(fid,chemin)) < 0) { /*ICI;*/ goto ERROR;}

  /*
   * On va chercher l'attribut dimension
   */
  if ( _MEDattrEntierLire(maaid,MED_NOM_DIM,&_meshdim) < 0) { /*ICI;*/ goto ERROR;}

  if ( (_spacedim = MEDdimEspaceLire(fid, maa) < 0 ) ) _spacedim=_meshdim;
  _nsubstring = _spacedim;

  /*
   * Si le Data Group "NOE" n'existe pas => erreur
   */
  if ((noeid = _MEDdatagroupOuvrir(maaid,MED_NOM_NOE)) < 0) { /*ICI;*/ goto ERROR;}

  /*
   * Si le maillage est de type MED_NON_STRUCTURE
   */
  if ( _MEDattrEntierLire(maaid,MED_NOM_TYP,&att) < 0) { /*ICI;*/ goto ERROR;}
  maillage_type = (med_maillage) att;

  if (maillage_type == MED_NON_STRUCTURE) {
    /*
     * On Ouvre le Data Set "COO" pour y lire des attributs
     */
    if ((dataset[0] = _MEDdatasetOuvrir(noeid,MED_NOM_COO)) < 0) { /*ICI;*/ goto ERROR;}

  } else {
    /*
     * Grille  MED_GRILLE_STANDARD --> dataset==COO sinon dataset==INDx
     */
    if ( _MEDattrEntierLire(maaid,MED_NOM_GTY,&att) < 0) goto ERROR;
    type = (med_type_grille) att;

    if (type != MED_GRILLE_STANDARD) {
      _ndataset=_meshdim;
      _nsubstring = 1;

      for (_i= 0; _i<_ndataset; ++_i) {
	if ((dataset[_i] = _MEDdatasetOuvrir(noeid,griddatasetname[_i])) < 0) { /*ICI;*/goto ERROR;}
      }
    } else {
      /*
       * On Ouvre le Data Set "COO" pour y lire des attributs
       */
      if ((dataset[0] = _MEDdatasetOuvrir(noeid,MED_NOM_COO)) < 0) { /*ICI;*/ goto ERROR;}
    }

  }


  /*
   * L'attribut "REP"
   */
  if ( (maillage_type == MED_STRUCTURE) && (type != MED_GRILLE_STANDARD) ) {
    *type_rep = MED_CART;
  } else
    if (  _MEDattrEntierLire(dataset[0],MED_NOM_REP,&type_rep_int) < 0 )
      { /*ICI;*/ goto ERROR;}
    else
      *type_rep = (med_repere) type_rep_int;

  for (_i= 0; _i<_ndataset; ++_i) {

    /*
     * Attribut "NOM"
     */
    _ptmp =&nom[MED_TAILLE_PNOM*_i];
    if ( _MEDattrStringLire(dataset[_i],MED_NOM_NOM,_nsubstring*MED_TAILLE_PNOM,
			    _ptmp) < 0)
      { /*ICI;*/ goto ERROR;}

    len=strlen(_ptmp);
    if ( len == 0 ) _nomlenwas0=MED_VRAI;
    if ( (len > 0 ) && _nomlenwas0 ) {
      MESSAGE("Un des noms d'axes est vide et d'autres non!");
    }
    if ( (len <  MED_TAILLE_PNOM ) && (len > 0) ) {
      for (_p=(MED_TAILLE_PNOM-1); _p>=len; --_p) *( _ptmp + _p)=' ';
      *(_ptmp+MED_TAILLE_PNOM)='\0';
    }

/*     ISCRUTE(_nsubstring); */
/*     ISCRUTE(dataset[_i]); */
/*     SSCRUTE(nom); */
/*     SSCRUTE(&nom[MED_TAILLE_PNOM*_i]); */

    /*
     * Attribut "UNI"
     */
    _ptmp =&unit[MED_TAILLE_PNOM*_i];
    if ( _MEDattrStringLire(dataset[_i],MED_NOM_UNI,_nsubstring*MED_TAILLE_PNOM,
			    _ptmp) < 0)
      { /*ICI;*/ goto ERROR;}

    len=strlen(_ptmp);
    if ( len == 0 ) _unitlenwas0=MED_VRAI;
    if ( (len > 0 ) && _unitlenwas0 ) {
      MESSAGE("Un des noms d'unité d'axes est vide et d'autres non!");
    }
    if ( (len <  MED_TAILLE_PNOM) && (len > 0) ) {
      for (_p=(MED_TAILLE_PNOM-1); _p>=len; --_p) *( _ptmp + _p)=' ';
      *(_ptmp+MED_TAILLE_PNOM)='\0';
    }

  }

  /*
   * On ferme tout
   */

  _ret = 0;
 ERROR:

  for (_i= 0; _i<_ndataset; ++_i)
    if (dataset[_i] > 0 )
      if ( _MEDdatasetFermer(dataset[_i]) < 0)
	_ret= -1;

  if (noeid > 0 )
    if ( _MEDdatagroupFermer(noeid) < 0)
      _ret= -1;
  if (maaid > 0 )
    if (  _MEDdatagroupFermer(maaid) < 0)
      _ret= -1;

  return _ret;
}
