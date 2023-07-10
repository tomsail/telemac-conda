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

#ifndef MED23_PROTO_H
#define MED23_PROTO_H

#include "medC_win_dll.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Interface de l'API MED */

/* Fichier */
MEDC_EXPORT med_idt
MEDouvrir(char *nom, med_mode_acces mode_acces);
MEDC_EXPORT med_err 
MEDfermer(med_idt fid); 
MEDC_EXPORT med_int
MEDlFichDes(med_idt fid);
MEDC_EXPORT med_err 
MEDfichEntete(med_idt fid, med_fich_info quoi, char str[]); 
MEDC_EXPORT med_err 
MEDfichDesEcr(med_idt fid, char *des); 
MEDC_EXPORT med_err
MEDunvCr(med_idt fid, char *maa);
MEDC_EXPORT med_err
MEDunvLire(med_idt fid, char *maa,char *nomu);
MEDC_EXPORT med_err
MEDformatConforme(const char * nomfich);
MEDC_EXPORT med_err
MEDversionConforme(const char *nom);
MEDC_EXPORT void
MEDversionDonner(med_int *majeur, med_int *mineur, med_int *release);
MEDC_EXPORT med_err 
MEDversionLire(med_idt fid, med_int *majeur, med_int *mineur, med_int *release);
MEDC_EXPORT med_idt
MEDmonter(med_idt fid, const char *acces, med_type_donnee type);
MEDC_EXPORT med_err
MEDdemonter(med_idt fid,med_idt mid,med_type_donnee type);


/* Maillage */
MEDC_EXPORT med_err 
MEDmaaCr(med_idt fid, char *maillage, med_int dim,med_maillage type,char *desc);
MEDC_EXPORT med_err
MEDdimEspaceCr(med_idt fid, char *maillage, med_int dim);
MEDC_EXPORT med_int
MEDdimLire(med_idt fid, char *maillage); 
MEDC_EXPORT med_int 
MEDdimEspaceLire(med_idt fid, char *maillage);
MEDC_EXPORT med_err 
MEDmaaInfo(med_idt fid, int indice, char *maillage, med_int *dim, 
	   med_maillage *type,char *desc); 
MEDC_EXPORT med_int 
MEDnMaa(med_idt fid);
MEDC_EXPORT med_err 
MEDnbnosoEcr(med_idt fid, char *nom_maillage,med_int n);
MEDC_EXPORT med_int
MEDnbnosoLire(med_idt fid,char *nom_maillage);
MEDC_EXPORT med_err 
MEDnbnoisEcr(med_idt fid, char *nom_maillage,med_int n);
MEDC_EXPORT med_int
MEDnbnoisLire(med_idt fid,char *nom_maillage);
MEDC_EXPORT med_err
MEDnbnomaEcr(med_idt fid, char *nom_maillage,med_int n);
MEDC_EXPORT med_int
MEDnbnomaLire(med_idt fid,char *nom_maillage);
MEDC_EXPORT med_err
MEDnatureGrilleEcr(med_idt fid,char *maillage,med_type_grille type);
MEDC_EXPORT med_err
MEDnatureGrilleLire(med_idt fid,char *maillage,med_type_grille *type);

/* EntMaillage */
MEDC_EXPORT med_err 
MEDconnEcr(med_idt fid,char *maa, med_int mdim, med_int *connectivite,med_mode_switch mode_switch,
	   med_int nbre, med_entite_maillage type_ent,
	   med_geometrie_element type_geo,med_connectivite type_conn);

MEDC_EXPORT med_err 
MEDconnLire(med_idt fid,char *maa,med_int mdim,med_int *connectivite,med_mode_switch mode_switch,
	    med_int * pfltab, med_size psize,
	    med_entite_maillage type_ent, med_geometrie_element type_geo,med_connectivite type_conn);
MEDC_EXPORT med_err 
MEDnomEcr(med_idt fid,char *maa, char *nom, med_int n,
	  med_entite_maillage type_ent,med_geometrie_element type_geo); 
MEDC_EXPORT med_err 
MEDnomLire(med_idt fid,char *maa, char *nom, med_int n, 
	   med_entite_maillage type_ent,med_geometrie_element type_geo); 
MEDC_EXPORT med_err 
MEDnumLire(med_idt fid,char *maa, med_int *num, med_int n, 
	   med_entite_maillage type_ent,med_geometrie_element type_geo); 
MEDC_EXPORT med_err 
MEDnumEcr(med_idt fid,char *maa, med_int *num, med_int n,
	  med_entite_maillage type_ent,med_geometrie_element type_geo);
MEDC_EXPORT med_err 
MEDcoordEcr(med_idt fid, char *maa, med_int mdim, med_float *coo, 
	    med_mode_switch mode_coo,med_int n,
	    med_repere type_rep, char *nom, char *unit);
MEDC_EXPORT med_err
MEDindicesCoordEcr(med_idt fid,char *maillage,med_int mdim,med_float *indices,
		   med_int n,med_int axe,char *comp,char *unit);
MEDC_EXPORT med_err
MEDstructureCoordEcr(med_idt fid,char *maillage,med_int mdim,med_int *structure);
MEDC_EXPORT med_err 
MEDcoordLire(med_idt fid, char *maa, med_int mdim, med_float *coo,
	     med_mode_switch mode_coo,med_int numco,
	     med_int * pfltab, med_size psize, med_repere *type_rep, char *nom, char *unit);
MEDC_EXPORT med_err
MEDindicesCoordLire(med_idt fid,char *maillage,med_int mdim,med_float *indices,
		    med_int n,med_int axe,char *comp,char *unit);
MEDC_EXPORT med_err
MEDstructureCoordLire(med_idt fid,char *maillage,med_int mdim,med_int *structure);
MEDC_EXPORT med_int
MEDnEntMaa(med_idt fid, char *maa, med_table quoi, med_entite_maillage type_ent, 
	   med_geometrie_element type_geo, med_connectivite type_conn); 
MEDC_EXPORT med_err
MEDpolygoneConnEcr(med_idt fid, char *maa, med_int *index, med_int ni, med_int *con,
		   med_entite_maillage type_ent, med_connectivite type_conn);
MEDC_EXPORT med_err
MEDpolygoneInfo(med_idt fid, char *maa, med_entite_maillage type_ent,med_connectivite type_conn,
		med_int *consize);
MEDC_EXPORT med_err
MEDpolygoneConnLire(med_idt fid, char *maa, med_int *index, med_int ni, med_int *con,
		    med_entite_maillage type_ent, med_connectivite type_conn);
MEDC_EXPORT med_err
MEDpolyedreConnEcr(med_idt fid,char *maa,med_int *indexp,med_int np,med_int *indexf,med_int nf,
		   med_int *con,med_connectivite type_conn);
MEDC_EXPORT med_err
MEDpolyedreInfo(med_idt fid, char *maa,med_connectivite type_conn,med_int *nf,
		med_int *consize);
MEDC_EXPORT med_err
MEDpolyedreConnLire(med_idt fid,char *maa,med_int *indexp,med_int np,med_int *indexf,med_int nf,
		    med_int *con,med_connectivite type_conn);

/* Resultat */
MEDC_EXPORT med_err MEDchampCr(med_idt fid, char *champ, med_type_champ type, char *comp,
		   char *unit,med_int ncomp);

MEDC_EXPORT
med_err MEDchampEcr(med_idt fid, char *maa, char *cha,
		    unsigned char *val, med_mode_switch interlace, med_int nbelem, char * locname, 
		    med_int numco, char * profil, med_mode_profil pflmod, 
		    med_entite_maillage type_ent, med_geometrie_element type_geo, 
		    med_int numdt,char * dt_unit, med_float dt, med_int numo);
MEDC_EXPORT med_err 
MEDchampLire(med_idt fid,char *maa, char *cha, unsigned char *val,med_mode_switch interlace,med_int numco,
	     char * locname, char *profil, med_mode_profil pflmod, 
	     med_entite_maillage type_ent, med_geometrie_element type_geo,
	     med_int numdt, med_int numo);
 
MEDC_EXPORT med_err
MEDchampInfo(med_idt fid,int indice,char *champ,
		     med_type_champ *type,char *comp,char *unit, 
		     med_int ncomp);

MEDC_EXPORT med_err 
MEDchampRefInfo(med_idt fid,char *champ,
		med_entite_maillage type_ent, med_geometrie_element type_geo,
		int indice, med_int numdt, med_int numo,
		char * maa, med_booleen * local, med_int * ngauss);
MEDC_EXPORT med_int 
MEDnChamp(med_idt fid, int indice); 

MEDC_EXPORT med_int 
MEDnChampRef(med_idt fid, char * cha, med_entite_maillage type_ent, med_geometrie_element type_geo, med_int numdt, med_int numo);

MEDC_EXPORT
med_int 
MEDnVal(med_idt fid, char *cha, med_entite_maillage type_ent, 
	med_geometrie_element type_geo,med_int numdt, med_int numo, char * maa,med_mode_profil pflmod);

/* Famille  */
MEDC_EXPORT med_err 
MEDfamEcr(med_idt fid,char *maa, med_int *fam, med_int n,
	  med_entite_maillage type_ent, med_geometrie_element type_geo); 
MEDC_EXPORT med_err 
MEDfamLire(med_idt fid,char *maa, med_int *fam, med_int n, 
	   med_entite_maillage type_ent,med_geometrie_element type_geo);
MEDC_EXPORT med_err 
MEDfamCr(med_idt fid,char* maa,char *famille,med_int numero, 
	 med_int *attr_ident, med_int *attr_val,char *attr_desc,med_int n_attr,
	 char *groupe , med_int n_groupe);
MEDC_EXPORT med_int 
MEDnFam(med_idt fid,char *maa);
MEDC_EXPORT med_int
MEDnAttribut(med_idt fid,char *maa, int indice);
MEDC_EXPORT med_int
MEDnGroupe(med_idt fid,char *maa, int indice);
MEDC_EXPORT med_err 
MEDfamInfo(med_idt fid,char *maa,int indice, char *famille, 
	   med_int *numero,
	   med_int *attr_ident, med_int *attr_val, char *attr_desc,
	   med_int *n_attr,char *groupe ,med_int *n_groupe);  
 
/* Equivalence    */
MEDC_EXPORT med_err 
MEDequivCr(med_idt fid,char *maa, char *eq, char *desc); 
MEDC_EXPORT med_err 
MEDequivLire(med_idt fid, char *maa, char *eq, med_int *corr, med_int n,
            med_entite_maillage typ_ent,med_geometrie_element typ_geo); 
MEDC_EXPORT med_err 
MEDequivEcr(med_idt fid, char *maa, char *eq, med_int *corr, med_int n, 
	    med_entite_maillage typ_ent, med_geometrie_element typ_geo); 
MEDC_EXPORT med_err 
MEDequivInfo(med_idt fid, char *maa, int ind, char *eq, char *des);
MEDC_EXPORT med_int 
MEDnEquiv(med_idt fid, char *maa);
MEDC_EXPORT med_int 
MEDnCorres(med_idt fid,char *maa,char *eq,med_entite_maillage typ_ent,
	   med_geometrie_element typ_geo); 




/* Routines concernant les joints */

MEDC_EXPORT med_err 
MEDjointEcr(med_idt fid, char *maa, char *jn, med_int *corrtab, med_int n,
	    med_entite_maillage typ_ent_local,   med_geometrie_element typ_geo_local,
	    med_entite_maillage typ_ent_distant, med_geometrie_element typ_geo_distant);
		


MEDC_EXPORT med_err 
MEDjointLire(med_idt fid, char *maa, char *jn, med_int *corrtab, med_int n,
             med_entite_maillage typ_ent_local,   med_geometrie_element typ_geo_local,
             med_entite_maillage typ_ent_distant, med_geometrie_element typ_geo_distant);

MEDC_EXPORT med_err 
MEDjointCr(med_idt fid,char *maa_lcl, char *jn, char *desc,
	   med_int dom,char *maa_dist);

MEDC_EXPORT med_err
MEDjointInfo(med_idt fid,  char *maa_lcl,  int ind, char *jn, char *des,
             med_int *dom, char *maa_dist);

MEDC_EXPORT med_int
MEDjointnCorres(med_idt fid, char *maa_lcl, char *jn,
		 med_entite_maillage typ_ent_local,   med_geometrie_element typ_geo_local,
		 med_entite_maillage typ_ent_distant, med_geometrie_element typ_geo_distant);

MEDC_EXPORT med_err
MEDjointTypeCorres(med_idt fid, char *maa_lcl, char *jn, int ind,
		   med_entite_maillage *typ_ent_local,   med_geometrie_element *typ_geo_local,
		   med_entite_maillage *typ_ent_distant, med_geometrie_element *typ_geo_distant);

MEDC_EXPORT med_int
MEDnJoint(med_idt fid, char *maa);


MEDC_EXPORT med_err
MEDglobalNumEcr(med_idt fid,char *maa, med_int *num, med_int n,
                med_entite_maillage type_ent, med_geometrie_element type_geo);


MEDC_EXPORT med_err
MEDglobalNumLire(med_idt fid,char *maa, med_int *num, med_int n,
		 med_entite_maillage type_ent,med_geometrie_element type_geo);


/* Routines de niveau intermediaire */
MEDC_EXPORT med_int
MEDnEntites(med_idt fid,char *maa,med_entite_maillage typ_ent, 
            med_connectivite typ_con);

MEDC_EXPORT med_err
MEDnoeudsLire(med_idt fid,char *maa,med_int mdim, med_float *coord,
	      med_mode_switch mode_coo,
	      med_repere *repere,char *nomcoo, char *unicoo,char *nom,
	      med_booleen *inom,med_int *num,med_booleen *inum,med_int *fam,
	      med_int nnoeuds);

MEDC_EXPORT med_err
MEDnoeudsEcr(med_idt fid,char *maa,med_int mdim,med_float *coord,
	     med_mode_switch mode_coo,
	     med_repere repere,char *nomcoo, char *unicoo,char *nom,
	     med_booleen inom,med_int *num,med_booleen inum,med_int *fam,
	     med_int nnoeuds);
MEDC_EXPORT med_err
MEDelementsEcr(med_idt fid,char *maa,med_int mdim,med_int *connectivite,med_mode_switch mode_switch,
	       char *nom,med_booleen inom,med_int *num,med_booleen inum,
	       med_int *fam,med_int nele,med_entite_maillage typ_ent, 
	       med_geometrie_element typ_geo,med_connectivite typ_conn);
MEDC_EXPORT med_err
MEDelementsLire(med_idt fid,char *maa,med_int mdim,med_int *connectivite,med_mode_switch mode_switch,
	       char *nom,med_booleen *inom,med_int *num,med_booleen *inum,
	       med_int *fam,med_int nele,med_entite_maillage typ_ent, 
	       med_geometrie_element typ_geo,med_connectivite typ_conn);
MEDC_EXPORT med_err
MEDgro2famCr(med_idt fid,char *maillage,char *groupes,med_int *index,med_int ngroup,med_int *entites,
	     med_int nent,med_entite_maillage type_ent,
	     med_geometrie_element *type_geo,med_int *indexgeo,med_int ngeo); 

/* Routines concernant les profils */
MEDC_EXPORT med_err 
MEDprofilInfo(med_idt fid, int indice, char *profilname, med_int *n); 

MEDC_EXPORT med_int 
MEDnProfil(med_idt fid);

MEDC_EXPORT med_err 
MEDprofilEcr(med_idt fid,med_int *pflval,med_int n,char *profilname);

MEDC_EXPORT med_int 
MEDnValProfil(med_idt fid, char *nom);

MEDC_EXPORT med_err 
MEDprofilLire(med_idt fid, med_int *pflval, char *profilname);

/* Routines concernant les points de GAUSS */

MEDC_EXPORT med_int 
MEDnGauss(med_idt fid);

MEDC_EXPORT med_err
MEDgaussEcr(med_idt fid, med_geometrie_element type_geo, med_float *refcoo, med_mode_switch mode_coo,
	    med_int ngauss, med_float *gscoo, med_float * wg, char * locname );

MEDC_EXPORT med_err 
MEDgaussLire(med_idt fid, med_float *refcoo, med_float *gscoo, med_float * wg, med_mode_switch mode_coo, char *profilname);

MEDC_EXPORT med_err 
MEDgaussInfo(med_idt fid, int indice, char * locname, med_geometrie_element * type_geo,
	     med_int * ngauss );

/* Routines concernant les pas de temps/ numéros d'ordre */

MEDC_EXPORT med_int
MEDnPasdetemps(med_idt fid,char *cha,med_entite_maillage type_ent, 
	       med_geometrie_element type_geo);

MEDC_EXPORT med_err
MEDpasdetempsInfo(med_idt fid,char *champ,
		  med_entite_maillage type_ent, med_geometrie_element type_geo,
		  int indice, med_int * ngauss, med_int * numdt, med_int * numo,
                  char * dt_unit, med_float * dt,  char * maa, med_booleen * local, med_int *nmaa);

/* Routines concernant les liens */
MEDC_EXPORT med_int 
MEDnValLien(med_idt fid, char *maa);

MEDC_EXPORT med_err
MEDlienEcr(med_idt fid, char *lienval,char *maa);

MEDC_EXPORT med_err
MEDlienLire(med_idt fid, char * lienval, char * maa);

MEDC_EXPORT med_int 
MEDnLien(med_idt fid );

MEDC_EXPORT med_err 
MEDlienInfo( med_idt fid, int indice, char * maa, med_int * n);

/* Variables scalaires */
MEDC_EXPORT med_err
MEDscalaireCr(med_idt fid,char *scalaire, med_type_champ type, char *desc);

MEDC_EXPORT med_err
MEDscalaireEntierEcr(med_idt fid, char *scalaire, med_int val,
		     med_int numdt, char *dt_unit, med_float dt, med_int numo);

MEDC_EXPORT med_err
MEDscalaireFlottantEcr(med_idt fid, char *scalaire, med_float val, 
		       med_int numdt, char *dt_unit, med_float dt, med_int numo);

MEDC_EXPORT med_int 
MEDnScalaire(med_idt fid);

MEDC_EXPORT med_err 
MEDscalaireInfo(med_idt fid,int indice,char *scalaire,
		med_type_champ *type,char *desc);

MEDC_EXPORT med_int 
MEDnScalairePasdetemps(med_idt fid,char *scalaire);

MEDC_EXPORT med_err 
MEDscalairePasdetempsInfo(med_idt fid,char *scalaire,int indice, 
			  med_int * numdt, char * dt_unit, med_float * dt,med_int * numo);

MEDC_EXPORT med_err 
MEDscalaireEntierLire(med_idt fid,char *scalaire, med_int  *val,med_int numdt, med_int numo);

MEDC_EXPORT med_err 
MEDscalaireFlottantLire(med_idt fid,char *scalaire, med_float  *val,med_int numdt, med_int numo);

#ifdef __cplusplus
}
#endif

#endif /* MED23_PROTO_H */




