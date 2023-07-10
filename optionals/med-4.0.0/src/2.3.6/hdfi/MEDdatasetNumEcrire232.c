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


#ifdef TRACEPFL
#define tracepfl(x) x
#else
#define tracepfl(x)
#endif

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
 *       - pfltab  (IN) : Tableau contenant les n° déléments à traiter (1....oo)
 *       - ngauss  (IN) : Nombre de points de GAUSS par élément
 *     - size (IN)     : Taille du tableau de valeurs
 *                        (référence tous les élements, cette taille  prend en compte le nombre de pts de gauss et la dimension )  
 *     - val  (IN)     : valeurs du tableau
 *     - modifpfl (IN) : Si psize != MED_NOPF :
 *                         - modifpfl == 1   indique que la taille ou le contenu du profil a changé
 *                         - modifpfl == 0   indique que la taille ou le contenu du profil n'a pas changé
 * - Resultat : 0 en cas de succes, -1 sinon
 */ 

void  _MEDdatasetNumEcrire232(int dummy,...) {

  va_list params;

  med_idt         pere;
  char *          nom;
  med_type_champ  type;
  med_mode_switch interlace;
  med_size        nbdim, fixdim, psize, * pfltab, *size;
  med_mode_profil pflmod;
  med_int         ngauss,modifpfl;
  unsigned char   *val; 
  med_err *       fret;              

  med_idt    dataset=0, dataspace=0, memspace=0;
  med_size   start_mem[1],start_data[1],*pflmem=NULL,*pfldsk=NULL;
  med_size   stride[1],count[1],pcount[1],pflsize[1];
  med_size   datasetsize[1], countncmp=0;
  med_err    ret=-1;
  int        i=0,j=0,index=0;
  med_idt    type_hdf=0;
  int        dim=0, firstdim=0, dimutil=0, lastdim=0 ;
  int        datasetsizeEqualTosizespace = 0;
  med_mode_acces MED_MODE_ACCES;

  va_start(params,dummy);
  pere      = va_arg(params,med_idt);
  nom       = va_arg(params,char *);
  type      = va_arg(params,med_type_champ);
  interlace = va_arg(params,med_mode_switch);
  nbdim     = va_arg(params,med_size);
  fixdim    = va_arg(params,med_size); 
  psize     = va_arg(params,med_size);
  pflmod    = va_arg(params,med_mode_profil);
  modifpfl  = va_arg(params,med_int);
  pfltab    = va_arg(params,med_size *);
  ngauss    = va_arg(params,med_int);
  size      = va_arg(params,med_size *);
  val       = va_arg(params,  unsigned char *);
  fret      = va_arg(params,  med_err *);

  
  if ( (MED_MODE_ACCES = _MEDmodeAcces(pere) ) == MED_UNDEF_MODE_ACCES ) {
    MESSAGE("Impossible de déterminer le mode d'acces au fichier.");
    goto ERROR;
  }

  if ( MED_MODE_ACCES == MED_LECTURE) {
    MESSAGE("Impossible d'écrire un dataset en mode MED_LECTURE.");
    goto ERROR;
  };

  switch(type)
    {
    case MED_FLOAT64 :
      type_hdf = H5T_NATIVE_DOUBLE;
      break;

    case MED_INT32 :
      type_hdf = H5T_NATIVE_INT;
      break;

    case MED_INT64 :
      type_hdf = H5T_NATIVE_LONG;
      break;

    default :
      goto ERROR;
    }

  /* Calcul de la taille du dataset cible*/
  if ( psize == MED_NOPF )
    datasetsize[0] = size[0];
  else
    datasetsize[0] = psize*ngauss*nbdim;

  /* On s'assure de l'existence d'un dataset.
     S'il n'existe pas, il est crée en mode MED_LECTURE_ECRITURE, une
     erreur est retournée en mode MED_LECTURE_AJOUT.
     S'il existe on lit sa taille. */
  if ( (dataset = H5Dopen(pere,nom)) < 0) {

    if ((dataspace = H5Screate_simple(1,datasetsize,NULL)) < 0) {
      MESSAGE("Impossible de créer un dataspace de taille : ");
      ISCRUTE_size(datasetsize[0]);
      goto ERROR;
    }
    if ((dataset = H5Dcreate(pere,nom,type_hdf,dataspace,
			     H5P_DEFAULT)) < 0) {
      MESSAGE("Impossible de créer un le dataset de nom : ");
      SSCRUTE(nom);
      goto ERROR;
    }

  } else {

    /* L'ajout s'entend au niveau de l'entité dataset qui ne doit pas préexister */
    if ( MED_MODE_ACCES == MED_LECTURE_AJOUT )  {
      MESSAGE("Impossible de créer un nouveau dataset en mode MED_LECTURE_AJOUT.");
      goto ERROR;
    }

    if ( (dataspace = H5Dget_space(dataset)) <0 ) {
      MESSAGE("Impossible de déterminer la taille du dataset existant : ");
      ISCRUTE_int(dataset);
      goto ERROR;
    }

    {
      hsize_t   sizespace   [H5S_MAX_RANK];
      hsize_t   maxsizespace[H5S_MAX_RANK];

      H5Sget_simple_extent_dims(dataspace, sizespace, maxsizespace);
      datasetsizeEqualTosizespace = ( (sizespace[0]) == datasetsize[0] );
    }

    /* Si un dataset prexiste en MED_LECTURE_ECRITURE
       Il est possible de compléter/écraser 
       les valeurs du dataset par les nouvelles valeurs si :
       - sa taille  ne change pas
       - son profil ne change pas (taille ou contenu)
    */
    
    if ( ( !datasetsizeEqualTosizespace ) || ( psize != MED_NOPF && modifpfl ) ) {
      H5Dclose(dataset); 
      if (H5Gunlink(pere, nom) < 0) goto ERROR;
      if ( (dataset = H5Dcreate(pere,nom,type_hdf,dataspace,H5P_DEFAULT)) < 0)
	goto ERROR;
    }

  }

  /* Verify fixdim is between [0, nbdim] ( 0 is MED_ALL ) */
  if ( fixdim > nbdim ) {
    MESSAGE("Impossible d'écrire sur une dimension fixée non comprise entre 0 et le nombre de dimensions : ");
    ISCRUTE_size(nbdim);
    goto ERROR;
  }
  
  switch(interlace) {  /* switch Interlace */
  case MED_FULL_INTERLACE :

    /*Initialisation des indices de boucle du traitement de l'entrelacement en fonction de la dimension fixee*/
    if ( fixdim != MED_ALL) {
      firstdim = fixdim-1;
      lastdim  = fixdim;
      dimutil  = 1;
    }
    else	{
      firstdim = 0;
      lastdim  = nbdim;
      dimutil  = nbdim;
    }

    /* ancienne formule             */
    /* count [0] = (*size)/(nbdim); */
    count[0] = datasetsize[0] / nbdim;


    if ( psize == MED_NOPF ) {

      tracepfl(printf("%s branche 1 : %lld\n", __FILE__, fixdim));

      /* Creation d'un data space mémoire de dimension 1, de longeur size, et de longeur maxi size */
      if ( (memspace = H5Screate_simple (1, size, NULL)) <0) {
	MESSAGE("Impossible de créer un memspace de taille : ");
	ISCRUTE_size(*size);
	goto ERROR;
      }

      stride[0] = nbdim;

      for (dim=firstdim; dim < lastdim; dim++) {

	start_mem[0] = dim;
	if ( (ret = H5Sselect_hyperslab (memspace, H5S_SELECT_SET, start_mem, stride,
					 count, NULL)) <0) {
	  MESSAGE("Impossible d'effectuer une sélection sur le memspace  : ");
	  ISCRUTE(memspace);
	  goto ERROR;
	}

	start_data[0] = dim*count[0];
	if ( (ret = H5Sselect_hyperslab (dataspace, H5S_SELECT_SET, start_data, NULL,
					 count, NULL)) <0) {
	  MESSAGE("Impossible d'effectuer une sélection sur le dataspace  : ");
	  ISCRUTE_int(dataspace);
	  goto ERROR;
	}

	if ((ret = H5Dwrite(dataset,type_hdf,memspace,dataspace,
			    H5P_DEFAULT, val)) < 0) {
	  MESSAGE("Erreur à l'écriture du dataset : ");
	  ISCRUTE_int(dataset);
	  MESSAGE("en mode MED_FULL_INTERLACE sans profil"); 
	  goto ERROR;
	}
      }

    } else { /* psize != MED_NOPF */

      pflsize [0] = psize*ngauss*nbdim;
      pcount  [0] = psize*ngauss*dimutil;
      pflmem      = (med_size *) malloc (sizeof(med_size)*pcount[0]);
      pfldsk      = (med_size *) malloc (sizeof(med_size)*pcount[0]);

      switch(pflmod) { /* gestion des du mode GLOBAL/COMPACT ne entrelacement FULL_INTERLACE*/
      case MED_GLOBAL :

	tracepfl(printf("%s branche 2 : %lld\n", __FILE__, fixdim));

	/* Creation d'un data space mémoire de dimension 1, de longeur size, et de longeur maxi size */
	if ( (memspace = H5Screate_simple (1, size, NULL)) <0) {
	  MESSAGE("Impossible de créer un memspace de taille : ");
	  ISCRUTE_size(*size);
	  goto ERROR;
	}

	for (dim=firstdim; dim < lastdim; dim++) {

	  for (i=0; i < psize; i++) {              /* i balaye les élements du profil */
	    for (j=0; j < ngauss; j++) {
	      index = i*ngauss+j + (dim-firstdim)*(psize*ngauss);
	      pflmem[index] = (pfltab[i]-1)*ngauss*nbdim + j*nbdim+dim;
	      /* ancienne formule                                       */
	      /* pfldsk[index] = dim*count[0] + (pfltab[i]-1)*ngauss+j; */
	      pfldsk[index] = dim*count[0] + i*ngauss+j;
	    }
	  }
	}

	if ( (ret = H5Sselect_elements(memspace,H5S_SELECT_SET, pcount[0], HDF5_SELECT_BUG pflmem ) ) <0) {
	  MESSAGE("Impossible d'effectuer une sélection sur le memspace  : ");
	  ISCRUTE(memspace);
	  goto ERROR;
	}

	if ( (ret = H5Sselect_elements(dataspace,H5S_SELECT_SET, pcount[0], HDF5_SELECT_BUG pfldsk ) ) <0) {
	  MESSAGE("Impossible d'effectuer une sélection sur le dataspace  : ");
	  ISCRUTE_int(dataspace);
	  goto ERROR;
	}

	break;

      case MED_COMPACT :

	tracepfl(printf("%s branche 3 : %lld\n", __FILE__, fixdim));

	/* Creation d'un data space mémoire de dimension 1, de la longeur du profil          */
	/* La dimension utilisée est ici nbdim, même pour un profil compact on suppose       */
	/*  que l'utilisateur a toutes les coordonées stockées, même si il en demande qu'une */

	if ( (memspace = H5Screate_simple (1, pflsize, NULL)) <0) {
	  MESSAGE("Impossible de créer un memspace de taille : ");
	  ISCRUTE_size(pflsize[0]);
	  goto ERROR;
	}

	for (dim=firstdim; dim < lastdim; dim++) {

	  for (i=0; i < psize; i++) {              /* i balaye les élements du profil */
	    for (j=0; j < ngauss; j++) {
	      index = i*ngauss+j + (dim-firstdim)*(psize*ngauss);
	      pflmem[index] = i*ngauss*nbdim + j*nbdim+dim;
	      /* ancienne formule                                       */
	      /* pfldsk[index] = dim*count[0] + (pfltab[i]-1)*ngauss+j; */
	      pfldsk[index] = dim*count[0] + i*ngauss+j;
	    }
	  }
	}

	if ( (ret = H5Sselect_elements(memspace,H5S_SELECT_SET, pcount[0], HDF5_SELECT_BUG pflmem ) ) <0) {
	  MESSAGE("Impossible d'effectuer une sélection sur le memspace  : ");
	  ISCRUTE(memspace);
	  goto ERROR;
	}

	if ( (ret = H5Sselect_elements(dataspace,H5S_SELECT_SET, pcount[0], HDF5_SELECT_BUG pfldsk ) ) <0)  {
	  MESSAGE("Impossible d'effectuer une sélection sur le dataspace  : ");
	  ISCRUTE_int(dataspace);
	  goto ERROR;
	}
		
	break;

      default :
	MESSAGE("Impossible de déterminer le mode d'utilisation du profil (MED_GLOBAL/MED_COMPACT)");
	goto ERROR;
      }

      if ((ret = H5Dwrite(dataset,type_hdf,memspace,dataspace,H5P_DEFAULT, val)) < 0){
	MESSAGE("Erreur à l'écriture du dataset : ");
	ISCRUTE_int(dataset);
	MESSAGE("en mode FULL_INTERLACE avec profil"); 
	goto ERROR;
      }

      free(pflmem);pflmem=NULL;
      free(pfldsk);pfldsk=NULL;

    } /* psize != MED_NOPF */
      

    break;

  case MED_NO_INTERLACE :
      
    /*Initialisation des indices de boucle du traitement de l'entrelacement en fonction de la dimension fixee*/

    /* ancienne formule           */
    /* count [0] = (*size)/nbdim; */
    count[0] = datasetsize[0] / nbdim;

    if ( psize == MED_NOPF ) { /* psize == MED_NOPF, MED_NO_INTERLACE */

      tracepfl(printf("%s branche 4 : %lld\n", __FILE__, fixdim));

      if ( fixdim != MED_ALL)
	start_data[0] = (fixdim-1)*count[0];
      else {
	count[0] = *size;
	start_data[0] =  0;
      };

      if ( (ret = H5Sselect_hyperslab (dataspace, H5S_SELECT_SET, start_data, NULL,
				       count, NULL)) <0) {
	MESSAGE("Impossible d'effectuer une sélection sur le dataspace  : ");
	ISCRUTE_int(dataspace);
	goto ERROR;
      }

      if ((ret = H5Dwrite(dataset,type_hdf,dataspace,dataspace,
			  H5P_DEFAULT, val)) < 0) {
	MESSAGE("Erreur à l'écriture du dataset : ");
	ISCRUTE_int(dataset);
	MESSAGE("en mode MED_NO_INTERLACE sans profil"); 
	goto ERROR;
      }

    } else {  /* psize != MED_NOPF, MED_NO_INTERLACE */
	
      if ( fixdim != MED_ALL) {
	firstdim = fixdim-1;
	lastdim  = fixdim;
	dimutil  = 1;
      } else {
	firstdim = 0;
	lastdim  = nbdim;
	dimutil  = nbdim;
      }

      pflsize [0] = psize*ngauss*nbdim;
      pcount  [0] = psize*ngauss*dimutil; /* nom pas très coherent avec count !!! A revoir */
      pfldsk     = (med_size *) malloc(sizeof(med_size)*pcount[0]);

      switch(pflmod) { /*switch plfmod pour GLOBAL/COMPACT */
      case MED_GLOBAL :

	tracepfl(printf("%s branche 5 : %lld\n", __FILE__, fixdim));

	/* ajout de memspace pour le compactage */
	  
	/* Creation d'un data space mémoire de dimension 1, de la longeur du profil          */
	/* La dimension utilisée est ici nbdim, même pour un profil compact on suppose       */
	/*  que l'utilisateur a toutes les coordonées stockées, même s'il en demande qu'une */
	  
	if ( (memspace = H5Screate_simple (1, size, NULL)) <0) {
	  MESSAGE("Impossible dde créer le memspace de taille ");
	  ISCRUTE_size(*size);
	  goto ERROR;
	}

	pflmem     = (med_size *) malloc (sizeof(med_size)*pcount[0]);
	countncmp  = size[0] / nbdim;

	for (dim=firstdim; dim < lastdim; dim++) {

	  for (i=0; i < psize; i++) {              /* i balaye le nbre d'élements du profil                */
	    for (j=0; j < ngauss; j++) {
	      index = i*ngauss+j + (dim-firstdim)*(psize*ngauss);
	      pflmem[index] = dim*countncmp + (pfltab[i]-1)*ngauss+j;
	      /* ancienne formule                                       */
	      /* pfldsk[index] = dim*count[0] + (pfltab[i]-1)*ngauss+j; */
	      pfldsk[index] = dim*count[0] + i*ngauss+j;
	    }
	  }
	}

	if ( (ret = H5Sselect_elements(memspace ,H5S_SELECT_SET,pcount[0], HDF5_SELECT_BUG pflmem ) ) <0) {
	  MESSAGE("Impossible d'effectuer une sélection sur le memspace  : ");
	  ISCRUTE(memspace);
	  goto ERROR;
	}

	if ( (ret = H5Sselect_elements(dataspace,H5S_SELECT_SET,pcount[0], HDF5_SELECT_BUG pfldsk ) ) <0) {
	  MESSAGE("Impossible d'effectuer une sélection sur le dataspace  : ");
	  ISCRUTE_int(dataspace);
	  goto ERROR;
	}

	if ((ret = H5Dwrite(dataset,type_hdf,memspace,dataspace,H5P_DEFAULT, val)) < 0) {
	  MESSAGE("Erreur à l'écriture du dataset : ");
	  ISCRUTE_int(dataset);
	  MESSAGE("en mode NO_INTERLACE avec profil GLOBAL"); 
	  goto ERROR;
	}

	free(pflmem);pflmem=NULL;

	break;

      case MED_COMPACT :

	tracepfl(printf("%s branche 6 : %lld\n", __FILE__, fixdim));

	/* memspace est en fait inutile pour le compactage */

	/* Creation d'un data space mémoire de dimension 1, de la longeur du profil          */
	/* La dimension utilisée est ici nbdim, même pour un profil compact on suppose       */
	/*  que l'utilisateur a toutes les coordonées stockées, même si il en demande qu'une */

	if ( (memspace = H5Screate_simple (1, pflsize, NULL)) <0) {
	  MESSAGE("Impossible de créer le memspace de taille : ");
	  ISCRUTE_llong(pflsize[0]);
	  goto ERROR;
	}

	pflmem     = (med_size *) malloc (sizeof(med_size)*pcount[0]);

	/* Le profil COMPACT est contigüe, mais il est possible que l'on selectionne uniquemenent une dimension*/

	index = 0;
	for (dim=firstdim; dim < lastdim; dim++) {

	  for (i=0; i < psize; i++) {              /* i balaye le nbre d'élements du profil                */
	    for (j=0; j < ngauss; j++) {
	      /*FORMULATION 1ere : index = i*ngauss+j + (dim-firstdim)*(psize*ngauss);*/
	      /*FORMULATION 1ere : pflmem[index] = dim*(psize*ngauss) + i*ngauss+j;*/
	      pflmem[index] = ( (dim*psize) + i )*ngauss + j;
	      /* ancienne formule                                       */
	      /* pfldsk[index] = dim*count[0] + (pfltab[i]-1)*ngauss+j; */
	      pfldsk[index] = dim*count[0] + i*ngauss+j;
	      index++;
	    }
	  }
	}

	if ( (ret = H5Sselect_elements(memspace ,H5S_SELECT_SET,pcount[0], HDF5_SELECT_BUG pflmem ) ) <0) {
	  MESSAGE("Impossible d'effectuer une sélection sur le memspace  : ");
	  ISCRUTE(memspace);
	  goto ERROR;
	}

	if ( (ret = H5Sselect_elements(dataspace,H5S_SELECT_SET,pcount[0], HDF5_SELECT_BUG pfldsk ) ) <0) {
	  MESSAGE("Impossible d'effectuer une sélection sur le dataspace  : ");
	  ISCRUTE_int(dataspace);
	  goto ERROR;
	}

	if ((ret = H5Dwrite(dataset,type_hdf,memspace,dataspace,H5P_DEFAULT, val)) < 0) {
	  MESSAGE("Erreur à l'écriture du dataset : ");
	  ISCRUTE_int(dataset);
	  MESSAGE("en mode MED_NO_INTERLACE avec profil COMPACT"); 
	  goto ERROR;
	}
	free(pflmem);pflmem=NULL;

	break;

      default :
	MESSAGE("Impossible de déterminer le mode d'utilisation du profil (MED_GLOBAL/MED_COMPACT en entrelacement MED_NO_INTERLACE"); 
	goto ERROR;

      }
      free(pfldsk);pfldsk=NULL;

    } /* fin du else ( psize != MED_NOPF ) */

    break;

  default :
    MESSAGE("Mode d'entrelacement inconnu.");
    goto ERROR;
  }
  
  ret = 0;
  
 ERROR:

  if (pflmem) free(pflmem);
  if (pfldsk) free(pfldsk);

  if ( memspace ) if ( (ret = H5Sclose(memspace)) < 0) {
    MESSAGE("Impossible de fermer le memspace : ");
    ISCRUTE(memspace);  ret = -1; 
  }

  if ( dataspace ) if ( (ret = H5Sclose(dataspace)) < 0) {
    MESSAGE("Impossible de fermer le dataspace : ");
    ISCRUTE_int(dataspace); ret = -1; 
  }

  if ( dataset ) if ( (ret = H5Dclose(dataset)) < 0) {
    MESSAGE("Impossible de fermer le dataset : ");
    ISCRUTE_int(dataset);   ret = -1; 
  }
  
  va_end(params);
  *fret = ret;
  return;
}
