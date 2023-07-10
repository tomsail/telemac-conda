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
 *       - pfltab  (IN) : Tableau contenant les n° déléments à traiter (1....oo)
 *       - ngauss  (IN) : Nombre de points de GAUSS par élément 
 *     - size (IN)     : Taille du tableau de valeurs
 *                        (référence tous les élements, cette taille  prend en compte le nombre de pts de gauss et la dimension )  
 *     - val  (IN)     : valeurs du tableau
 * - Resultat : 0 en cas de succes, -1 sinon
 */ 

void  _MEDdatasetNumEcrire231(int dummy,...) {

  va_list params;

  med_idt         pere;
  char *          nom;
  med_type_champ  type;
  med_mode_switch interlace;
  med_size        nbdim, fixdim, psize, * pfltab, *size;
  med_mode_profil pflmod;
  med_int         ngauss;
  unsigned char   *val; 
  med_int         modifpfl;
  med_err *       fret;              

  med_idt    dataset, dataspace = 0, memspace = 0;
  med_size   start_mem[1],start_data[1],*pflmem,*pfldsk;
  med_size   stride[1],count[1],pcount[1],pflsize[1];
  med_err    ret;
  int        i,j,index;
  med_idt    type_hdf;
  int        dim, firstdim, dimutil, lastdim ;
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
 
  /* Verify fixdim is between [0, nbdim] ( 0 is MED_ALL ) */
  if ( fixdim > nbdim ) 
    goto Fail;

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
      goto Fail;
    }

  if ( (MED_MODE_ACCES = _MEDmodeAcces(pere) ) == MED_UNDEF_MODE_ACCES ) {
    MESSAGE("Impossible de déterminer le mode d'acces au fichier ");
    goto Fail;
  }
  
  if ((dataset = H5Dopen(pere,nom)) < 0)
    {
      /* Whatever the size of the profil is we create a dataset with the size of the value array                   */
      /* Then if we used the MED_LECTURE_AJOUT mode we can append a new dimension to a previous one in the dataset */
      /* When we'll use the compression mode, the space used by unused values would be easily compressed           */
  
      if ((dataspace = H5Screate_simple(1,size,NULL)) < 0)
	goto Fail;
      if ((dataset = H5Dcreate(pere,nom,type_hdf,dataspace,
			       H5P_DEFAULT)) < 0)
	goto Fail;      
    }
  else
    if ( MED_MODE_ACCES == MED_LECTURE_AJOUT )
      {
	H5Dclose(dataset);
	goto Fail;
      }
    else
      if ((dataspace = H5Dget_space(dataset)) <0)
	goto Fail;


  switch(interlace) 
    {  /* switch Interlace */
    case MED_FULL_INTERLACE :
      
      /*Initialisation des indices de boucle du traitement de l'entrelacement en fonction de la dimension fixee*/
      if ( fixdim != MED_ALL) 
	{ 
	  firstdim = fixdim-1;
	  lastdim  = fixdim;
	  dimutil  = 1;
	} else	{
	  firstdim = 0;
	  lastdim  = nbdim;
	  dimutil  = nbdim; 
	}

      count [0] = (*size)/(nbdim);


      if ( psize == MED_NOPF ) {  

	/* Creation d'un data space mémoire de dimension 1, de longeur size, et de longeur maxi size */
	if ( (memspace = H5Screate_simple (1, size, NULL)) <0)
	  goto Fail;
	
	stride[0] = nbdim;  

	for (dim=firstdim; dim < lastdim; dim++) {
	  
	  start_mem[0] = dim;
	  if ( (ret = H5Sselect_hyperslab (memspace, H5S_SELECT_SET, start_mem, stride, 
					   count, NULL)) <0)
	    goto Fail; 
	  
	  start_data[0] = dim*count[0];
	  if ( (ret = H5Sselect_hyperslab (dataspace, H5S_SELECT_SET, start_data, NULL, 
					   count, NULL)) <0)
	    goto Fail; 
	  
	  if ((ret = H5Dwrite(dataset,type_hdf,memspace,dataspace,
			      H5P_DEFAULT, val)) < 0)
	    goto Fail;
	}
	
      } else { /* psize != MED_NOPF */
	
	pflsize [0] = psize*ngauss*nbdim;
	pcount  [0] = psize*ngauss*dimutil;
	pflmem      = (med_size *) malloc (sizeof(med_size)*pcount[0]);
	pfldsk      = (med_size *) malloc (sizeof(med_size)*pcount[0]);
	
	switch(pflmod)
	  { /* switch pflmod pout FULL_INTERLACE*/
	  case MED_GLOBAL :

	    /* Creation d'un data space mémoire de dimension 1, de longeur size, et de longeur maxi size */
	    if ( (memspace = H5Screate_simple (1, size, NULL)) <0)
	      goto Fail;

	    for (dim=firstdim; dim < lastdim; dim++) {
	      
	      for (i=0; i < psize; i++)              /* i balaye les élements du profil */
		for (j=0; j < ngauss; j++) {         
		  index = i*ngauss+j + (dim-firstdim)*(psize*ngauss);
		  pflmem[index] = (pfltab[i]-1)*ngauss*nbdim + j*nbdim+dim;
		  pfldsk[index] = dim*count[0] + (pfltab[i]-1)*ngauss+j;	     
		}
	    }
	     
	    if ( (ret = H5Sselect_elements(memspace,H5S_SELECT_SET, pcount[0], HDF5_SELECT_BUG pflmem ) ) <0) 
	      goto Fail; 
	      
	    if ( (ret = H5Sselect_elements(dataspace,H5S_SELECT_SET, pcount[0], HDF5_SELECT_BUG pfldsk ) ) <0) 
	      goto Fail; 
	    
	    break;
	    
	  case MED_COMPACT :

	    /* Creation d'un data space mémoire de dimension 1, de la longeur du profil          */
	    /* La dimension utilisée est ici nbdim, même pour un profil compact on suppose       */
	    /*  que l'utilisateur a toutes les coordonées stockées, même si il en demande qu'une */ 

	    if ( (memspace = H5Screate_simple (1, pflsize, NULL)) <0)
	      goto Fail;
	    
	    for (dim=firstdim; dim < lastdim; dim++) {
	      
	      for (i=0; i < psize; i++)              /* i balaye les élements du profil */
		for (j=0; j < ngauss; j++) {         
		  index = i*ngauss+j + (dim-firstdim)*(psize*ngauss);
		  pflmem[index] = i*ngauss*nbdim + j*nbdim+dim;
		  pfldsk[index] = dim*count[0] + (pfltab[i]-1)*ngauss+j;	     
		}
	    }
	    
	    if ( (ret = H5Sselect_elements(memspace,H5S_SELECT_SET, pcount[0], HDF5_SELECT_BUG pflmem ) ) <0) 
	      goto Fail; 
	    
	    if ( (ret = H5Sselect_elements(dataspace,H5S_SELECT_SET, pcount[0], HDF5_SELECT_BUG pfldsk ) ) <0) 
	      goto Fail; 
	     
	    break;
	  
	  default :
	    goto Fail; 
	  }

	if ((ret = H5Dwrite(dataset,type_hdf,memspace,dataspace,H5P_DEFAULT, val)) < 0)
	  goto Fail;
	
	free(pflmem);
	free(pfldsk);
      }
      
      
      break;
      
    case MED_NO_INTERLACE :

      /*Initialisation des indices de boucle du traitement de l'entrelacement en fonction de la dimension fixee*/

      count[0] = (*size)/nbdim;

      if ( psize == MED_NOPF ) {  
	
	if ( fixdim != MED_ALL) 
	  start_data[0] = (fixdim-1)*count[0];
	else {
	  count[0] = *size;
	  start_data[0] =  0;
	};
	
	if ( (ret = H5Sselect_hyperslab (dataspace, H5S_SELECT_SET, start_data, NULL, 
					 count, NULL)) <0)
	  goto Fail; 
	
	if ((ret = H5Dwrite(dataset,type_hdf,dataspace,dataspace,
			    H5P_DEFAULT, val)) < 0)
	  goto Fail;
	
      } else {

	if ( fixdim != MED_ALL) 
	  { 
	    firstdim = fixdim-1;
	    lastdim  = fixdim;
	    dimutil  = 1;
	  } else	{
	    firstdim = 0;
	    lastdim  = nbdim;
	    dimutil  = nbdim; 
	  }
	
	pflsize [0] = psize*ngauss*nbdim;
  	pcount  [0] = psize*ngauss*dimutil; /* nom pas très coherent avec count !!! A revoir */	
	pfldsk     = (med_size *) malloc(sizeof(med_size)*pcount[0]);

	switch(pflmod)
	  { /*switch plfmod pour NO_INTERLACE */
	  case MED_GLOBAL :
	    
	    for (dim=firstdim; dim < lastdim; dim++) {
	      
	      for (i=0; i < psize; i++)              /* i balaye le nbre d'élements du profil                */
		for (j=0; j < ngauss; j++) { 
		  index = i*ngauss+j + (dim-firstdim)*(psize*ngauss);
		  pfldsk[index] = dim*count[0]+(pfltab[i]-1)*ngauss+j;	    
		}
	    }
	    
	    if ( (ret = H5Sselect_elements(dataspace,H5S_SELECT_SET,pcount[0], HDF5_SELECT_BUG pfldsk ) ) <0) 
	      goto Fail;
	    
	    if ((ret = H5Dwrite(dataset,type_hdf,dataspace,dataspace,H5P_DEFAULT, val)) < 0)
	      goto Fail;
	    
	    break;
	    
	  case MED_COMPACT :
	    
	    /* Creation d'un data space mémoire de dimension 1, de la longeur du profil          */
	    /* La dimension utilisée est ici nbdim, même pour un profil compact on suppose       */
	    /*  que l'utilisateur a toutes les coordonées stockées, même si il en demande qu'une */ 

	    if ( (memspace = H5Screate_simple (1, pflsize, NULL)) <0)
	      goto Fail;

	    pflmem     = (med_size *) malloc (sizeof(med_size)*pcount[0]);
	    
	    /* Le profil COMPACT est contigüe, mais il est possible que l'on selectionne uniquemenent une dimension*/
	   
	    index = 0;
	    for (dim=firstdim; dim < lastdim; dim++) {
	      
	      for (i=0; i < psize; i++)              /* i balaye le nbre d'élements du profil                */
		for (j=0; j < ngauss; j++) {
		  /*FORMULATION 1ere : index = i*ngauss+j + (dim-firstdim)*(psize*ngauss);*/
	          /*FORMULATION 1ere : pflmem[index] = dim*(psize*ngauss) + i*ngauss+j;*/
		  pflmem[index] = ( (dim*psize) + i )*ngauss + j;
		  pfldsk[index] = dim*count[0]  + (pfltab[i]-1)*ngauss+j;
		  index++;
		}
	    }
	     
	    if ( (ret = H5Sselect_elements(memspace ,H5S_SELECT_SET,pcount[0], HDF5_SELECT_BUG pflmem ) ) <0) 
	      goto Fail; 
	      
	    if ( (ret = H5Sselect_elements(dataspace,H5S_SELECT_SET,pcount[0], HDF5_SELECT_BUG pfldsk ) ) <0) 
	      goto Fail;
	   
	    if ((ret = H5Dwrite(dataset,type_hdf,memspace,dataspace,H5P_DEFAULT, val)) < 0)
	      goto Fail;

	    free(pflmem);
	    
	    break;
	    
	  default :
	    goto Fail;	    
	    
	  }
   
	free(pfldsk);
	
      };

      break;
      
    default :
      goto Fail;
    }
  
  
  if (memspace) 
    if ((ret = H5Sclose(memspace)) < 0)
      goto Fail;
  
  if ((ret = H5Sclose(dataspace)) < 0)
    goto Fail;
  
  if ((ret = H5Dclose(dataset)) < 0)
    goto Fail;      
  
 Success:
  va_end(params);
  *fret=0;
  return;

 Fail:
  va_end(params);
  *fret = -1;
  return;
}
