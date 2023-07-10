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


#include "med_config.h"
#include <med.h>
#include <med_outils.h>
#include <hdf5.h>
#include <string.h>

med_err _MED21datasetNumLire(med_idt pere,char *nom,med_type_champ type,
			   med_mode_switch interlace, med_size nbdim, med_size fixdim,
			   med_size psize, med_ssize * pfltab, med_int ngauss,
			   unsigned char *val,hid_t hdf_file)
{
  med_idt    dataset, dataspace = 0, memspace = 0, type_hdf = 0;
  med_size  start_mem[1],start_data[1],*pflmem=0,*pfldsk=0;
  med_size   stride[1],count[1],pcount[1],size[1],pflsize[1];
  med_err    ret;
  int        i,j,index;
  hid_t      datatype;
  size_t     typesize;
  int        dim, firstdim, dimutil, lastdim;
  med_mode_profil pflmod;
  
  /* Verify fixdim is between [0, nbdim] ( 0 is MED_ALL ) */
  if (  fixdim > nbdim ) 
    return -1;
 
  /* block pflmod to MED_COMPACT (until med2.2) */
  pflmod = MED_COMPACT;

  switch(type)
    {
    case MED_FLOAT64 :
      if (H5Tequal(hdf_file,H5T_IEEE_F64BE))
	type_hdf = H5T_IEEE_F64LE;
      if (H5Tequal(hdf_file,H5T_IEEE_F64LE))
	type_hdf = H5T_IEEE_F64BE;
      break;

    case MED_INT32 :
      type_hdf = H5T_NATIVE_INT;
      break;

    case MED_INT64 :
      type_hdf = H5T_NATIVE_LONG;
      break;

    default :
      return -1;
    }

  /* Ouverture du Dataset à lire */
  if ((dataset = H5Dopen(pere,nom)) < 0)
    return -1;

  /* Interrogation de la taille du dataset */
  if ( (datatype  = H5Dget_type(dataset )) < 0)  return -1;
  if ( (typesize  = H5Tget_size(datatype)) == 0) return -1;
  size[0] = H5Dget_storage_size(dataset) / typesize; 
  if ( H5Tclose(datatype) < 0) return -1;

  /* Create dataspace */
  if ((dataspace = H5Screate_simple(1,size,NULL)) < 0)
    return -1;
  
  switch(interlace)
    {
    case MED_FULL_INTERLACE :

      /*Initialisation des indices de boucle du traitement de l'entrelacement en fonction de la dimension fixee*/
      if ( fixdim != MED_ALL) 
	{ 
	  firstdim = fixdim-1;
	  lastdim  = fixdim;
	  dimutil  = 1;
	} else	{
	  firstdim = 0;
	  lastdim = nbdim;
	  dimutil  = nbdim; 
	}

      count [0] = (*size)/(nbdim);
      

      /*rem: Pas de vérification de l'assertion (*size)=n*nbdim */
      if ( psize == MED_NOPF ) {  

      /* Creation d'un data space mémoire de dimension 1, de longeur size, et de longeur maxi size */
      if ( (memspace = H5Screate_simple (1, size, NULL)) <0)
	return -1;

	stride[0] = nbdim;  

	for (dim=firstdim; dim < lastdim; dim++) {
	  	  
	  start_mem[0] = dim;
	  if ( (ret = H5Sselect_hyperslab (memspace, H5S_SELECT_SET, start_mem, stride, 
					   count, NULL)) <0)
	    return -1; 
	  
	  start_data[0] = dim*count[0];
	  if ( (ret = H5Sselect_hyperslab (dataspace, H5S_SELECT_SET, start_data, NULL, 
					   count, NULL)) <0)
	    return -1; 
	  
	  if ((ret = H5Dread(dataset,type_hdf,memspace,dataspace,
			     H5P_DEFAULT, val)) < 0)
	    return -1;
	}
	
      } else {

	pflsize [0] = psize*ngauss*nbdim;
	pcount  [0] = psize*ngauss*dimutil;
	pflmem     = (med_size *) malloc (sizeof(med_size)*pcount[0]);
	pfldsk     = (med_size *) malloc (sizeof(med_size)*pcount[0]);
	
	switch(pflmod)
	  { /* switch pflmod pour FULL_INTERLACE*/
	  case MED_GLOBAL :

	    /* Creation d'un data space mémoire de dimension 1, de longeur size, et de longeur maxi size */
	    if ( (memspace = H5Screate_simple (1, size, NULL)) <0)
	      return -1;

	    for (dim=firstdim; dim < lastdim; dim++) {
	      
	      for (i=0; i < psize; i++)              /* i balaye les élements du profil */
		for (j=0; j < ngauss; j++) {         
		  index = i*ngauss+j + (dim-firstdim)*(psize*ngauss);
		  pflmem[index] = (pfltab[i]-1)*ngauss*nbdim + j*nbdim+dim;
		  pfldsk[index] = dim*count[0] + (pfltab[i]-1)*ngauss+j;	     
		}
	    }
	    
	    if ( (ret = H5Sselect_elements(memspace ,H5S_SELECT_SET, pcount[0], HDF5_SELECT_BUG pflmem ) ) <0) 
	      return -1; 
	    
	    if ( (ret = H5Sselect_elements(dataspace,H5S_SELECT_SET, pcount[0], HDF5_SELECT_BUG pfldsk ) ) <0) 
	      return -1; 
	    
	    break;
	
	  case MED_COMPACT :
	
	    /* Creation d'un data space mémoire de dimension 1, de la longeur du profil          */
	    /* La dimension utilisée est ici nbdim, même pour un profil compact on suppose       */
	    /*  que l'utilisateur a toutes les coordonées stockées, même si il en demande qu'une */ 
	    
	    if ( (memspace = H5Screate_simple (1, pflsize, NULL)) <0)
	      return -1;
	    
	    for (dim=firstdim; dim < lastdim; dim++) {
	      
	      for (i=0; i < psize; i++)              /* i balaye les élements du profil */
		for (j=0; j < ngauss; j++) {         
		  index = i*ngauss+j + (dim-firstdim)*(psize*ngauss);
		  pflmem[index] = i*ngauss*nbdim + j*nbdim+dim;
		  pfldsk[index] = dim*count[0] + (pfltab[i]-1)*ngauss+j;	     
		}	      
	    }
	    
	    if ( (ret = H5Sselect_elements(memspace ,H5S_SELECT_SET, pcount[0], HDF5_SELECT_BUG pflmem ) ) <0) 
	      return -1; 
	    
	    if ( (ret = H5Sselect_elements(dataspace,H5S_SELECT_SET, pcount[0], HDF5_SELECT_BUG pfldsk ) ) <0) 
	      return -1; 
	    
	    break;

	  default :
	    return -1; 
	  }
	
	if ((ret = H5Dread(dataset,type_hdf,memspace,dataspace,H5P_DEFAULT, val)) < 0)
	  return -1;
	
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
	  return -1; 
	
	if ((ret = H5Dread(dataset,type_hdf,dataspace,dataspace,
			   H5P_DEFAULT, val)) < 0)
	  return -1;
	
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
	pfldsk      = (med_size *) malloc(sizeof(med_size)*pcount[0]);
	
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
	      return -1;
	    
	    if ((ret = H5Dread(dataset,type_hdf,dataspace,dataspace,H5P_DEFAULT, val)) < 0)
	      return -1;
	      
	    break;
	    
	  case MED_COMPACT :
	    
	    /* Creation d'un data space mémoire de dimension 1, de la longeur du profil          */
	    /* La dimension utilisée est ici nbdim, même pour un profil compact on suppose       */
	    /*  que l'utilisateur a toutes les coordonées stockées, même si il en demande qu'une */ 

	    if ( (memspace = H5Screate_simple (1, pflsize, NULL)) <0)
	      return -1;

	    pflmem     = (med_size *) malloc (sizeof(med_size)*pcount[0]);
	    
	    /* Le profil COMPACT est contigüe, mais il est possible que l'on selectionne uniquemenent une dimension*/

	    for (dim=firstdim; dim < lastdim; dim++) {
	      
	      for (i=0; i < psize; i++)              /* i balaye le nbre d'élements du profil                */
		for (j=0; j < ngauss; j++) {
		  index = i*ngauss+j + (dim-firstdim)*(psize*ngauss);
	          pflmem[index] = dim*(psize*ngauss) + (pfltab[i]-1)*ngauss+j;
		  pfldsk[index] = dim*count[0]  + (pfltab[i]-1)*ngauss+j;	    
		}
	    }
	    
	    if ( (ret = H5Sselect_elements(memspace ,H5S_SELECT_SET, pcount[0], HDF5_SELECT_BUG pflmem ) ) <0) 
	      return -1; 
	    
	    if ( (ret = H5Sselect_elements(dataspace,H5S_SELECT_SET,pcount[0], HDF5_SELECT_BUG pfldsk ) ) <0) 
	      return -1;	  
	    
	    if ((ret = H5Dread(dataset,type_hdf,memspace,dataspace,H5P_DEFAULT, val)) < 0)
	      return -1;
	    
	    break;
	    
	  default :
	    return -1;	    
	    
	  }
	
	free(pfldsk);
	
      };
      
      break;
      
    default :
      return -1;
    }
  
  

  if (memspace) 
    if ((ret = H5Sclose(memspace)) < 0)
      return -1;

  if ((ret = H5Sclose(dataspace)) < 0)
    return -1;
  
  if ((ret = H5Dclose(dataset)) < 0)
    return -1;      

  return 0;
}
