/*  This file is part of MED.
 *
 *  COPYRIGHT (C) 1999 - 2013  EDF R&D, CEA/DEN
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
#include <H5FDpublic.h>

#include <assert.h>
#ifdef PPRO_NT_CALL
#define F_OK 0
#else
#include <unistd.h>
#endif

/* #define AFF_MEMFILE \ */
/* XSCRUTE(udata->app_image_ptr);\ */
/* ISCRUTE_long(udata->app_image_size);\ */
/* ISCRUTE_int(udata->ref_count);\ */
/* XSCRUTE(udata->fapl_image_ptr);\ */
/* ISCRUTE_long(udata->fapl_image_size);\ */
/* ISCRUTE_int(udata->fapl_ref_count);\ */
/* XSCRUTE(udata->vfd_image_ptr);\ */
/* ISCRUTE_long(udata->vfd_image_size);\ */
/* ISCRUTE_int(udata->vfd_ref_count);\ */
/* ISCRUTE_int(udata->flags); */

/*
 * - Nom de la fonction : _MEDmemFileOpen
 * - Description : creation d'un fichier HDF
 * - Parametres :
 *     - nom (IN) : le nom du fichier
 * - Resultat : ID du fichier en cas de succes, -1 sinon
 */
#define SUCCEED    0
#define FAIL    (-1)


/* callbacks prototypes for file image ops */
static void *image_malloc(size_t size, H5FD_file_image_op_t file_image_op, void *udata);
static void *image_memcpy(void *dest, const void *src, size_t size, H5FD_file_image_op_t file_image_op, void *udata);
static void *image_realloc(void *ptr, size_t size, H5FD_file_image_op_t file_image_op, void *udata);
static herr_t image_free(void *ptr, H5FD_file_image_op_t file_image_op, void *udata);
static void *udata_copy(void *udata);
static herr_t udata_free(void *udata);

typedef med_memfile H5LT_file_image_ud_t;

static void *
image_malloc(size_t size, H5FD_file_image_op_t file_image_op, void *_udata)
{
    H5LT_file_image_ud_t *udata = (H5LT_file_image_ud_t *)_udata;
    void * return_value = NULL;

/* ISCRUTE_long(size); */
/* AFF_MEMFILE; */

    switch ( file_image_op ) {
        /* the app buffer is "copied" to only one FAPL. Afterwards, FAPLs can be "copied" */
        case H5FD_FILE_IMAGE_OP_PROPERTY_LIST_SET:
	  /*ICI;*/
            if (udata->fapl_image_ptr != NULL)                    goto out;
            if (udata->fapl_image_size != 0)                      goto out;
            if (udata->fapl_ref_count != 0)                       goto out;
	    /*ICI;*/
            if (udata->app_image_ptr == NULL) {
	    /*ICI;*/
                if (udata->app_image_size != 0)                    goto out;
	    /*ICI;*/
	    /* Allocation d'une image mémoire (non fournie) */
                if (NULL == (udata->app_image_ptr = malloc(size))) goto out; 
                udata->app_image_size = size;
	    }
	    /*ICI;*/
            if (udata->app_image_size != size)  goto out;
	    /*ICI;*/

            udata->fapl_image_ptr  = udata->app_image_ptr;
            udata->fapl_image_size = udata->app_image_size;
            return_value = udata->fapl_image_ptr;
            udata->fapl_ref_count++;
	    break;

	case H5FD_FILE_IMAGE_OP_PROPERTY_LIST_COPY:
	  /*ICI;*/
            if (udata->fapl_image_ptr == NULL)                   goto out;
            if (udata->fapl_image_size != size)                  goto out;
            if (udata->fapl_ref_count == 0)                      goto out;
            return_value = udata->fapl_image_ptr;
            udata->fapl_ref_count++;
	    break;

        case H5FD_FILE_IMAGE_OP_PROPERTY_LIST_GET:
	  /*ICI;*/
            goto out;

        case H5FD_FILE_IMAGE_OP_FILE_OPEN:
            /* FAPL buffer is "copied" to only one VFD buffer */
	  /*ICI;*/
            if (udata->vfd_image_ptr  != NULL )                  goto out;
            if (udata->vfd_image_size != 0    )                  goto out;
            if (udata->vfd_ref_count  != 0    )                  goto out;

	    //si l'on positionne une image mémoire fapl et qu'un fichier de même nom existe 
	    //une ouverture RD_ONLY ou RDWR génère une erreur dans H5FD_core_open
	    //il faut ne faut donc pas positionner d'image mais
	    //utiliser directement le buffer utilisateur app_image_ptr 
	    if (udata->fapl_image_ptr  == NULL ) { /*Une image mémoire est-elle positionée*/
	      /*ICI;*/
	      /*Si le buffer utilisateur existe, je l'utilise, sinon le l'alloue */
	      if ( (udata->app_image_ptr) && (udata->app_image_size = 0 ) )  goto out;
	      if ( !(udata->app_image_ptr) && (udata->app_image_size > 0 ) ) goto out;
	      /* Si le buffer  utilisateur est plus grand, on le garde à sa taille */
	      /*ICI;*/
	      if   (udata->app_image_size < size) { 
		/*ICI;*/
		if  (NULL == ( udata->app_image_ptr =
			       realloc(udata->app_image_ptr,size))) goto out;
		udata->app_image_size = size;
		/*ICI;*/
	      }
	      udata->vfd_image_ptr  = udata->app_image_ptr;
	      udata->vfd_image_size = udata->app_image_size;
	      /*ICI;*/
	      /* if (    (udata->app_image_size < size)  */
	      /* 	      || (!(udata->app_image_ptr)) ) { /\*Si le buffer utilisateur existe, je l'utilise*\/ */
	      /* 	/\* Allocation d'une image mémoire de la taille du fichier disque  <size> */
	      /* 	   (espace de l'image non fournie) *\/ */
	      /* 	ICI; */
	      /* 	if  (NULL == (udata->app_image_ptr=realloc(udata->app_image_ptr,size))) goto out; */
	      /* 	udata->app_image_size = size; */
	      /* 	udata->vfd_image_ptr = udata->app_image_ptr; */
	      /* 	ICI; */
	      /* } */
	    } else {
	      /* On ouvre un fichier mémoire qui provient forcément d'un fapl*/
	      if (udata->fapl_image_size != size )               goto out;
	      if (udata->fapl_ref_count   == 0   )               goto out;
	      udata->vfd_image_ptr = udata->fapl_image_ptr;
	      udata->vfd_image_size = size;
	      /*ICI;*/
	    }
	    /*ICI;*/
            udata->vfd_ref_count++;
            return_value = udata->vfd_image_ptr;
            break;

	/* added unused labels to shut the compiler up */
	case H5FD_FILE_IMAGE_OP_NO_OP:
	case H5FD_FILE_IMAGE_OP_PROPERTY_LIST_CLOSE:
	case H5FD_FILE_IMAGE_OP_FILE_RESIZE:
	case H5FD_FILE_IMAGE_OP_FILE_CLOSE:
        default:
            goto out;
    } /* end switch */

    return(return_value);

out:
    ICI;
    return NULL;
} /* end image_malloc() */


static void *
image_memcpy(void *dest, const void *src, size_t size, H5FD_file_image_op_t file_image_op,
    void *_udata)
{
    H5LT_file_image_ud_t *udata = (H5LT_file_image_ud_t *)_udata;

/* XSCRUTE(src); */
/* XSCRUTE(dest); */
/* ISCRUTE_long(size); */
/* AFF_MEMFILE; */

    switch(file_image_op) {
        case H5FD_FILE_IMAGE_OP_PROPERTY_LIST_SET:
            if (dest != udata->fapl_image_ptr)                 goto out;
            if (src != udata->app_image_ptr)                   goto out;
            if (size != udata->fapl_image_size)                goto out;
            if (size != udata->app_image_size)                 goto out;
            if (udata->fapl_ref_count == 0)                    goto out;
            break;

        case H5FD_FILE_IMAGE_OP_PROPERTY_LIST_COPY:
            if (dest != udata->fapl_image_ptr)                 goto out;
            if (src != udata->fapl_image_ptr)                  goto out;
            if (size != udata->fapl_image_size)                goto out;
            if (udata->fapl_ref_count < 2)                     goto out;
            break;

        case H5FD_FILE_IMAGE_OP_PROPERTY_LIST_GET:
            goto out;

        case H5FD_FILE_IMAGE_OP_FILE_OPEN:
            if (dest != udata->vfd_image_ptr)                  goto out;
            if (src != udata->fapl_image_ptr)                  goto out;
            if (size != udata->vfd_image_size)                 goto out;
            if (size != udata->fapl_image_size)                goto out;
            if (udata->fapl_ref_count == 0)                    goto out;
            if (udata->vfd_ref_count != 1)                     goto out;
            break;

	/* added unused labels to shut the compiler up */
	case H5FD_FILE_IMAGE_OP_NO_OP:
	case H5FD_FILE_IMAGE_OP_PROPERTY_LIST_CLOSE:
	case H5FD_FILE_IMAGE_OP_FILE_RESIZE:
	case H5FD_FILE_IMAGE_OP_FILE_CLOSE:
        default:
            goto out;
    } /* end switch */

    return(dest);

out:
    ICI;
    return NULL;
} /* end image_memcpy() */


static void *
image_realloc(void *ptr, size_t size, H5FD_file_image_op_t file_image_op, void *_udata)
{
    H5LT_file_image_ud_t *udata = (H5LT_file_image_ud_t *)_udata;
    void * return_value  = NULL;
 
 
    /* realloc() is not allowed if the image is open in read-only mode */
    if ((udata->flags == MED_ACC_RDONLY))         goto out; 
    if (file_image_op == H5FD_FILE_IMAGE_OP_FILE_RESIZE) {

/* XSCRUTE(ptr); */
/* ISCRUTE_long(size); */
/* AFF_MEMFILE; */

        if (!ptr) { /*peut être appelée ainsi par les apis hdf d'écriture ex:datasetwrite */
	  /*ICI;*/
	  /* if (udata->fapl_ref_count != 0) goto out; */
	  if (udata->vfd_ref_count   != 0    )    goto out;
	  if (udata->vfd_image_ptr   != NULL )    goto out;
	  if (udata->vfd_image_size  != 0    )    goto out;

	  /* if ( (udata->app_image_ptr != udata->fapl_image_ptr) && */
	  /*      (udata->app_image_ptr != udata->vfd_image_ptr) ) goto out; */
	  /* if ( (udata->app_image_size != udata->fapl_image_size) && */
	  /*      (udata->app_image_size != udata->vfd_image_size) ) goto out; */

	  /* if (size > udata->vfd_image_size ) goto out; */
  
	  udata->vfd_image_ptr  = udata->app_image_ptr;  
          udata->vfd_image_size = udata->app_image_size;  
	  udata->vfd_ref_count++;
	}

        if ( (udata->vfd_image_ptr != ptr) && ptr )                  { /* ICI; */ goto out; }
						                     
        if ( (udata->vfd_ref_count != 1)          )                  { /* ICI; */ goto out; }
						                     
	if ( size > udata->vfd_image_size         )                  { /* ICI; */
	  if (NULL == (udata->vfd_image_ptr = realloc(ptr, size)))   { /* ICI; */ goto out; }
	  udata->vfd_image_size  = size;
	  /* met en cohérence les pointeur et les tailles des fapl et app */
	  assert(!(udata->fapl_ref_count)); /*ENLEVER ET ASSERT MEME SI COPIES DE FAPL?*/
	  udata->fapl_image_ptr  = udata->vfd_image_ptr;
	  udata->fapl_image_size = size;
	  udata->app_image_ptr   = udata->vfd_image_ptr;
	  udata->app_image_size  = size;

	}
	return_value = udata->vfd_image_ptr;

    } /* end if */
    else
        goto out;

    return(return_value);
 out:
    ICI;
    return NULL;
} /* end image_realloc() */


static herr_t
image_free(void *ptr, H5FD_file_image_op_t file_image_op, void *_udata)
{
    H5LT_file_image_ud_t *udata = (H5LT_file_image_ud_t *)_udata;
/* XSCRUTE(ptr); */
/* AFF_MEMFILE; */

    switch(file_image_op) {
        case H5FD_FILE_IMAGE_OP_PROPERTY_LIST_CLOSE:
	  /* ICI; */
	    if (udata->fapl_image_ptr != ptr)             goto out;
            if (udata->fapl_ref_count == 0  )             goto out;
            udata->fapl_ref_count--;

            /* release the shared buffer only if indicated by the respective 
	       flag and there are no outstanding references */ 
            // if (udata->fapl_ref_count == 0 && udata->vfd_ref_count == 0 &&
            //         !(udata->flags & H5LT_FILE_IMAGE_DONT_RELEASE)) {
            //     HDfree(udata->fapl_image_ptr);
            //     udata->app_image_ptr = NULL;
	    udata->fapl_image_ptr  = NULL;
	    udata->fapl_image_size = 0;
            //     udata->vfd_image_ptr = NULL;
            // } /* end if */
            break;

        case H5FD_FILE_IMAGE_OP_FILE_CLOSE:
	  /* ICI; */
            if (udata->vfd_ref_count != 1)                goto out;

	    assert(udata->vfd_image_ptr  == udata->app_image_ptr);
	    assert(udata->vfd_image_size == udata->app_image_size);

            udata->vfd_ref_count--;

udata->fapl_image_ptr = NULL;
udata->fapl_image_size = 0;
udata->fapl_ref_count  = 0;
                udata->vfd_image_ptr  = NULL;
                udata->vfd_image_size = 0;
            // } /* end if */
            break;

	/* added unused labels to keep the compiler quite */
	case H5FD_FILE_IMAGE_OP_NO_OP:
	case H5FD_FILE_IMAGE_OP_PROPERTY_LIST_SET:
	case H5FD_FILE_IMAGE_OP_PROPERTY_LIST_COPY:
	case H5FD_FILE_IMAGE_OP_PROPERTY_LIST_GET:
	case H5FD_FILE_IMAGE_OP_FILE_OPEN:
	case H5FD_FILE_IMAGE_OP_FILE_RESIZE:
	default:
            goto out;
    } /* end switch */

    return(SUCCEED);

out:
    ICI;
    return(FAIL);
} /* end image_free() */

static void *
udata_copy(void *_udata)
{
    H5LT_file_image_ud_t *udata = (H5LT_file_image_ud_t *)_udata;

/* AFF_MEMFILE; */
/* ISCRUTE_long(udata->ref_count); */

    if (udata->ref_count == 0) goto out;
    udata->ref_count++;

    return(udata);

out:
    ICI;
    return NULL;
} /* end udata_copy */


static herr_t
udata_free(void *_udata)
{
    H5LT_file_image_ud_t *udata = (H5LT_file_image_ud_t *)_udata;
/* AFF_MEMFILE; */
/* ISCRUTE_long(udata->ref_count); */

    if (udata->ref_count == 0) goto out;
    udata->ref_count--;

    /* checks that there are no references outstanding before deallocating udata */
    /* if (udata->ref_count == 0 && udata->fapl_ref_count == 0 && */
    /*         udata->vfd_ref_count == 0) */
        /* HDfree(udata); */

    return(SUCCEED);

out:        
    ICI;
    return(FAIL);
} /* end udata_free */

/* End of callbacks definitions for file image operations */


med_idt _MEDmemFileOpen(const char * const filename, med_memfile * const memfile, const med_bool filesync,
			  const med_access_mode accessmode)
{
  med_idt _fid=-1,_gid=-1;
  med_int _major   = MED_NUM_MAJEUR;
  med_int _minor   = MED_NUM_MINEUR;
  med_int _release = MED_NUM_RELEASE;
  hid_t   _fapl    = H5P_DEFAULT;
  med_access_mode _accessmode = accessmode;
  med_bool         file_exist = MED_FALSE;

#define KB              1024U
#define CORE_INCREMENT  (4*KB)

  H5FD_file_image_callbacks_t  callbacks = {image_malloc, image_memcpy,
					    image_realloc, image_free,
					    udata_copy, udata_free,
					    (void *)(memfile)};
  memfile->flags = accessmode;

  file_exist=!access(filename,F_OK);

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();


  if ( (_fapl = H5Pcreate (H5P_FILE_ACCESS)) < 0 ) {
    MED_ERR_(_fid,MED_ERR_CREATE,MED_ERR_PROPERTY,MED_ERR_FILEVERSION_MSG);
    goto ERROR;
  }

#if H5_VERS_MINOR > 10
#error "Don't forget to change the compatibility version of the library !"
#endif
  if ( H5Pset_libver_bounds( _fapl, H5F_LIBVER_18, H5F_LIBVER_18) ) {
    MED_ERR_(_fid,MED_ERR_INIT,MED_ERR_PROPERTY,MED_ERR_FILEVERSION_MSG);
    goto ERROR;
  }

  if ( H5Pset_file_image_callbacks(_fapl, &callbacks) < 0) {
    MED_ERR_(_fid,MED_ERR_INIT,MED_ERR_PROPERTY,MED_ERR_MEMFILE_MSG);
    goto ERROR;
  }
  /* H5Pset_file_image_callbacks :
     Appel de udata->malloc 
     Attention : Le udata->free n'est jamais appelé
   */
/*   MESSAGE("-------- 0 ---------");*/

/*  ISCRUTE(filesync);*/
  if ( H5Pset_fapl_core(_fapl, (size_t)CORE_INCREMENT, filesync) < 0) {
    MED_ERR_(_fid,MED_ERR_INIT,MED_ERR_PROPERTY,MED_ERR_MEMFILE_MSG);
    goto ERROR;
  }

  /*MESSAGE("-------- 0b ---------");*/
   /* ISCRUTE(file_exist); */
  if (    (memfile->app_image_size != 0) 
       && ( (!file_exist) || (_accessmode == MED_ACC_CREAT) )
     ) {
    assert(memfile->app_image_ptr); 
/*    ISCRUTE_long(memfile->app_image_size);*/
    H5Pset_file_image(_fapl, memfile->app_image_ptr, memfile->app_image_size);
    /*le contenu sera réinitialisé  par H5FCreate */
   /*MESSAGE("-------- 1 ---------");*/
  }
  /*
    H5Pset_file_image (qui copie _fapl) :
    Appel de image_malloc H5FD_FILE_IMAGE_OP_PROPERTY_LIST_SET
    puis  de image_copy   H5FD_FILE_IMAGE_OP_PROPERTY_LIST_SET
   */

  switch(_accessmode)
    {
    case MED_ACC_RDWR :
    case MED_ACC_RDONLY :
   /*MESSAGE("-------- 2 ---------");*/
      if ((_fid = H5Fopen(filename,_accessmode, _fapl)) < 0) {
	MED_ERR_(_fid,MED_ERR_OPEN,MED_ERR_FILE,filename);
	goto ERROR;
      }
  /*
    H5Fopen :
   */
    /*MESSAGE("-------- 2b ---------");*/
      break;

    case MED_ACC_CREAT :
   /*MESSAGE("-------- 3 ---------");*/
      _accessmode= MED_ACC_RDWR;
      if ((_fid = H5Fcreate(filename,H5F_ACC_TRUNC,
			    H5P_DEFAULT,_fapl)) < 0) {
	MED_ERR_(_fid,MED_ERR_CREATE,MED_ERR_FILE,filename);
	goto ERROR;
      }
  /*
    H5Fcreate avec sync==false (normalement idem avec syn==true)  :
    Appel de image_malloc H5FD_FILE_IMAGE_OP_FILE_OPEN
    puis  de image_copy   H5FD_FILE_IMAGE_OP_FILE_OPEN
    puis  de image_free   H5FD_FILE_IMAGE_OP_FILE_CLOSE 
    (l'image est vierge, les écritures ds la nouvelle image donnent lieu à des  réallocations)
   */
   /*MESSAGE("-------- 3b ---------");*/
      break;

    case MED_ACC_RDEXT :
    default :
     MED_ERR_(_fid,MED_ERR_RANGE,MED_ERR_ACCESS,filename);
      goto ERROR;
    }


  if ( H5Pclose(_fapl) < 0 ) {
    MED_ERR_(_fid,MED_ERR_CLOSE,MED_ERR_PROPERTY,"");
    _fid=-1;goto ERROR;
  }
  /*
    H5Pclose  :
    Appel de image_free H5FD_FILE_IMAGE_OP_PROPERTY_LIST_CLOSE
   */
  _MEDsetModeAcces(_fid,_accessmode);

  if (accessmode == MED_ACC_CREAT) {
   /*MESSAGE("-------- 3b0 ---------");*/
    if ((_gid = _MEDdatagroupCreer(_fid,MED_INFOS)) < 0) {
      MED_ERR_(_fid,MED_ERR_CREATE,MED_ERR_FILE,MED_INFOS);
      goto ERROR;
    }

    /* Numero de versions de MED */
   /*MESSAGE("-------- 3b1 ---------");*/
    if ( _MEDattributeIntWr(_gid,MED_NOM_MAJEUR,&_major)) {
      MED_ERR_(_fid,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_NOM_MAJEUR);
      goto ERROR;
    }
   /*MESSAGE("-------- 3b2 ---------");*/

    if ( _MEDattributeIntWr(_gid,MED_NOM_MINEUR,&_minor) < 0) {
      MED_ERR_(_fid,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_NOM_MINEUR);
      goto ERROR;
    }
   /*MESSAGE("-------- 3b3 ---------");*/

    if (_MEDattributeIntWr(_gid,MED_NOM_RELEASE,&_release) < 0) {
      MED_ERR_(_fid,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_NOM_RELEASE);
      goto ERROR;
    }
   /*MESSAGE("-------- 3b4 ---------");*/

    /* On ferme tout */
    if ( _MEDdatagroupFermer(_gid) < 0) {
      MED_ERR_(_fid,MED_ERR_CLOSE,MED_ERR_DATAGROUP,MED_INFOS);
      goto ERROR;
   /*MESSAGE("-------- 3b5 ---------");*/
    }
  }

  _MEDsetModeAcces(_fid,_accessmode);
  _MEDfileVersion(_fid);
 /*MESSAGE("-------- 4 ---------");*/

 ERROR:

  /* H5Eprint1(stderr); */
  return _fid;

}
