/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 *  For details of the HDF libraries, see the HDF Documentation at:
 *    http://hdfgroup.org/HDF5/doc/
 *
 */

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include "hdf5.h"
#include "h5util.h"
#include <jni.h>
#include <stdlib.h>
#include <string.h>
#include "h5aImp.h"

extern JavaVM *jvm;
extern jobject visit_callback;

#ifdef __cplusplus
#define CBENVPTR (cbenv)
#define CBENVPAR
#define JVMPTR (jvm)
#define JVMPAR
#define JVMPAR2
#else
#define CBENVPTR (*cbenv)
#define CBENVPAR cbenv,
#define JVMPTR (*jvm)
#define JVMPAR jvm
#define JVMPAR2 jvm,
#endif

/********************/
/* Local Prototypes */
/********************/

static herr_t H5AwriteVL_asstr (JNIEnv *env, hid_t attr_id, hid_t mem_id, jobjectArray buf);
static herr_t H5AwriteVL_str (JNIEnv *env, hid_t attr_id, hid_t mem_id, jobjectArray buf);
static herr_t H5AreadVL_asstr (JNIEnv *env, hid_t attr_id, hid_t mem_id, jobjectArray buf);
static herr_t H5AreadVL_str (JNIEnv *env, hid_t attr_id, hid_t mem_id, jobjectArray buf);
static herr_t H5A_iterate_cb(hid_t g_id, const char *name, const H5A_info_t *info, void *op_data);


/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Acreate
 * Signature: (JLjava/lang/String;JJJ)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Acreate
    (JNIEnv *env, jclass clss, jlong loc_id, jstring name, jlong type_id,
          jlong space_id, jlong create_plist)
{
    hid_t       attr_id = -1;
    const char *aName;

    PIN_JAVA_STRING(name, aName);
    if (aName != NULL) {
        attr_id = H5Acreate2((hid_t)loc_id, aName, (hid_t)type_id, (hid_t)space_id, (hid_t)create_plist, (hid_t)H5P_DEFAULT);

        UNPIN_JAVA_STRING(name, aName);

        if (attr_id < 0)
            h5libraryError(env);
    }

    return (jlong)attr_id;
} /* end Java_hdf_hdf5lib_H5__1H5Acreate */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aopen_name
 * Signature: (JLjava/lang/String;)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Aopen_1name
    (JNIEnv *env, jclass clss, jlong loc_id, jstring name)
{
    hid_t       attr_id = -1;
    const char *aName;

    PIN_JAVA_STRING(name, aName);
    if (aName != NULL) {
        attr_id = H5Aopen_name((hid_t)loc_id, aName);

        UNPIN_JAVA_STRING(name,aName);

        if (attr_id < 0)
            h5libraryError(env);
        }

    return (jlong)attr_id;
} /* end Java_hdf_hdf5lib_H5__1H5Aopen_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aopen_idx
 * Signature: (JI)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Aopen_1idx
    (JNIEnv *env, jclass clss, jlong loc_id, jint idx)
{
    hid_t attr_id =  H5Aopen_idx((hid_t)loc_id, (unsigned int) idx);

    if (attr_id < 0)
        h5libraryError(env);

    return (jlong)attr_id;
} /* end Java_hdf_hdf5lib_H5__1H5Aopen_1idx */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Awrite
 * Signature: (JJ[B)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Awrite
    (JNIEnv *env, jclass clss, jlong attr_id, jlong mem_type_id, jbyteArray buf)
{
    herr_t   status = -1;
    jbyte   *byteP;
    jboolean isCopy;

    if (buf == NULL) {
        h5nullArgument( env,"H5Awrite:  buf is NULL");
    } /* end if */
    else {
        byteP = ENVPTR->GetByteArrayElements(ENVPAR buf, &isCopy);
        if (byteP == NULL) {
            h5JNIFatalError(env,"H5Awrite: buf is not pinned");
        } /* end if */
        else {
            status = H5Awrite((hid_t)attr_id, (hid_t)mem_type_id, byteP);

            /* free the buffer without copying back */
            ENVPTR->ReleaseByteArrayElements(ENVPAR buf, byteP, JNI_ABORT);

            if (status < 0)
                h5libraryError(env);
        }
    } /* end else */

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Awrite */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aread
 * Signature: (JJ[B)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Aread
    (JNIEnv *env, jclass clss, jlong attr_id, jlong mem_type_id, jbyteArray buf)
{
    herr_t   status = -1;
    jbyte   *byteP;
    jboolean isCopy;

    if (buf == NULL) {
        h5nullArgument( env,"H5Aread:  buf is NULL");
    } /* end if */
    else {
        byteP = ENVPTR->GetByteArrayElements(ENVPAR buf, &isCopy);
        if (byteP == NULL) {
            h5JNIFatalError( env,"H5Aread: buf is not pinned");
        } /* end if */
        else {
            status = H5Aread((hid_t)attr_id, (hid_t)mem_type_id, byteP);
            if (status < 0) {
                ENVPTR->ReleaseByteArrayElements(ENVPAR buf, byteP, JNI_ABORT);
                h5libraryError(env);
            } /* end if */
            else  {
                ENVPTR->ReleaseByteArrayElements(ENVPAR buf, byteP, 0);
            } /* end else */
        } /* end else */
    } /* end else */

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Aread */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5AwriteVL
 * Signature: (JJ[Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5AwriteVL
    (JNIEnv *env, jclass clss, jlong attr_id, jlong mem_type_id, jobjectArray buf)
{
    herr_t  status = -1;
    htri_t  isStr = 0;
    htri_t  isVlenStr = 0;
    htri_t  isComplex = 0;

    if (buf == NULL) {
        h5nullArgument(env, "H5AwriteVL:  buf is NULL");
    } /* end if */
    else {
        isStr = H5Tdetect_class((hid_t)mem_type_id, H5T_STRING);
        if (H5Tget_class((hid_t)mem_type_id) == H5T_COMPOUND) {
            unsigned i;
            int nm = H5Tget_nmembers(mem_type_id);
            for(i = 0; i <nm; i++) {
                hid_t nested_tid = H5Tget_member_type((hid_t)mem_type_id, i);
                isComplex = H5Tdetect_class((hid_t)nested_tid, H5T_COMPOUND) ||
                            H5Tdetect_class((hid_t)nested_tid, H5T_VLEN);
                H5Tclose(nested_tid);
            }
        }
        else if (H5Tget_class((hid_t)mem_type_id) == H5T_VLEN) {
            isVlenStr = 1; /* strings created by H5Tvlen_create(H5T_C_S1) */
        }
        if (isStr == 0 || isComplex>0 || isVlenStr) {
            status = H5AwriteVL_asstr(env, (hid_t)attr_id, (hid_t)mem_type_id, buf);
        }
        else if (isStr > 0) {
            status = H5AwriteVL_str(env, (hid_t)attr_id, (hid_t)mem_type_id, buf);
        }
    } /* end else */

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Awrite_1VL */

herr_t
H5AwriteVL_str
    (JNIEnv *env, hid_t aid, hid_t tid, jobjectArray buf)
{
    herr_t  status = -1;
    char  **wdata;
    jsize   size;
    jint    i;

    size = ENVPTR->GetArrayLength(ENVPAR (jarray) buf);

    wdata = (char**)HDcalloc((size_t)size + 1, sizeof(char*));
    if (!wdata) {
        h5JNIFatalError(env, "H5AwriteVL_str:  cannot allocate buffer");
    } /* end if */
    else {
        HDmemset(wdata, 0, (size_t)size * sizeof(char*));
        for (i = 0; i < size; ++i) {
            jstring obj = (jstring) ENVPTR->GetObjectArrayElement(ENVPAR (jobjectArray) buf, i);
            if (obj != 0) {
                jsize length = ENVPTR->GetStringUTFLength(ENVPAR obj);
                const char *utf8 = ENVPTR->GetStringUTFChars(ENVPAR obj, 0);

                if (utf8) {
                    wdata[i] = (char*)HDmalloc((size_t)length + 1);
                    if (wdata[i]) {
                        HDmemset(wdata[i], 0, ((size_t)length + 1));
                        HDstrncpy(wdata[i], utf8, (size_t)length);
                    } /* end if */
                } /* end if */

                ENVPTR->ReleaseStringUTFChars(ENVPAR obj, utf8);
                ENVPTR->DeleteLocalRef(ENVPAR obj);
            } /* end if */
        } /* end for (i = 0; i < size; ++i) */

        status = H5Awrite((hid_t)aid, (hid_t)tid, wdata);

        for (i = 0; i < size; i++) {
            if(wdata[i]) {
                HDfree(wdata[i]);
            } /* end if */
        } /* end for */
        HDfree(wdata);

        if (status < 0)
            h5libraryError(env);
    } /* end else */

    return (jint)status;
}

herr_t
H5AwriteVL_asstr
    (JNIEnv *env, hid_t aid, hid_t tid, jobjectArray buf)
{
    char  **strs;
    jstring jstr;
    jint    i;
    jint    n;
    hid_t   sid;
    hsize_t dims[H5S_MAX_RANK];
    herr_t  status = -1;

    n = ENVPTR->GetArrayLength(ENVPAR buf);
    strs =(hvl_t*)HDcalloc((size_t)n, sizeof(hvl_t));

    if (strs == NULL) {
        h5JNIFatalError(env, "H5AwriteVL_asstr:  failed to allocate buff for read variable length strings");
    } /* end if */
    else {
        status = H5Awrite(aid, tid, strs);

        if (status < 0) {
            dims[0] = (hsize_t)n;
            sid = H5Screate_simple(1, dims, NULL);
            H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, strs);
            H5Sclose(sid);
            HDfree(strs);
            h5JNIFatalError(env, "H5AwriteVL_str: failed to read variable length strings");
        } /* end if */
        else {
            for (i=0; i < n; i++) {
                jstr = ENVPTR->NewStringUTF(ENVPAR strs[i]);
                ENVPTR->SetObjectArrayElement(ENVPAR buf, i, jstr);
                H5free_memory (strs[i]);
            } /* end for */

            /*
            for repeatedly reading a dataset with a large number of strs (e.g., 1,000,000 strings,
            H5Dvlen_reclaim() may crash on Windows because the Java GC will not be able to collect
            free space in time. Instead, use "H5free_memory(strs[i])" above to free individual strings
            after it is done.
            H5Dvlen_reclaim(tid, mem_sid, xfer_plist_id, strs);
            */

            HDfree(strs);
        } /* end else */
    } /* end else */

    return status;
} /* end H5AwriteVL_str */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5AreadVL
 * Signature: (JJJJJ[Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5AreadVL
    (JNIEnv *env, jclass clss, jlong attr_id, jlong mem_type_id, jobjectArray buf)
{
    herr_t  status = -1;
    htri_t  isStr = 0;
    htri_t  isVlenStr = 0;
    htri_t  isComplex = 0;

    if (buf == NULL) {
        h5nullArgument(env, "H5AreadVL:  buf is NULL");
    } /* end if */
    else {
        isStr = H5Tdetect_class((hid_t)mem_type_id, H5T_STRING);
        if (H5Tget_class((hid_t)mem_type_id) == H5T_COMPOUND) {
            unsigned i;
            int nm = H5Tget_nmembers(mem_type_id);
            for(i = 0; i <nm; i++) {
                hid_t nested_tid = H5Tget_member_type((hid_t)mem_type_id, i);
                isComplex = H5Tdetect_class((hid_t)nested_tid, H5T_COMPOUND) ||
                            H5Tdetect_class((hid_t)nested_tid, H5T_VLEN);
                H5Tclose(nested_tid);
            }
        }
        else if (H5Tget_class((hid_t)mem_type_id) == H5T_VLEN) {
            isVlenStr = 1; /* strings created by H5Tvlen_create(H5T_C_S1) */
        }
        if (isStr == 0 || isComplex>0 || isVlenStr) {
            status = H5AreadVL_asstr(env, (hid_t)attr_id, (hid_t)mem_type_id, buf);
        }
        else if (isStr > 0) {
            status = H5AreadVL_str(env, (hid_t)attr_id, (hid_t)mem_type_id, buf);
        }
    } /* end else */

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Aread_1VL */

herr_t
H5AreadVL_asstr
    (JNIEnv *env, hid_t aid, hid_t tid, jobjectArray buf)
{
    jint    i;
    jint    n;
    hid_t   sid;
    jstring jstr;
    h5str_t h5str;
    hvl_t  *rdata;
    hsize_t dims[H5S_MAX_RANK];
    size_t  size;
    size_t  max_len = 0;
    herr_t  status = -1;

    /* Get size of string array */
    n = ENVPTR->GetArrayLength(ENVPAR buf);
    /* we will need to read n number of hvl_t structures */
    rdata = (hvl_t*)HDcalloc((size_t)n, sizeof(hvl_t));
    if (rdata == NULL) {
        h5JNIFatalError(env, "H5AreadVL_asstr:  failed to allocate buff for read");
    } /* end if */
    else {
        status = H5Aread(aid, tid, rdata);

        if (status < 0) {
            dims[0] = (hsize_t)n;
            sid = H5Screate_simple(1, dims, NULL);
            H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, rdata);
            H5Sclose(sid);
            HDfree(rdata);
            h5JNIFatalError(env, "H5AreadVL_asstr: failed to read data");
        } /* end if */
        else {
            /* calculate the largest size of all the hvl_t structures read */
            max_len = 1;
            for (i=0; i < n; i++) {
                if ((rdata + i)->len > max_len)
                    max_len = (rdata + i)->len;
            }

            /* create one malloc to hold largest element */
            size = H5Tget_size(tid) * max_len;
            HDmemset(&h5str, 0, sizeof(h5str_t));
            h5str_new(&h5str, 4 * size);

            if (h5str.s == NULL) {
                dims[0] = (hsize_t)n;
                sid = H5Screate_simple(1, dims, NULL);
                H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, rdata);
                H5Sclose(sid);
                HDfree(rdata);
                h5JNIFatalError(env, "H5AreadVL_asstr:  failed to allocate buf");
            } /* end if */
            else {
                H5T_class_t tclass = H5Tget_class(tid);
                /* convert each element to char string */
                for (i=0; i < n; i++) {
                    h5str.s[0] = '\0';
                    h5str_vlsprintf(&h5str, aid, tid, rdata+i, 0);
                    jstr = ENVPTR->NewStringUTF(ENVPAR h5str.s);
                    ENVPTR->SetObjectArrayElement(ENVPAR buf, i, jstr);
                } /* end for */
                h5str_free(&h5str);

                dims[0] = (hsize_t)n;
                sid = H5Screate_simple(1, dims, NULL);
                H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, rdata);
                H5Sclose(sid);
                HDfree(rdata);
            } /* end else */
        } /* end else */
    } /* end else */

    return status;
}

herr_t
H5AreadVL_str
    (JNIEnv *env, hid_t aid, hid_t tid, jobjectArray buf)
{
    char  **strs;
    jstring jstr;
    jint    i;
    jint    n;
    hid_t   sid;
    hsize_t dims[H5S_MAX_RANK];
    herr_t  status = -1;

    n = ENVPTR->GetArrayLength(ENVPAR buf);
    strs =(char**)HDcalloc((size_t)n, sizeof(char*));

    if (strs == NULL) {
        h5JNIFatalError(env, "H5AreadVL_str:  failed to allocate buff for read variable length strings");
    } /* end if */
    else {
        status = H5Aread(aid, tid, strs);

        if (status < 0) {
            dims[0] = (hsize_t)n;
            sid = H5Screate_simple(1, dims, NULL);
            H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, strs);
            H5Sclose(sid);
            HDfree(strs);
            h5JNIFatalError(env, "H5AreadVL_str: failed to read variable length strings");
        } /* end if */
        else {
            for (i=0; i < n; i++) {
                jstr = ENVPTR->NewStringUTF(ENVPAR strs[i]);
                ENVPTR->SetObjectArrayElement(ENVPAR buf, i, jstr);
                H5free_memory (strs[i]);
            } /* end for */

            /*
            for repeatedly reading a dataset with a large number of strs (e.g., 1,000,000 strings,
            H5Dvlen_reclaim() may crash on Windows because the Java GC will not be able to collect
            free space in time. Instead, use "H5free_memory(strs[i])" above to free individual strings
            after it is done.
            H5Dvlen_reclaim(tid, mem_sid, xfer_plist_id, strs);
            */

            HDfree(strs);
        } /* end else */
    } /* end else */

    return status;
} /* end H5AreadVL_str */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aget_space
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Aget_1space
    (JNIEnv *env, jclass clss, jlong attr_id)
{
    hid_t retVal = -1;

    retVal = H5Aget_space((hid_t)attr_id);
    if (retVal < 0)
        h5libraryError(env);

    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Aget_1space */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aget_type
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Aget_1type
    (JNIEnv *env, jclass clss, jlong attr_id)
{
    hid_t retVal = -1;

    retVal = H5Aget_type((hid_t)attr_id);
    if (retVal < 0)
        h5libraryError(env);

    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Aget_1type */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aget_name
 * Signature: (J)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Aget_1name
    (JNIEnv *env, jclass clss, jlong attr_id)
{
    char    *aName;
    jstring  str = NULL;
    ssize_t  buf_size;

    /* get the length of the name */
    buf_size = H5Aget_name((hid_t)attr_id, 0, NULL);
    if (buf_size <= 0) {
        h5badArgument(env, "H5Aget_name:  buf_size <= 0");
    } /* end if */
    else {
        buf_size++; /* add extra space for the null terminator */
        aName = (char*)HDmalloc(sizeof(char) * (size_t)buf_size);
        if (aName == NULL) {
            h5outOfMemory(env, "H5Aget_name:  malloc failed");
        } /* end if */
        else {
            buf_size = H5Aget_name((hid_t)attr_id, (size_t)buf_size, aName);
            if (buf_size < 0) {
                HDfree(aName);
                h5libraryError(env);
            } /* end if */
            else {
                /* save the string; */
                str = ENVPTR->NewStringUTF(ENVPAR aName);
                HDfree(aName);
            } /* end else */
        } /* end else */
    } /* end else */
    return str;
} /* end Java_hdf_hdf5lib_H5_H5Aget_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aget_num_attrs
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Aget_1num_1attrs
    (JNIEnv *env, jclass clss, jlong loc_id)
{
    int retVal = -1;

    retVal = H5Aget_num_attrs((hid_t)loc_id);
    if (retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Aget_1num_1attrs */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Adelete
 * Signature: (JLjava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Adelete
    (JNIEnv *env, jclass clss, jlong loc_id, jstring name)
{
    herr_t      status = -1;
    const char *aName;

    PIN_JAVA_STRING(name, aName);
    if (aName != NULL) {
        status = H5Adelete((hid_t)loc_id, aName);

        UNPIN_JAVA_STRING(name, aName);

        if (status < 0)
            h5libraryError(env);
    }

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Adelete */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aclose
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5__1H5Aclose
    (JNIEnv *env, jclass clss, jlong attr_id)
{
    herr_t retVal = -1;

    if (attr_id > 0)
        retVal = H5Aclose((hid_t)attr_id);

    if (retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Aclose */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Acreate2
 * Signature: (JLjava/lang/String;JJJJ)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Acreate2
    (JNIEnv *env, jclass clss, jlong loc_id, jstring name, jlong type_id,
        jlong space_id, jlong create_plist, jlong access_plist)
{
    hid_t       status = -1;
    const char *aName;

    PIN_JAVA_STRING(name, aName);
    if (aName != NULL) {
        status = H5Acreate2((hid_t)loc_id, aName, (hid_t)type_id,
            (hid_t)space_id, (hid_t)create_plist, (hid_t)access_plist );

        UNPIN_JAVA_STRING(name, aName);

        if (status < 0)
            h5libraryError(env);
    }

    return (jlong)status;
} /* end Java_hdf_hdf5lib_H5__1H5Acreate2 */


/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Aopen
 * Signature: (JLjava/lang/String;J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Aopen
    (JNIEnv *env, jclass clss, jlong obj_id, jstring name, jlong access_plist)

{
    hid_t       retVal = -1;
    const char *aName;

    PIN_JAVA_STRING(name, aName);
    if (aName != NULL) {
        retVal = H5Aopen((hid_t)obj_id, aName, (hid_t)access_plist);

        UNPIN_JAVA_STRING(name, aName);

        if (retVal < 0)
            h5libraryError(env);
    }

    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Aopen */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Aopen_by_idx
 * Signature: (JLjava/lang/String;IIJJJ)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Aopen_1by_1idx
    (JNIEnv *env, jclass clss, jlong loc_id, jstring name, jint idx_type, jint order, jlong n, jlong aapl_id, jlong lapl_id)
{
    hid_t       retVal = -1;
    const char *aName;

    PIN_JAVA_STRING(name, aName);
    if (aName != NULL) {
        retVal = H5Aopen_by_idx((hid_t)loc_id, aName, (H5_index_t)idx_type,
                (H5_iter_order_t)order, (hsize_t)n, (hid_t)aapl_id, (hid_t)lapl_id);

        UNPIN_JAVA_STRING(name, aName);

        if (retVal < 0)
            h5libraryError(env);
    }

    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Aopen_1by_1idx */

/*
* Class:     hdf_hdf5lib_H5
* Method:    _H5Acreate_by_name
* Signature: (JLjava/lang/String;Ljava/lang/String;JJJJJ)J
*/
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Acreate_1by_1name
    (JNIEnv *env, jclass clss, jlong loc_id, jstring obj_name, jstring attr_name, jlong type_id, jlong space_id, jlong acpl_id, jlong aapl_id, jlong lapl_id)
{
    hid_t       retVal = -1;
    const char *aName;
    const char *attrName;

    PIN_JAVA_STRING_TWO(obj_name, aName, attr_name, attrName);
    if (aName != NULL && attrName != NULL) {
        retVal = H5Acreate_by_name((hid_t)loc_id, aName, attrName, (hid_t)type_id,
                (hid_t)space_id, (hid_t)acpl_id, (hid_t)aapl_id, (hid_t)lapl_id);

        UNPIN_JAVA_STRING_TWO(obj_name, aName, attr_name, attrName);

        if (retVal < 0)
            h5libraryError(env);
    }

    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Acreate_1by_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aexists_by_name
 * Signature: (JLjava/lang/String;Ljava/lang/String;J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Aexists_1by_1name
    (JNIEnv *env, jclass clss, jlong loc_id, jstring obj_name, jstring attr_name, jlong lapl_id)
{
    htri_t      bval = JNI_FALSE;
    const char *aName;
    const char *attrName;

    PIN_JAVA_STRING_TWO(obj_name, aName, attr_name, attrName);
    if (aName != NULL && attrName != NULL) {
        bval = H5Aexists_by_name((hid_t)loc_id, aName, attrName, (hid_t)lapl_id);

        UNPIN_JAVA_STRING_TWO(obj_name, aName, attr_name, attrName);

        if (bval > 0)
            bval = JNI_TRUE;
        else if (bval < 0)
            h5libraryError(env);
    }

    return (jboolean)bval;
} /* end Java_hdf_hdf5lib_H5_H5Aexists_1by_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Arename
 * Signature: (JLjava/lang/String;Ljava/lang/String)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Arename
    (JNIEnv *env, jclass clss, jlong loc_id, jstring old_attr_name, jstring new_attr_name)
{
    herr_t      retVal = -1;
    const char *oName;
    const char *nName;

    PIN_JAVA_STRING_TWO(old_attr_name, oName, new_attr_name, nName);
    if (oName != NULL && nName != NULL) {
        retVal = H5Arename((hid_t)loc_id, oName, nName);

        UNPIN_JAVA_STRING_TWO(old_attr_name, oName, new_attr_name, nName);

        if (retVal < 0)
            h5libraryError(env);
    }

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Arename */


/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Arename_by_name
 * Signature: (JLjava/lang/String;Ljava/lang/String;Ljava/lang/String;J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Arename_1by_1name
    (JNIEnv *env, jclass clss, jlong loc_id, jstring obj_name, jstring old_attr_name, jstring new_attr_name, jlong lapl_id)
{
    herr_t      retVal = -1;
    const char *aName;
    const char *oName;
    const char *nName;

    PIN_JAVA_STRING_THREE(obj_name, aName, old_attr_name, oName, new_attr_name, nName);
    if (aName != NULL && oName != NULL && nName != NULL) {
        retVal = H5Arename_by_name((hid_t)loc_id, aName, oName, nName, (hid_t)lapl_id);

        UNPIN_JAVA_STRING_THREE(obj_name, aName, old_attr_name, oName, new_attr_name, nName);

        if (retVal < 0)
            h5libraryError(env);
    }

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Arename_1by_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aget_name_by_idx
 * Signature: (JLjava/lang/String;IIJJ)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Aget_1name_1by_1idx
    (JNIEnv *env, jclass clss, jlong loc_id, jstring obj_name, jint idx_type, jint order, jlong n, jlong lapl_id)
{
    size_t   buf_size;
    char    *aValue;
    jlong    status_size;
    jstring  str = NULL;
    const char *aName;

    PIN_JAVA_STRING(obj_name, aName);
    if (aName != NULL) {
        /* get the length of the attribute name */
        status_size = H5Aget_name_by_idx((hid_t)loc_id, aName, (H5_index_t)idx_type,
                (H5_iter_order_t) order, (hsize_t) n, (char*)NULL, (size_t)0, (hid_t)lapl_id);

        if(status_size < 0) {
            UNPIN_JAVA_STRING(obj_name, aName);
            h5libraryError(env);
        } /* end if */
        else {
            buf_size = (size_t)status_size + 1;/* add extra space for the null terminator */

            aValue = (char*)HDmalloc(sizeof(char) * buf_size);
            if (aValue == NULL) {
                UNPIN_JAVA_STRING(obj_name, aName);
                h5outOfMemory(env, "H5Aget_name_by_idx:  malloc failed ");
            } /* end if */
            else {
                status_size = H5Aget_name_by_idx((hid_t)loc_id, aName, (H5_index_t)idx_type,
                        (H5_iter_order_t) order, (hsize_t) n, (char*)aValue, (size_t)buf_size, (hid_t)lapl_id);

                UNPIN_JAVA_STRING(obj_name, aName);

                if (status_size < 0) {
                    HDfree(aValue);
                    h5libraryError(env);
                } /* end if */
                else {
                    str = ENVPTR->NewStringUTF(ENVPAR aValue);
                    HDfree(aValue);
                    if (str == NULL) {
                        /* exception -- fatal JNI error */
                        h5JNIFatalError(env, "H5Aget_name_by_idx:  return string not created");
                    } /* end if */
                } /* end else */
            } /* end else */
        } /* end else */
    }
    return str;
} /* end Java_hdf_hdf5lib_H5_H5Aget_1name_1by_1idx */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aget_storage_size
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Aget_1storage_1size
    (JNIEnv *env, jclass clss, jlong attr_id)
{
    hsize_t retVal = (hsize_t)-1;

    retVal = H5Aget_storage_size((hid_t)attr_id);
/* probably returns '0' if fails--don't do an exception */
    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Aget_1storage_1size */


/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aget_info
 * Signature: (J)Lhdf/hdf5lib/structs/H5A_info_t;
 */
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5Aget_1info
    (JNIEnv *env, jclass clss, jlong attr_id)
{
    herr_t     status = -1;
    H5A_info_t ainfo;
    jvalue     args[4];
    jobject    ret_obj = NULL;

    status = H5Aget_info((hid_t)attr_id, &ainfo);

    if (status < 0) {
       h5libraryError(env);
    } /* end if */
    else {
        args[0].z = ainfo.corder_valid;
        args[1].j = ainfo.corder;
        args[2].i = ainfo.cset;
        args[3].j = (jlong)ainfo.data_size;
        CALL_CONSTRUCTOR("hdf/hdf5lib/structs/H5A_info_t", "(ZJIJ)V", args);
    } /* end else */
    return ret_obj;
} /* end Java_hdf_hdf5lib_H5_H5Aget_1info */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aget_info_by_idx
 * Signature: (JLjava/lang/String;IIJJ)Lhdf/hdf5lib/structs/H5A_info_t;
 */
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5Aget_1info_1by_1idx
    (JNIEnv *env, jclass clss, jlong loc_id, jstring obj_name, jint idx_type, jint order, jlong n, jlong lapl_id)
{
    herr_t      status;
    H5A_info_t  ainfo;
    jvalue      args[4];
    jobject     ret_obj = NULL;
    const char *aName;

    PIN_JAVA_STRING(obj_name, aName);
    if (aName != NULL) {
        status = H5Aget_info_by_idx((hid_t)loc_id, aName, (H5_index_t)idx_type,
                (H5_iter_order_t)order, (hsize_t)n, &ainfo, (hid_t)lapl_id);

        UNPIN_JAVA_STRING(obj_name, aName);

        if (status < 0) {
            h5libraryError(env);
        } /* end if */
        else {
            args[0].z = ainfo.corder_valid;
            args[1].j = ainfo.corder;
            args[2].i = ainfo.cset;
            args[3].j = (jlong)ainfo.data_size;
            CALL_CONSTRUCTOR("hdf/hdf5lib/structs/H5A_info_t", "(ZJIJ)V", args);
        } /* end else */
    }
    return ret_obj;
} /* end Java_hdf_hdf5lib_H5_H5Aget_1info_1by_1idx */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aget_info_by_name
 * Signature: (JLjava/lang/String;Ljava/lang/String;J)Lhdf/hdf5lib/structs/H5A_info_t;
 */
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5Aget_1info_1by_1name
    (JNIEnv *env, jclass clss, jlong loc_id, jstring obj_name, jstring attr_name, jlong lapl_id)
{
    const char *aName;
    const char *attrName;
    herr_t      status;
    H5A_info_t  ainfo;
    jvalue      args[4];
    jobject     ret_obj = NULL;

    PIN_JAVA_STRING_TWO(obj_name, aName, attr_name, attrName);
    if (aName != NULL && attrName != NULL) {
        status = H5Aget_info_by_name((hid_t)loc_id, aName, attrName, &ainfo, (hid_t)lapl_id);

        UNPIN_JAVA_STRING_TWO(obj_name, aName, attr_name, attrName);

        if (status < 0) {
            h5libraryError(env);
        } /* end if */
        else {
            args[0].z = ainfo.corder_valid;
            args[1].j = ainfo.corder;
            args[2].i = ainfo.cset;
            args[3].j = (jlong)ainfo.data_size;
            CALL_CONSTRUCTOR("hdf/hdf5lib/structs/H5A_info_t", "(ZJIJ)V", args);
        } /* end else */
    }
    return ret_obj;
} /* end Java_hdf_hdf5lib_H5_H5Aget_1info_1by_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Adelete_by_name
 * Signature: (JLjava/lang/String;Ljava/lang/String;J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Adelete_1by_1name
    (JNIEnv *env, jclass clss, jlong loc_id, jstring obj_name, jstring attr_name, jlong lapl_id)
{
    herr_t      retVal = -1;
    const char *aName;
    const char *attrName;

    PIN_JAVA_STRING_TWO(obj_name, aName, attr_name, attrName);
    if (aName != NULL && attrName != NULL) {
        retVal = H5Adelete_by_name((hid_t)loc_id, aName, attrName, (hid_t)lapl_id);

        UNPIN_JAVA_STRING_TWO(obj_name, aName, attr_name, attrName);

        if (retVal < 0)
            h5libraryError(env);
    }

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Adelete_1by_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aexists
 * Signature: (JLjava/lang/String;)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Aexists
    (JNIEnv *env, jclass clss, jlong obj_id, jstring attr_name)
{
    htri_t      bval = JNI_FALSE;
    const char *aName;

    PIN_JAVA_STRING(attr_name, aName);
    if (aName != NULL) {
        bval = H5Aexists((hid_t)obj_id, aName);

        UNPIN_JAVA_STRING(attr_name, aName);

        if (bval > 0)
            bval = JNI_TRUE;
        else if (bval < 0)
            h5libraryError(env);
    }

    return (jboolean)bval;
} /* end Java_hdf_hdf5lib_H5_H5Aexists */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Adelete_by_idx
 * Signature: (JLjava/lang/String;IIJJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Adelete_1by_1idx
    (JNIEnv *env, jclass clss, jlong loc_id, jstring obj_name, jint idx_type, jint order, jlong n, jlong lapl_id)
{
    herr_t      status = -1;
    const char *aName;

    PIN_JAVA_STRING(obj_name, aName);
    if (aName != NULL) {
        status = H5Adelete_by_idx((hid_t)loc_id, aName, (H5_index_t)idx_type, (H5_iter_order_t)order, (hsize_t)n, (hid_t)lapl_id);

        UNPIN_JAVA_STRING(obj_name, aName);

        if (status < 0)
            h5libraryError(env);
    }
} /* end Java_hdf_hdf5lib_H5_H5Adelete_1by_1idx */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Aopen_by_name
 * Signature: (JLjava/lang/String;Ljava/lang/String;JJ)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Aopen_1by_1name
    (JNIEnv *env, jclass clss, jlong loc_id, jstring obj_name, jstring attr_name, jlong aapl_id, jlong lapl_id)

{
    hid_t       status = -1;
    const char *aName;
    const char *oName;

    PIN_JAVA_STRING_TWO(obj_name, oName, attr_name, aName);
    if (oName != NULL && aName != NULL) {
        status = H5Aopen_by_name((hid_t)loc_id, oName, aName, (hid_t)aapl_id, (hid_t)lapl_id);

        UNPIN_JAVA_STRING_TWO(obj_name, oName, attr_name, aName);

        if (status < 0)
            h5libraryError(env);
    }

    return (jlong)status;
} /* end Java_hdf_hdf5lib_H5__1H5Aopen_1by_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aget_create_plist
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Aget_1create_1plist
    (JNIEnv *env, jclass clss, jlong attr_id)
{
    hid_t retVal = -1;

    retVal = H5Aget_create_plist((hid_t)attr_id);
    if (retVal < 0)
        h5libraryError(env);

    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Aget_1create_1plist */

static herr_t
H5A_iterate_cb
    (hid_t g_id, const char *name, const H5A_info_t *info, void *op_data) {
    JNIEnv    *cbenv;
    jint       status = -1;
    jclass     cls;
    jmethodID  mid;
    jstring    str;
    jmethodID  constructor;
    jvalue     args[4];
    jobject    cb_info_t = NULL;

    if(JVMPTR->AttachCurrentThread(JVMPAR2 (void**)&cbenv, NULL) == 0) {
        cls = CBENVPTR->GetObjectClass(CBENVPAR visit_callback);
        if (cls != 0) {
            mid = CBENVPTR->GetMethodID(CBENVPAR cls, "callback", "(JLjava/lang/String;Lhdf/hdf5lib/structs/H5A_info_t;Lhdf/hdf5lib/callbacks/H5A_iterate_t;)I");
            if (mid != 0) {
                str = CBENVPTR->NewStringUTF(CBENVPAR name);

                args[0].z = info->corder_valid;
                args[1].j = info->corder;
                args[2].i = info->cset;
                args[3].j = (jlong)info->data_size;
                /* get a reference to your class if you don't have it already */
                cls = CBENVPTR->FindClass(CBENVPAR "hdf/hdf5lib/structs/H5A_info_t");
                if (cls != 0) {
                    /* get a reference to the constructor; the name is <init> */
                    constructor = CBENVPTR->GetMethodID(CBENVPAR cls, "<init>", "(ZJIJ)V");
                    if (constructor != 0) {
                        cb_info_t = CBENVPTR->NewObjectA(CBENVPAR cls, constructor, args);
                        if (cb_info_t == NULL) {
                            printf("FATAL ERROR:  hdf/hdf5lib/structs/H5A_info_t: Creation failed\n");
                        }
                        else {
                            status = CBENVPTR->CallIntMethod(CBENVPAR visit_callback, mid, g_id, str, cb_info_t, op_data);
                        }
                    } /* end if (constructor != 0) */
                } /* end if (cls != 0) */
            } /* end if (mid != 0) */
        } /* end if (cls != 0) */
    } /* end if */
    JVMPTR->DetachCurrentThread(JVMPAR);

    return (herr_t)status;
} /* end H5A_iterate_cb */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aiterate
 * Signature: (JIIJLjava/lang/Object;Ljava/lang/Object;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Aiterate
    (JNIEnv *env, jclass clss, jlong grp_id, jint idx_type, jint order,
          jlong idx, jobject callback_op, jobject op_data)
{
    hsize_t       start_idx = (hsize_t)idx;
    herr_t        status = -1;

    ENVPTR->GetJavaVM(ENVPAR &jvm);
    visit_callback = callback_op;

    if ((op_data == NULL) || (callback_op == NULL)) {
        h5nullArgument(env,  "H5Literate_by_name:  op_data or callback_op is NULL");
    } /* end if */
    else {
        status = H5Aiterate2((hid_t)grp_id, (H5_index_t)idx_type, (H5_iter_order_t)order, (hsize_t*)&start_idx, (H5A_operator2_t)H5A_iterate_cb, (void*)op_data);

        if (status < 0)
            h5libraryError(env);
    } /* end else */

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Aiterate */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aiterate_by_name
 * Signature: (JLjava/lang/String;IIJLjava/lang/Object;Ljava/lang/Object;J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Aiterate_1by_1name
    (JNIEnv *env, jclass clss, jlong grp_id, jstring name, jint idx_type, jint order,
          jlong idx, jobject callback_op, jobject op_data, jlong access_id)
{
    const char   *lName;
    hsize_t       start_idx = (hsize_t)idx;
    herr_t        status = -1;

    ENVPTR->GetJavaVM(ENVPAR &jvm);
    visit_callback = callback_op;

    if ((op_data == NULL) || (callback_op == NULL)) {
        h5nullArgument(env,  "H5Literate_by_name:  op_data or callback_op is NULL");
    } /* end if */
    else {
        PIN_JAVA_STRING(name, lName);
        if (lName != NULL) {
            status = H5Aiterate_by_name((hid_t)grp_id, lName, (H5_index_t)idx_type, (H5_iter_order_t)order, (hsize_t*)&start_idx, (H5A_operator2_t)H5A_iterate_cb, (void*)op_data, (hid_t)access_id);

            UNPIN_JAVA_STRING(name, lName);

            if (status < 0)
                h5libraryError(env);
        }
    } /* end else */

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Aiterate_1by_1name */

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
