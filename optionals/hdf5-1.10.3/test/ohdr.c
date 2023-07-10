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

/* Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Tuesday, November 24, 1998
 */
#include "h5test.h"
#include "H5Iprivate.h"

/*
 * This file needs to access private datatypes from the H5O package.
 * This file also needs to access the object header testing code.
 */
#define H5O_FRIEND        /*suppress error about including H5Opkg      */
#define H5O_TESTING
#include "H5Opkg.h"

/*
 * This file needs to access private datatypes from the H5G package.
 */
#define H5G_FRIEND        /*suppress error about including H5Gpkg      */
#include "H5Gpkg.h"

#include "H5CXprivate.h"        /* API Contexts                         */

const char *FILENAME[] = {
    "ohdr",
    NULL
};

/* The tbogus.h5 is generated from gen_bogus.c in HDF5 'test' directory.
 * To get this data file, define H5O_ENABLE_BOGUS in src/H5Oprivate, rebuild
 * the library and simply compile gen_bogus.c with that HDF5 library and run it.
 */
#define FILE_BOGUS "tbogus.h5"

/*  */
#define FILE_OHDR_SWMR "ohdr_swmr.h5"
#define DSET_NAME "COMPACT_DSET"
#define OBJ_VERSION_LATEST 2

/*
 *  Verify that messages are moved forward into a "continuation message":
 *    Create an object header with several continuation chunks
 *    Remove a message in the last chunk
 *    The remaining message(s) in the last chunk should be moved forward into the continuation message
 *    The process will repeat when the continuation message is big enough to hold all the
 *        messages in the last chunk.
 *    Result: the number of chunks should be reduced
 */
static herr_t
test_cont(char *filename, hid_t fapl)
{
    hid_t    file=-1;
    H5F_t    *f = NULL;
    H5O_hdr_info_t hdr_info;
    H5O_loc_t    oh_locA, oh_locB;
    time_t    time_new;
    const char    *short_name = "T";
    const char    *long_name = "This is the message";
    size_t    nchunks;

    TESTING("object header continuation block");

    HDmemset(&oh_locA, 0, sizeof(oh_locA));
    HDmemset(&oh_locB, 0, sizeof(oh_locB));

    /* Create the file to operate on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR
    if (H5AC_ignore_tags(f) < 0) {
        H5_FAILED();
        H5Eprint2(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */

    if(H5O_create(f, (size_t)H5O_MIN_SIZE, (size_t)0, H5P_GROUP_CREATE_DEFAULT, &oh_locA/*out*/) < 0)
            FAIL_STACK_ERROR

    if(H5O_create(f, (size_t)H5O_MIN_SIZE, (size_t)0, H5P_GROUP_CREATE_DEFAULT, &oh_locB/*out*/) < 0)
            FAIL_STACK_ERROR

    time_new = 11111111;

    if(H5O_msg_create(&oh_locA, H5O_NAME_ID, 0, 0, &long_name) < 0)
        FAIL_STACK_ERROR

    if(H5O_msg_create(&oh_locB, H5O_MTIME_ID, 0, 0, &time_new) < 0)
        FAIL_STACK_ERROR
    if(H5O_msg_create(&oh_locB, H5O_MTIME_ID, 0, 0, &time_new) < 0)
        FAIL_STACK_ERROR
    if(H5O_msg_create(&oh_locB, H5O_MTIME_ID, 0, 0, &time_new) < 0)
        FAIL_STACK_ERROR

    if(H5O_msg_create(&oh_locA, H5O_MTIME_NEW_ID, 0, 0, &time_new) < 0)
        FAIL_STACK_ERROR

    if(H5O_msg_create(&oh_locB, H5O_MTIME_ID, 0, 0, &time_new) < 0)
        FAIL_STACK_ERROR

    if(H5O_msg_create(&oh_locA, H5O_NAME_ID, 0, 0, &short_name) < 0)
        FAIL_STACK_ERROR

    if(1 != H5O_link(&oh_locA, 1))
        FAIL_STACK_ERROR
    if(1 != H5O_link(&oh_locB, 1))
        FAIL_STACK_ERROR
    if(H5AC_flush(f) < 0)
        FAIL_STACK_ERROR
    if(H5O_expunge_chunks_test(&oh_locA) < 0)
        FAIL_STACK_ERROR

    if(H5O_get_hdr_info(&oh_locA, &hdr_info) < 0)
        FAIL_STACK_ERROR
    nchunks = hdr_info.nchunks;

    /* remove the 1st H5O_NAME_ID message */
    if(H5O_msg_remove(&oh_locA, H5O_NAME_ID, 0, FALSE) < 0)
        FAIL_STACK_ERROR

    if(H5O_get_hdr_info(&oh_locA, &hdr_info) < 0)
        FAIL_STACK_ERROR

    if(hdr_info.nchunks >= nchunks)
        TEST_ERROR

    if(H5O_close(&oh_locA, NULL) < 0)
        FAIL_STACK_ERROR
    if(H5O_close(&oh_locB, NULL) < 0)
        FAIL_STACK_ERROR
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5O_close(&oh_locA, NULL);
        H5O_close(&oh_locB, NULL);
        H5Fclose(file);
    } H5E_END_TRY;

    return FAIL;
} /* end test_cont() */

/*
 *  Verify that object headers are held in the cache until they are linked
 *      to a location in the graph, or assigned an ID.  This is done by
 *      creating an object header, then forcing it out of the cache by creating
 *      local heaps until the object header is evicted from the cache, then
 *      modifying the object header.  The refcount on the object header is
 *      checked as verifying that the object header has remained in the cache.
 */
static herr_t
test_ohdr_cache(char *filename, hid_t fapl)
{
    hid_t    file = -1;              /* File ID */
    hid_t       my_fapl;                /* FAPL ID */
    H5AC_cache_config_t mdc_config;     /* Metadata cache configuration info */
    H5F_t    *f = NULL;              /* File handle */
    H5HL_t      *lheap, *lheap2, *lheap3; /* Pointer to local heaps */
    haddr_t     lheap_addr, lheap_addr2, lheap_addr3; /* Local heap addresses */
    H5O_loc_t    oh_loc;                 /* Object header location */
    time_t    time_new;               /* Time value for modification time message */
    unsigned    rc;                     /* Refcount for object */

    TESTING("object header creation in cache");

    /* Make a copy of the FAPL */
    if((my_fapl = H5Pcopy(fapl)) < 0)
        FAIL_STACK_ERROR

    /* Tweak down the size of the metadata cache to only 64K */
    mdc_config.version = H5AC__CURR_CACHE_CONFIG_VERSION;
    if(H5Pget_mdc_config(my_fapl, &mdc_config) < 0)
        FAIL_STACK_ERROR
    mdc_config.set_initial_size = TRUE;
    mdc_config.initial_size = 32 * 1024;
    mdc_config.max_size = 64 * 1024;
    mdc_config.min_size = 8 * 1024;
    if(H5Pset_mdc_config(my_fapl, &mdc_config) < 0)
        FAIL_STACK_ERROR

    /* Create the file to operate on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, my_fapl)) < 0)
        FAIL_STACK_ERROR
    if(H5Pclose(my_fapl) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR
    if(H5AC_ignore_tags(f) < 0)
        FAIL_STACK_ERROR

    /* Create object (local heap) that occupies most of cache */
    if(H5HL_create(f, (31 * 1024), &lheap_addr) < 0)
        FAIL_STACK_ERROR

    /* Protect local heap (which actually pins it in the cache) */
    if(NULL == (lheap = H5HL_protect(f, lheap_addr, H5AC__READ_ONLY_FLAG)))
        FAIL_STACK_ERROR

    /* Create an object header */
    HDmemset(&oh_loc, 0, sizeof(oh_loc));
    if(H5O_create(f, (size_t)2048, (size_t)1, H5P_GROUP_CREATE_DEFAULT, &oh_loc/*out*/) < 0)
        FAIL_STACK_ERROR

    /* Query object header information */
    rc = 0;
    if(H5O_get_rc(&oh_loc, &rc) < 0)
        FAIL_STACK_ERROR
    if(0 != rc)
        TEST_ERROR

    /* Create object (local heap) that occupies most of cache */
    if(H5HL_create(f, (31 * 1024), &lheap_addr2) < 0)
        FAIL_STACK_ERROR

    /* Protect local heap (which actually pins it in the cache) */
    if(NULL == (lheap2 = H5HL_protect(f, lheap_addr2, H5AC__READ_ONLY_FLAG)))
        FAIL_STACK_ERROR

    /* Unprotect local heap (which actually unpins it from the cache) */
    if(H5HL_unprotect(lheap2) < 0)
        FAIL_STACK_ERROR

    /* Create object header message in new object header */
    time_new = 11111111;
    if(H5O_msg_create(&oh_loc, H5O_MTIME_NEW_ID, 0, 0, &time_new) < 0)
        FAIL_STACK_ERROR

    /* Create object (local heap) that occupies most of cache */
    if(H5HL_create(f, (31 * 1024), &lheap_addr3) < 0)
        FAIL_STACK_ERROR

    /* Protect local heap (which actually pins it in the cache) */
    if(NULL == (lheap3 = H5HL_protect(f, lheap_addr3, H5AC__READ_ONLY_FLAG)))
        FAIL_STACK_ERROR

    /* Unprotect local heap (which actually unpins it from the cache) */
    if(H5HL_unprotect(lheap3) < 0)
        FAIL_STACK_ERROR

    /* Query object header information */
    /* (Note that this is somewhat of a weak test, since it doesn't actually
     *  verify that the object header was evicted from the cache, but it's
     *  very difficult to verify when an entry is evicted from the cache in
     *  a non-invasive way -QAK)
     */
    rc = 0;
    if(H5O_get_rc(&oh_loc, &rc) < 0)
        FAIL_STACK_ERROR
    if(0 != rc)
        TEST_ERROR

    /* Decrement reference count o object header */
    if(H5O_dec_rc_by_loc(&oh_loc) < 0)
        FAIL_STACK_ERROR

    /* Close object header created */
    if(H5O_close(&oh_loc, NULL) < 0)
        FAIL_STACK_ERROR

    /* Unprotect local heap (which actually unpins it from the cache) */
    if(H5HL_unprotect(lheap) < 0)
        FAIL_STACK_ERROR

    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Fclose(file);
    } H5E_END_TRY;

    return FAIL;
} /* test_ohdr_cache() */

/*
 *  To exercise the coding for the re-read of the object header for SWMR access.
 *  When the object header is read in H5O_load() of H5Ocache.c, the library initially reads
 *  H5O_SPEC_READ_SIZE (512, currently)  bytes for decoding, then reads the
 *  remaining bytes later if the object header is greater than H5O_SPEC_READ_SIZE
 *  bytes.  For SWMR access, the read should be done all at one time.
 */
static herr_t
test_ohdr_swmr(hbool_t new_format)
{
    hid_t fid = -1;                /* File ID */
    hid_t fapl = -1;            /* File access property list */
    hid_t did = -1;                /* Dataset ID */
    hid_t sid = -1;             /* Dataspace ID */
    hid_t plist = -1;            /* Dataset creation property list */
    size_t compact_size = 1024;    /* The size of compact dataset */
    int *wbuf = NULL;            /* Buffer for writing */
    hsize_t dims[1];            /* Dimension sizes */
    size_t u;                    /* Iterator */
    int n;                      /* Data variable */
    H5O_info_t obj_info;        /* Information for the object */

    if(new_format) {
        TESTING("exercise the coding for the re-read of the object header for SWMR access: latest-format");
    } else {
        TESTING("exercise the coding for the re-read of the object header for SWMR access: non-latest-format");
    } /* end if */

    /* File access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        FAIL_STACK_ERROR

    /* Create the file with/without latest format: ensure version 2 object header for SWMR */
    if(new_format)  {
        /* Set to use latest library format */
        if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
            FAIL_STACK_ERROR

        if((fid = H5Fcreate(FILE_OHDR_SWMR, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            TEST_ERROR
    } /* end if */
    else {
        if((fid = H5Fcreate(FILE_OHDR_SWMR, H5F_ACC_TRUNC|H5F_ACC_SWMR_WRITE, H5P_DEFAULT, fapl)) < 0)
            TEST_ERROR
    } /* end else */

    /* Initialize data */
    wbuf = (int *)HDcalloc(compact_size, sizeof(int));
    n = 0;
    for(u = 0; u < compact_size; u++)
        wbuf[u] = n++;

    /* Create a small data space for compact dataset */
    dims[0] = (hsize_t)compact_size;
    if((sid = H5Screate_simple(1, dims, NULL)) < 0)
        FAIL_STACK_ERROR

    /* Create property list for compact dataset creation */
    if((plist = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR

    /* Set the layout for the compact dataset */
    if(H5Pset_layout(plist, H5D_COMPACT) < 0)
        FAIL_STACK_ERROR

    /* Create a compact dataset */
    if((did = H5Dcreate2(fid, DSET_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, plist, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Write to the compact dataset */
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf) < 0)
        FAIL_STACK_ERROR

    /* Close the dataset */
    if(H5Dclose(did) < 0)
        FAIL_STACK_ERROR

    /* Close the file */
    if(H5Fclose(fid) < 0)
        FAIL_STACK_ERROR

    /* Open the file for SWMR write with/without latest format */
    if((fid = H5Fopen(FILE_OHDR_SWMR, H5F_ACC_RDWR|H5F_ACC_SWMR_WRITE, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Open the compact dataset */
    if((did = H5Dopen2(fid, DSET_NAME, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Get the object information */
    if(H5Oget_info2(did, &obj_info, H5O_INFO_HDR) < 0)
        FAIL_STACK_ERROR

    if(new_format)
        if(obj_info.hdr.version != OBJ_VERSION_LATEST)
            FAIL_STACK_ERROR

    /* The size of object header should be greater than the speculative read size of H5O_SPEC_READ_SIZE */
    /* This will exercise the coding for the re-read of the object header for SWMR access */
    if(obj_info.hdr.space.total < H5O_SPEC_READ_SIZE)
        TEST_ERROR;

    /* Close the dataset */
    if(H5Dclose(did) < 0)
        FAIL_STACK_ERROR

    /* Close the file */
    if(H5Fclose(fid) < 0)
        FAIL_STACK_ERROR

    /* Close the dataspace */
    if(H5Sclose(sid) < 0)
        FAIL_STACK_ERROR

    /* Close the dataset creation property list */
    if(H5Pclose(plist) < 0)
        FAIL_STACK_ERROR

    /* Close the file access property list */
    if(H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR

    /* Remove the test file */
    if(HDremove(FILE_OHDR_SWMR) < 0)
        FAIL_STACK_ERROR

    /* Free the buffer */
    HDfree(wbuf);

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Fclose(fid);
        H5Dclose(did);
        H5Sclose(sid);
        H5Pclose(plist);
        H5Pclose(fapl);
        HDremove(FILE_OHDR_SWMR);
        HDfree(wbuf);
    } H5E_END_TRY;

    return FAIL;
} /* test_ohdr_swmr() */

/*
 *  To test objects with unknown messages in a file with:
 *      a) H5O_BOGUS_VALID_ID:
 *       --the bogus_id is within the range of H5O_msg_class_g[]
 *      b) H5O_BOGUS_INVALID_ID:
 *         --the bogus_id is outside the range of H5O_msg_class_g[]
 *
 *   The test file is FILE_BOGUS: "tbogus.h5" generated with gen_bogus.c
 *   --objects that have unknown header messages with H5O_BOGUS_VALID_ID in "/"
 *   --objects that have unknown header messages with H5O_BOGUS_INVALID_ID in "/group"
 *
 *   The test also uses the test file FILENAME[0] (ohdr.h5): the parameter "filename"
 */
static herr_t
test_unknown(unsigned bogus_id, char *filename, hid_t fapl)
{
    hid_t fid = -1;    /* file ID */
    hid_t gid = -1;    /* group ID */
    hid_t did = -1;    /* Dataset ID */
    hid_t sid = -1;     /* Dataspace ID */
    hid_t aid = -1;     /* Attribute ID */
    hid_t loc = -1;    /* location: file or group ID */
    hid_t fid_bogus = -1;    /* bogus file ID */
    hid_t gid_bogus = -1;    /* bogus group ID */
    hid_t loc_bogus = -1;    /* location: bogus file or group ID */
    char testfile[256];

    /* create a different name for a local copy of the data file to be
       opened with rd/wr file permissions in case build and test are
       done in the source directory. */
    HDstrncpy(testfile, FILE_BOGUS, strlen(FILE_BOGUS));
    testfile[strlen(FILE_BOGUS)]='\0';
    HDstrncat(testfile, ".copy", 5);

    /* Make a copy of the data file from svn. */
    if(h5_make_local_copy(FILE_BOGUS, testfile) < 0)
      FAIL_STACK_ERROR

    TESTING("object with unknown header message and no flags set");

    /* Open filename */
    if((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Open FILE_BOGUS */
    if((fid_bogus = H5Fopen(testfile, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Set up location ID depending on bogus_id */
    if(bogus_id == H5O_BOGUS_INVALID_ID) {
        /* Open "group" in FILE_BOGUS */
        if((gid_bogus = H5Gopen2(fid_bogus, "group", H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR
        loc_bogus = gid_bogus;

        /* Create "group" in filename */
        if((gid = H5Gcreate2(fid, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR
        loc = gid;

    } else { /* H5O_BOGUS_VALID_ID */
        loc_bogus = fid_bogus;
        loc = fid;
    } /* end else */

   /* Open the dataset with the unknown header message, but no extra flags */
    if((did = H5Dopen2(loc_bogus, "Dataset1", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR
    if(H5Dclose(did) < 0)
        FAIL_STACK_ERROR

    PASSED();

    TESTING("object in r/o file with unknown header message & 'fail if unknown and open for write' flag set");

    /* Open the dataset with the unknown header message, and "fail if unknown and open for write" flag */
    if((did = H5Dopen2(loc_bogus, "Dataset2", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR
    if(H5Dclose(did) < 0)
        FAIL_STACK_ERROR

    PASSED();

    TESTING("object in r/o file with unknown header message & 'fail if unknown always' flag set");

    /* Attempt to open the dataset with the unknown header message, and "fail if unknown always" flag */
    H5E_BEGIN_TRY {
        did = H5Dopen2(loc_bogus, "Dataset3", H5P_DEFAULT);
    } H5E_END_TRY;
    if(did >= 0) {
        H5Dclose(did);
        TEST_ERROR
    } /* end if */

    PASSED();

    TESTING("object with unknown header message & 'mark if unknown' flag set");

    /* Copy object with "mark if unknown" flag on message into file (FILENAME[0]) that can be modified */
    if(H5Ocopy(loc_bogus, "Dataset4", loc, "Dataset4", H5P_DEFAULT, H5P_DEFAULT) < 0)
        FAIL_STACK_ERROR

    /* Closing: filename */
    if(bogus_id == H5O_BOGUS_INVALID_ID)
        if(H5Gclose(gid) < 0)
            FAIL_STACK_ERROR
    if(H5Fclose(fid) < 0)
       FAIL_STACK_ERROR

    /* Re-open filename, with read-only permissions */
    if((fid = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Set up location ID depending on bogus_id */
    if(bogus_id == H5O_BOGUS_INVALID_ID) {
        /* Open "group" in filename */
        if((gid = H5Gopen2(fid, "group", H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR
        loc = gid;
    } else
        loc = fid;

    /* Open the dataset with the "mark if unknown" message */
    if((did = H5Dopen2(loc, "Dataset4", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Check that the "unknown" message was _NOT_ marked */
    if(H5O_check_msg_marked_test(did, FALSE) < 0)
        FAIL_STACK_ERROR

    /* Close the dataset */
    if(H5Dclose(did) < 0)
        FAIL_STACK_ERROR

    /* Close "group" in filename depending on bogus_id */
    if(bogus_id == H5O_BOGUS_INVALID_ID)
        if(H5Gclose(gid) < 0)
            FAIL_STACK_ERROR

    /* Close filename (to flush change to object header) */
    if(H5Fclose(fid) < 0)
        FAIL_STACK_ERROR

    /* Re-open filename */
    if((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Set up location ID depending on bogus_id */
    if(bogus_id == H5O_BOGUS_INVALID_ID) {
        /* Open "group" in filename */
        if((gid = H5Gopen2(fid, "group", H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR
        loc = gid;
    } else
        loc = fid;

    /* Open the dataset with the "mark if unknown" message */
    if((did = H5Dopen2(loc, "Dataset4", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Create data space */
    if((sid = H5Screate(H5S_SCALAR)) < 0)
        FAIL_STACK_ERROR

    /* Create an attribute, to get the object header into write access */
    if((aid = H5Acreate2(did, "Attr", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Close dataspace */
    if(H5Sclose(sid) < 0)
        FAIL_STACK_ERROR

    /* Close attribute */
    if(H5Aclose(aid) < 0)
        FAIL_STACK_ERROR

    /* Close the dataset */
    if(H5Dclose(did) < 0)
        FAIL_STACK_ERROR

    /* Close "group" in filename depending on bogus_id */
    if(bogus_id == H5O_BOGUS_INVALID_ID)
        if(H5Gclose(gid) < 0)
            FAIL_STACK_ERROR

    /* Close filename (to flush change to object header) */
    if(H5Fclose(fid) < 0)
        FAIL_STACK_ERROR

    /* Re-open filename */
    if((fid = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Set up location ID depending on bogus_id */
    if(bogus_id == H5O_BOGUS_INVALID_ID) {
        /* Open "group" in filename */
        if((gid = H5Gopen2(fid, "group", H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR
        loc = gid;
    } else
        loc = fid;

    /* Re-open the dataset with the "mark if unknown" message */
    if((did = H5Dopen2(loc, "Dataset4", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Check that the "unknown" message was marked */
    if(H5O_check_msg_marked_test(did, TRUE) < 0)
        FAIL_STACK_ERROR

    /* Close the dataset */
    if(H5Dclose(did) < 0)
        FAIL_STACK_ERROR

    /* Closing: filename */
    if(bogus_id == H5O_BOGUS_INVALID_ID)
        if(H5Gclose(gid) < 0)
            FAIL_STACK_ERROR
    if(H5Fclose(fid) < 0)
        FAIL_STACK_ERROR

    PASSED();

    /* Closing: FILE_BOGUS */
    if(bogus_id == H5O_BOGUS_INVALID_ID)
        if(H5Gclose(gid_bogus) < 0)
            FAIL_STACK_ERROR
    if(H5Fclose(fid_bogus) < 0)
        FAIL_STACK_ERROR

    TESTING("object in r/w file with unknown header message & 'fail if unknown and open for write' flag set");

    /* Open FILE_BOGUS with RW intent this time */
    if((fid_bogus = H5Fopen(testfile, H5F_ACC_RDWR, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Set up location ID */
    if(bogus_id == H5O_BOGUS_INVALID_ID) {
        /* Open "group" in FILE_BOGUS */
        if((gid_bogus = H5Gopen2(fid_bogus, "group", H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR
        loc_bogus = gid_bogus;
    } else
        loc_bogus = fid_bogus;

    /* Attempt to open the dataset with the unknown header message, and "fail if unknown and open for write" flag */
    H5E_BEGIN_TRY {
        did = H5Dopen2(loc_bogus, "Dataset2", H5P_DEFAULT);
    } H5E_END_TRY;
    if(did >= 0) {
        H5Dclose(did);
        TEST_ERROR
    } /* end if */

    PASSED();

    TESTING("object in r/w file with unknown header message & 'fail if unknown always' flag set");

    /* Attempt to open the dataset with the unknown header message, and "fail if unknown always" flag */
    H5E_BEGIN_TRY {
        did = H5Dopen2(loc_bogus, "Dataset3", H5P_DEFAULT);
    } H5E_END_TRY;
    if(did >= 0) {
        H5Dclose(did);
        TEST_ERROR
    } /* end if */

    /* Closing: FILE_BOGUS */
    if(bogus_id == H5O_BOGUS_INVALID_ID)
        if(H5Gclose(gid_bogus) < 0)
            FAIL_STACK_ERROR
    if(H5Fclose(fid_bogus) < 0)
        FAIL_STACK_ERROR

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Fclose(fid);
        H5Gclose(gid);
        H5Fclose(fid_bogus);
        H5Gclose(gid_bogus);
        H5Dclose(did);
        H5Sclose(sid);
        H5Aclose(aid);
    } H5E_END_TRY;

    return FAIL;
} /* test_unknown() */

#define STR_EARLIEST "earliest"
#define STR_V18 "v18"
#define STR_LATEST "latest"
static char *
version_string(H5F_libver_t libver)
{
    char *str = NULL;

    str = (char *) HDmalloc(20);
    if (str == NULL) {
        HDfprintf(stderr, "Allocation failed\n");
        HDexit(1);
    }

    switch(libver) {
      case H5F_LIBVER_EARLIEST:
          HDstrcpy(str, STR_EARLIEST);
          break;

      case H5F_LIBVER_V18:
          HDstrcpy(str, STR_V18);
          break;

      case H5F_LIBVER_V110:
          HDassert(H5F_LIBVER_LATEST == H5F_LIBVER_V110);
          HDstrcpy(str, STR_LATEST);
          break;

      case H5F_LIBVER_ERROR:
      case H5F_LIBVER_NBOUNDS:
      default:
          HDsprintf(str, "%ld", (long)libver);
          break;
    } /* end switch */

    /* Return the formed version bound string */
    return str;
} /* end of version_string */


/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Exercise private object header behavior and routines
 *
 * Return:      Success: 0
 *              Failure: 1
 *
 * Programmer:    Robb Matzke
 *              Tuesday, November 24, 1998
 *
 * Modification:
 *              - Added loop of combinations of low/high library format bounds
 *                (BMR, Feb 2018)
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t fapl = -1;
    hid_t file = -1;
    H5F_t *f = NULL;
    char  filename[1024];
    H5O_hdr_info_t hdr_info;  /* Object info */
    H5O_loc_t      oh_loc;    /* Object header locations */
    H5F_libver_t low, high;   /* File format bounds */
    time_t time_new, ro;
    char msg[80];             /* Message for file format version */
    int    i;                 /* Local index variable */
    hbool_t     api_ctx_pushed = FALSE;             /* Whether API context pushed */
    herr_t ret;               /* Generic return value */

    /* Reset library */
    h5_reset();
    fapl = h5_fileaccess();
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Push API context */
    if(H5CX_push() < 0) FAIL_STACK_ERROR
    api_ctx_pushed = TRUE;

    /* Loop through all the combinations of low/high library format bounds */
    for(low = H5F_LIBVER_EARLIEST; low < H5F_LIBVER_NBOUNDS; low++) {
      for(high = H5F_LIBVER_EARLIEST; high < H5F_LIBVER_NBOUNDS; high++) {
        char *low_string = NULL;
        char *high_string = NULL;

        /* Set version bounds before opening the file */
        H5E_BEGIN_TRY {
            ret = H5Pset_libver_bounds(fapl, low, high);
        } H5E_END_TRY;

        if (ret < 0) /* Invalid low/high combinations */
            continue;

        /* Display info about testing */
        low_string = version_string(low);
        high_string = version_string(high);
        sprintf(msg, "Using file format version: (%s, %s)", low_string,
                high_string);
        HDputs(msg);
        HDfree(high_string);
        HDfree(low_string);

        /* test on object continuation block */
        if(test_cont(filename, fapl) < 0)
            TEST_ERROR

        /* Create the file to operate on */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR
        if(H5AC_ignore_tags(f) < 0) {
            H5_FAILED();
            H5Eprint2(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /*
         * Test object header creation
         * (using default group creation property list only because it's convenient)
         */
        TESTING("object header creation");
        HDmemset(&oh_loc, 0, sizeof(oh_loc));
        if(H5O_create(f, (size_t)64, (size_t)0, H5P_GROUP_CREATE_DEFAULT, &oh_loc/*out*/) < 0)
            FAIL_STACK_ERROR
        PASSED();

        /* create a new message */
        TESTING("message creation");
        time_new = 11111111;
        if(H5O_msg_create(&oh_loc, H5O_MTIME_NEW_ID, 0, 0, &time_new) < 0)
            FAIL_STACK_ERROR
        if(1 != H5O_link(&oh_loc, 1))
            FAIL_STACK_ERROR
        if(H5AC_flush(f) < 0)
            FAIL_STACK_ERROR
        if(H5AC_expunge_entry(f, H5AC_OHDR, oh_loc.addr, H5AC__NO_FLAGS_SET) < 0)
            FAIL_STACK_ERROR
        if(NULL == H5O_msg_read(&oh_loc, H5O_MTIME_NEW_ID, &ro))
            FAIL_STACK_ERROR
        if(ro != time_new)
            TEST_ERROR
        PASSED();

        /*
         * Test modification of an existing message.
         */
        TESTING("message modification");
        time_new = 33333333;
        if(H5O_msg_write(&oh_loc, H5O_MTIME_NEW_ID, 0, 0, &time_new) < 0)
            FAIL_STACK_ERROR
        if(H5AC_flush(f) < 0)
            FAIL_STACK_ERROR
        if(H5AC_expunge_entry(f, H5AC_OHDR, oh_loc.addr, H5AC__NO_FLAGS_SET) < 0)
            FAIL_STACK_ERROR
        if(NULL == H5O_msg_read(&oh_loc, H5O_MTIME_NEW_ID, &ro))
            FAIL_STACK_ERROR
        if(ro != time_new)
            TEST_ERROR

        /* Make certain that chunk #0 in the object header can be encoded with a 1-byte size */
        if(H5O_get_hdr_info(&oh_loc, &hdr_info) < 0)
            FAIL_STACK_ERROR
        if(hdr_info.space.total >=256)
            TEST_ERROR

        PASSED();

        /*
         * Test creation of a bunch of messages one after another to see
         * what happens when the object header overflows in core.
         * (Use 'old' MTIME message here, because it is large enough to be
         *  replaced with a continuation message (the new one is too small)
         *  and the library doesn't understand how to migrate more than one
         *  message from an object header currently - QAK - 10/8/03)
         */
        TESTING("object header overflow in memory");
        for(i = 0; i < 40; i++) {
            time_new = (i + 1) * 1000 + 1000000;
            if(H5O_msg_create(&oh_loc, H5O_MTIME_ID, 0, 0, &time_new) < 0)
                FAIL_STACK_ERROR
        } /* end for */
        if(H5AC_flush(f) < 0)
            FAIL_STACK_ERROR
        if(H5AC_expunge_entry(f, H5AC_OHDR, oh_loc.addr, H5AC__NO_FLAGS_SET) < 0)
            FAIL_STACK_ERROR

        /* Make certain that chunk #0 in the object header will be encoded with a 2-byte size */
        if(H5O_get_hdr_info(&oh_loc, &hdr_info) < 0)
            FAIL_STACK_ERROR
        if(hdr_info.space.total < 256)
            TEST_ERROR

        PASSED();

        /* Close & re-open file & object header */
        /* (makes certain that an object header in the new format that transitions
         *  between 1-byte chunk #0 size encoding and 2-byte chunk #0 size encoding
         *  works correctly - QAK)
         */
        TESTING("close & re-open object header");
        if(H5O_close(&oh_loc, NULL) < 0)
            FAIL_STACK_ERROR
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR
        if (H5AC_ignore_tags(f) < 0)
            FAIL_STACK_ERROR
        oh_loc.file = f;
        if(H5O_open(&oh_loc) < 0)
            FAIL_STACK_ERROR
        PASSED();

        /*
         * Test creation of a bunch of messages one after another to see
         * what happens when the object header overflows on disk.
         */
        TESTING("object header overflow on disk");
        for(i = 0; i < 10; i++) {
            time_new = (i + 1) * 1000 + 10;
            if(H5O_msg_create(&oh_loc, H5O_MTIME_NEW_ID, 0, 0, &time_new) < 0)
                FAIL_STACK_ERROR
            if(H5AC_flush(f) < 0)
                FAIL_STACK_ERROR
            if(H5AC_expunge_entry(f, H5AC_OHDR, oh_loc.addr, H5AC__NO_FLAGS_SET) < 0)
                FAIL_STACK_ERROR
        } /* end for */
        PASSED();

        /*
         * Delete all time messages.
         */
        TESTING("message deletion");
        if(H5O_msg_remove(&oh_loc, H5O_MTIME_NEW_ID, H5O_ALL, TRUE) < 0)
            FAIL_STACK_ERROR
        if(H5O_msg_remove(&oh_loc, H5O_MTIME_ID, H5O_ALL, TRUE) < 0)
            FAIL_STACK_ERROR
        if(H5O_msg_read(&oh_loc, H5O_MTIME_NEW_ID, &ro))
            FAIL_STACK_ERROR
        if(H5O_msg_read(&oh_loc, H5O_MTIME_ID, &ro))
            FAIL_STACK_ERROR
        PASSED();


        /*
         * Constant message handling.
         * (can't write to them, but should be able to remove them)
         */
        TESTING("constant message handling");
        time_new = 22222222;
        if(H5O_msg_create(&oh_loc, H5O_MTIME_NEW_ID, H5O_MSG_FLAG_CONSTANT, 0, &time_new) < 0)
            FAIL_STACK_ERROR
        if(H5AC_flush(f) < 0)
            FAIL_STACK_ERROR
        if(H5AC_expunge_entry(f, H5AC_OHDR, oh_loc.addr, H5AC__NO_FLAGS_SET) < 0)
            FAIL_STACK_ERROR
        if(NULL == H5O_msg_read(&oh_loc, H5O_MTIME_NEW_ID, &ro))
            FAIL_STACK_ERROR
        if(ro != time_new)
            TEST_ERROR
        time_new = 33333333;
        H5E_BEGIN_TRY {
            ret = H5O_msg_write(&oh_loc, H5O_MTIME_NEW_ID, 0, 0, &time_new);
        } H5E_END_TRY;
        if(ret >= 0)
            TEST_ERROR
        if(H5O_msg_remove(&oh_loc, H5O_MTIME_NEW_ID, H5O_ALL, TRUE) < 0)
            FAIL_STACK_ERROR
        PASSED();


        /* release resources */
        TESTING("object header closing");
        if(H5O_close(&oh_loc, NULL) < 0)
            FAIL_STACK_ERROR
        PASSED();

        /* Close the file we created */
        if(H5Fclose(file) < 0)
            TEST_ERROR

        /* Test reading datasets with undefined object header messages
         * and the various "fail/mark if unknown" object header message flags
         */
        HDputs("Accessing objects with unknown header messages: H5O_BOGUS_VALID_ID");
        if(test_unknown(H5O_BOGUS_VALID_ID, filename, fapl) < 0)
            TEST_ERROR
        HDputs("Accessing objects with unknown header messages: H5O_BOGUS_INVALID_ID");
        if(test_unknown(H5O_BOGUS_INVALID_ID, filename, fapl) < 0)
            TEST_ERROR

        /* Test object header creation metadata cache issues */
        if(test_ohdr_cache(filename, fapl) < 0)
            TEST_ERROR

      } /* high */
    } /* low */

    /* Verify symbol table messages are cached */
    if(h5_verify_cached_stabs(FILENAME, fapl) < 0) TEST_ERROR

    /* A test to exercise the re-read of the object header for SWMR access */
    if(test_ohdr_swmr(TRUE) < 0) TEST_ERROR
    if(test_ohdr_swmr(FALSE) < 0) TEST_ERROR

    /* Pop API context */
    if(api_ctx_pushed && H5CX_pop() < 0) FAIL_STACK_ERROR
    api_ctx_pushed = FALSE;

    HDputs("All object header tests passed.");
    h5_cleanup(FILENAME, fapl);
    return 0;

error:
    puts("*** TESTS FAILED ***");
    H5E_BEGIN_TRY {
        H5Fclose(file);
    } H5E_END_TRY;

    if(api_ctx_pushed) H5CX_pop();

    return 1;
} /* end main() */

