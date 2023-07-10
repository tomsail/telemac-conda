INCLUDE_DIRECTORIES(
  ${HDF5_INCLUDE_DIRS}
  ${MPI_INCLUDE_DIRS}
  ${PROJECT_BINARY_DIR}/include
  ${PROJECT_SOURCE_DIR}/include
)

SET(testc_SOURCES test1.c test2.c test3.c test4.c test5.c 
    test6.c test7.c test8.c test9.c test10.c test10_mem.c
    test10_f32_i32.c test10_f32_i64.c test10_f32.c test10_i32.c test10_i64.c
    test11.c test11b.c 
    test12.c test13.c test14.c test15.c test16.c test17.c test18.c 
    test20.c test21.c test22.c test23.c test24.c test25.c test26.c test27.c 
    test28.c test29.c test30.c test31.c test32.c test33.c)

SET(testc_NOT_AUTO_LAUNCH test13 test15 test30 test31 test32 test33)

# Test20 needs files from dumps.ref:
FILE(COPY "dumps.ref/test2-med.hdf" DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
FILE(COPY "dumps.ref/test10-med.hdf" DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

FOREACH(testc_SOURCE ${testc_SOURCES})
  GET_FILENAME_COMPONENT(testc_BASE ${testc_SOURCE} NAME_WE)
  LIST(FIND testc_NOT_AUTO_LAUNCH ${testc_BASE} IS_STDALONE_TEST)
  # Shared lib tests
  IF(MEDFILE_BUILD_SHARED_LIBS)
    SET(testc_SHARED_BIN "${testc_BASE}c")
    ADD_EXECUTABLE(${testc_SHARED_BIN} ${testc_SOURCE})
    TARGET_LINK_LIBRARIES(${testc_SHARED_BIN} medC)
    SET_TARGET_PROPERTIES(${testc_SHARED_BIN} PROPERTIES OUTPUT_NAME ${testc_BASE})
    GET_TARGET_PROPERTY(TMP ${testc_SHARED_BIN} OUTPUT_NAME)
    INSTALL(TARGETS ${testc_SHARED_BIN} DESTINATION bin/testc)
    IF(${IS_STDALONE_TEST} EQUAL -1)
      ADD_TEST("Test${testc_SHARED_BIN}" ${TMP})
    ENDIF(${IS_STDALONE_TEST} EQUAL -1)
  ENDIF()
  
  IF(MEDFILE_BUILD_STATIC_LIBS)
    # Static lib tests
    SET(testc_STATIC_BIN "${testc_BASE}c_static")
    ADD_EXECUTABLE(${testc_STATIC_BIN} ${testc_SOURCE})    
    TARGET_LINK_LIBRARIES(${testc_STATIC_BIN} medC_static)
    MED_SET_DEFINITIONS(${testc_STATIC_BIN} MED3_USESTATIC)
    SET_TARGET_PROPERTIES(${testc_STATIC_BIN} PROPERTIES OUTPUT_NAME "${testc_BASE}_static")
    GET_TARGET_PROPERTY(TMP ${testc_STATIC_BIN} OUTPUT_NAME)
    INSTALL(TARGETS ${testc_STATIC_BIN} DESTINATION bin/testc)
  ENDIF()
  
  # Other
  IF(${IS_STDALONE_TEST} EQUAL -1)
    ADD_TEST("Test${testc_STATIC_BIN}" ${TMP})
  ENDIF(${IS_STDALONE_TEST} EQUAL -1)
ENDFOREACH(testc_SOURCE ${testsc_BINARIES})
