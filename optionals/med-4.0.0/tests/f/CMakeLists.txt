INCLUDE_DIRECTORIES(
  ${PROJECT_BINARY_DIR}/include
  ${PROJECT_SOURCE_DIR}/include
)

ADD_DEFINITIONS(-DTEST10_FILENAME=\"test10f.med\")

SET(testf_SOURCES 
    test10f.f
#    test11b.f90   
#    test11.f90   
    test12.f
    test13.f90
    test14.f
    test15.f90
    test16.f
    test17.f90
    test18.f
    test1.f
#    test20.f     ##  more complex logic for this one -- see autotools procedure.  
    test21.f
    test22.f
    test23.f
    test24.f
    test25.f
    test26.f
    test27.f
    test28.f
    test29.f
    test2.f
    test30.f90
    test31.f
    test32.f
    test33.f
    test3.f
    test4.f
    test5.f90
    test6.f
    test7.f90
    test8.f
    test9.f90
)  

SET(testf_10
    test10.F90
    test10_f32.F90
    test10_f32_i32.F90
    test10_f32_i64.F90
    test10_i32.F90
    test10_i64.F90
)

LIST(APPEND testf_SOURCES ${testf_10})

# test10* need specific definitions
FOREACH(testf_SRC ${testf_10})
    GET_FILENAME_COMPONENT(_shortName ${testf_SRC} NAME_WE) 
    SET_SOURCE_FILES_PROPERTIES(${testf_SRC} COMPILE_DEFINTIONS TEST10_FILENAME=\"${_shortName}.med\")
ENDFOREACH()

# Some tests need files from dumps.ref:
#FILE(COPY "../c/dumps.ref/test10.med" DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
#FILE(COPY "dumps.ref/test11f.med" DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
#FILE(COPY "dumps.ref/test11f.med" DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

FOREACH(testf_SOURCE ${testf_SOURCES})
  GET_FILENAME_COMPONENT(testf_BASE ${testf_SOURCE} NAME_WE)
  # Shared lib tests
  IF(MEDFILE_BUILD_SHARED_LIBS)
    SET(testf_SHARED_BIN "${testf_BASE}f")
    ADD_EXECUTABLE(${testf_SHARED_BIN} ${testf_SOURCE})
    TARGET_LINK_LIBRARIES(${testf_SHARED_BIN} med)
    SET_TARGET_PROPERTIES(${testf_SHARED_BIN} PROPERTIES OUTPUT_NAME ${testf_BASE})
    GET_TARGET_PROPERTY(TMP ${testf_SHARED_BIN} OUTPUT_NAME)
    INSTALL(TARGETS ${testf_SHARED_BIN} DESTINATION bin/testf)
    ADD_TEST("Test${testf_SHARED_BIN}" ${TMP})
  ENDIF()
  
  # Static lib tests
  IF(MEDFILE_BUILD_STATIC_LIBS)
    SET(testf_STATIC_BIN "${testf_BASE}f_static")
    ADD_EXECUTABLE(${testf_STATIC_BIN} ${testf_SOURCE})
    TARGET_LINK_LIBRARIES(${testf_STATIC_BIN} med_static)
    SET_TARGET_PROPERTIES(${testf_STATIC_BIN} PROPERTIES OUTPUT_NAME "${testf_BASE}_static")
    GET_TARGET_PROPERTY(TMP ${testf_STATIC_BIN} OUTPUT_NAME)
    INSTALL(TARGETS ${testf_STATIC_BIN} DESTINATION bin/testf)
    ADD_TEST("Test${testf_STATIC_BIN}" ${TMP})
  ENDIF()
    
ENDFOREACH(testf_SOURCE ${testsf_BINARIES})
