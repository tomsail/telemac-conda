ADD_SUBDIRECTORY(2.3.6)

CONFIGURE_FILE(med.h.in ${CMAKE_CURRENT_BINARY_DIR}/med.h @ONLY)
CONFIGURE_FILE(med_utils.h.in ${CMAKE_CURRENT_BINARY_DIR}/med_utils.h @ONLY)
CONFIGURE_FILE(med_parameter.hf.in ${CMAKE_CURRENT_BINARY_DIR}/med_parameter.hf @ONLY)

CONFIGURE_FILE(H5public_extract.h.in ${CMAKE_CURRENT_BINARY_DIR}/H5public_extract.h @ONLY)

SET(medC_HEADERS_H
  ${CMAKE_CURRENT_BINARY_DIR}/med.h
  ${CMAKE_CURRENT_BINARY_DIR}/med_utils.h
  medlibrary.h
  medfile.h
  medmesh.h
  medfamily.h
  medfield.h
  medprofile.h
  medlocalization.h
  medparameter.h
  medequivalence.h
  medinterp.h
  medfilter.h
  medsubdomain.h
  medstructelement.h
  medlink.h
  med_err.h
  med_exit_if.h
  med_proto.h
  medC_win_dll.h
  MEDerreur.hxx
)

SET(medF_HEADERS_H
  med.hf    
  med.hf77
  med.hf90
  ${CMAKE_CURRENT_BINARY_DIR}/med_parameter.hf
)

INSTALL(FILES ${medC_HEADERS_H} DESTINATION ${INSTALL_INCLUDE_DIR})
INSTALL(FILES ${medF_HEADERS_H} DESTINATION ${INSTALL_INCLUDE_DIR})
