if(MEDFILE_BUILD_DOC)
#-- Configure the Template Doxyfile for our specific project

  FILE(READ ${PROJECT_SOURCE_DIR}/doc/dox/MEDfile.als            DOXYMEDFILE               )
  FILE(READ ${PROJECT_SOURCE_DIR}/doc/dox/MEDmesh.als            DOXYMEDMESH               )
  FILE(READ ${PROJECT_SOURCE_DIR}/doc/dox/MEDsupportMesh.als     DOXYMEDSUPPORTMESH        )
  FILE(READ ${PROJECT_SOURCE_DIR}/doc/dox/MEDfilter.als          DOXYMEDFILTER             )
  FILE(READ ${PROJECT_SOURCE_DIR}/doc/dox/MEDlibrary.als         DOXYMEDLIBRARY            )
  FILE(READ ${PROJECT_SOURCE_DIR}/doc/dox/MEDfield.als           DOXYMEDFAMILY             )
  FILE(READ ${PROJECT_SOURCE_DIR}/doc/dox/MEDfamily.als          DOXYMEDEQUIVALENCE        )
  FILE(READ ${PROJECT_SOURCE_DIR}/doc/dox/MEDequivalence.als     DOXYMEDFIELD              )
  FILE(READ ${PROJECT_SOURCE_DIR}/doc/dox/MEDlink.als            DOXYMEDLINK               )
  FILE(READ ${PROJECT_SOURCE_DIR}/doc/dox/MEDjoint.als           DOXYMEDJOINT              )
  FILE(READ ${PROJECT_SOURCE_DIR}/doc/dox/MEDlocalization.als    DOXYMEDLOCALIZATION       )
  FILE(READ ${PROJECT_SOURCE_DIR}/doc/dox/MEDinterp.als          DOXYMEDSTRUCTELEMENT      )
  FILE(READ ${PROJECT_SOURCE_DIR}/doc/dox/MEDstructElement.als   DOXYMEDINTERP             )
  FILE(READ ${PROJECT_SOURCE_DIR}/doc/dox/MEDparameter.als       DOXYMEDPARAMETER          )
  FILE(READ ${PROJECT_SOURCE_DIR}/doc/dox/MEDprofile.als         DOXYMEDPROFILE            )

  CONFIGURE_FILE(aliases.als.cmake.in
                  ${PROJECT_BINARY_DIR}/doc/dox/aliases.als  @ONLY IMMEDIATE)

  FILE(READ ${PROJECT_BINARY_DIR}/doc/dox/aliases.als   DOXYALIASES )
#Be careful : The cmake doc generation doesn't work yet !
  CONFIGURE_FILE(Doxyfile.cfg.cmake.in
    ${PROJECT_BINARY_DIR}/doc/dox/Doxyfile.cfg  @ONLY IMMEDIATE)

endif(MEDFILE_BUILD_DOC)
