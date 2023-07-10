
paraview_add_plugin(SerafinReader
  VERSION "1.0"
  MODULES SerafinReaderModule
  MODULE_FILES "${CMAKE_CURRENT_SOURCE_DIR}/SerafinReaderModule/vtk.module"
  SERVER_MANAGER_XML sources.xml
)
