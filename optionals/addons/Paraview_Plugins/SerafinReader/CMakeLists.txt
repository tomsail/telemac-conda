cmake_minimum_required(VERSION 3.8)
project(SerafinReader)
find_package(ParaView REQUIRED)

include(GNUInstallDirs)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_INSTALL_BINDIR}")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_INSTALL_LIBDIR}")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_INSTALL_LIBDIR}")

paraview_plugin_scan(
  ENABLE_BY_DEFAULT YES
  PLUGIN_FILES      "${CMAKE_CURRENT_SOURCE_DIR}/plugin/paraview.plugin"
  PROVIDES_PLUGINS  plugins
  REQUIRES_MODULES  required_modules)

foreach(module IN LISTS required_modules)
  if(NOT TARGET "${module}")
    message("Missing required module: ${module}")
    return()
  endif()
endforeach()

set(BUILD_SHARED_LIBS ON)
paraview_plugin_build(
  RUNTIME_DESTINATION "${CMAKE_INSTALL_BINDIR}"
  LIBRARY_DESTINATION "${CMAKE_INSTALL_LIBDIR}"
  PLUGINS ${plugins}
  AUTOLOAD ${plugins})
