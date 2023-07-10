set(classes
  vtkSerafinReader
)

vtk_module_add_module(SerafinReaderModule
  FORCE_STATIC
  CLASSES ${classes}
)
