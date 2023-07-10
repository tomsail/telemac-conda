SET(fi236_SOURCES
  entmaaf.f
  jntf.f
  gauss.f
  exit.f
  resf_i.f
  interf.f
  fichf.f
  maaf.f
  prof.f
  resf_r.f
  famf.f
  equf.f
  scalaire.f
  )

ADD_LIBRARY(_fi236 OBJECT ${fi236_SOURCES})
