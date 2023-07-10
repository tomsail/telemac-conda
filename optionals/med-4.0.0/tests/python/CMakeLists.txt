SET(_tst_scripts
  # Il faudrait gérer les différents test10_ avec
  # les fichiers paramètres.
  # test10.py
  # test11.py
  test12.py
  test13.py
  test1.py
  test21.py
  test22.py
  test23.py
  test24.py
  test27.py
  test28.py
  test29.py
  test30.py
  test2.py
  test3.py
  test4.py
  test5.py
  test6.py
  test7.py
  test8.py
  test9.py
  test_medfile.py
  test_medmesh.py
)

SET(_to_install
  ${_tst_scripts}
  UseCase_MEDinterp_1.py
  UseCase_MEDinterp_2.py
  UseCase_MEDinterp_3.py
)

FOREACH(_s IN LISTS _tst_scripts)
  STRING(REPLACE ".py" "_Python" _tst_name ${_s})
  FILE(COPY ${_s} DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
  ADD_TEST(NAME ${_tst_name}
           COMMAND "/usr/bin/env" "PYTHONPATH=${PROJECT_BINARY_DIR}/python" "${PYTHON_EXECUTABLE}" "${CMAKE_CURRENT_BINARY_DIR}/${_s}")
ENDFOREACH()

INSTALL(FILES ${_to_install} DESTINATION bin/testpy)

