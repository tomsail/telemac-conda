!== Copyright (C) 2000-2022 EDF-CEREMA ==
!
!   This file is part of MASCARET.
!
!   MASCARET is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   MASCARET is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with MASCARET.  If not, see <http://www.gnu.org/licenses/>
!

module M_MESSAGE_C
!***********************************************************************
! PROGICIEL : MASCARET       N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
! === CE MODULE CONTIENT LES MESSAGES D'ERREUR LONGS ET COURTS ===

!=========================== Instructions ==============================

!-------------------------------------------------------------------------
character (*), parameter ::                                             &
banniere = '("=========================================================================")'

!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_1   = '("Computation error.",/,                                         &
          & "Cross Section # ",i5," is dry.")'
character (*), parameter ::                                                &
err_1c  = '("Cross Section # ",i5," is dry.")'
!-------------------------------------------------------------------------
character (*), parameter ::                                             &
err_3  = '("Error opening file : ",A,/,                   &
         & "for reading.",/,                                            &
         & "File not found.",/,      &
         & "Check the path to the file.")'
character (*), parameter ::                                             &
err_3c = '("Error opening file ",A," for reading")'
!-------------------------------------------------------------------------
character (*), parameter ::                                             &
err_4  = '("Error opening file : ",A,/,                   &
         & "for writing.",/,                                            &
         & "Check the path to the file.")'
character (*), parameter ::                                             &
err_4c = '("Error opening file ",A," for writing")'
!-------------------------------------------------------------------------
character (*), parameter ::                                             &
err_5  = '("Error allocating memory for : ",A,".",/,                  &
         & "Check the available memory.")'
character (*), parameter ::                                             &
err_5c = '("Error allocating memory for : ",A)'
!-------------------------------------------------------------------------
character (*), parameter ::                                             &
err_6  = '("Error deallocating memory for :  ",A,".")'
character (*), parameter ::                                             &
err_6c = '("Error deallocating memory for : ",A)'
!-------------------------------------------------------------------------
character (*), parameter ::                                             &
err_11 = '("Error reading file : ",A,",",/,   &
         & "for the value #",i5)'
character (*), parameter ::                                             &
err_11c= '("Error reading file : ",A," for the value # ",i5)'
!-------------------------------------------------------------------------
character (*), parameter ::                                             &
err_17  = '("Interpolation error.",/,                 &
          & "Requested interpolation # ",i3," is greater than", &
          & " the size of tables : ",i3,/,                        &
          & "Increase the number of points for the corresponding graph ")'
character (*), parameter ::                                             &
err_17c = '("Requested interpolation # ",i3," > size of tables : ",    &
          & " = ",i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                              &
err_20  = '("Interpolation error.",/,                  &
          & "Point abscissa is out of bounds : ",/,          &
          & "X = ",g15.7," , Xmin = ",g15.7," , Xmax = ",g15.7,/,        &
          & "The range for the function is probably too small.")'
character (*), parameter ::                                              &
err_20c = '("Point abscissa is out of bounds (interpolation) : X = ",  &
          & g15.7," Xmin = ",g15.7," Xmax = ",g15.7)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_21  = '("Rating curve : ",A,",",/,            &
          & "No possible interpolation.",     &
          & "The number of points is not greater than one.")'
character (*), parameter ::                                                &
err_21c = '("Not enough points for the hydraulic graph : ",A)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_22  = '("Hydraulic graph ",A,",",/,                                    &
          & "Detection of a cusp at point # ",i6,/,&
          & "which is forbidden.",/,                                      &
          & "Please modify the graph.")'
character (*), parameter ::                                                &
err_22c = '("Hydraulic graph : ",A," : Cusp at point # ",i6)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_25  = '("Point abscissa X = ",f12.3," is not in the domain.")'
character (*), parameter ::                                                &
err_25c = '("Abscissa X= ",f12.3," out of bounds")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                 &
err_28  = '("Error while meshing.",/,                              &
          & "The list of nodes cannot be defined  : ",/,&
          & " X0 = ",g12.3," Xn = ",g12.3," Number of sections = ",   &
          & i4)'
character (*), parameter ::                                                &
err_28c = '("Error while meshing X0=",g12.3," Xn=",g12.3," #Sections=",i4)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_29  = '("Error while meshing.",/,                             &
          & "The list of nodes cannot be defined  : ",/,&
          & "First cross-section = ",i5,", Last cross-section = ",i5,",    &
          & Step = ",g12.3)'
character (*), parameter ::                                                &
err_29c = '("Error while meshing First cross-section =",i5,&
          & " Last cross-section =",i5," Step =", g12.3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_30  = '("Error on a meshing parameter.",/,                          &
          & "The computation method ",i5, " is not implemented.")'
character (*), parameter ::                                                &
err_30c = '("Computation method (= ",i5,") for meshing is not valid.")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_31  = '("Computation error.",/,                                         &
          & "For the time : ",g12.2,/,                                    &
          & "the water depth is not positive",/,                          &
          & "at the cross-section # ",i5,", Reach # ",i3, &
          & " Relative abscissa", " X = ",g12.3," : ",/,                 &
          & "Z = ",g12.3,", Zbottom = ",g12.3)'
character (*), parameter ::                                                &
err_31c = '("At time ",g12.2," , water depth <= 0 at the cross-section # ",i5,   &
          & " Reach # ",i3," Xrelative= ",g12.3," Z=",g12.3," Zbottom=",g12.3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_32  = '("Computation error.",/,                                         &
          & "Non-convergence of the algorithm for computing discharges",/, &
          & " at the junction # ",i5)'
character (*), parameter ::                                                &
err_32c = '("Non-convergence of the algorithm for computing discharges",   &
          & " at the junction # ",i5)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_33  = '("Computation error.",/,                                         &
          & "Element # ",i4," of the table IAPPEL ",/,&
          & "is unknown.",/,                &
          & "Please contact the developer team (LIDO).")'
character (*), parameter ::                                                &
err_33c = '("Element # ",i4," of the table IAPPEL is unknown")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_34  = '("Computation error.",/,                                         &
          & "The local variable NumPassage (= ",i2,") is not greater than "    &
          & " 1 and lesser than 2.",/,                                   &
          & "Please contact the developer team (LIDO).")'
character (*), parameter ::                                                &
err_34c = '("The value of the local variable NumPassage (= ",i2,") is not valid")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_35  = '("Computation error.",/,                                         &
          & "For the time : ",g12.2,",",/,                                &
          & "The level (m) for the critical point is lesser than the bottom level " &
          & "at the cross-section # ",i5,",",/                                                &
          & "Reach # ",i3,", Relative abscissa X = ",g12.3," :",/,&
          & "Zcritical = ",g12.3,", Zbottom = ",g12.3)'
character (*), parameter ::                                                &
err_35c = '("At time ",g12.2," Zcritical <= Zbottom at the cross-section ",i5,       &
          & " Reach ",i3," Xrelative ",g12.3," Zcritical=",g12.3," Zbottom=",g12.3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_36  = '("Computation error.",/,                                         &
          & "At time T = ",g12.2,/,                                  &
          & "Super critical flow : Froude Number = ",g10.2,/,                &
          & "for a water level Z = ",g12.3," at the downstream node of reach # ",i3,".",/,  &
          & "The used algorithm cannot deal with ",/,  &
          & "a boundary condition compatible with this flow velocity")'
character (*), parameter ::                                                &
err_36c = '("At time = ",g12.2," Froude = ",g10.2," for Z = ",g12.3,         &
          & " at the downstream node of reach # ",i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_37  = '("Computation error.",/,                                         &
          & "At time T = ",g12.2,/,                                  &
          & "Water depth is not positive at the downstream node of reach # ",i3," :",/,&
          & "Z = ",g12.3,", Zbottom = ",g12.3)'
character (*), parameter ::                                                &
err_37c = '("At time ",g12.2," water depth <= 0 at the downstream node of reach ",   &
          & i3," Z = ",g12.3," Zbottom = ",g12.3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_38  = '("Computation error.",/,                                         &
          & "At time ",   &
          & g12.2,/,                                                       &
          & "Non-convergent algorithm for the reach # ",i3,",",/,      &
          & "at the cross-section # ",i5)'
character (*), parameter ::                                                &
err_38c = '("Non-convergent algorithm  at time ",g12.2," Reach ",   &
          & i3," Cross-section ",i5)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_39  = '("Computation error.",/,                                         &
          & "Froude Number = ",f10.2," too high,",/,                  &
          & "at the cross-section # ",i5,", reach # ",i3,", relative abscissa",   &
          & " X = ",f12.2,/,                                               &
          & "The steady kernel only deals with Froude number",&
          & " not exceeding the value",    &
          & " : Fr = 5")'
character (*), parameter ::                                                &
err_39c = '("Froude = ",f10.2," > 1 Cross-section ",i5," Reach ",i3,              &
          & " Xrelative = ",f12.2)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_40  = '("Computation error.",/,                                         &
          & "Main channel wetted perimeter is not strictly positive",/,     &
          & "at the cross-section : ",A,/, &
          & "with value P1 = ",g12.2)'
character (*), parameter ::                                                &
err_40c = '("Wetted perimeter <=0 at ",A," : P1 = ",g12.2)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_41  = '("Computation error.",/,                                         &
          & "Main channel wetted area is not strictly positive",/,             &
          & "at the cross-section : S1 = ",g12.2)'
character (*), parameter ::                                                &
err_41c = '("Wetted area <=0 : S1 = ",g12.2)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_42  = '("Computation error.",/,                                         &
          & "Singularity # ",i3,", (type 1),",/,                    &
          & "for the current hydraulic state at time = ",g12.3,/,    &
          & "the upstream discharge is not a value of the graph",/,&
          & "Qupstream = ",g12.3,", Qmin = ",g12.3,", Qmax = ",g12.3)'
character (*), parameter ::                                                &
err_42c = '("Singularity # ",i3," (type 1), at time ",g12.3,            &
          & ", Qupstream = ",g12.3," Qmin = ",g12.3," Qmax = ",g12.3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_43  = '("Computation error.",/,                                         &
          & "Singularity # ",i3,", (type 2 )",/,                   &
          & "for the current hydraulic state at time = ",g12.3,/,    &
          & "the upstream level is lesser than the crest level :",/,     &
          & "Zupstream = ",g12.3," Zcrest = ",g12.3)'
character (*), parameter ::                                                &
err_43c = '("Singularity # ",i3," (type 2), at time ",g12.3,            &
          & ", Zupstream = ", g12.3," Zcrest = ",g12.3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_44  = '("Computation error.",/,                                         &
          & "Singularity # ",i3,", (type :",i2,"),",/,              &
          & "for the current hydraulic state at time = ",g12.3,",",/,&
          & "The algorithm fails to determine the upstream level.")'
character (*), parameter ::                                                &
err_44c = '("Singularity # ",i3," (type 2), at time ",g12.3," : ",      &
          & "The algorithm fails to determine the upstream level.")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_46  = '("Computation error.",/,                                         &
          & "Singularity # ",i3," (type 3) :",/,                    &
          & "for the current hydraulic state at time = ",g12.3,/,    &
          & "singularity type = ",i3, "unknown")'
character (*), parameter ::                                                &
err_46c = '("Singularity # ",i3," (type 3), at time ",g12.3,            &
          & " : singularity type = ",i3," unknown")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_47  = '("Computation error.",/,                                         &
          & "At the junction # ",i3,", the number of downstream reaches = ",i3,    &
          & " is greater than 2,",/,"with one without flow.",/,      &
          & "The steady kernel cannot be used.")'
character (*), parameter ::                                                &
err_47c = '("At the junction # ",i3,", ",i3," one downstream reach without a flow",  &
          & " is detected")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                 &
err_48  = '("Computation error.",/,                                          &
          & "At the junction # ",i3,", the algorithm for computing discharges ",/, &
          & "is not convergent.")'
character (*), parameter ::                                                 &
err_48c = '("At the junction # ",i3,", the algorithm for computing discharges ",&
          & " is not convergent.")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_49  = '("Computation error.",/,                                         &
          & "At the junction # ",i3,", the local variable NumPassage",   &
          & " is not equal to 1 neither 2.",/,                              &
          & "Please contact the developer team (LIDO).")'
character (*), parameter ::                                                &
err_49c = '("At the junction # ",i3,", the local variable NumPassage",           &
          & " is not equal to 1 neither 2.")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_50  = '("Error reading geometry file : ", A,".",/,          &
          & "problem with the header.")'
character (*), parameter ::                                                &
err_50c = '("Error reading geometry file : ",A)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_51  = '("Error reading geometry file : ",A,",",/,           &
          & "for the header of cross-section # ",i3".")'
character (*), parameter ::                                                &
err_51c = '("Error reading geometry file : ",A,",",        &
          & " for the header of cross-section # ",i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_52  = '("Error reading geometry file.",/,                   &
          & "problem with the tables ",A," of the cross-section # ",i3,".")'
character (*), parameter ::                                                &
err_52c = '("Error reading geometry file,",                &
          & " problem with the tables ",A," of the cross-section # ",i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_53  = '("Error reading geometry file.",/,                   &
          & "The Strickler coefficient",/,    &
          & "of the cross-section # ",i3," is not positive,",/,                &
          & "which is not allowed.")'
character (*), parameter ::                                                &
err_53c = '("Strickler coefficient <= 0 at the cross-section # ",i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_54  = '("Error reading geometry file.",/,                   &
          & "At the cross-section # ",i3,/,                                          &
          & "Levels of the left and right sides are different.")'
character (*), parameter ::                                                &
err_54c = '("Geometry file, Cross-section # ",i3,                         &
          & ", Levels of the left and right sides are different.")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_55  = '("Error reading geometry file.",/,                   &
          & "At the cross-section # ",i3,", the limit of the left bank ",A,/,        &
          & " is equal to or greater than the limit of the right bank.")'
character (*), parameter ::                                                &
err_55c = '("Geometry file, at the cross-section # ",i3,                         &
          & ", the limit of the left bank ",A," is greater than the limit right bank")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_56  = '("Error reading geometry file.",/,                   &
          & "At the cross-section # ",i3,", the limits of the main channel",/,            &
          & "are not within the limits of the floodplain area.")'
character (*), parameter ::                                                &
err_56c = '("Geometry file, at the cross-section # ",i3,                         &
          & ", limits of the main channel are outside the limits of the floodplain")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_57  = '("Error reading geometry file.",/,                   &
          & "At the cross-section # ",i3,", one of the limits  ",A,/,   &
          & " is not a point of the geometry")'
character (*), parameter ::                                                &
err_57c = '("Geometry file, at the cross-section ",i3,                 &
          & ", one of the limits ",A," is not a point of the geometry")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_58  = '("Error reading geometry file.",/,                   &
          & "At the cross-section # ",i3," the limit ",A," is outside",/,          &
          & "the geometry.")'
character (*), parameter ::                                                &
err_58c = '("Geometry file, at the cross-section # ",i3,                 &
          & ", the limit ",A," is outside the geometry")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                 &
err_59  = '("Error reading geometry file.",/,                    &
          & "The definition of the cross-sections is only possible by using points.")'
character (*), parameter ::                                                 &
err_59c = '("The definition of the cross-sections is only possible by using points.")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_60  = '("Error reading geometry file.",/,                   &
          & "problem with the reach # ",i3)'
character (*), parameter ::                                                &
err_60c = '("Error reading geometry file : ",     &
          & " problem with the reach # ",i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_61  = '("Error reading geometry file.",/,                   &
          & "the number of the first cross-section (reach # ",i3,") is lesser than",&
          & " the number of the first cross-section (reach # ",i3,").")'
character (*), parameter ::                                                &
err_61c = '("Geometry file, number of the first cross-section (reach # "       &
          & ,i3,") <= number of the first cross-section (reach # ",i3,").")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_62  = '("Error reading geometry file.",/,                   &
          & "For the reach # ",i3," the number of the last cross-section = ",i3,/,&
          & "is greater than the total number of cross-sections = ",i3,)'
character (*), parameter ::                                                &
err_62c = '("Geometry file, for the reach # ",i3,   &
          & " last cross-section : ",i3," > total number of cross-sections : ",i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_63  = '("Error reading geometry file.",/,                   &
          & "The abscissa of the cross-section # ",i3," : X = ",g12.3," is lesser than",/,&
          & "the abscissa of the cross-section # ",i3," : X = ",g12.3)'
character (*), parameter ::                                                &
err_63c = '("Geometry file, abscissa of the cross-section ",i3,        &
          & " X = ",g12.3," < abscissa of the cross-section ",i3," : X = ",g12.3)'
!-------------------------------------------------------------------------
! Lecture de la ligne d'eau initiale
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_64  = '("Error reading initial conditions",/,  &
          & "(OPTHYCA format).",/,                                       &
          & "problem with the first line of the file")'
character (*), parameter ::                                                &
err_64c = '("Error reading file on the initial conditions",           &
          & " (problem with the first line)")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_65  = '("Error reading initial conditions",/,  &
          & "(OPTHYCA format).",/,                                       &
          & "The keyword : ",A," is not found on the first line of the file.",/,&
          & "Check the format compliance.")'
character (*), parameter ::                                                &
err_65c = '("Keyword : ",A," not found on the first line of the file",    &
          & " for the initial conditions")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_66  = '("Error reading initial conditions",    &
          & "(OPTHYCA format).",/,                                      &
          & "problem with the header describing variable # ",i3)'
character (*), parameter ::                                                &
err_66c = '("Error reading initial conditions : problem with the header describing variable # ",i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_67  = '("Error reading initial conditions",    &
          & "(OPTHYCA format).",/,                                      &
          & "No variable found in the header file",/,        &
          & "Check the format compliance.")'
character (*), parameter ::                                                &
err_67c = '("No variable found in the header file of the initial conditions")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_68  = '("Error reading initial conditions",    &
          & "(OPTHYCA format).",/,                                      &
          & "Variable : ",A," is not in the header file",/,   &
          & "An initial state is defined with the following variables :",/,    &
          & "Level Z (m), Main channel discharge Q1 (m3/s) and floodplain discharge Q2")'
character (*), parameter ::                                                &
err_68c = '("Variable ",A," is not in the header file of the initial conditions")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_69  = '("Error reading initial conditions ", &
          & "(OPTHYCA format),",/,                                      &
          & "problem with the variables of the cross-section # ",i5)'
character (*), parameter ::                                                &
err_69c = '("Error reading initial conditions for the cross-section # ",i5)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_70  = '("Error reading initial conditions ", &
          & "(LIDO format),",/,                                         &
          & "problem with the commentaries in the header file (3 lines)")'
character (*), parameter ::                                                &
err_70c = '("Error reading initial conditions,",            &
          & " check the commentaries in the header file (3 lines)")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_71  = '("Error reading initial conditions ", &
          & "(LIDO format),",/,                                         &
          & "problem with the number of cross-sections and/or the number of reaches")'
character (*), parameter ::                                                &
err_71c = '("Error reading initial conditions,"  ,            &
          & " check the number of cross-sections or reaches")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_72  = '("Error reading initial conditions ", &
          & "(LIDO format),",/,                                         &
          & "problem with the line # ",i3," of numbers of the first and last cross-section",  &
          & " of each reach")'
character (*), parameter ::                                                &
err_72c = '("Error reading initial conditions,",              &
          & " with the line # ",i3," of numbers of the first and last cross-section")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_73  = '("Error reading initial conditions ", &
          & "(LIDO format),",/,                                         &
          & "problem with the table ",A,".")'
character (*), parameter ::                                                &
err_73c = '("Error reading initial conditions with the table ",A)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_74  = '("Error reading initial conditions",/, &
          & " problem with the table ",A,", at line # ",i3)'
character (*), parameter ::                                                &
err_74c = '("Error reading initial conditions with the table ",A,", at line # ",i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_75  = '("Error reading initial conditions",/, &
          & "problem with a variable label")'
character (*), parameter ::                                                &
err_75c = '("Error reading initial conditions with a variable label")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_76  = '("Error reading initial conditions",/,  &
          & "(OPTHYCA format),",/,                                      &
          & "problem with the cross-section # ",i5)'
character (*), parameter ::                                                &
err_76c = '("Error reading initial conditions with the cross-section # ",i5)'
!-------------------------------------------------------------------------
! LIRE_CHAINE_S
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_77  = '("Error reading geometry file",/,                    &
          & "(LIDO 3.0 format),",/,                                     &
          & "problem with a line of commentary")'
character (*), parameter ::                                                &
err_77c = '("Error reading geometry file with a line of commentary")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_78  = '("Error reading geometry file",/,                    &
          & "(LIDO 3.0 format),",/,                                     &
          & "The keyword ""PROFIL"" is not found for the first cross-section.",/,&
          & "Check the format compliance.")'
character (*), parameter ::                                                &
err_78c = '("Keyword not found for the first cross-section")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_79  = '("Error reading geometry file",/,                    &
          & "(LIDO 3.0 format),",/,                                     &
          & "problem with the parameters of the cross-section # ",i3)'
character (*), parameter ::                                                &
err_79c = '("Error reading the geometry of the cross-section # ",i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_80  = '("Error reading geometry file",/,                    &
          & "(LIDO 3.0 format),",/,                                     &
          & "problem with the parameters of the cross-section # ",i3,/,             &
          & "at point # ",i5)'
character (*), parameter ::                                                &
err_80c = '("Error reading the geometry of the cross-section # ",i3," at point # ",i5)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_81  = '("Error reading geometry file",/,                    &
          & "(LIDO 3.0 format).",/,                                     &
          & "At the cross-section : ",A,", two limits for the main channel ",A,            &
          & " have been detected",/,                                        &
          & "A cross-section has only one main channel")'
character (*), parameter ::                                                &
err_81c = '("At the cross-section : ",A,", detection of two limits for the main channel ",A)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_82  = '("Error reading geometry file,",/,                   &
          & "(LIDO 3.0 format),",/,                                     &
          & "problem with the header for the cross-section # ",i5)'
character (*), parameter ::                                                &
err_82c = '("Error with the header for the description of the cross-section # ",i5)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                 &
err_83  = '("Error reading geometry file,",/,                    &
          & "(LIDO 3.0 format),",/,                                      &
          & "the number of cross-sections is too small")'
character (*), parameter ::                                                &
err_83c = '(i3,"Not enough cross-sections in the geometry file")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_84  = '("Computation error.",/,                                         &
          & "A number of reach has not be found ",/,   &
          & "for the cross-section # ",i3)'
character (*), parameter ::                                                &
err_84c = '("No reach associated with the cross-section # ",i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                 &
err_85  = '("Error reading geometry file,",/,                    &
          & "(LIDO 3.0 format),",/,                                      &
          & "The reach # ",i3," has only one cross-section.",/,              &
          & "Two cross-sections or more are necessary")'
character (*), parameter ::                                                 &
err_85c = '("Reach # ",i3," has only one cross-section")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                 &
err_86  = '("Error while computing automatic head losses at junctions.",/,&
          & "The reach # ",i3," at junction # ",i3," has no flow discharge.")'
character (*), parameter ::                                                 &
err_86c = '("Q=0 for reach # ",i3," at junction # ",i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                 &
err_87  = '("Error while computing automatic head losses at junctions.",/,&
          & "The number of upstream reaches : ",i3," at junction # ",i3," is greater than 2")'
character (*), parameter ::                                                 &
err_87c = '("number of upstream reaches ",i3," at junction ",i3," greater than 2")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                 &
err_88  = '("Error while computing automatic head losses at junctions.",/,&
          & "The number of downstream reahces : ",i3," at junction # ",i3," is greater than 1")'
character (*), parameter ::                                                 &
err_88c = '("number of downstream reaches ",i3," at junction ",i3," greater than 1")'
!-------------------------------------------------------------------------
character (*), parameter ::                                              &
err_100  = '("Computation error.",/,                                      &
           & "The water depth : ",f7.3," is greater than the vertical discretisation ",f7.3,/,&
           & "at the cross-section # ",i4,".",/,                      &
           & "Increase the step of vertical cross-section discretisation or the number of steps.")'
character (*), parameter ::                                              &
err_100c = '("Water depth : ",f7.3," greater than the vertical discretisation",f7.3,&
           & " at cross-section # ",i4)'
!-------------------------------------------------------------------------
character (*), parameter ::                                              &
err_101  = '("Computation error.",/,                                      &
           & "Number of equations : ",i2," is insufficient for the use of BISSND solver.")'
character (*), parameter ::                                              &
err_101c = '("Number of equations : ",i2," is insufficient for BISSND solver.")'
!-------------------------------------------------------------------------
character (*), parameter ::                                              &
err_102  = '("Computation error.",/,                                      &
           & "Problem with the number of Right-Hand Sides = ",i3)'
character (*), parameter ::                                              &
err_102c = '("Problem with the number of Right-Hand Sides = ",i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                              &
err_103  = '("Computation error.",/,                                      &
           & "Non-invertible matrix : determinant = ",f6.3)'
character (*), parameter ::                                              &
err_103c = '("Non-invertible matrix : determinant = ",f6.3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                               &
err_104  = '("Computation error.",/,                                       &
           & "Problem with the width : ALARG = ",f12.9," at node : ",i5,/,&
           & "for the vertical discretisation indices : ",i3," and ",i3)'
character (*), parameter ::                                                     &
err_104c = '("Problem with the width : ALARG = ",f12.9," at node : ",i5,&
           & "for the vertical discretisation indices : ",i3," and ",i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                              &
err_105  = '("Computation error.",/,                                      &
           & "Courant number too large at junction # ",i3,/,  &
           & "Increase the number of iterations.")'
character (*), parameter ::                                                    &
err_105c = '("Courant number too large at junction # ",i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                              &
err_106  = '("Computation error.",/,                                      &
           & "The dichotomy algorithm does not converge.")'
character (*), parameter ::                                                    &
err_106c = '("The dichotomy algorithm does not converge.")'
!-------------------------------------------------------------------------
character (*), parameter ::                                              &
err_107  = '("Computation error.",/,                                      &
           & "Problem with the geometry at junction # ",i3)'
character (*), parameter ::                                                    &
err_107c = '("Problem with the geometry at junction # ",i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                              &
err_108  = '("Computation error.",/,                                      &
           & "Negative initial water depth at the cross-section # ",i5,/,&
           & "water depth = ",f9.3)'
character (*), parameter ::                                                    &
err_108c = '("Negative initial water depth at the cross-section # ",i5,&
           & " Water depth = ",f9.3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_201  = '("Error with the vertical discretisation.",/,                                  &
           & "Cross-section # ",i5," , le number of channels is greater than",/,    &
           & "the maximum number = ",i5)'
character (*), parameter ::                                                &
err_201c = '("Cross-section # ",i5,", number of channels > max = ",i5)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_202  = '("Error with the vertical discretisation.",/,                                  &
           & "Limits of bank : ",A," at the cross-section # ",i4,/,              &
           & "are not well-defined.")'
character (*), parameter ::                                                &
err_202c = '("Limits of bank : ",A," at the cross-section #  ",i4," are wrong")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_205  = '("Error with the vertical discretisation.",/,                                  &
           & "Negative cross-section ",A," # ",i4,".",/,              &
           & "Check for possible cusps.")'
character (*), parameter ::                                                &
err_205c = '("Negative cross-section ",A," # "i4)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_206  = '("Error with the vertical discretisation.",/,                                  &
           & "The dichotomy algorithm does not converge.")'
character (*), parameter ::                                                &
err_206c = '("The dichotomy algorithm does not converge")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_300  = '("Error with a parameter.",/,                         &
           & "The kernel version is not supported.",/,&
           & "Only the version ''V3P0'' is allowed.")'
character (*), parameter ::                                                &
err_300c = '("The kernel version is not supported")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_301  = '("Error with the parameters file.",/,      &
           & "Option : ",A,/,                                           &
           & "is not compatible with the kernel ",A,".")'
character (*), parameter ::                                                &
err_301c = '("Option ",A," is not compatible with the kernel ",A)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_302  = '("Error with the parameters file.",/,      &
           & "Option : ",A,/,                                           &
           & " is only compatible with the kernel ",A,".")'
character (*), parameter ::                                                &
err_302c = '("Option ",A," only compatible with the kernel ",A)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_303  = '("Graph ",i3," : ",A,",",/,                       &
          & "The final time is lesser than the simulation time.")'
character (*), parameter ::                                                &
err_303c = '("Final time for the graph # ",i3," : ",A," < simulation time")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_305  = '("Error with the parameters file.",/,      &
           & "No Parameter value : ",A,/,                 &
           & "Possible values are : ",A,".")'
character (*), parameter ::                                                &
err_305c = '("Possible values for parameter ",A," are : ",A)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_306  = '("Error with the parameters file.",/,      &
           & "Parameter : ",A,",",/,                               &
           & "Negative value unauthorized.")'
character (*), parameter ::                                                &
err_306c = '("Parameter : ",A,", Value < 0 (not allowed)")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_307  = '("Error with the parameters file.",/,      &
           & "Ineffective flow area - limits for the floodplain area.",/,    &
           & "For the cross-section # ",i3,", values are not compatible",/,&
           & "with the points of the geometry.")'
character (*), parameter ::                                                &
err_307c = '("Data for the limits of the floodplain area"                            &
           & " cross-section # ",i3,", values are not compatible",                    &
           & " with the points of the geometry")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_308  = '("Error with the parameters file.",/,      &
           & "The distance between reaches is not positive",/,                   &
           & "It must be an integer value.")'
character (*), parameter ::                                                &
err_308c = '("Parameter : distance between reaches is not positive")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_309  = '("Error with the parameters file.",/,      &
           & "Hydraulic network.",/,                                        &
           & "The number of reaches = ",i3," in the parameters file",/,               &
           & "is not the same number of the geometry file = ",i3)'
character (*), parameter ::                                                &
err_309c = '("Number of reaches : ",i3," in the parameters file",             &
           & " != Number of reaches in the geometry file: ",i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_310  = '("Error with the parameters file.",/,      &
           & "1D Mesh.",/,                              &
           & "Abscissa of the cross-section # ",i5," is lesser than or equal to",/,       &
           & "the abscissa of the previous cross-section.")'
character (*), parameter ::                                                &
err_310c = '("Abscissa of the cross-section # ",i5,                                 &
           & " <= the abscissa of the previous cross-section")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_311  = '("Error with the parameters file.",/,      &
           & "Item ",A,",",/,                                    &
           & "for the zone # ",i3,", the value is strictly negative.")'
character (*), parameter ::                                                &
err_311c = '("Item ",A,                                                  &
           & ", value < 0 for the zone # ",i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_312  = '("Error with the parameters file.",/,      &
           & "For the choice of the post-processing,",/,                        &
           & "the value : ",i3," is unknown.",/,                  &
           & "Possible values are : ",/,                            &
           & "1 : OPTHYCA (OPTHYCA format)",/,                             &
           & "2 : RUBENS  ( LIDOP and LIDONP formats)")'
character (*), parameter ::                                                &
err_312c = '("Post-processing value : ",i3," unknown.")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_313  = '("Error with the parameters file.",/,      &
           & "Extreme abscissas of a reach,",/,&
           & "for the reach # ",i3,", the abscissa of the first point",/,&
           & "is greater than or equal to the abscissa of the last point.")'
character (*), parameter ::                                                &
err_313c = '("Reach # ",i3,", first point abscissa >= last point abscissa.")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_314  = '("Error with the parameters file.",/,      &
           & "Item ",A,",",/,                                    &
           & "The limits for the zone # ",i3,"are not well-defined : ",/,    &
           & "the last value is lesser than the first value.")'
character (*), parameter ::                                                &
err_314c = '("Item ",A,                                                  &
           & ", limits for the zone #",i3,"not well-defined")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_315  = '("Error with the parameters file.",/,      &
           & "Item ",A,",",/,                                        &
           & "The value # ",i3,"is not positive.")'
character (*), parameter ::                                                &
err_315c = '("Item ",A,", ",                                         &
           & i3," negative value")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_316  = '("Error with the parameters file.",/,      &
           & "Graphs.",/,                            &
           & "Time unit value for the graph # ",i3,/,&
           & "is not defined.",/,&
           & "Possible values are between 1 and ",i3)'
character (*), parameter ::                                                &
err_316c = '("Time unit for the graph # ",i3," is not non between 1 and ",i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_317  = '("Error with the parameters file.",/,      &
           & "Graphs.",/,                            &
           & "Type value for the graph # ",i3," is not defined.")'
character (*), parameter ::                                                &
err_317c = '("Type for the graph ",i3," is not defined")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                 &
err_318  = '("Error with the parameters file.",/,       &
           & "Graphs.",/,                             &
           & "Data type for the graph # ",i3," is not",&
           & " valid.")'
character (*), parameter ::                                                 &
err_318c = '("Data type for the graph # ",i3," is not valid.")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_319  = '("Error with the parameters file.",/,      &
           & "Definition of the graph # ",i3,/,&
           & "The number of points is not positive.")'
character (*), parameter ::                                                &
err_319c = '("The number of points for the graph ",i3,              &
           & " is <= 0")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_320  = '("Error with the parameters file.",/,      &
           & "Main dam.",/,                          &
           & "The reach number is not valid.")'
character (*), parameter ::                                                &
err_320c = '("The reach number for the main dam is <= 0 or",                 &
           & " > total number of reaches")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_321  = '("Error with the parameters file.",/,      &
           & "Main dam.",/,                          &
           & "The type of dam breaking : ",i3," is not valid.",/,              &
           & "Possible values are :",/,                             &
           & "1 : instantaneous",/,                                          &
           & "2 : progressive")'
character (*), parameter ::                                                &
err_321c = '("Type of dam breaking : ",i3," for the main dam is not valid")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_322  = '("Error with the parameters file.",/,      &
           & "Main dam.",/,                          &
           & "Initial water level : ",f12.3," is not positive.")'
character (*), parameter ::                                                &
err_322c = '("Initial water level : ",f12.3," for the main dam <= 0")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_323  = '("Error with the parameters file.",/,      &
           & "Main dam.",/,                          &
           & "Relative abscissa : ",f12.3," m, is not",/,&
           & "between the abscissas of the first and last cross-sections for the reach # ",i3)'
character (*), parameter ::                                                &
err_323c = '("Dam abscissa : ",f12.3," is not between the abscissas",  &
           & " of the first and the last cross-sections for the reach # ",i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_324  = '("Error with the parameters file.",/,      &
           & "Singularities.",/,                             &
           & "Singularity type # ",i3," = ",i3," is not defined.")'
character (*), parameter ::                                                &
err_324c = '("Singularity type # ",i3," : ",i3," is not defined")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_325  = '("Error with the parameters file.",/,      &
           & "Incorrect reach number for the cell ",&
           & " # ",i3,/,"of the mesh.")'
character (*), parameter ::                                                &
err_325c = '("Incorrect reach number for the cell ",&
           & " # ",i3," of the mesh")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_326  = '("Error with the parameters file.",/,      &
           & "Singularities.",/,                             &
           & "The crest level of the singularity # ",i3," : ",f12.3,/,    &
           & "is not positive.")'
character (*), parameter ::                                                &
err_326c = '("Crest level of the singularity # ",i3," : ",f12.3,         &
           & " <= 0")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_327  = '("Error with the parameters file.",/,      &
           & "Singularities.",/,                             &
           & "The discharge coefficient of the singularity # ",i3," : ",f12.3,/,&
           & "is not between 0 and 1.")'
character (*), parameter ::                                                &
err_327c = '("Discharge coefficient of the singularity #",i3," : ",f12.3,  &
           & " <= 0. or >= 1")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_328  = '("Error with the parameters file.",/,      &
           & "Singularities.",/,                             &
           & "The dam breaking water level for the singularity # ",i3," : ",f12.3,/,  &
           & " is lesser than its crest level")'
character (*), parameter ::                                                &
err_328c = '("Dam breaking water level for the singularity # ",i3," : ",f12.3,       &
           & " <= crest level")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_329  = '("Error with the parameters file.",/,      &
           & "Singularities.",/,                             &
           & "The graph number for the singularity # ",i3," : ",i3,/,    &
           & "is not a positive value")'
character (*), parameter ::                                                &
err_329c = '("Graph number for the singularity # ",i3," : ",i3,         &
           & " <= 0")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_330  = '("Error with the parameters file.",/,      &
           & "Graph # ",i3,/,                  &
           & "For the graph # ",i3," data cannot be entered by keyboard,",/,&
           & "only by files.")'
character (*), parameter ::                                                &
err_330c = '("Graph # ",i3," of type #",i3," cannot be entered",   &
           & " by keyboard")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_331  = '("Error with the parameters file.",/,      &
           & "Boundary conditions.",/,              &
           & "The type value for the open boundary # ",i3, &
           & " is negative",/,                                                &
           & "null or greater than the maximum value : ",i3)'
character (*), parameter ::                                                &
err_331c = '("Type for the open boundary # ",i3,                       &
           & " <= 0 or > maximum value = ",i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_332  = '("Error with the parameters file.",/,      &
           & "Definition of ",A,/,                                        &
           & "the reach number : ",i3," of element # ",i3,/,          &
           & "is negative or null, or greater than the total number of reaches.")'
character (*), parameter ::                                                &
err_332c = '("Definition of ",A,", the reach number ",i3,        &
           & " of element # ",i3," <= 0 or > total number of reaches")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_333  = '("Error with the parameters file.",/,      &
           & "Definition of local head losses.",/,             &
           & "The coefficient # ",i3," : ",f12.3,/,  &
           & "is not a positive value.")'
character (*), parameter ::                                                &
err_333c = '("Coefficient of the local head loss # ",i3," : ",f12.3," < 0")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_334  = '("Error with the parameters file.",/,      &
           & "Mesh Definition - Grid Map.",/,                       &
           & "The relative abscissa of ",A," for the cell # ",i3,/,        &
           & "is not between the abscissas of the first and",/, &
           & "the last cross-sections for the reach # ",i3)'
character (*), parameter ::                                                &
err_334c = '("Relative abscissa of ",A," for the cell # ",i3,                &
           & " is not between the abscissas of the first and",             &
           & " the last cross-sections for the reach # ",i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_335  = '("Error with the parameters file.",/,      &
           & "Definition of the boundaries.",/,                     &
           & "The relative abscissa of ",A," for the reach # ",i3,/,       &
           & "is not between the abscissas of the first",/,       &
           & "and the last cross-sections.")'
character (*), parameter ::                                                &
err_335c = '("Relative abscissa of ",A," for the reach # ",i3,               &
           & " is not between the abscissas of the first and the last ",             &
           & " cross-sections")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_336  = '("Error with the parameters file.",/,      &
           & "Inflow Discharges.",/,                         &
           & "The length of the inflow discharge # ",i3," = ",i3," is negative.")'
character (*), parameter ::                                                &
err_336c = '("Length of the inflow discharge # ",i3," : ",i3,                &
           & " < 0")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_337  = '("Error with the parameters file.",/,      &
           & "Inflow Discharges.",/,                         &
           & "The graph number of the inflow discharge # ",i3," : ",i3,/,   &
           & "is not positive")'
character (*), parameter ::                                                &
err_337c = '("Graph number of the inflow discharge # ",i3," : ",i3,        &
           & " <= 0")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_338  = '("Error with the parameters file.",/,      &
           & "Inflow Discharges.",/,                         &
           & "The graph number of the inflow discharge # ",i3," = ",i3,/,   &
           & "is unknown.")'
character (*), parameter ::                                                &
err_338c = '("Graph number of the inflow discharge # ",i3," : ",i3,        &
           & " is unknown")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_340  = '("Error with the parameters file.",/,      &
           & "Lateral Weirs.",/,                               &
           & "The type of lateral weir # ",i3," = ",i3," is not defined.",/,  &
           & "Possible values are 1 or 2.")'
character (*), parameter ::                                                &
err_340c = '("Type of lateral weir # ",i3," : ",i3," is not defined")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_341  = '("Error with the parameters file.",/,      &
           & "Dry areas.",/,                             &
           & "Relative abscissa of ",A," for the zone # ",i3,/,          &
           & "is not between the abscissas of the first",/,       &
           & "and last cross-sections of the reach # ",i3)'
character (*), parameter ::                                                &
err_341c = '("Relative Abscissa of ",A," for the zone # ",i3,                  &
           & " is not between the abscissas of the first",             &
           & " and last cross-sections of the reach # ",i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_342  = '("Error with the parameters file.",/,      &
           & "Lateral Weirs.",/,                               &
           & "The length of the lateral weir # ",i3," = ",i3," is negative.")'
character (*), parameter ::                                                &
err_342c = '("Length of the lateral weir # ",i3," : ",i3," < 0")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_343  = '("Error with the parameters file.",/,      &
           & "Lateral Weirs.",/,                               &
           & "Crest level # ",i3," = ",i3,              &
           & " is not positive.")'
character (*), parameter ::                                                &
err_343c = '("Crest level of the lateral weir # ",i3," : ",i3," <= 0")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_344  = '("Error with the parameters file.",/,      &
           & "Lateral Weirs.",/,                               &
           & "The discharge coefficient of the lateral weir # ",i3," : ",i3,/,     &
           & "is not between 0 and 1.")'
character (*), parameter ::                                                &
err_344c = '("Discharge coefficient of the lateral weir # ",i3," : ",i3,          &
           & " <= 0. or >= 1.")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_345  = '("Error with the parameters file.",/,      &
           & "Lateral Weirs.",/,                               &
           & "The graph number of the lateral weir # ",i3," : ",i3,/,         &
           & "is lesser than or equal to 0, or greater than the total number",/,       &
           & "of graphs : ",i3,".")'
character (*), parameter ::                                                &
err_345c = '("Graph number of the lateral weir # ",i3," : ",i3,              &
           & " <= 0 or > total number of graphs : ",i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_346  = '("Error with the parameters file.",/,      &
           & "Lateral Weirs.",/,                               &
           & "The lateral weir # ",i3," has a downstream limit with a relative ",/,&
           & "abscissa : ",f12.3," greater than downstream boundary",/,           &
           & "reach # ",i3)'
character (*), parameter ::                                                 &
err_346c = '("Lateral weir ",i3," has a downstream limit with a relative ",                 &
           & " abscissa ",f12.3," greater than downstream boundary",  &
           & " reach ",i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_347  = '("Error with the parameters file.",/,      &
           & "Singularity # ",i3,".",/,                  &
           & "Relative abscissa : ",f12.3," m,",/,            &
           & "is not between the first and the last",/,&
           & "cross-sections of reach # ",i3)'
character (*), parameter ::                                                &
err_347c = '("Abscissa of the singularity # ",i3," : ",f12.3,              &
           & " not between the first and the last", &
           & " cross-sections of reach # ",i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_348  = '("Error with the parameters file.",/,      &
           & "Junctions.",/,                               &
           & "The angle for the reach # ",i3," junction # ",i3," = ",f12.3,/,&
           & "is lesser than 90 or greater than 270 degree.")'
character (*), parameter ::                                                &
err_348c = '("Angle for the reach # ",i3," junction # ",i3,         &
           & " : ",f12.3," < 90 or > 270 degree")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_349 = '("Error with the parameters file.",/,       &
          & "Wrong data.",/,                        &
          & "The unsteady kernels",/,      &
          & "need initial conditions",/,&
          & "(if no restart computation).")'
character (*), parameter ::                                                &
err_349c = '("Unsteady kernels need initial conditions",   &
          & " (if no restart computation)")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_350  = '("Error with the parameters file.",/,      &
          & "Wrong data.",/,                       &
          & "Initial conditions cannot be entered by keyboard"/,,&
          & "with the use of a previous mesh.")'
character (*), parameter ::                                                &
err_350c = '("Initial conditions cannot be entered by keyboard", &
           & " with the use of a previous mesh")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_351  = '("Error with the parameters file.",/,      &
           & "The numbers of reach limits linked to a junction.",/, &
           & "The extreme limit # ",i3," of the junction # ",i3," is negative.")'
character (*), parameter ::                                                &
err_351c = '("The extreme limit # ",i3," of the junction # ",i3," is negative")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_352  = '("Computation Error. Double-sweep algorithm.",/, &
           & "At junction # ",i3,", no downstream reach.")'
character (*), parameter ::                                                &
err_352c = '("At junction # ",i3,", no downstream reach")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_353  = '("Computation Error. Double-sweep algorithm.",/, &
           & "At junction # ",i3,", the downstream reach ",/,&
           & "is probably misdirected.")'
character (*), parameter ::                                                &
err_353c = '("At junction # ",i3,", the downstream reach is misdirected ?")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_354  = '("Computation Error. Double-sweep algorithm.",/, &
           & "At junction # ",i3,", one of the downstream reach",/,&
           & "is an upstream one.",/,                               &
           & "Please modify the network definition.")'
character (*), parameter ::                                                &
err_354c = '("At junction # ",i3,", one of the downstream reach",            &
           & " is an upstream one")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_355  = '("Computation Error. Double-sweep algorithm.",/, &
           & "At junction # ",i3,", infinite loop, the algorithm does not converge.")'
character (*), parameter ::                                                &
err_355c = '("At junction # ",i3,", infinite loop - ",     &
           & " Double-sweep algorithm")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                 &
err_357  = '("Error with the parameters file.",/,       &
           & "Wrong type of graph for the inflow # ",i3,/,          &
           & "The graph # ",i3," of type # ",i3," is not a flow hydrograph,",/,&
           & "which is the only possible type for an inflow discharge.")'
character (*), parameter ::                                                 &
err_357c = '("For the inflow # ",i3,", the graph # ",i3," of type # ",i3,   &
           & " is not a flow hydrograph")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                 &
err_358  = '("Error with the parameters file.",/,       &
           & "Wrong type of boundary condition",/,&
           & "between an open boundary and the associated type of graph.",/,         &
           & "The graph # ",i3," of type # ",i3," is not compatible",/,  &
           & "with boundary condition # ",i3," of type # ",i3)'
character (*), parameter ::                                                 &
err_358c = '("The graph # ",i3," of type # ",i3," is incompatible",             &
           & " with the boundary condition # ",i3," of type ",i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                 &
err_359  = '("Error with the parameters file.",/,       &
           & "Wrong type of graph for a singularity.",/,&
           & "The graph # ",i3," of type # ",i3," is not compatible",/,  &
           & "with the singularity # ",i3," of type # ",i3,".")'
character (*), parameter ::                                                 &
err_359c = '("The graph # ",i3," of type # ",i3," is incompatible",             &
           & " with the singularity # ",i3," of type ",i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                  &
err_360  = '("Error with the parameters file.",/,        &
           & "Wrong type of graph for the lateral weir # ",i3,/,        &
           & "The graph # ",i3," of type # ",i3," is not of type : Z=f(Q),",/,&
           & "only this type is possible for a lateral weir.")'
character (*), parameter ::                                                  &
err_360c = '("For the lateral weir # ",i3,", the graph # ",i3," of type ",i3,    &
           & " is not of type : Z=f(Q)")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_361  = '("Error with the parameters file.",/,      &
           & "Head loss # ",i3,".",/,              &
           & "Relative abscissa : ",f12.3," is not between",/,&
           & "the first and the last cross-section of the reach # ",i3)'
character (*), parameter ::                                                &
err_361c = '("Abscissa of the head loss # ",i3," : ",f12.3,          &
           & " is not between the first and the last cross-section of the reach # ",i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_362  = '("Error with the parameters file.",/,      &
           & "Inflow # ",i3,".",/,                       &
           & "Relative abscissa : ",f12.3," is not between",/,&
           & "the first and the last cross-section of the reach # ",i3)'
character (*), parameter ::                                                &
err_362c = '("Abscissa of the inflow ",i3," : ",f12.3,                   &
           & " is not between the first and the last cross-section of the reach # ", i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_363  = '("Error with the parameters file.",/,      &
           & "Inflow # ",i3,".",/,                       &
           & "Last abscissa of the inflow : ",f12.3," is not between",/,&
           & "the first and the last cross-section of the reach # ",i3)'
character (*), parameter ::                                                &
err_363c = '("Last abscissa of the inflow # ",i3," : ",f12.3,            &
           & " is not between the first and the last cross-section of the reach # ",i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_364  = '("Error with the parameters file.",/,      &
           & "Lateral weir # ",i3,".",/,                       &
           & "Relative abscissa : ",f12.3," is not between",/,&
           & "is not between the first and the last cross-section of the reach # ",i3)'
character (*), parameter ::                                                &
err_364c = '("Abscissa of the lateral weir # ",i3," : ",f12.3,                   &
           & " is not between the first and the last cross-section of the reach # ",i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_365  = '("Error with the parameters file.",/,      &
           & "Lateral weir # ",i3,".",/,                       &
           & "Last abscissa of the lateral weir : ",f12.3," is not between",/,&
           & "is not between the first and the last cross-section of the reach # ",i3)'
character (*), parameter ::                                                &
err_365c = '("Last abscissa of the lateral weir # ",i3," : ",f12.3,            &
           & " is not between the first and the last cross-section of the reach # ", i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_366  = '("Error with the parameters file.",/,      &
           & "Dissipation zones.",/,                      &
           & "The parameter value must be between 1 and ",i3)'
character (*), parameter ::                                                &
err_366c = '("The parameter value for the dissipation zone is not between",&
           & " 1 and ",i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_367  = '("Error with the parameters file.",/,      &
           & "Dissipation zones.",/,                      &
           & "The reahc number of the dissipation zone # ",i3," is not positive.")'
character (*), parameter ::                                                &
err_367c = '("Reach number of the dissipation zone # ",i3," is <= 0")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_368  = '("Error with the parameters file.",/,      &
           & "Singularities.",/,                             &
           & "The type of the singularity # ",i3," = ",i3,/,               &
           & "is not compatible with the steady kernel.")'
character (*), parameter ::                                                &
err_368c = '("Type of the singularity # ",i3," = ",i3," is not compatible with",&
           & " the steady kernel")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_369  = '("Error with the parameters file : ",A,".",/,                          &
           & "The number of points is not the product of the number of reference",/,&
           & "discharges times the number of downstream levels.")'
character (*), parameter ::                                                &
err_369c = '("In the file ",A," Number of points /= Number of reference", &
           & " discharges x Number of downstream levels")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_370  = '("Error with the parameters file.",/,      &
           & "Singularities.",/,                             &
           & "The width of the controlled gate for singularity # ",i3," : ",f12.3,/,&
           & "is lesser than 0.")'
character (*), parameter ::                                                &
err_370c = '("width of the controlled gate for singularity # ",i3," : ",f12.3,  &
           & " <= 0.")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_371  = '("Error with the parameters file.",/,      &
           & "Keyword : ''TYPE DE MAILLAGE'' not defined.",/,                 &
           & "Possible values are between 1 and ",i3)'
character (*), parameter ::                                                &
err_371c = '("Keyword ''TYPE DE MAILLAGE'' > ",i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_372  = '("Error with the parameters file.",/,      &
           & "Points for the saved results,"/,    &
           & "The reach number of the point # ",i3," is lesser than 1.")'
character (*), parameter ::                                                &
err_372c = '("Reach number (for a saved result) # ",i3," < 1")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_373  = '("Error with the parameters file.",/,      &
           & "Points for the saved results,"/,    &
           & "Abscissa of the point # ",i3," is negative.")'
character (*), parameter ::                                                &
err_373c = '("Abscissa of a point for a saved result # ",i3," < 0.")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_374  = '("Error with the parameters file.",/,      &
           & "Item ",A,",",/,                                    &
           & "for the zone # ",i3,", the value is not positive")'
character (*), parameter ::                                                &
err_374c = '("Item ",A,                                                  &
           & ", value <= 0 for the cell # ",i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_375  = '("Error with the parameters file.",/,      &
           & "Item ",A,/,                                        &
           & "The zones ",i3," and ",i3," are not connected.")'
character (*), parameter ::                                                &
err_375c = '("Item ",A,                                                  &
           & " Zones not connected: ",i3," and ",i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_376  = '("Error with the parameters file.",/,      &
           & "Item ",A,/,                                        &
           & "The cross-section # ",i3," appears twice in the list.")'
character (*), parameter ::                                                &
err_376c = '("Item ",A,", ",                                             &
           & "cross-section # ",i3," appears twice")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                 &
err_377  = '("Error with the initial condition entered by keyboard.",/,                 &
           & "Detection of a cusp for the abscissas at the point #",i3,/,                                       &
           & "Please modify the corresponding graph.")'
character (*), parameter ::                                                 &
err_377c = '("Cusp at point # ",i3," for the abscissas",    &
           & " of the initial condition")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_378  = '("Error with the parameters file.",/,      &
           & "Ineffective flow areas - limits of the floodplain.",/,    &
           & "For the cross-section # ",i3,", a limit of the floodplain",/,&
           & "is in the main channel.")'
character (*), parameter ::                                                &
err_378c = '("At the cross-section # ",i3,", a limit of the floodplain",&
           & " is in the main channel")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_379  = '("Error reading the graph file : ",A,/,  &
           & "No time unit in the first line of the file",/,      &
           & "Time unit expected for temporal series")'
character (*), parameter ::                                                &
err_379c = '("First line of the graph file ",A,         &
           & " has no time unit")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_380  = '("Error reading the graph file : ",A,/,  &
           & "for the line with the unit time.")'
character (*), parameter ::                                                &
err_380c = '("Error reading the graph file : ",A,&
           & " for the line with the unit time")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_381  = '("Error with the parameters file.",/,      &
           & "Vertical discretisation of the cross-sections.",/,                &
           & "The number of the last cross-section for the last zone :",i3,/,&
           & "is not equal to the number of cross-sections in the geometry ",i3,/,&
           & "Some cross-sections are not linked to zone of vertical discretisation.")'
character (*), parameter ::                                                &
err_381c = '("Number of the last cross-section for the last zone ",i3,&
           & " is not equal to the number of cross-sections : ",i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_382  = '("Error at the first line of the mesh file,",/,                &
           & "with ",A)'
character (*), parameter ::                                                &
err_382c = '("Error with the mesh file ",&
           & " with ",A)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_383  = '("Error reading the mesh file,",/,                &
           & "at line #",i3)'
character (*), parameter ::                                                &
err_383c = '("Error reading the mesh file at line ",i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_384  = '("Error with the parameters file.",/,      &
           & "The largest number for the open boundaries : ",i3,/,&
           & "is greater than the number of graphs : ",i3)'
character (*), parameter ::                                                &
err_384c = '("The largest number for the open boundaries ",i3," > number of graphs ",i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_385  = '("Error with the parameters file.",/,      &
           & "The open boundary # ",i3,/,                        &
           & "is not of type : ",A,", which is not allowed",/,&
           & "for a downstream condition with the steady kernel.")'
character (*), parameter ::                                                &
err_385c = '("Downstream boundary condition # ",i3," is not of type ",A)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_386  = '("Error with the parameters file.",/,      &
           & "Item ",A,/,                                        &
           & "At least one boundary condition with a graph is necessary")'
character (*), parameter ::                                                &
err_386c = '("At least one boundary condition with a graph is necessary")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_387  = '("Abscissa does not match any reach.",/,      &
           & "Item ",A)'

character (*), parameter ::                                                &
err_387c = '("Abscissa does not match any reach")'
!-------------------------------------------------------------------------
character (*), parameter ::                                              &
err_500  = '("Error with the parameters file.",/,    &
           & "The format ",i3," is unknown"  ,/,                  &
           & "Possible formats are :         ",/,                  &
           & "1 : OPTHYCA (OPTHYCA post-processing)",/,                  &
           & "2 : LIDOP   (RUBENS post-processing) ",/,                  &
           & "3 : LIDONP  (RUBENS post-processing) ")'
character (*), parameter ::                                                    &
err_500c  = '("Unknown format for results : ",i3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                              &
err_600  = '("Computation error.",/,                                      &
           & "The water depth is not positive in the reach # ",i3,/,          &
           & "at the relative abscissa : ",f12.3)'
character (*), parameter ::                                                    &
err_600c = '("Water Depth <= 0 Reach ",i3," Abscissa ",f12.3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                              &
err_601  = '("Computation error."/,                           &
           & "For law ",i3," The value of the water level : ",f12.3,/,&
           & "is out of bounds [",2f12.3,"] graph Z=f(Q).")'
character (*), parameter ::                                                    &
err_601c = '("Z=f(Q) # ",i3,": Z ",f8.3," out of bounds [",2f8.3,"].")'
!-------------------------------------------------------------------------
character (*), parameter ::                                               &
err_602  = '("Computation error.",/,                                       &
           & "The type of the singularity # ",i3," : ",i3," is undefined.")'
character (*), parameter ::                                               &
err_602c = '("Type of singularity # ",i3," : ",i3," undefined")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_603  = '("Computation error.",/,                                        &
           & "An extreme node is not an open boundary,",/,&
           & "or located at a junction.",/,                                     &
           & "See the network definition.")'
character (*), parameter ::                                                &
err_603c = '("An extreme node is not an open boundary",           &
           & " or located at a junction")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_605  = '("Computation error.",/,                                        &
           & "Non-invertible matrix.")'
character (*), parameter ::                                                &
err_605c = '("Non-invertible matrix")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_606  = '("Computation error.",/,                                        &
           & "At time : ",f12.3," at the open boundary # ",i3," : ",A,/,       &
           & "The algorithm for the solution of the normal flow conditions does not converge.")'
character (*), parameter ::                                                &
err_606c = '("At time ",f12.3,"at the open boundary # ",i3," : ",A," The algorithm for the solution of ",&
           & " the normal flow conditions does not converge")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_701  = '("Computation error.",/,                                        &
           & "The open boundary # ",i3," : ",A,/,                        &
           & "is with a supercritical flow which is incompatible with the chosen kernel",/,&
           & "Choose the unsteady supercritical kernel")'
character (*), parameter ::                                                &
err_701c = '("Open boundary ",i3," : ",A," is with a supercritical flow")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_702  = '("Computation error at time : ",f12.3,/,                       &
           & "The open boundary # ",i3," : ",A,/,                        &
           & "is with a supercritical flow whereas the associated graph",/,&
           & "has only one hydraulic variable instead of two.")'
character (*), parameter ::                                                &
err_702c = '("At time ",f12.3,"open boundary # ",i3," : ",A," is with", &
           & " a supercritical flow and has only one hydraulic variable")'
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_703  = '("The link to a storage area # ",i4,", has an abscissa ",f12.3, &
           &" between two cross-sections with one on an extreme node of the reach # : ",i4)'
character (*), parameter ::                                                &
err_703c = '("The link ",i4,", with the abscissa ",f12.3," is on an extreme node of the reach # : ",i4)'
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_704  = '("Parse error or IO error while reading the data file .xcas")'
character (*), parameter ::                                                &
err_704c = '("Error reading XCAS")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_705  = '("Computation error.",/,                                         &
          & "Cross section layout problem",/,            &
          & "Debord model, operand is not positive : ",g12.2)'
character (*), parameter ::                                                &
err_705c = '("Debord model, operand is not positive : ",g12.2)'

! Messages d'erreur de COURLIS
!-------------------------------------------------------------------------
character (*), parameter ::                                              &
err_400 = '("Erreur du nombre de profils lus dans Courlis.",/,        &
         & "Le nombre de profils du fichier geometrique pour Courlis est de "    &
         & ,i3,/,"et n''est pas egal au nombre de profils de Mascaret.")'
character (*), parameter ::                                                &
err_400c = '("Geometrie Courlis : Nb-Profils_Courlis /= Nb_Profils_Mascaret")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_401 = '("Erreur sur un profil :",/,                                &
         & "Au profil ",I3,", Courlis compte ",I3," points alors que ",/,        &
         & "Mascaret en denombre ",I3,".")'
character (*), parameter ::                                                &
err_401c = '("Courlis, profil ",I3," : Nb_pts_Courlis /= Nb_pts_Mascaret!")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_402 = '("Erreur sur un profil :",/,                                &
         & "Au Profil n0",I3," l''abscisse curviligne ne concorde pas",/,        &
         & "avec celle du profil de Mascaret.")'
character (*), parameter ::                                                &
err_402c = '("Courlis, profil",I3,", non-concordance de l''abscisses curviligne.")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_403 = '("Erreur sur un profil :",/,                                &
         & "Au Profil n0",I3,", point ",I3,", l''abscisse angulaire ne",/,        &
         & "concorde pas avec celle du profil de Mascaret.")'
character (*), parameter ::                                                &
err_403c = '("Courlis, profil",I3,",point",I3," X_Mascaret /= X_Courlis!")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_404 = '("Erreur sur un profil :",/,                                &
         & "Au Profil n0",I3,", point ",I3,", la cote de l''interface eau-"        &
         & "sediment",/,"ne concorde pas avec la cote du profil Mascaret.")'
character (*), parameter ::                                                &
err_404c = '("Courlis, profil",I3,",point",I3," Y_Mascaret /= Z(1)_Courlis!")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_405 = '("Erreur sur un profil :",/,                                &
         & "Au profil n0 ",I3," un lit majeur a ete detecte.",/,                &
         & "Il faut un lit mineur unique.")'
character (*), parameter ::                                                &
err_405c = '("Courlis, profil n0 ",I3,", presence d''un lit majeur!")'
!-------------------------------------------------------------------------
character (*), parameter ::                                            &
err_406 = '("Erreur sur un profil :",/,                                &
         & "Au profil n0 ",I3," les interfaces ",I3," et ",I3," se chevauchent.")'
character (*), parameter ::                                                &
err_406c = '("Courlis, profil n0 ",I3,", chevauchement d''interfaces!")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_407 = '("Erreur de compatibilite d''options de calcul :",/,        &
         & A," n''est pas compatible avec l''option COURLIS.")'
character (*), parameter ::                                                &
err_407c = '(A," est incompatible avec l''option COURLIS.")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_408 = '("Erreur de parametres pour l''option COURLIS.",/,            &
         & "Le parametre ",A,/,"doit etre ",A)'
character (*), parameter ::                                                &
err_408c = '("Erreur COURLIS : ",A," doit etre ",A)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_409 = '("WARNING de l''option COURLIS.",/,A,A)'
character (*), parameter ::                                                &
err_409c = '("WARNING COURLIS !",A,A)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_410 = '("Erreur de choix du mode de saisie pour : ",A,/,            &
         & "Le mode choisi n''est ni par CLAVIER ni par FICHIER.")'
character (*), parameter ::                                                &
err_410c = '("Erreur COURLIS : choix errone du mode de saisie pour ",A)'
!-------------------------------------------------------------------------
character (*), parameter ::                                               &
err_411 = '("Erreur de lecture des parametres sedimentaires.",/,        &
         & "La valeur du parametre ",A," n''est pas correcte.")'
character (*), parameter ::                                                &
err_411c = '("Erreur COURLIS (sediments), parametre ",A," incorrect.")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_412 = '("Erreur en lecture du fichier ",A," (format OPTHYCA).",/,    &
          & "La variable : ",A," n''est pas presente dans le fichier.",/,        &
          & "Le fichier des concentrations initiales en sable et vase doit ",/,    &
          & "comporter les variables les abscisses curvilignes (X) et ",/,        &
          & "les concentrations initiales en vase (CVASE) et en sable (CSABLE).")'
character (*), parameter ::                                                &
err_412c = '("Erreur : Fichier ",A," : variable ",A," non presente.")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_413 = '("Erreur en lecture du fichier geometrie Courlis ",A,/,    &
          & "a la lecture des valeurs du ",I3,"eme profil, au ",I5,"eme point.")'
character (*), parameter ::                                                &
err_413c = '("Erreur : Fichier geometrie ",A,": erreur au profil ",I3,", point",I5)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_414 = '("Erreur : forme de convection non prevue.")'
character (*), parameter ::                                                &
err_414c = '("Erreur : forme de convection non prevue.")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_415  = '("Erreur : La section mouillee du profil ",I3," est negative.")'
character (*), parameter ::                                                &
err_415c = '("Erreur : Section mouillee negative au profil ", I3)'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_416 = '("Loi de concentration n0 ",i3," : ",A,",",/,                &
         & "Le temps final de cette loi est inferieur au temps de la simulation.")'
character (*), parameter ::                                                &
err_416c = '("Tps final de la loi de concentration n0 ",i3," : ",A," < tps de simulation")'
!-------------------------------------------------------------------------
!PRE-COURLIS
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_430 = '("PRE-COURLIS : Erreur sur un profil ",/,                    &
         & "Profil n0 ",I3,", interface ",I2," : l''abscisse angulaire",/,        &
         & "d''une des extremites ne concorde pas avec celle de la 1ere interface.")'
character (*), parameter ::                                                &
err_430c = '("PRE-COURLIS : profil",I3,", interface ",I2,                &
          & "Extremites X_Interface1  /= X_Interface !")'
!-------------------------------------------------------------------------
! Convection MS2018 : err_508 existe deja dans ModulesTracer/m_message_tracer_c.f90
!-------------------------------------------------------------------------
!character (*), parameter ::                            &
!err_508  = '("Tracer - Erreur de valeurs de parametres pour l''option convection.",/,    &
!          & "Le parametre ",A,/,"doit etre ",A)'
!character (*), parameter ::                                            &
!err_508c = '("Tracer : ",A," doit etre ",A)'
!-------------------------------------------------------------------------


end module M_MESSAGE_C
