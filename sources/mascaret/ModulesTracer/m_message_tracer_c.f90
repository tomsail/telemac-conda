!== Copyright (C) 2000-2022 EDF-CEREMA ==
!
!   This file is part of MASCARET-TRACER.
!
!   MASCARET-TRACER is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   MASCARET-TRACER is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with MASCARET-TRACER.  If not, see <http://www.gnu.org/licenses/>
!

module M_MESSAGE_TRACER_C
!***********************************************************************
! PROGICIEL : TRACER       M.LUCK
!
! VERSION : V8P4R0            EDF-CEREMA
!***********************************************************************
! === CE MODULE CONTIENT LES MESSAGES D'ERREUR LONGS ET COURTS ===


   !=========================== Instructions ==============================

   !-------------------------------------------------------------------------
   character (*), parameter :: &
   banniere = '("=========================================================================")'
   !------------------------------------------------------------------------------------
   ! Messages d'erreur lies a l'option TRACER
   !------------------------------------------------------------------------------------
   character (*), parameter :: &
   err_506  = '("TRACER - Error : calculation options are incompatible :",/, &
             & "The ",A," must be a multiple of the coupling frequency Hydraulic-Tracer.")'
   character (*), parameter :: &
   err_506c = '("TRACER : ",A," must be a multiple of the coupling frequency.")'
   !------------------------------------------------------------------------------------
   character (*), parameter :: &
   err_507  = '("TRACER - Error : calculation options are incompatible :",/, &
             & "The option ",A," is not compatible with the water quality library TRACER.")'
   character (*), parameter :: &
   err_507c = '(A," est incompatible avec TRACER.")'
   !------------------------------------------------------------------------------------
   character (*), parameter :: &
   err_508  = '("TRACER - Error with some parameters for the water quality library TRACER.",/, &
             & "The parameter ",A,/,"must be ",A)'
   character (*), parameter :: &
   err_508c = '("TRACER : ",A," must be ",A)'
   !------------------------------------------------------------------------------------
   character (*), parameter :: &
   err_510  = '("TRACER - Error with the choice of the entering method : ",A,/, &
             & "The chosen method is not KEYBOARD neither FILE.")'
   character (*), parameter :: &
   err_510c = '("TRACER : unknown entering method for parameters ",A)'
   !------------------------------------------------------------------------------------
   character (*), parameter :: &
   err_512  = '("Error with the parameters file.",/,      &
              & "Choice of the post-processing tool for TRACER,",/,                 &
              & "the value : ",i3," is unknown.",/,                  &
              & "Possible values are : ",/,                            &
              & "1 : OPTHYCA (OPTHYCA format)",/,                             &
              & "2 : RUBENS  (LIDONP format)")'
   character (*), parameter ::                                                &
   err_512c = '("Value : ",i3," for the post-processing of TRACER is unknown.")'
   !-------------------------------------------------------------------------
   character (*), parameter :: &
   err_516  = '("Error with the parameters file.",/,      &
              & "Graphs for TRACER.",/,                                  &
              & "The value of the time unit for the graph #",i3," (entered by keyboard)",/,&
              & "is undefined.",/,&
              & "Possible values are between 1 and ",i3)'
   character (*), parameter ::                                                &
   err_516c = '("Time unit for graph # ",i3,"of TRACER is not between entre 1 and ",i3)'
   !------------------------------------------------------------------------------------
   character (*), parameter :: &
   err_517  = '("Graph for the concentration of a tracer # ",i3," : ",A,",",/, &
             & "The final time of the graph is lesser than the simulation time.")'
   character (*), parameter :: &
   err_517c = '("Final time of the graph (Tracer) # ",i3," : ",A," < simulation time")'
   !------------------------------------------------------------------------------------
   character (*), parameter :: &
   err_568  = '("Error reading file on the initial conditions for the concentration of tracers", &
             & "(OPTHYCA format).",/,                                        &
             & "The variable : ",A," is not in the header file")'
   character (*), parameter ::                                                  &
   err_568c = '("Variable ",A," not in the file",                  &
             & " of the initial conditions for the concentration of tracers")'
   !-------------------------------------------------------------------------
   character (*), parameter ::                                                 &
   err_577  = '("Error on the initial conditions for the concentration of tracers (keyboard entering method).",/, &
           & "Detection of a cusp for the abscissas,",/,&
           & "at point ",i3,/,                                       &
           & "Please modify the corresponding graph.")'
   character (*), parameter ::                                                 &
   err_577c = '("Cusp at point ",i3," for the abscissas",    &
              & " of the initial concentration of tracers")'
   !------------------------------------------------------------------------------------
   character (*), parameter ::                                                &
   err_579  = '("Error reading file for a graph of tracers : ",A,/,       &
              & "The first line has no unit time",/,      &
              & "which is not allowed for temporal series.")'
   character (*), parameter ::                                                &
   err_579c = '("First line of the graph file for a tracer ",A,              &
              & " has no unit time")'
   !-------------------------------------------------------------------------
   character (*), parameter :: &
   err_580  = '("TRACER - Error : Main channel wetted area at the cross-section ",I4," is not positive.")'
   character (*), parameter :: &
   err_580c = '("TRACER : Main channel wetted area at the cross-section ", I4," is not positive")'
   !------------------------------------------------------------------------------------
   character (*), parameter :: &
   err_581  = '("TRACER - Error : The water depth at the cross-section ",I4," is not positive.")'
   character (*), parameter :: &
   err_581c = '("TRACER : The water depth is not positive at the cross-section ", I4)'
   !------------------------------------------------------------------------------------
   character (*), parameter :: &
   err_582  = '("TRACER - Error : Detection of an output reach at junction ",I4)'
   character (*), parameter :: &
   err_582c = '("TRACER : output reach at junction ",I4)'
   !------------------------------------------------------------------------------------
   character (*), parameter :: &
   err_583  = '("Error with the parameters file.",/,      &
              & "Definition of TRACER sources.",/,                           &
              & "The graph number for the source # ",i3," : ",i3,/,         &
              & "is not positive")'
   character (*), parameter :: &
   err_583c = '("Graph number for the source # ",i3," : ",i3,              &
              & " <= 0")'
   !------------------------------------------------------------------------------------
   character (*), parameter :: &
   err_584  = '("Error with the parameters file.",/,      &
              & "Definition of TRACER sources.",/,                           &
              & "The graph number for the source # ",i3," = ",i3,/,         &
              & "does not match a graph number for tracer.")'
   character (*), parameter :: &
   err_584c = '("The graph number for the source # ",i3," : ",i3,              &
              & " oes not match a graph number for tracer")'
   !------------------------------------------------------------------------------------
   character (*), parameter :: &
   err_585  = '("Error with the parameters file.",/,      &
              & "Definition of the Boundary Conditions for TRACER.",/,                &
              & "The graph number for the BC # ",i3," : ",i3,/,             &
              & "is not positive")'
   character (*), parameter :: &
   err_585c = '("Graph number for the BC # ",i3," : ",i3,                  &
              & " <= 0")'
   !------------------------------------------------------------------------------------
   character (*), parameter :: &
   err_586  = '("Error with the parameters file.",/,      &
              & "Definition of the Boundary Conditions for TRACER.",/,                &
              & "The graph number for the BC # ",i3," = ",i3,/,             &
              & "does not match any TRACER graph.")'
   character (*), parameter :: &
   err_586c = '("The graph number for the BC # ",i3," : ",i3,                  &
           & " does not match any TRACER graph")'
   !------------------------------------------------------------------------------------
   character (*), parameter :: &
   err_587  = '("TRACER - Error : Negative Flow Discharge with the hydraulic network ",I4)'
   character (*), parameter :: &
   err_587c = '("TRACER : Negative Flow Discharge - Number of nodes ",I4)'
   !------------------------------------------------------------------------------------

End Module M_MESSAGE_TRACER_C
