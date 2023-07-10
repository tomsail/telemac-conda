/*  This file is part of MED.
 *
 *  COPYRIGHT (C) 1999 - 2019  EDF R&D, CEA/DEN
 *  MED is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  MED is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with MED.  If not, see <http://www.gnu.org/licenses/>.
 */

/*
 * StructElement use case 1 : write struct elements model in a file
 * STEP 1 : suppport mesh creation
 * STEP 2 : struct element model creation
 * STEP 3 : computation mesh creation
 */

#include <med.h>
#define MESGERR 1
#include <med_utils.h>

#include <string.h>

int main (int argc, char **argv) {
  med_idt fid;
  char elementname[MED_NAME_SIZE+1]="";
  const char ballsupportname[MED_NAME_SIZE+1]="BALL_SUPPORT_MESH";
  const char beamsupportname[MED_NAME_SIZE+1]="BEAM_SUPPORT_MESH";
  const char computmeshname[MED_NAME_SIZE+1]="COMPUT_MESH";
  const char beamsectionname[MED_NAME_SIZE+1]="BEAM_SECTION_MESH";
  const med_int elementdim = 3;
  med_int nnode;
  med_geometry_type geotype=MED_NONE;
  med_int ncomp;
  const med_float ballmeshnodescoo[3] = {0.0, 0.0, 0.0 };
  const med_float beammeshnodescoo[3*7] = {0.0,0.0,0.0,
					   0.0,0.0,2.0,
					   0.0,0.0,4.0,
					   0.0,0.0,5.0,
					   0.0,0.0,7.0,
					   0.0,0.0,10.0,
					   0.0,0.0,11.0 };
  const med_float beamsectioncoo[9*3] = {-0.2,-0.2,0.0,
					 0.0,-0.2,0.0,
					 0.2,-0.2,0.0,
					 -0.2, 0.0,0.0,
					 0.0, 0.0,0.0,
					 0.2, 0.0,0.0,
					 -0.2, 0.2,0.0,
					 0.0, 0.2,0.0,
					 0.2, 0.2,0.0 };
  const med_int seg2connectivity[2*6] = {1,2, 2,3, 3,4, 4,5, 5,6, 6,7};
  med_int spacedim, meshdim,nseg2;
  /*                                         123456789012345612345678901234561234567890123456 */
  const char axisname[3*MED_SNAME_SIZE+1] = "x               y               z               ";
  const char unitname[3*MED_SNAME_SIZE+1] = "cm              cm              cm              ";
  const med_float attvalue[6] = {0.2,0.3,0.4,0.4,0.3,0.2};
  /*                                            1234567890123456789012345678901234567890123456789012345678901234        */
  const char attprovalue[2*MED_NAME_SIZE+1] = {"EXTREMITY_1_____________________________________________________" \
                                               "EXTREMITY_2_____________________________________________________" };
  const char profilename[MED_NAME_SIZE+1] = "EXTREMITY_PROFILE_NAME";
  const med_int profilesize = 2;
  const med_int profile[2] = {1,6};
  const med_float meshcoo[3*12] = { 0.0, 0.0, 0.0,
				    1.1, 1.1, 1.1,
				    2.2, 2.2, 2.2,
				    10., 10., 10.,
				    12., 12., 12.,
				    60., 20., 20.,
				    70., 20., 20.,
				    80., 20., 20.,
				    90., 20., 20.,
				    100., 20., 20.,
				    110., 20., 20.,
				    120., 20., 20.
  };
  const med_int beamconnectivity[12] = {  6,7,
					  7,8,
					  8,9,
					  9,10,
					  10,11,
					  11,12 };
  med_int nentity;
  const med_int labels[3] = { 1, 2, 3 }; /* nodes numbers */
  const med_int ballconnectivity[2] = { 4, 5 }; /* nodes numbers */
  const med_float balldiameter[2] = { 2.0, 5.8 };
  const med_int nquad4=4;
  const med_int beamsectionconnectivity[4*4] = { 4,5,2,1,
						 5,6,3,2,
						 7,8,5,4,
						 8,9,6,5};
  int ret=-1;

  /* file creation */
  fid = MEDfileOpen("UsesCase_MEDstructElement_1.med",MED_ACC_CREAT);
  if (fid < 0) {
    MESSAGE("ERROR : file creation");
    goto ERROR;
  }

  /* STEP 1 : support meshes creation */
  spacedim =  3;
  meshdim = 3;

  /* Mesh 1 : support mesh for ball model */
  if (MEDsupportMeshCr(fid, ballsupportname, spacedim, meshdim, "Support mesh for a ball model",
		       MED_CARTESIAN, axisname, unitname) < 0) {
    MESSAGE("ERROR : creating a support mesh ...");
    goto ERROR;
  }
  /* 1 node and no cell in the mesh */
  nnode = 1;
  if (MEDmeshNodeCoordinateWr(fid, ballsupportname, MED_NO_DT, MED_NO_IT, 0.0,
			      MED_FULL_INTERLACE, nnode, ballmeshnodescoo) < 0) {
    MESSAGE("ERROR : write nodes coordinates ...");
    goto ERROR;
  }

  /* Mesh 2 :support mesh for beam model */
  if (MEDsupportMeshCr(fid, beamsupportname, spacedim, meshdim, "Support mesh for a beam model",
		       MED_CARTESIAN, axisname, unitname) < 0) {
    MESSAGE("ERROR : creating a support mesh ...");
    goto ERROR;
  }
  /* 7 nodes and 6 MED_SEG2 */
  nnode = 7;
  if (MEDmeshNodeCoordinateWr(fid, beamsupportname, MED_NO_DT, MED_NO_IT, 0.0,
			      MED_FULL_INTERLACE, nnode,  beammeshnodescoo) < 0) {
    MESSAGE("ERROR : write nodes coordinates ...");
    goto ERROR;
  }
  nseg2 = 6;
  if (MEDmeshElementConnectivityWr(fid, beamsupportname, MED_NO_DT, MED_NO_IT, 0.0, MED_CELL, MED_SEG2,
				   MED_NODAL, MED_FULL_INTERLACE, nseg2, seg2connectivity) < 0) {
    MESSAGE("ERROR : write cells connectivity ...");
    goto ERROR;
  }

  /* Mesh 3 : support mesh to define a section for integration points of
     a struct element */
  if (MEDsupportMeshCr(fid, beamsectionname, spacedim, meshdim, "Support mesh for a section of the beam model",
		       MED_CARTESIAN, axisname, unitname) < 0) {
    MESSAGE("ERROR : creating a support mesh ...");
    goto ERROR;
  }

  nnode = 9;
  if (MEDmeshNodeCoordinateWr(fid, beamsectionname, MED_NO_DT, MED_NO_IT, 0.0,
			      MED_FULL_INTERLACE, nnode, beamsectioncoo) < 0) {
    MESSAGE("ERROR : write nodes coordinates ...");
    goto ERROR;
  }

  if (MEDmeshElementConnectivityWr(fid, beamsectionname, MED_NO_DT, MED_NO_IT, 0.0, MED_CELL, MED_QUAD4,
				   MED_NODAL, MED_FULL_INTERLACE, nquad4, beamsectionconnectivity) < 0) {
    MESSAGE("ERROR : write cells connectivity ...");
    goto ERROR;
  }

  /* STEP 2 */
  /* particle model creation : no support mesh */
  strcpy(elementname,MED_PARTICLE_NAME);
  if ((geotype = MEDstructElementCr(fid, elementname, elementdim, MED_NO_MESHNAME,
				    MED_NONE,MED_NONE)) < 0) {
    MESSAGE("ERROR : creating struct element");
    goto ERROR;
  }
  ncomp=1;
  if (MEDstructElementVarAttCr(fid, elementname,
			       MED_PARTICLE_LABEL, MED_ATT_INT, ncomp) < 0) {
    MESSAGE("ERROR : creating struct element");
    goto ERROR;
  }

  /* ball model creation */
  strcpy(elementname,MED_BALL_NAME);
  if ((geotype = MEDstructElementCr(fid, elementname, elementdim, ballsupportname,
				    MED_NODE,MED_NONE)) < 0) {
    MESSAGE("ERROR : creating struct element");
    goto ERROR;
  }
  ncomp=1;
  if (MEDstructElementVarAttCr(fid, elementname,
			       MED_BALL_DIAMETER, MED_ATT_FLOAT64, ncomp) < 0) {
    MESSAGE("ERROR : creating struct element");
    goto ERROR;
  }


  /* A beam */
  strcpy(elementname,MED_BEAM_NAME);
  if ((geotype = MEDstructElementCr(fid, elementname, elementdim, beamsupportname,
				    MED_CELL,MED_SEG2)) < 0) {
    MESSAGE("ERROR : creating struct element");
    goto ERROR;
  }
  ncomp=1;
  /* a first constant attribute */
  if (MEDstructElementConstAttWr(fid, elementname,
				 MED_BEAM_THICKNESS, MED_ATT_FLOAT64, ncomp,
				 MED_CELL,(void*) attvalue) < 0) {
    MESSAGE("ERROR : creating struct element");
    goto ERROR;
  }
  /* a second constant attribute defined with a profile for the first and the
     last segment */
  /* create the profile */
    if (MEDprofileWr(fid, profilename, profilesize, profile ) < 0) {
    MESSAGE("ERROR : create profile ...");
    goto ERROR;
  }
  /* write the constant attribute */
  if (MEDstructElementConstAttWithProfileWr(fid,
					    elementname,
					    "BEAM_EXTREMITIES_LABELS",
					    MED_ATT_NAME,
					    ncomp,
					    MED_CELL,
					    profilename,
					    (void*) attprovalue) < 0) {
    MESSAGE("ERROR : creating struct element");
    goto ERROR;
  }

  /* STEP 3 : Computation mesh creation */

  /* mesh creation */
  if (MEDmeshCr(fid, computmeshname, spacedim, meshdim,  MED_UNSTRUCTURED_MESH,
		"Computation mesh", "s", MED_SORT_DTIT,
		MED_CARTESIAN, axisname, unitname) < 0) {
    MESSAGE("ERROR : creating computation mesh ...");
    goto ERROR;
  }

  /* mesh node creation */
  nnode = 12;
  if (MEDmeshNodeCoordinateWr(fid, computmeshname, MED_NO_DT, MED_NO_IT, MED_UNDEF_DT,
			      MED_FULL_INTERLACE, nnode, meshcoo) < 0) {
    MESSAGE("ERROR : writing nodes coordinates ...");
    goto ERROR;
  }

  /* 1 beam */
  nentity = 1;
  SSCRUTE(elementname);
  geotype = MEDstructElementGeotype(fid,elementname);
  ISCRUTE(geotype);

  if (MEDmeshElementConnectivityWr(fid,computmeshname, MED_NO_DT, MED_NO_IT, MED_UNDEF_DT,
				   MED_STRUCT_ELEMENT, geotype, MED_NODAL,
				   MED_FULL_INTERLACE, nentity, beamconnectivity) < 0 ) {
    MESSAGE("ERROR : beam connectivity ...");
    goto ERROR;
  }

  /* Get the dynamic geometry type of each struct element model.
     Then for each type, write the connectivity and variable(s) attribute(s) */

  /* 3 particles in the mesh */
  strcpy(elementname,MED_PARTICLE_NAME);
  geotype = MEDstructElementGeotype(fid,elementname);
  nentity = 3;
  ISCRUTE(geotype);
  if (MEDmeshElementConnectivityWr(fid, computmeshname, MED_NO_DT, MED_NO_IT, 0.0,
				   MED_STRUCT_ELEMENT, geotype , MED_NODAL, MED_FULL_INTERLACE,
				   nentity, 0) < 0) {
    MESSAGE("ERROR : writing particles connectivity ...");
    goto ERROR;
  }

  /* no support mesh => no connectivity, the particles are localized with an association between
     the mesh nodes and the label attribute defined in the struct model */
  if (MEDmeshStructElementVarAttWr(fid, computmeshname, MED_NO_DT, MED_NO_IT,
				   geotype, MED_PARTICLE_LABEL,
				   nentity, labels) < 0 ) {
    MESSAGE("ERROR : writing variable attributes  ...");
    goto ERROR;
  }


  /* 2 balls */
  strcpy(elementname,MED_BALL_NAME);
  nentity = 2;
  geotype = MEDstructElementGeotype(fid,elementname);
  if (MEDmeshElementConnectivityWr(fid,computmeshname, MED_NO_DT, MED_NO_IT, MED_UNDEF_DT,
				   MED_STRUCT_ELEMENT, geotype, MED_NODAL,
				   MED_FULL_INTERLACE, nentity, ballconnectivity) < 0 ) {
    MESSAGE("ERROR : writing balls connectivity");
    goto ERROR;
  }

  /* variable attribute : write ball diameter */
  if (MEDmeshStructElementVarAttWr(fid, computmeshname, MED_NO_DT, MED_NO_IT,
				   geotype, MED_BALL_DIAMETER,
				   nentity, balldiameter) < 0 ) {
    MESSAGE("ERROR : writing variable attributes ...");
    goto ERROR;
  }


  ret=0;
 ERROR:

  /* close file */
  if (MEDfileClose(fid) < 0) {
    MESSAGE("ERROR : file closing");
    ret=-1;
  }

  return ret;
}

