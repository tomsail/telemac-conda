#!/usr/bin/env python3
"""@author TELEMAC-MASCARET Consortium
   @brief Refine a SERAFIN mesh
"""

import sys
import os
import subprocess as sp

from execution.telemac_cas import TelemacCas
from data_manip.extraction.shapefile_reader import read_shape_data
import numpy as np


def run_refine(input_file, output_file, root_dir, bnd_file, zone_gis_file=None):
    """
    Run a refinement using stbtel

    @param input_file (string) Name of the input file
    @param output_file (string) Name of the output_file
    @param root_dir (string) Path to the root of Telemac
    @param bnd_file (string) Boundary file
    """

    # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    # ~~~~ Identifying input and output informations ~~~~~~~~~~~~~~~~~~~

    # Treatment in case we are doing a refinement
    output_name, _ = os.path.splitext(output_file)
    # Writting the steering file
    cas_name = 'stb.cas'
    if root_dir == None:
        root_dir = os.environ.get('HOMETEL')
        if root_dir == '':
            # If root dir is in neither HOMETEL or options
            # It is computed from the path of the script
            python_dir = os.path.dirname(os.path.realpath(__file__))
            root_dir = os.path.dirname(
                os.path.dirname(os.path.dirname(python_dir)))

    tel_cas = TelemacCas(cas_name,
                         os.path.join(root_dir,
                                      "sources",
                                      "stbtel",
                                      "stbtel.dico"),
                         access='w')

    tel_cas.set("UNIVERSAL FILE", input_file)
    tel_cas.set("BOUNDARY UNIVERSAL FILE", bnd_file)
    tel_cas.set("GEOMETRY FILE FOR TELEMAC", output_name + ".slf")
    tel_cas.set("BOUNDARY CONDITIONS FILE", output_name + ".cli")
    tel_cas.set("MESH GENERATOR", "SELAFIN")
    tel_cas.set("CUTTING ELEMENTS IN FOUR", True)

    if zone_gis_file is not None:
        zone_gis = read_shape_data(zone_gis_file)
        for poly in zone_gis:
            np_poly = np.array(poly)
            zone_x_coord = np_poly[0][:, 0].tolist()[:-1]
            zone_y_coord = np_poly[0][:, 1].tolist()[:-1]

        tel_cas.set(
            "ABSCISSAE OF THE VERTICES OF THE POLYGON TO REFINE THE MESH",
            zone_x_coord)
        tel_cas.set(
            "ORDINATES OF THE VERTICES OF THE POLYGON TO REFINE THE MESH",
            zone_y_coord)
        tel_cas.set(
            "NUMBER OF VERTICES OF THE POLYGON TO REFINE THE MESH",
            len(zone_x_coord))

    tel_cas.write(cas_name)
    del(tel_cas)

    # Running stbtel
    path_stbtel = "stbtel.py"
    if root_dir is not None:
        path_stbtel = os.path.join(root_dir, "scripts",
                                   "python3", path_stbtel)
    stbtel_args = [path_stbtel, cas_name, "--mpi"]
    if root_dir is not None:
        stbtel_args += ["-r", root_dir]
    print("Calling: " + " ".join(stbtel_args))
    if sys.platform == "win32":
        code = sp.call(stbtel_args, shell=True)
    else:
        code = sp.call(stbtel_args)

    if code != 0:
        sys.exit(code)
    else:
        # Remove the case file
        os.remove(cas_name)


def stbtel_refine_parser(subparser):
    """
    Adding argument to parser for stbtel refinment

    @param subparser (ArgumentParser) the parser to update

    @return (ArgumentParser) the updated parser
    """

    parser = subparser.add_parser('refine',
                                  help='Refinment of the mesh using stbtel')
    parser.add_argument(
        "input_file", default="",
        help="name of the input file also defines the input format")
    # output name option
    parser.add_argument(
        dest="output_file", default="",
        help="name of the output file also defines the output format")
    # the boundary file option
    parser.add_argument(
        "-b", "--boundary-file",
        dest="bnd_file", default="",
        help="name of the boundary file")
    # shape file to define a zone of refinement
    parser.add_argument(
        "-zg", "--zone-gis-file",
        dest="zone_gis_file", default=None,
        help="name of the GIS containing one polygon delimiting a local\
              refinement zone")
    # root directory
    parser.add_argument(
        "-r", "--root-dir",
        dest="root_dir", default=None,
        help="specify the root, default is taken from config file")

    return subparser
