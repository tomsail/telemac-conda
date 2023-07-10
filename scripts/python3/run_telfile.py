#!/usr/bin/env python3
"""@author TELEMAC-MASCARET Consortium

   Manipulation of Telemac files (mesh, results files)
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
from argparse import ArgumentParser
from os import path
# ~~> dependencies towards other modules
from data_manip.computation.amp2wave import amp2wave
from pretel.compute_weight import connect_tel2tom
from pretel.extract_contour import extract_contour, write_gis_file
from pretel.clc import add_clc_in_file
from pretel.manip_telfile import scan, diff, diff_ascii, alter, merge, \
                                 MERGE_KINDS
from utils.exceptions import TelemacException

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

def amp2wave_parser(subparser):
    """
    Add arguments for a conversion of amp2wave

    @param subparser (argumentParser) argument parser

    @return (argumentParser) the updated argument parser
    """
    parser = subparser.add_parser(\
        'amp2wave',
        help='Computing wave surface file from amplitude ans phase file')
    parser.add_argument(
        "amp_file",
        help="path of the amplitude and phase file.")
    parser.add_argument(
        "wave_file",
        help="path of the wave file.")
    parser.add_argument(
        "--start-time", default=2066.07, type=float,
        help="Time from which the wave file will start.")
    parser.add_argument(
        "--time-step", default=0.34, type=float,
        help="Time step for the wave file.")
    parser.add_argument(
        "--end-time", default=2018.75, type=float,
        help="Time for which the wave file will end.")
    parser.add_argument(
        "--force", default=False, action='store_true',
        help="Overwrite wave file if it exists")

    return subparser

def tel2tom_parser(subparser):
    """ Generate parser for tel2tom """
    parser = subparser.add_parser('tel2tom',\
            help="Compute weight for each node and index to interpolate from "
                 "telemac2d file to tomawac file and reverse")
    parser.add_argument("t2d_file", metavar="Telemac File", help="First file")
    parser.add_argument("--t2d-bnd", metavar="Telemac Boundary File",
                        required=True,
                        help="First file boundary file")

    parser.add_argument("tom_file", metavar="Tomawac File", help="Second file")
    parser.add_argument("--tom-bnd", metavar="Tomawac Boundary File",
                        required=True,
                        help="Second file boundary file")

    parser.add_argument("--t2d-contour", default=None,
                        help="Polygon to apply to Telemac2d file")
    parser.add_argument("--tom-contour", default=None,
                        help="Polygon to apply to Tomawac file")

    return subparser

def contour_parser(subparser):
    """ Generate parser for extract_contour """
    parser = subparser.add_parser('contour',\
            help="Compute contour (in Shape file format)from mesh file")
    parser.add_argument("mesh_file", metavar="Telemac File", help="Mesh file")
    parser.add_argument("shp_file", help="Shape file")
    parser.add_argument("-b", "--bnd-file", default=None, help="Boundary file")

    return subparser

def clc_parser(subparser):
    """ Generate parser for extract_contour """
    parser = subparser.add_parser('clc',\
            help="Compute Corinne Land Cover data on mesh")
    parser.add_argument("mesh_file", metavar="Telemac File", help="Mesh file")
    parser.add_argument(
        "-v", dest="varname", default="FRICTION",
        help="Name of the variable containing Corinne Land Cover data "
             "(default: FRICTION)")
    parser.add_argument("--year", default=2012,
                        choices=[2012],
                        help="Year to extract data from")
    parser.add_argument("-o", dest="res_file", metavar="Telemac File",
                        help="Output file if not given will write in mesh_file")

    return subparser

def scan_parser(subparser):
    """ Generate parser for scan """
    parser = subparser.add_parser('scan',\
            help="Dumping file information")
    parser.add_argument("tel_file", metavar="Telemac File", help="File to scan")
    parser.add_argument(
        "-b", "--boundary_file", dest="bnd_file",
        metavar="Telemac Boundary File", help="Boundary file")
    parser.add_argument(
        "--data", default=False, action='store_true',
        help="Display data information")

    return subparser

def diff_parser(subparser):
    """ Generate parser for diff """
    parser = subparser.add_parser('diff',\
        help="Creates a file containing the difference between two other files")
    parser.add_argument("tel_file1", help="File1")
    parser.add_argument("tel_file2", help="File2")
    parser.add_argument("diff_file", default=None, help="File2-file1")

    return subparser

def diff_ascii_parser(subparser):
    """ Generate parser for diff """
    parser = subparser.add_parser('diff_ascii',\
        help="Print comparaison between two files")
    parser.add_argument("tel_file1", help="File1")
    parser.add_argument("tel_file2", help="File2")

    parser.add_argument(\
        "-e", "--epsilon", dest="epsilon", type=float, default="0.",
        help="Threshold below which values are considered equals")

    return subparser

def alter_parser(subparser):
    """ Generate parser for alter """
    parser = subparser.add_parser('alter',\
        help="Makes modifications "
             "(precision;endian,renaming variables,translate...) "
             "to a TELEMAC-MASCARET file")
    parser.add_argument("input_file", help="Input file")
    parser.add_argument("output_file", help="Altered file")
    parser.add_argument("-b", dest="bnd_file", help="Boundary_file")

    parser.add_argument("--force", action='store_true',
                        help="Remove output file if it exists")


    # toogle precision
    group = parser.add_argument_group('Format modification')
    group.add_argument(\
        "--float", action="store_true",
        dest="toogle_precision", default=False,
        help="switch between DOUBLE and SINGLE precision float")

    # toogle endianess
    group.add_argument(\
        "--endian", action="store_true",
        dest="toogle_endian", default=False,
        help="switch between endian encoddings")

    # Title
    group = parser.add_argument_group('Header modification')
    group.add_argument(\
        "--title",
        dest="title", default=None,
        help="set the title of the file")

    # Date and time
    group = parser.add_argument_group('Date modification')
    group.add_argument(\
        "--datetime",
        dest="datetime", type=str, default=None,
        help="set the start date of the file (yyyy-mm-dd hh:mm:ss)")

    # Variables
    group = parser.add_argument_group('Variable modification')
    group.add_argument(\
        "--vars",
        dest="xvars", default=None,
        help="specify which variables should remain (','-delimited)")

    group.add_argument(\
        "--rename",
        dest="rename_var", default=None,
        help="change the name of a VARIABLE: 'OLD VAR=NEW VAR'")

    group.add_argument(\
        "--var?",
        dest="modif_var", default=None,
        help="will modify var? ((var*? * val )+ var+?) "\
             "operations on that VARIABLE name")
    group.add_argument(\
        "--var+?",
        dest="add_var", type=float, default=0.,
        help="adds to the VARIABLE")
    group.add_argument(\
        "--var*?",
        dest="mul_var", type=float, default=1.,
        help="scales the VARIABLE")

    # Time
    group = parser.add_argument_group('Time modification')
    group.add_argument(\
        "-t", "--times",
        dest="times", type=float, default=None,
        nargs="+",
        help="specify the first frame included")
    group.add_argument(\
        "-f", "--from",
        dest="tfrom", type=int, default=0,
        help="specify the first frame included")
    group.add_argument(\
        "-s", "--stop",
        dest="tend", type=int, default=-1,
        help="specify the last frame included "
             "(negative from the end)")
    group.add_argument(\
        "-d", "--step",
        dest="tstep", type=int, default=1,
        help="specify the step for the extraction of frames")

    group.add_argument(\
        "--reset-time", action="store_true",
        dest="reset_time", default=False,
        help="reset first time to zero by remove t0 to all times")

    group.add_argument(\
        "--T+?",
        dest="add_time", type=float, default=0.,
        help="adds to the ATs")
    group.add_argument(\
        "--T*?",
        dest="mul_time", type=float, default=1.,
        help="scales the ATs")


    group = parser.add_argument_group('Coordinates modification')
    # Change coordinates
    group.add_argument(\
        "--sph2ll",
        dest="sph2ll", type=int, default=[None, None], nargs=2,
        help="convert from spherical to longitude-latitude")
    group.add_argument(\
        "--ll2sph",
        dest="ll2sph", type=int, default=[None, None], nargs=2,
        help="convert from longitude-latitude to spherical")
    group.add_argument(\
        "--ll2utm",
        dest="ll2utm", default=None,
        help="zone info convert from longitude-latitude to UTM (giving XXX "
        "will let the script pick the zone otherwise specify zone with "
        "number + letter (for example 24S))")
    group.add_argument(\
        "--utm2ll",
        dest="utm2ll", default=None,
        help="zone info to convert from UTM to longitude-latitude (for "
        "example 24S)")


    group.add_argument(\
        "--X+?",
        dest="add_x", type=float, default=0.,
        help="adds to the meshx")
    group.add_argument(\
        "--X*?",
        dest="mul_x", type=float, default=1.,
        help="scales the meshx")
    group.add_argument(\
        "--Y+?",
        dest="add_y", type=float, default=0.,
        help="adds to the meshy")
    group.add_argument(\
        "--Y*?",
        dest="mul_y", type=float, default=1.,
        help="scales the meshy")

    group.add_argument(\
        "--orig",
        dest="orig", type=int, default=[0, 0], nargs=2,
        help="Origin of coordinates (X_ORIG, Y_ORIG in file)")

    group.add_argument(\
            "--rotate",
            dest="rotate", type=float,
            default=None,
            help="Angle of rotation (counter clockwise) to apply in degree")
    group.add_argument(\
            "--rotation-point",
            dest="rot_pt", type=float, nargs=2,
            default=[0, 0],
            help="Point around which to do the rotation")
    group.add_argument(\
        "--center", action="store_true",
        dest="center", default=False,
        help="Apply rotation with center of mesh as rotation point")

    group.add_argument(\
        "--proj",
        dest="proj", default=None,
        help="Proj using pyproj format "
             "(CRSCODE1/CRSCODE2 for example EPSG:4326/EPSG:27561)")

    group.add_argument(\
        "--disable-auto-precision",
        dest="no_auto_pre", default=False,
        action="store_true",
        help="Do not correct SERAFIN to SERAFIND if the coordinates "
             "demand it")

    return subparser

def merge_parser(subparser):
    """ Generate parser for merge """
    parser = subparser.add_parser('merge',\
            help="Merge files into one file "
                 "(time: merge times, var: merge var)")
    parser.add_argument("input_files", nargs='+', help="Files to merge")
    parser.add_argument("output_file", help="Merged file")

    parser.add_argument(\
        "-k", "--kind", dest="kind", default='time', choices=MERGE_KINDS,
        help="Type of merge: {}".format(MERGE_KINDS))

    parser.add_argument("-b", dest="bnd_file", help="Boundary_file")

    parser.add_argument("--force", action='store_true',
                        help="Remove output file if it exists")


    return subparser

def main():
    """
    Main function
    """

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    print('\n\nInterpreting command line options\n'+'~'*72+'\n')

    parser = ArgumentParser()
    subparser = parser.add_subparsers(\
            help='run_telfile commands to do', dest='command')

    subparser = tel2tom_parser(subparser)
    subparser = amp2wave_parser(subparser)
    subparser = contour_parser(subparser)
    subparser = clc_parser(subparser)
    subparser = scan_parser(subparser)
    subparser = diff_parser(subparser)
    subparser = diff_ascii_parser(subparser)
    subparser = alter_parser(subparser)
    subparser = merge_parser(subparser)

    options = parser.parse_args()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads code name ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if options.command == 'tel2tom':
        connect_tel2tom(options.t2d_file, options.tom_file,
                        tel_bnd=options.t2d_bnd,
                        tom_bnd=options.tom_bnd,
                        contour_tom=options.tom_contour,
                        contour_tel=options.t2d_contour)
    elif options.command == 'contour':
        domains_bnd = extract_contour(options.mesh_file, \
                        bnd_file=options.bnd_file)

        write_gis_file(domains_bnd, options.shp_file)
    elif options.command == 'clc':
        add_clc_in_file(options.mesh_file, options.varname, options.year,
                        options.res_file)
    elif options.command == 'scan':
        scan(options.tel_file, options.bnd_file, options.data)
    elif options.command == 'diff':
        diff(options.tel_file1, options.tel_file2, options.diff_file)
    elif options.command == 'diff_ascii':
        is_diff = diff_ascii(options.tel_file1, options.tel_file2,
                             options.epsilon)
        cmp_str = "differents" if is_diff else "identical"
        print("\n   The files are {} for eps={}\n"\
              .format(cmp_str, options.epsilon))
    elif options.command == 'alter':
        if options.sph2ll != [None, None]:
            sph2ll = True
            longitude = options.sph[0]
            latitude = options.sph[1]
        else:
            sph2ll = False
            longitude = None
            latitude = None

        if options.ll2sph != [None, None]:
            ll2sph = True
            longitude = options.sph[0]
            latitude = options.sph[1]
        else:
            ll2sph = False
            longitude = None
            latitude = None

        if options.ll2utm is not None:
            ll2utm = True
            zone = int(options.ll2utm[:-1])
            zone_letter = options.ll2utm[-1]
        else:
            ll2utm = False
            zone = None
            zone_letter = None

        if options.utm2ll is not None:
            utm2ll = True
            zone = int(options.utm2ll[:-1])
            zone_letter = options.utm2ll[-1]
        else:
            utm2ll = False
            zone = None
            zone_letter = None

        alter(options.input_file, options.output_file,
              bnd_file=options.bnd_file,
              title=options.title, in_datetime=options.datetime,
              toogle_precision=options.toogle_precision,
              toogle_endian=options.toogle_endian,
              add_x=options.add_x, mul_x=options.mul_x,
              add_y=options.add_y, mul_y=options.mul_y,
              orig=options.orig,
              rotate=options.rotate, rot_pt=options.rot_pt,
              center=options.center,
              proj=options.proj,
              sph2ll=sph2ll, ll2sph=ll2sph,
              longitude=longitude, latitude=latitude,
              utm2ll=utm2ll, ll2utm=ll2utm,
              zone=zone, zone_letter=zone_letter,
              in_vars=options.xvars, rename_var=options.rename_var,
              modif_var=options.modif_var,
              add_var=options.add_var, mul_var=options.mul_var,
              tfrom=options.tfrom, tstep=options.tstep, tend=options.tend,
              reset_time=options.reset_time,
              times=options.times,
              add_time=options.add_time, mul_time=options.mul_time,
              auto_precision=not options.no_auto_pre,
              force=options.force)
    elif options.command == 'merge':
        list_files = [path.basename(ffile) for ffile in options.input_files]
        print("  ~> Merging {} into {} kind={}"\
              .format(", ".join(list_files),
                      options.output_file,
                      options.kind))
        merge(options.input_files, options.output_file, options.bnd_file,
              kind=options.kind, force=options.force)
    elif options.command == 'amp2wave':
        amp2wave(options.amp_file, options.wave_file,
                 options.start_time, options.time_step, options.end_time,
                 force=options.force)
    else:
        raise TelemacException(\
            '\nDo not know what to do with '
            'this code name: {}'.format(options.command))

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    print('\n\nMy work is done\n\n')

    sys.exit(0)

if __name__ == "__main__":
    main()
