"""@author TELEMAC-MASCARET Consortium

   Manipulation of Telemac files (mesh, results files)
"""
from os import path, remove
from datetime import datetime
from collections import OrderedDict

import numpy as np

from data_manip.extraction.telemac_file import TelemacFile
from data_manip.conversion import convert_utm as utm
from utils.exceptions import TelemacException
from pretel.meshes import needs_double_precision

MERGE_KINDS = ['time', 'var']

def scan(tel_file, bnd_file, data):
    """
    Ascii dump of content of TelemacFile

    @param tel_file (str) Name of the telemac file
    @param bnd_file (str) Name of the boundary file
    @param data (boolean) If True display data information as well
    """

    res = TelemacFile(tel_file, bnd_file=bnd_file)

    res.print_info(full=data)

def diff(tel_file1, tel_file2, diff_file):
    """
    Creates a file that is the difference between two files (file1 - file2) of
    the same shape (same mesh same number of variable and same number of
    records)

    @param tel_file1 (str) Name of the first file
    @param tel_file1 (str) Name of the second file
    @param diff_file (str) Name of the file containing the difference
    """
    res1 = TelemacFile(tel_file1)
    res2 = TelemacFile(tel_file2)
    if path.exists(diff_file):
        print(" ~> Deleting eixisting diff_file: {}".format(diff_file))
        remove(diff_file)
    dif = TelemacFile(diff_file, access='w')

    res1.read()
    res2.read()

    if res1._values.shape != res2._values.shape:
        raise TelemacException(
            "Different shape between files (ntimestep, nvar, npoin):\n"
            "{}: {}\n{}: {}"
            .format(tel_file1, res1._values.shape,
                    tel_file2, res2._values.shape))

    dif.read(res1)

    for time in range(res1.ntimestep):
        for var in range(res1.nvar):
            dif._values[time, var, :] -= res2._values[time, var, :]

    dif.write()

def diff_ascii(tel_file1, tel_file2, epsilon):
    """
    print a summary of the diff between two files

    @param tel_file1 (str) Path of file1
    @param tel_file2 (str) Path of file2
    @param epsilon (float) Threshold below which two values are consireded
    equals
    """

    res1 = TelemacFile(tel_file1)
    fname1 = path.basename(res1.file_name)
    res2 = TelemacFile(tel_file2)
    fname2 = path.basename(res2.file_name)

    failed = False

    print('\n\nHeader differences: \n'+72*'~'+'\n')

    # ~~> File formats
    if res1.endian != res2.endian:
        print('\n  <> File endianess:\n')
        print('     + {} is in {}'.format(fname1, res1.endian))
        print('     + {} is in {}'.format(fname2, res2.endian))
    if res1.fformat != res2.fformat:
        print('\n  <> File formats:\n')
        precision = "SINGLE" if res1 in ['SERAFIN'] else "DOUBLE"
        print('     + {} is {} PRECISION'\
              .format(fname1, precision))
        precision = "SINGLE" if res2 in ['SERAFIN'] else "DOUBLE"
        print('     + {} is {} PRECISION'\
              .format(fname2, precision))

    # ~~> File geometries
    mes = ''
    if res1.nelem2 != res2.nelem2:
        mes = mes + '     + nelem2 = {} in {}\n'.format(res1.nelem2, fname1)
        mes = mes + '     * nelem2 = {} in {}\n'.format(res2.nelem2, fname2)
        mes = mes + '\n'
    if res1.npoin2 != res2.npoin2:
        mes = mes + '     + npoin2 = {} in {}\n'.format(res1.npoin2, fname1)
        mes = mes + '     * npoin2 = {} in {}\n'.format(res2.npoin2, fname2)
        mes = mes + '\n'
    if res1.nplan != res2.nplan:
        mes = mes + '     + nplan = {} in {}\n'.format(res1.nplan, fname1)
        mes = mes + '     * nplan = {} in {}\n'.format(res2.nplan, fname2)
        mes = mes + '\n'
    if mes != '':
        print('\n  <> Geometry:\n'+mes)
        failed = True
        print('\n  /!\\different geometries. The files are not comparables.\n')
        return failed

    # ~~> File trangulations
    diff1 = res1.ikle2 - res2.ikle2
    if np.argwhere(diff1 > [0, 0, 0]):
        print('\n  <> 2D Triangulation:\n')
        print('     + number of mismatches: '+\
                repr(len(np.argwhere(diff1 == [0, 0, 0]).T[0])))
        print('     + mismatched elements: '+\
                repr(np.argwhere(diff1 == [0, 0, 0]).T[0][::3]))

    # ~~> File geo-localisation
    diff1 = np.sqrt(np.power((res1.meshx-res2.meshx), 2) + \
                    np.power((res1.meshy-res2.meshy), 2))
    points = np.argwhere(diff1 > epsilon).ravel()
    if points:
        print('\n  <> Geo-Localisation:\n')
        print('     + maximum distance between points : '+\
              str(max(diff1[points])))
        pt_th = 100*len(points)/len(diff1)
        print('     + number of points above clocelyness threshold : '+\
              str(len(points))+' (i.points. '+str(pt_th)+'% of points)')
        print('     + node numbers : '+
              repr(np.arange(res1.npoin3)[points]))

    # ~~> File contents

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Data differences ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    var_failed = True
    mes = '\n  <> List of variable names:\n'
    cmn_vars = []
    mes = mes + '\n     + '+fname1
    for var in res1.varnames:
        if var in res2.varnames:
            mes = mes + '\n        = '+var
            cmn_vars.append(var)
        else:
            mes = mes + '\n        * '+var
            var_failed = True
    mes = mes + '\n     + '+fname2
    for var in res2.varnames:
        if var in res1.varnames:
            mes = mes + '\n        = '+var
        else:
            mes = mes + '\n        * '+var
            var_failed = True
    print(mes)

    if not cmn_vars:
        failed = True
        print('\n  /!\\ no common variables. The files are not comparables.\n')

    # ~~> File reference dates and times
    if max(np.array(res1.datetime) - np.array(res2.datetime)) > 0:
        print('\n  <> Different reference dates:')
        print('     + {}: {}'.format(fname1, res1.datetime))
        print('     + {}: {}'.format(fname2, res2.datetime))

    # ~~> File time frames
    time_failed = False
    mes = '\n  <> List of time frames:\n'
    if res1.ntimestep != res2.ntimestep:
        print('\n  <> Different Number of time steps:')
        print('     + {}: {}'.format(fname1, res1.ntimestep))
        print('     + {}: {}'.format(fname2, res2.ntimestep))
        time_failed = True
    # ~~> correct if not bypassed
    diff1 = np.setdiff1d(res1.times, res2.times)
    if diff1.size != 0:
        time_failed = True
        mes = mes + '\n     + frames only in '+fname1+' : '+\
                ', '.join(['{0:.2f}'.format(i) for i in diff1])
    diff1 = np.setdiff1d(res2.times, res1.times)
    if diff1.size != 0:
        time_failed = True
        mes = mes + '\n     + frames only in '+fname2+' : '+\
                ', '.join(['{0:.2f}'.format(i) for i in diff1])
    times = np.intersect1d(res1.times, res2.times)
    if times.size != 0:
        mes = mes + '\n     + frames in both files: '+\
              ', '.join([str(i) for i in times])
        print(mes)
    else:
        failed = True
        print('\n  /!\\ no common time frames. '\
                'The files are not comparables.\n')
        return failed

    if failed:
        return failed

    print('\n\nData differences: \n'+72*'~'+'\n')

    # Do compraison for common times and variables
    data_failed = False
    for time in times:
        itime1 = np.where(res1.times == time)[0][0]
        itime2 = np.where(res2.times == time)[0][0]
        for var in cmn_vars:
            var1 = res1.get_data_value(var, itime1)
            var2 = res2.get_data_value(var, itime2)
            time1 = res1.times[itime1]
            time2 = res2.times[itime2]
            diff1 = np.absolute(var1 - var2).ravel()
            points = np.argwhere(diff1 > epsilon).ravel()
            if points.size != 0:
                data_failed = True
                print("\n  <> Frame: {itime1} / {itime2} (times: {time1} "
                      "/ {time2}), Variable: {var}\n"\
                      .format(itime1=itime1,
                              itime2=itime2,
                              time1=time1,
                              time2=time2,
                              var=var))
                print('     + max difference: ', max(diff1[points]))
                print('     + number of values above threshold : '+
                      str(len(points))+' (i.points. '+
                      str(100*len(points)/len(diff1))+
                      '% of points)')
                print('     + node numbers :          '+
                      repr(np.arange(res1.npoin3)[points]))
                print('     + values at those nodes : '+
                      repr(diff1[np.arange(res1.npoin3)[points]]))
    if not data_failed:
        print('  <> No differences in data to the epsilon: {}'.format(epsilon))

    return data_failed or var_failed or time_failed or failed


def alter(input_file, output_file,
          bnd_file=None,
          title=None, in_datetime=None,
          toogle_precision=False,
          toogle_endian=False,
          add_x=0., mul_x=1., add_y=0., mul_y=1.,
          orig=[0, 0],
          rotate=None, rot_pt=[0, 0], center=False,
          proj=None,
          sph2ll=False, ll2sph=False,
          longitude=None, latitude=None,
          utm2ll=False, ll2utm=False,
          zone=None, zone_letter=None,
          in_vars=None, rename_var=None,
          modif_var=None, add_var=0., mul_var=1.,
          times=None,
          tfrom=0, tstep=1, tend=-1,
          reset_time=False, add_time=0., mul_time=1.,
          auto_precision=True,
          force=False):
    """
    Apply modifications to a TelemacFile to create a new one

    @param input_file (str) Input file
    @param output_file (str) Output file
    @param bnd_file (str) Boundary file
    @param title (str) Title to set
    @param in_datetime (str) Date and time to set (format: YYYY-MM-DD HH:MM:SS)
    @param input_file (str) Input file
    @param toogle_precision (bool) If True will invert precision
    @param toogle_endian (bool) If True will invert endianess
    @param add_x (float) Value added to X coordinates (X=mul_x*X+add_x)
    @param mul_x (float) Value multiplicated to X coordinates (X=mul_x*X+add_x)
    @param add_y (float) Value added to Y coordinates (Y=mul_y*Y+add_y)
    @param mul_y (float) Value multiplicated to Y coordinates (Y=mul_y*Y+add_y)
    @param orig (int, int) Origin of coordinates (x_orig, y_orig in file)
    @param rotate (float) Angle for rotation (counter clockwise) in degree
    @param rot_pt (float, float) Coordinates of the point arond which to do the
    rotation (default (0,0))
    @param center (bool) If True will use center of the mesh as rotation point
    @param sph2ll (bool) Converter coordinates from spherical to long/lat
    @param ll2sph (bool) Converter coordinates from long/lat to spherical
    @param longitude (int) longitude for conversion
    @param latitude (int) atitude for conversion
    @param utm2ll (bool) Converter coordinates from UTM to long/lat
    @param ll2utm (bool) Converter coordinates from long/lat to UTM
    @param zone (int) zone for UTM conversion
    @param zone_letter (int) zone letter for UTM conversion
    @param in_vars (str) List of variables to keep in file ("," separated)
    @param rename_var (str) Command to rename a variable
                            (format: old_name=new_name)
    @param modif_var (str) Name of the variable to modify
                           (VAR=mul_var*VAR+add_var)
    @param add_var (float) Value added to variable
    @param mul_var (float) Value multiplicated to variable
    @param times (int) list of times to add in the output file
    @param tfrom (int) First record to add in output file
    @param tstep (int) Step between each record to add in output file
    @param tend (int) Last Record to add in output file
    @param reset_time (bool) If True reset first record time to zero and
                             substract its value to all the other times
    @param add_time (float) Value added to times (T=mul_time*T+add_time)
    @param mul_time (float) Value multiplicated to times (T=mul_time*T+add_time)
    @param force (bool) Will remove output file if it already exist
    """
    inres = TelemacFile(input_file, bnd_file=bnd_file)

    fformat = inres.fformat
    if toogle_precision:
        if inres.fformat == 'SERAFIN':
            print(" ~> Switching to double precision")
            fformat = 'SERAFIND'
        elif inres.fformat == 'SERAFIND':
            print(" ~> Switching to single precision")
            fformat = 'SERAFIN'
        else:
            print("Precision can only be switched if the file "
                  "is in SERAFIN or SERAFIND")

    if toogle_endian:
        if inres.fformat not in ['SERAFIN', 'SERAFIND']:
            print("Endianess can only be switched if the file "
                  "is in SERAFIN or SERAFIND")
        else:
            endian = inres.get_endianess()
            if endian == 'LITTLE_ENDIAN':
                print(" ~> Switching to BIG_ENDIAN")
                inres.set_endianess('BIG_ENDIAN')
            else:
                print(" ~> Switching to LITTLE_ENDIAN")
                inres.set_endianess('LITTLE_ENDIAN')

    # Changing title
    if title is None:
        title = inres.title
    else:
        print(" ~> New title: "+title)

    # Changin date
    if in_datetime is not None:
        my_datetime = datetime.strptime(in_datetime, '%Y-%m-%d %H:%M:%S')
        date = [my_datetime.year, my_datetime.month, my_datetime.day,
                my_datetime.hour, my_datetime.minute, my_datetime.second]
        print(" ~> New date: {}".format(my_datetime))
    else:
        date = inres.datetime


    # Defining origin
    out_orig = [inres.x_orig, inres.y_orig]

    if orig != [0, 0]:
        out_orig = orig

    coord_modif = False

    if mul_x != 1. or add_x != 0.:
        print(" ~> modification of coord x: {}*x+{}".format(mul_x, add_x))
        meshx = inres.meshx*mul_x + add_x
        coord_modif = True
    else:
        meshx = inres.meshx

    if mul_y != 1. or add_y != 0.:
        print(" ~> modification of coord y: {}*y+{}".format(mul_y, add_y))
        meshy = inres.meshy*mul_y + add_y
        coord_modif = True
    else:
        meshy = inres.meshy

    if rotate is not None:
        if center:
            rot_x = (np.max(meshx) - np.min(meshx))/2
            rot_y = (np.max(meshy) - np.min(meshy))/2
        else:
            rot_x = rot_pt[0]
            rot_y = rot_pt[1]
        print(" ~> Rotation of {} around ({},{})".format(rotate, rot_x, rot_y))
        angle = rotate * np.pi/180.
        x = np.cos(angle)*(meshx -rot_x) - np.sin(angle)*(meshy -rot_y)
        y = np.sin(angle)*(meshx -rot_x) + np.cos(angle)*(meshy -rot_y)
        meshx = x + rot_x
        meshy = y + rot_y
        coord_modif = True

    if proj is not None:
        try:
            import pyproj
        except ImportError:
            raise TelemacException("Pyproj is mandatory to do projection")

        projs = proj.split("/")
        if len(projs) != 2:
            raise TelemacException("Wrong format for projection "
                                   "it should de CRS1/CRS2")
        in_proj = pyproj.CRS(projs[0])
        out_proj = pyproj.CRS(projs[1])
        print(" ~> Projecting from {} to {}".format(in_proj.name,
                                                    out_proj.name))
        meshx, meshy = pyproj.transform(in_proj, out_proj, meshx, meshy)
        coord_modif = True

    # converter mesh coordinates
    if sph2ll:
        print(" ~> Converting from spherical to Longitude/Latitude")
        meshx, meshy = spherical2longlat(meshx, meshy, longitude, latitude)
        coord_modif = True
    if ll2sph:
        print(" ~> Converting from Longitude/Latitude to spherical")
        meshx, meshy = longlat2spherical(meshx, meshy, longitude, latitude)
        coord_modif = True
    if ll2utm:
        print(" ~> Converting from Longitude/Latitude to UTM")
        meshx, meshy = longlat2utm(meshx, meshy, zone, zone_letter)
        coord_modif = True
    if utm2ll:
        print(" ~> Converting from UTM to Longitude/Latitude")
        meshx, meshy = utm2longlat(meshx, meshy, zone, zone_letter)
        coord_modif = True

    if coord_modif and fformat == "SERAFIN" and auto_precision:
        is_double = needs_double_precision(meshx, meshy, inres.ikle3)
        if is_double:
            print("WARNING:")
            print("Changing format to SERAFIND because segment are too small")
            fformat = "SERAFIND"

    # List of variable to keep vars (, separated)
    if in_vars is not None:
        out_vars = []
        in_varnames = in_vars.split(",")
        print(" ~> Modif in variables:")
        for name, unit in zip(inres.varnames, inres.varunits):
            if name in in_varnames:
                print(" "*4+"Using {} {}".format(name, unit))
                out_vars.append((name, unit))
            else:
                print(" "*4+"Removing {} {}".format(name, unit))
        in_vars = out_vars
    else:
        in_vars = list(zip(inres.varnames, inres.varunits))
        out_vars = list(zip(inres.varnames, inres.varunits))

    # Renaming variable
    if rename_var is not None:
        old_name, new_name = rename_var.split("=")
        print("{} -> {}".format(old_name, new_name))

        tmp_vars = out_vars
        out_vars = []
        renamed = False
        for name, unit in tmp_vars:
            name2 = name.replace(old_name, new_name)
            if name2 != name:
                renamed = True
                print(" ~> Renaming {} -> {}".format(name, name2))
            out_vars.append((name2, unit))
        if not renamed:
            raise TelemacException(
                "Issue with you renaming no variable matched "
                "the conversion:\n{}".format(rename_var))


    # Records to keep (from, stop, step)
    if times is not None:
        out_records = []
        for itime, in_time in enumerate(inres.times):
            for time in times:
                if abs(time - in_time) < 1e-06:
                    out_records.append(itime)

        if out_records == []:
            raise TelemacException(\
                "Could not find times matching your input:\n"\
                "files times: {}\n"\
                "Selected times: {}".format(inres.times, times))
    else:
        if tend < 0:
            tend = inres.ntimestep + tend
        if tfrom < 0:
            tfrom = inres.ntimestep + tfrom

        out_records = range(tfrom, tend+1, tstep)

    # Build times array
    out_times = inres.times[out_records]

    if len(out_times) != inres.ntimestep:
        records = ["{}".format(i) for i in out_records]
        print(" ~> New range of records: [{}]".format(", ".join(records)))
        print(" ~> New range of times: {}".format(out_times))

    if reset_time:
        out_times -= out_times[0]
        print(" ~> reseting time from 0: {}".format(out_times))

    out_times = out_times*mul_time + add_time

    if mul_time != 1. or add_time != 0.:
        print(" ~> modification of times: {}*time+{}"\
              .format(mul_time, add_time))

    # Compute out_bnd_file name
    if bnd_file is not None:
        root, ext = path.splitext(output_file)
        out_ext = '.bnd' if fformat == 'MED' else '.cli'
        out_bnd_file = root + out_ext
    else:
        out_bnd_file = None

    if force:
        if path.exists(output_file):
            print(" ~> Removing: "+output_file)
            remove(output_file)
        if bnd_file is not None:
            if path.exists(out_bnd_file):
                print(" ~> Removing: "+out_bnd_file)
                remove(out_bnd_file)

    outres = TelemacFile(output_file, fformat=fformat,
                         bnd_file=out_bnd_file,
                         access='w')

    outres.add_header(title, date)

    outres.add_mesh(meshx, meshy, inres.ikle3,
                    z=None, nplan=inres.nplan,
                    orig=(out_orig[0], out_orig[1]))

    # Adding boundary information
    outres._nptfr = inres.nptfr
    outres._ipob3 = inres.ipob3
    if bnd_file is not None:
        outres._typ_bnd_elem = inres.typ_bnd_elem
        outres._nelebd = inres.nelebd
        outres._bnd_info = inres.bnd_info
        outres._ikle_bnd = inres.ikle_bnd
        outres._nbor = inres.nbor

    # Time steps must be added first otherwise an aditional record will be
    # added when we add a variable and there a re no time step
    for time in out_times:
        outres.add_time_step(time)
    for name, unit in out_vars:
        outres.add_variable(name, unit)

    for i, (out_name, _) in enumerate(out_vars):
        out_itime = 0
        in_name, _ = in_vars[i]
        for itime in out_records:
            value = inres.get_data_value(in_name, itime)
            if out_name == modif_var:
                print(" ~> Modifying {} at {:.2f}s: {}*val+{}"\
                      .format(out_name, out_times[out_itime], mul_var, add_var))
                value = value*mul_var + add_var
            outres.add_data_value(out_name, out_itime, value)
            out_itime += 1

    outres.write()

    inres.close()
    outres.close()

    outres = TelemacFile(output_file)


def spherical2longlat(x, y, longitude, latitude):
    """
    convert coordinates from spherical to longitude and latitude

    @param x (np.array) X coordinates
    @param y (np.array) Y coordinates
    @param longitude (float) Longitude
    @param latitude (float) Latitude

    """
    radius = 6371000.
    long0 = np.deg2rad(longitude)
    lat0 = np.deg2rad(latitude)
    const = np.tan(lat0/2. + np.pi/4.)
    meshx = np.rad2deg(x/radius + long0)
    expo = np.exp(y/radius)
    meshy = np.rad2deg(2.*np.arctan(const*expo) - np.pi/2.)

    return meshx, meshy

def longlat2spherical(x, y, longitude, latitude):
    """
    convert coordinates from Longitude and latitude to Spherical

    @param x (np.array) X coordinates
    @param y (np.array) Y coordinates
    @param longitude (float) Longitude
    @param latitude (float) Latitude

    """
    radius = 6371000.
    long0 = np.deg2rad(longitude)
    lat0 = np.deg2rad(latitude)
    meshx = radius * (np.deg2rad(x) - long0)
    meshy = radius * \
              (np.log(np.tan(np.deg2rad(y)/2. + np.pi/4.)) \
                      - np.log(np.tan(lat0/2. + np.pi/4.)))

    return meshx, meshy

def longlat2utm(x, y, zone=None, zone_letter=None):
    """
    convert coordinates from Longitude and latitude to UTM

    @param x (np.array) X coordinates
    @param y (np.array) Y coordinates
    @param zone (float) Zone
    @param zone_letter (float) Zone letter

    @returns (x, y) converted coordinates
    """

    meshx, meshy, zone, zone_letter = \
              utm.from_latlon(x, y,
                              force_zone_number=zone,
                              force_zone_letter=zone_letter)
    return meshx, meshy

def utm2longlat(x, y, zone, zone_letter):
    """
    convert coordinates from UTM to Longitude and latitude

    @param x (np.array) X coordinates
    @param y (np.array) Y coordinates
    @param zone (float) Zone
    @param zone_letter (float) Zone letter

    @returns (x, y) converted coordinates
    """
    meshx, meshy = \
              utm.to_latlon(x, y,
                            zone, zone_letter)

    return meshx, meshy

def merge(input_files, output_file, bnd_file, kind='time', force=False):
    """
    Merge time or variable records of several files into one.

    @param input_files (list) List of input files to merge
    @param bnd_file (str) Boundary file
    @param output_file (sre) Name of output file
    @param kind (str) - 'time' All files must have the same
                        variables and it will merge the times (in incresing
                        order)
                      - 'var' All files must have the same times ands it will
                        merge the variables
    @param force (bool) Will remove output file if it already exist
    """

    # First check that the mesh are identical
    in_ress = OrderedDict()
    in_res0 = TelemacFile(input_files[0], bnd_file=bnd_file)

    for ffile in input_files[1:]:
        in_ress[ffile] = TelemacFile(ffile)

    npoin3 = in_res0.npoin3
    nelem3 = in_res0.nelem3
    ndp3 = in_res0.ndp3
    ikle3 = in_res0.ikle3

    for ffile, in_res in in_ress.items():
        if in_res.npoin3 != npoin3:
            raise TelemacException(
                "Not the same number of points in {}:{} should be {}"\
                .format(ffile, in_res.npoin3, npoin3))
        if in_res.nelem3 != nelem3:
            raise TelemacException(
                "Not the same number of elements in {}:{} should be {}"\
                .format(ffile, in_res.nelem3, nelem3))
        if in_res.ndp3 != ndp3:
            raise TelemacException(
                "Not the same number of points in {}:{} should be {}"\
                .format(ffile, in_res.ndp3, ndp3))
        if np.any(in_res.ikle3 != ikle3):
            raise TelemacException(
                "Not the same connectivity in {}".format(ffile))

    if kind == 'time':
        # Check that we have the same variables
        varnames = in_res0.varnames
        for ffile, in_res in in_ress.items():
            if in_res.varnames != varnames:
                raise TelemacException(
                    "Not the same variables in {}:{} should be {}"\
                    .format(ffile, in_res.varnames, varnames))
    elif kind == 'var':
        # Check that we have the same times
        times = in_res0.times
        for ffile, in_res in in_ress.items():
            if np.any(in_res.times != times):
                raise TelemacException(
                    "Not the same times in {}:{} should be {}"\
                    .format(ffile, in_res.times, times))
    else:
        raise TelemacException(\
            "Unknown option for kind: {} should be within {}"
            .format(kind, MERGE_KINDS))

    if force:
        if path.exists(output_file):
            print(" ~> Removing: "+output_file)
            remove(output_file)

    out_res = TelemacFile(output_file, access='w')

    out_res.read_mesh(in_res0)

    if kind == 'var':
        out_vars = OrderedDict()
        # Initialize with variables from in_res0
        for var, unit in zip(in_res0.varnames, in_res0.varunits):
            out_vars[var] = (unit, in_res0)

        # Identify new variables with their corresponding file
        for in_res in in_ress.values():
            for var, unit in zip(in_res.varnames, in_res.varunits):
                if var not in out_vars:
                    out_vars[var] = (unit, in_res)

        # Times dont matter here
        out_times = []
        for record, time in enumerate(in_res0.times):
            out_times.append((time, record, None))

    elif kind == 'time':
        time_list = []

        # Initialize with times from in_res0
        out_times = []
        for record, time in enumerate(in_res0.times):
            out_times.append((time, record, in_res0))
            time_list.append(time)

        # Append time records from other files
        for in_res in in_ress.values():
            for record, time in enumerate(in_res.times):
                if time not in time_list:
                    out_times.append((time, record, in_res))
                    time_list.append(time)
                else:
                    print(
                        f"Skipping duplicate time {time} from {in_res.file_name}")

        # Sort time records
        out_times.sort(key=lambda t: t[0])

        # Var dont matter here
        out_vars = OrderedDict()
        for var, unit in zip(in_res0.varnames, in_res0.varunits):
            out_vars[var] = (unit, None)
    else:
        raise TelemacException(
            f"Unknown option for kind: {kind} should be within {MERGE_KINDS}")

    for time, _, _ in out_times:
        out_res.add_time_step(time)
    for var, (unit, _) in out_vars.items():
        out_res.add_variable(var, unit)

    for out_record, (time, record, time_res) in enumerate(out_times):
        if kind == 'time':
            in_res = time_res
        for var, (unit, var_res) in out_vars.items():
            if kind == 'var':
                in_res = var_res

            value = in_res.get_data_value(var, record)
            out_res.add_data_value(var, out_record, value)

    out_res.write()

    if bnd_file is not None:
        out_res.import_group_info(in_res0)

    out_res.close()
    in_res0.close()
    for in_res in in_ress.values():
        in_res.close()
