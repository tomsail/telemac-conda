
"""
Validation script for pretel
"""
from os import path, remove
import numpy as np

from vvytel.vnv_study import AbstractVnvStudy
from data_manip.extraction.telemac_file import TelemacFile
from pretel.manip_telfile import alter
from utils.exceptions import TelemacException

def create_file_from_scratch(file_name, step=0.5):
    """
    Example of creation of mesh from stracth

    @param (str) file_name Name of the mesh to create
    @param (str) step size of triangle edge
    """
    print("Creating from scratch {}".format(file_name))
    nvar = 1

    points = np.array(\
            [[step*0, step*0],
             [step*2, step*0],
             [step*4, step*0],
             [step*1, step*2],
             [step*3, step*2],
             [step*2, step*4]])
    ikle = [[0, 1, 3],
            [1, 4, 3],
            [1, 2, 4],
            [3, 4, 5],
            ]

    if path.exists(file_name):
        remove(file_name)

    res = TelemacFile(file_name, access='w')
    res.add_header('Zelda triforce', date=[1986, 2, 21, 0, 0, 0])
    res.add_mesh(points[:, 0], points[:, 1], ikle)
    for ivar in range(nvar):
        res.add_variable('variable {}'.format(ivar), '')

    res.write()
    res.close()

def test_title():
    """
    Test of alter title
    """
    print("\n ~> Testing alter title")

    file_name = "r2d_gouttedo.slf"
    out_file = "test-title.slf"

    title = 'Dummy title for test'

    alter(file_name, out_file, title=title, force=True)

    res = TelemacFile(out_file)

    if res.title != title:
        if res.title[:72].strip(' ') != title:
            raise TelemacException(\
                "Test title:\nres: *{}*\nini: *{}*".format(res.title, title))

    res.close()

def test_datetime():
    """
    Test of alter datetime
    """
    print("\n ~> Testing alter datetime")

    file_name = "r2d_gouttedo.slf"
    out_file = "test-datetime.slf"

    date = '1986-06-14 22:11:00'

    alter(file_name, out_file, in_datetime=date, force=True)

    res = TelemacFile(out_file)

    if np.any(res.datetime != [1986, 6, 14, 22, 11, 0]):
        raise TelemacException(\
            "Test datetime:\nres: {}\nini: {}".format(res.datetime, [1986, 6, 14, 22, 11, 0]))

    res.close()

def test_endian():
    """
    Test of alter endian
    """
    print("\n ~> Testing alter endian")

    file_name = "r2d_bowl_vf_gb_DoublePrecision_LittleEndian.slf"
    out_file = "test-endian.slf"

    alter(file_name, out_file, toogle_endian=True, force=True)

    res = TelemacFile(out_file)

    if res.endian != 'BIG_ENDIAN':
        raise TelemacException(\
            "Test endian:\nres: {}\nini: {}".format(res.endian, 'BIG_ENDIAN'))

    res.close()

def test_precision():
    """
    Test of alter precision
    """
    print("\n ~> Testing alter precision double 2 single")

    file_name = "r2d_bowl_vf_gb_DoublePrecision_LittleEndian.slf"
    out_file = "test-precision.slf"

    alter(file_name, out_file, toogle_precision=True, force=True)

    res = TelemacFile(out_file)

    if res.fformat != 'SERAFIN':
        raise TelemacException(\
            "Test alter precision double 2 single failed")

    res.close()

def test_precision2():
    """
    Test of alter precision
    """
    print("\n ~> Testing alter precision single 2 double")

    file_name = "r2d_bowl_vf_gb_SinglePrecision_LittleEndian.slf"
    out_file = "test-precision2.slf"

    alter(file_name, out_file, toogle_precision=True, force=True)

    res = TelemacFile(out_file)

    if res.fformat != 'SERAFIND':
        raise TelemacException(\
            "Test alter precision  single 2 double failed")

    res.close()

def test_auto_precision():
    """
    Test of alter precision
    """
    print("\n ~> Testing alter auto precision")

    file_name = "triforce.slf"
    out_file = "triforce_modif.slf"

    create_file_from_scratch(file_name)

    # First we multiply triforce by 1e-6 this should create a SERAFIND
    alter(file_name, out_file, mul_x=1e-6,
          mul_y=1e-6, force=True)

    res = TelemacFile(out_file)

    if res.fformat != 'SERAFIND':
        raise TelemacException(\
            "Test alter auto precision part 1 failed")

    res.close()

    alter(file_name, out_file, mul_x=1e-4,
          mul_y=1e-4, force=True)

    res = TelemacFile(out_file)

    if res.fformat != 'SERAFIN':
        raise TelemacException(\
            "Test alter auto precision part 2 failed")

    res.close()


def test_modif_coord():
    """
    Test of alter coordinates
    """
    print("\n ~> Testing alter modif_coord")

    file_name = "r2d_bowl_vf_gb_DoublePrecision_BigEndian.slf"
    out_file = "test-coord.slf"

    res = TelemacFile(file_name)
    x = res.meshx
    y = res.meshy
    res.close()
    x = 2.*x + 200.
    y = 3.*y + 20.

    alter(file_name, out_file, add_x=200., mul_x=2.,
          add_y=20., mul_y=3., force=True)

    res = TelemacFile(out_file)

    if np.any(np.abs(res.meshx-x)/max(res.meshx) > 1e-6) or \
       np.any(np.abs(res.meshy-y)/max(res.meshy) > 1e-6):
        print(res.meshx-x)
        print(res.meshy-y)
        raise TelemacException(\
            "Test alter coord failed")

    res.close()

def test_rotate():
    """
    Test of alter rotate
    """
    print("\n ~> Testing alter rotate")

    file_name = "r2d_gouttedo.slf"
    out_file = "test-rotate.slf"

    angle = 90 * np.pi/180.


    res = TelemacFile(file_name)
    x = np.cos(angle)*res.meshx - np.sin(angle)*res.meshy
    y = np.sin(angle)*res.meshx + np.cos(angle)*res.meshy
    res.close()

    alter(file_name, out_file, rotate=90, force=True)

    res = TelemacFile(out_file)

    if np.any(np.abs((res.meshx-x)/max(np.abs(res.meshx))) > 1e-6) or \
       np.any(np.abs((res.meshy-y)/max(np.abs(res.meshy))) > 1e-6):
        print(res.meshx-x)
        print(res.meshy-y)
        raise TelemacException(\
            "Test alter rotate failed")

    res.close()

def test_proj():
    """
    Test of alter proj
    """
    print("\n ~> Testing alter proj")
    try:
        import pyproj
    except ImportError:
        print("Pyproj not Avaialable doing nothing")
        return

    # TODO: Better test that check results
    file_name = "bts3d-v7-latlon.slf"
    out_file = "test-lamber93.slf"

    alter(file_name, out_file, proj="EPSG:4326/EPSG:2154", force=True)

def test_orig():
    """
    Test of alter orig
    """
    print("\n ~> Testing alter orig")


    file_name = "r2d_gouttedo.slf"
    out_file = "test-orig.slf"
    x_orig = 2000
    y_orig = 6000

    alter(file_name, out_file, orig=[x_orig, y_orig], force=True)

    res = TelemacFile(out_file)

    if res.x_orig != x_orig:
        raise TelemacException(\
            "Test x_orig:\nres: *{}*\nini: *{}*".format(res.x_orig, x_orig))
    if res.y_orig != y_orig:
        raise TelemacException(\
            "Test y_orig:\nres: *{}*\nini: *{}*".format(res.y_orig, y_orig))

    res.close()


def test_ll2utm():
    """
    Test of alter ll2utm
    """
    print("\n ~> Testing alter ll2utm")

    # TODO: Better test that check results
    file_name = "bts3d-v7-latlon.slf"
    out_file = "test-ll2utm.slf"

    alter(file_name, out_file, ll2utm=True, force=True)

def test_utm2ll():
    """
    Test of alter utm2ll
    """
    print("\n ~> Testing alter utm2ll")

    # TODO: Better test that check results
    file_name = "bts3d-v7-utm.slf"
    out_file = "test-utm2ll.slf"

    alter(file_name, out_file, utm2ll=True, zone=28, zone_letter='H',
          force=True)

def test_ll2sph():
    """
    Test of alter ll2sph
    """
    print("\n ~> Testing alter ll2sph")

    # TODO: Better test that check results
    file_name = "bts3d-v7-latlon.slf"
    out_file = "test-ll2sph.slf"

    alter(file_name, out_file, ll2sph=True, longitude=20., latitude=20.,
          force=True)

def test_sph2ll():
    """
    Test of alter sph2ll
    """
    print("\n ~> Testing alter sph2ll")

    # TODO: Better test that check results
    file_name = "geo_spherique.slf"
    out_file = "test-sph2ll.slf"

    alter(file_name, out_file, sph2ll=True, longitude=20., latitude=20.,
          force=True)

def test_rename_var():
    """
    Test of alter rename_var
    """
    print("\n ~> Testing alter rename_var")

    file_name = "r2d_gouttedo.slf"
    out_file = "test-rename-var.slf"

    rename_var = 'WATER DEPTH=TOTO'

    alter(file_name, out_file, rename_var=rename_var, force=True)

    res = TelemacFile(out_file)

    if 'TOTO' not in res.varnames:
        raise TelemacException(\
            "Test rename var failed {}".format(res.varnames))

    res.close()

def test_remove_var():
    """
    Test of alter remove
    """
    print("\n ~> Testing alter xvars")

    file_name = "r2d_gouttedo.slf"
    out_file = "test-remove-var.slf"

    list_var = 'WATER DEPTH,VELOCITY U'

    alter(file_name, out_file, in_vars=list_var, force=True)

    res = TelemacFile(out_file)

    if 'VELOCITY V' in res.varnames:
        raise TelemacException(\
            "Test remove var failed {}".format(res.varnames))

    res.close()

def test_modif_var():
    """
    Test of alter modif var
    """
    print("\n ~> Testing alter modif var")

    file_name = "r2d_gouttedo.slf"
    out_file = "test-modif-var.slf"

    var_name = "WATER DEPTH"

    res = TelemacFile(file_name)
    val = res.get_data_value("WATER DEPTH", -1)
    res.close()
    val = 3.*val + 20.

    alter(file_name, out_file, modif_var=var_name,
          add_var=20., mul_var=3., force=True)

    res = TelemacFile(out_file)

    res_val = res.get_data_value("WATER DEPTH", -1)

    if np.any(np.abs(res_val-val)/max(res_val) > 1e-6):
        raise TelemacException(\
            "Test alter modif_var failed")

    res.close()

def test_reset_time():
    """
    Test of alter reset_time
    """
    print("\n ~> Testing alter reset_time")

    file_name = "r2d_gouttedo_part2.slf"
    out_file = "test-reset-time.slf"

    alter(file_name, out_file, reset_time=True, force=True)

    res = TelemacFile(out_file)

    if res.times[0] != 0.:
        raise TelemacException(\
            "Test reset time failed {}".format(res.varnames))

    res.close()

def test_modif_records():
    """
    Test of alter modif_records
    """
    print("\n ~> Testing alter modif records")

    file_name = "r2d_gouttedo.slf"
    out_file = "test-modif-records.slf"

    res = TelemacFile(file_name)
    times = res.times
    res.close()

    alter(file_name, out_file, tfrom=1, tend=-1, tstep=2, force=True)

    res = TelemacFile(out_file)

    if np.any(times[range(1, len(times), 2)] != res.times):
        raise TelemacException(\
            "Test modif records failed {}".format(res.times))

    res.close()

def test_modif_time():
    """
    Test of alter modif time
    """
    print("\n ~> Testing alter modif time")

    file_name = "r2d_gouttedo.slf"
    out_file = "test-modif-time.slf"

    res = TelemacFile(file_name)
    times = res.times
    res.close()
    times = 3.*times + 20.

    alter(file_name, out_file,
          add_time=20., mul_time=3., force=True)

    res = TelemacFile(out_file)

    if np.any(np.abs(res.times-times)/max(res.times) > 1e-6):
        raise TelemacException(\
            "Test alter modif_time failed")

    res.close()

def test_select_time():
    """
    Test of alter select time
    """
    print("\n ~> Testing alter select time")

    file_name = "r2d_gouttedo.slf"
    out_file = "test-select-time.slf"

    times = [0.2, 2.0, 3.0, 3.6]

    alter(file_name, out_file,
          times=times, force=True)

    res = TelemacFile(out_file)

    if np.any(np.abs(res.times-times)/max(res.times) > 1e-6):
        raise TelemacException(\
            "Test alter select_time failed")

    res.close()


class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 0
        self.tags = ['python3']

    def _pre(self):
        """
        Defining the studies
        """

    def _check_results(self):
        """
        Post-treatment processes
        """
        test_title()
        test_datetime()
        test_endian()
        test_auto_precision()
        test_precision()
        test_precision2()
        test_modif_coord()
        test_rotate()
        test_proj()
        test_orig()
        test_ll2utm()
        test_utm2ll()
        test_ll2sph()
        test_sph2ll()
        test_rename_var()
        test_remove_var()
        test_modif_var()
        test_reset_time()
        test_modif_records()
        test_modif_time()
        test_select_time()


    def _post(self):
        """
        Post-treatment processes
        """
