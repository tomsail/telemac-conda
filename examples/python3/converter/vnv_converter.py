
"""
Validation script for converter
"""
from os import path, remove
import numpy as np
from vvytel.vnv_study import AbstractVnvStudy
from data_manip.extraction.telemac_file import TelemacFile


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


class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 0
        self.tags = ['python3', 'med']

    def _pre(self):
        """
        Defining the studies
        """

        # Conversion from Serafin to Med geometry + boundary
        self.add_command(\
            'vnv_srf2med_geo',
            'converter.py srf2med mesh.slf -b mesh.cli mesh2.slf.med')


        # Conversion from Serafin to Med 2d result file only
        self.add_command('vnv_srf2med_r2d',
                         'converter.py srf2med r2d.slf r2d.slf.med')


        # Conversion from Serafin to Med 2d result file only
        self.add_command('vnv_srf2med_r3d',
                         'converter.py srf2med r3d.slf r3d.slf.med')


        # Conversion from Serafin to VTK 2d result file only
        self.add_command('vnv_srf2vtk_r2d',
                         'converter.py srf2vtk r2d.slf r2d.vtk')


        # Conversion from Serafin to VTK 2d result file only
        self.add_command('vnv_srf2vtk_r3d',
                         'converter.py srf2vtk r3d.slf r3d.vtk')


        # Conversion from MED to Serafin geometry + boundary
        self.add_command(\
            'vnv_med2srf_geo',
            'converter.py med2srf mesh.med -b mesh.bnd mesh.med.slf')


        # Conversion from MED to Serafin 2d result file only
        self.add_command('vnv_med2srf_r2d',
                         'converter.py med2srf r2d.med r2d.med.slf')


        # Conversion from MED to Serafin 2d result file only
        self.add_command('vnv_med2srf_r3d',
                         'converter.py med2srf r3d.med r3d.med.slf')


        # Refining a mesh file splitting each triangle in 4
        self.add_command('vnv_refinement',
                         'converter.py refine mesh.slf -b mesh.cli meshx4.slf')


        # Converting kenue polygon file into a Serafin file
        self.add_command('kenue2shp-1',
                         'converter.py kenue2shp polygon.i2s')


        # Converting kenue polygon file into a Serafin file
        self.add_command('kenue2shp-2',
                         'converter.py kenue2shp polylines.i2s')

        # Testing that when converting from med to serafin file a file that
        # need double precision it is converted to serafind instead
        create_file_from_scratch("mesh_single.med", step=0.5)
        self.add_command('vnv_precision_single',
                         'converter.py med2srf mesh_single.med mesh_single.srf')

        create_file_from_scratch("mesh_double.med", step=0.5*1e-7)
        self.add_command('vnv_precision_double',
                         'converter.py med2srf mesh_double.med mesh_double.srf')

        self.add_command('vnv_precision_force_single',
                         'converter.py med2srf mesh_double.med mesh_double2.srf'
                         ' --disable-auto-precision')

    def _check_results(self):
        """
        Post-treatment processes
        """
        res = TelemacFile("mesh_single.srf")
        if res.fformat != "SERAFIN":
            raise Exception(\
                "Conversion med to srf for single precision did not work")

        res.close()

        res = TelemacFile("mesh_double.srf")
        if res.fformat != "SERAFIND":
            raise Exception(\
                "Conversion med to srf for double precision did not work")

        res.close()

        res = TelemacFile("mesh_double2.srf")
        if res.fformat != "SERAFIN":
            raise Exception(\
                "Conversion med to srf for double precision did not work")

        res.close()


    def _post(self):
        """
        Post-treatment processes
        """
