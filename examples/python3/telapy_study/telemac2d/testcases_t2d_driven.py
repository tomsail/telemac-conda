# coding: utf-8

import numpy as np
from shapely.geometry import Point, Polygon

from telapy.tools.study_t2d_driven import Telemac2DTestCase


class Garonne(Telemac2DTestCase):
    """
    Class Garonne
    =============
    """

    def __init__(self, steering_file, user_fortran, ks_area="ks_area.txt",
                 path=".", name="Garonne",
                 config={'Ks': [True, True, True, True], 'Q': True}):
        """ Constructor

        :param str steering_file: steering file
        :param str user_fortran: user Fortran file
        :param str name: name of the test case
        :param list config: parameters of the test case (boolean)
        """
        Telemac2DTestCase.__init__(self, steering_file, user_fortran, ks_area, path, name)
        self.config = config
        ks_area = np.loadtxt(self.ks_area)
        x_1 = ks_area[0:314, 1]
        y_1 = ks_area[0:314, 2]
        x_2 = ks_area[314:932, 1]
        y_2 = ks_area[314:932, 2]
        x_3 = ks_area[932:1963, 1]
        y_3 = ks_area[932:1963, 2]
        z_1 = [[x_1[i], y_1[i]] for i, _ in enumerate(x_1)]
        z_2 = [[x_2[i], y_2[i]] for i, _ in enumerate(x_2)]
        z_3 = [[x_3[i], y_3[i]] for i, _ in enumerate(x_3)]
        self.ks_zones = [Polygon(z_1), Polygon(z_2), Polygon(z_3)]

    def __call__(self, t2d, x_val):
        """ Change the values of Ks and Q

        :param Telemac2d t2d: instance of Telemac2d
        :param list x_val: inputs
        """
        if x_val is None:
            return

        i = 0
        # Perturbations on Strickler - friction coeff at each node ############
        coord_x, coord_y, mesh_tri = t2d.get_mesh()
        if len(self.config['Ks']) == 1 and self.config['Ks'][0]:
            val_ks = x_val[i]
            for i_pts in range(t2d.get('MODEL.NPOIN')):
                t2d.set("MODEL.CHESTR", val_ks, i=i_pts)
            i += 1
        elif len(self.config['Ks']) > 1 and sum(self.config['Ks']) > 0:
            for i_pts in range(t2d.get('MODEL.NPOIN')):
                p_xy = Point(coord_x[i_pts], coord_y[i_pts])
                if p_xy.within(self.ks_zones[0]):
                    num_zone = 1  # lit mineur amont
                elif p_xy.within(self.ks_zones[1]):
                    num_zone = 2  # lit mineur milieu
                elif p_xy.within(self.ks_zones[2]):
                    num_zone = 3  # lit mineur aval
                else:
                    num_zone = 0  # lit majeur
                if self.config['Ks'][num_zone]:
                    val_ks = x_val[sum(self.config['Ks'][0:num_zone + 1]) - 1]
                    t2d.set("MODEL.CHESTR", val_ks, i=i_pts)
            i += sum(self.config['Ks'])

        # Perturbation on upstream flow rate #################################
        if self.config['Q']:
            val_q = x_val[i]
            t2d.set("MODEL.DEBIT", val_q, i=0)


class Gironde(Telemac2DTestCase):
    """
    Class Gironde
    =============
    """

    def __init__(self, steering_file, user_fortran, ks_area="ks_area.txt", path=".", name="Gironde",
                 config={
                     'Ks': [True, True, True, True],
                     'CDZ': True,
                     'Q2': True,
                     'Q3': True}):
        """ Constructor

        :param str steering_file: steering file
        :param str user_fortran: user Fortran file
        :param str name: name of the test case
        :param list config: parameters of the test case (boolean)
        """
        Telemac2DTestCase.__init__(self, steering_file, user_fortran, ks_area, path, name)
        self.config = config

    def __call__(self, t2d, x_val):
        """ Change the values of Ks1, Ks2, Ks3, Ks4, CDZ, Q2 and Q3

        :param Telemac2d t2d: instance of Telemac2d
        :param list x_val: inputs
        """
        i = 0
        # Perturbations on Strickler - friction coeff at each node ############
        coord_x, coord_y, mesh_tri = t2d.get_mesh()
        if len(self.config['Ks']) == 1 and self.config['Ks'][0]:
            val_ks = x_val[i]
            for i_pts in range(t2d.get('MODEL.NPOIN')):
                t2d.set("MODEL.CHESTR", val_ks, i=i_pts)
            i += 1
        if len(self.config['Ks']) > 1 and sum(self.config['Ks']) > 0:
            for i_pts in range(t2d.get('MODEL.NPOIN')):
                if coord_y[i_pts] - coord_x[i_pts] > 10000.0:
                    num_zone = 1  # ZONE 1 : domaine maritime
                elif coord_x[i_pts] <= 370000.0:
                    num_zone = 2  # ZONE 2 : estuaire moyen
                elif (coord_x[i_pts] <= 374000.0) & (coord_y[i_pts] >= 282000.0):
                    num_zone = 3  # ZONE 3 : fluvial inferieur
                else:
                    num_zone = 4  # ZONE 4 : fluvial amont
                if self.config['Ks'][num_zone - 1]:
                    val_ks = x_val[sum(self.config['Ks'][0:num_zone]) - 1]
                    t2d.set("MODEL.CHESTR", val_ks, i=i_pts)
            i += sum(self.config['Ks'])
        # Perturbation on CDZ (wind influence coefficient) ####################
        if self.config['CDZ']:
            val_cdz = x_val[i]
            t2d.set("MODEL.FAIR", val_cdz)
            i += 1
        # Perturbation on upstream flow rates #################################
        if self.config['Q2']:
            val_q2 = x_val[i]
            t2d.set("MODEL.DEBIT", val_q2, i=1)
            i += 1
        if self.config['Q3']:
            val_q3 = x_val[i]
            t2d.set("MODEL.DEBIT", val_q3, i=2)
            i += 1
