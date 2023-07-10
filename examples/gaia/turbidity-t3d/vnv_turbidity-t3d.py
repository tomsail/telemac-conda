"""
Validation script for turbidity-t3d
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
from data_manip.extraction.telemac_file import TelemacFile

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 5
        self.tags = ['telemac3d', 'gaia']

    def _pre(self):
        """
        Defining the studies
        """

        # Turbidity T3D+GAI scalar mode
        self.add_study('vnv_turbidity_scalar',
                       'telemac3d',
                       't3d_turbidity.cas')


        # Turbidity T3D+GAI parallel mode
        cas = TelemacCas('t3d_turbidity.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_turbidity_para',
                       'telemac3d',
                       't3d_turbidity_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
#        self.check_epsilons('vnv_turbidity_scalar:GAIRES',
#                            'gai_ref_turbidity-t3d.slf',
#                            eps=[1e-3])
#
#        # Comparison with the last time frame of the reference file.
#        self.check_epsilons('vnv_turbidity_para:GAIRES',
#                            'gai_ref_turbidity-t3d.slf',
#                            eps=[1e-3])
#
#        # Comparison with the last time frame of the reference file.
#        self.check_epsilons('vnv_turbidity_para:GAIRES',
#                            'vnv_turbidity_scalar:GAIRES',
#                            eps=[1e-3])
#
#        # Comparison with the last time frame of the reference file.
#        self.check_epsilons('vnv_turbidity_scalar:T3DRES',
#                            'f3d_turbidity-t3d.slf',
#                            eps=[1.])
#
#        # Comparison with the last time frame of the reference file.
#        self.check_epsilons('vnv_turbidity_para:T3DRES',
#                            'f3d_turbidity-t3d.slf',
#                            eps=[1.])
#
#        # Comparison with the last time frame of the reference file.
#        self.check_epsilons('vnv_turbidity_para:T3DRES',
#                            'vnv_turbidity_scalar:T3DRES',
#                            eps=[1.])
    def _post(self):
        """
        Post-treatment processes
        """
        from os import environ, path
        from data_manip.computation.triangulation import triangulation_from_data
        from postel.plot1d import plot1d
        from postel.plot2d import plot2d_scalar_map, plot2d_triangle_mesh,plot2d_scalar_filled_contour
        from postel.plot_vnv import vnv_plot2d,vnv_plot1d_polylines
        import matplotlib.pyplot as plt
        import numpy as np

        # Getting the name of the geometry and result file of the sequential run

        vnv_scalar_gaia = self.get_study_file('vnv_turbidity_scalar:GAIRES')
        vnv_scalar_hydro3D = self.get_study_file('vnv_turbidity_scalar:T3DRES')
        vnv_scalar_hydro2D = self.get_study_file('vnv_turbidity_scalar:T3DHYD')
        res_gai = TelemacFile(vnv_scalar_gaia)
        res_hydro3D = TelemacFile(vnv_scalar_hydro3D)
        res_hydro2D = TelemacFile(vnv_scalar_hydro2D)

        #======================================================================
        # DESCRIPTION PLOTS:
        #
        # No Plotting mesh since too fine

        # Plotting 3D mesh section (vertical mesh)
        vnv_plot2d('ELEVATION Z',
                   res_hydro3D,
                   poly=[[5., 10.], [5., 50.]],
                   record=-1,
                   plot_mesh=True,
                   x_label='$x$ (m)', y_label='$z$ (m)',
                   fig_size=(12, 7),
                   fig_name='img/res_mesh_section')
        # Plotting bed elevation over polyline for time step 0
        vnv_plot1d_polylines(\
            'BOTTOM',
            res_gai,
            legend_labels='initial bed level',
            fig_size=(8, 2),
            record=0,
            poly=[[5.,0.],[5.,1000.]],
            poly_number=[500],
#            ylim=[0,0.15],
            fig_name='img/InitialBottom')
        #
        #Plotting elevation of the bottom at the end of the simulation vs measurements
        fig, ax = plt.subplots(figsize=(10,3))

        poly_points = [[5.,0.],[5.,1000.]]

        poly_number =[500]

        poly_coord, abs_curv,bottom=res_gai.get_timeseries_on_polyline('BOTTOM', poly_points, poly_number)
        poly_coord, abs_curv,free_surf=res_hydro2D.get_timeseries_on_polyline('FREE SURFACE', poly_points, poly_number)

        plot1d(ax, abs_curv ,bottom[:,0],
               x_label='y (m)',
               plot_label='Initial bed elevation')

        plot1d(ax, abs_curv ,free_surf[:,0],
               x_label='y (m)',
               plot_label='Initial free surface elevation')

        ax.legend()
        plt.savefig('img/initial_state.png')
        plt.close('all')

        #Plotting elevation of the bottom at the end of the simulation vs measurements

        fig, ax = plt.subplots(figsize=(16,8))

        # points defining the polyline

        poly_points = [[5.0, 0.0], [5.0, 1000.0]]

        # number of points per segment of the polyline

        poly_number=[500]

        # slice at final time step of the elevation variable
        _, abs_curv, values_poly_Z=\
            res_hydro3D.get_data_on_vertical_plane('ELEVATION Z', -1, poly_points, poly_number)

        # slice at final state of the concentration

        _,_, conc = res_hydro3D.get_data_on_vertical_plane('COH SEDIMENT1', -1, poly_points, poly_number)

        # creation of a mesh from the elevation value and curvilinear coordinate of the polyline

        mesh = triangulation_from_data(abs_curv, values_poly_Z)

        # be careful the value extracted from polyline over each plane must be a vector and not a matrix
        conc2d=conc.flatten()
        plot2d_scalar_filled_contour(fig, ax, mesh, conc2d, data_name='Concentration (g/l)',
                                     x_label="Distance (m)", y_label="Z (m)")

        # save the plot

        plt.savefig('img/slice_conc.png')
        plt.close('all')

