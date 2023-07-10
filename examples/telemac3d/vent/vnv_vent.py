
"""
Validation script for vent
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
        self.rank = 1
        self.tags = ['telemac3d', 'postel3d']

    def _pre(self):
        """
        Defining the studies
        """

        # vent scalar mode
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_vent.cas')


        # vent parallel mode
        cas = TelemacCas('t3d_vent.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_vent_par.cas',
                       cas=cas)

        del cas

        # post-treatment
        self.add_study('p3d',
                       'postel3d',
                       'p3d_vent.cas')



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_vent.slf',
                            eps=[1.E-6, 1.E-7, 1.E-7, 1.E-8, 1.E-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_vent.slf',
                            eps=[1.E-6, 1.E-7, 1.E-7, 1.E-8, 1.E-8])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1.E-6, 1.E-7, 1.E-8, 1.E-8, 1.E-8])


    def _post(self):
        """
        Post-treatment processes
        """
        from data_manip.computation.triangulation import triangulation_from_data
        from postel.plot2d import plot2d_vectors
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_polylines
        import matplotlib.pyplot as plt
                # Getting files
        vnv_1_t3dres = self.get_study_file('vnv_1:T3DRES')
        res = TelemacFile(vnv_1_t3dres)
        vnv_1_t3dhyd = self.get_study_file('vnv_1:T3DHYD')
        res_vnv_1_t3dhyd = TelemacFile(vnv_1_t3dhyd)

        #Plotting Horizontal Mesh
        vnv_plot2d('BOTTOM',
                   res_vnv_1_t3dhyd,
                   plot_mesh=True,
                   fig_size=(10, 3),
                   fig_name='img/Mesh')

        #Plotting Vertical Mesh
        vnv_plot2d('ELEVATION Z',
                   res,
                   plot_mesh=True,
                   record=0,
                   poly=[[0, 50], [500, 50]],
                   y_label='z (m)',
                   fig_size=(10, 3),
                   fig_name='img/MeshV')

        #Plotting Vertical Mesh
        vnv_plot2d('ELEVATION Z',
                   res,
                   plot_mesh=True,
                   record=0,
                   xlim=[200, 300],
                   ylim=[-30, 0],
                   poly=[[0, 50], [500, 50]],
                   y_label='z (m)',
                   fig_size=(10, 3),
                   fig_name='img/MeshV_zoom')


        # Plotting WATER DEPTH over polyline over records res_vnv_1_t3dhyd.ntimestep
        vnv_plot1d_polylines(\
                'WATER DEPTH',
                res_vnv_1_t3dhyd,
                record=[i for i in range(0, res_vnv_1_t3dhyd.ntimestep)],
                poly=[[0, 50], [500, 50]],
                fig_size=(10, 3),
                fig_name='img/freeSurface')

        # Plotting vertical split
        vnv_plot2d(\
                   'VELOCITY V',
                   res,
                   poly=[[0, 50], [500, 50]],
                   y_label='z (m)',
                   record=-1,
                   cbar_label='Velocity (m/s)',
                   filled_contours=True,
                   fig_size=(10, 4),
                   fig_name='img/fieldVelo')

        # Ploting vertical slice as vectors
        # TODO: Do a better plot

        # points defining the polyline
        poly_points = [[0., 50.], [500., 50.]]

        # number of points per segment of the polyline
        poly_number = res.discretize_polyline(poly_points)

        # slice at initial time step (-1) of the elevation variable
        _, abs_curv, values_poly_z =\
               res.get_data_on_vertical_plane(\
                 'ELEVATION Z', -1, poly_points, poly_number)

        # slice at initial time step (-1) of the velocity u variable
        _, _, vel_u =\
               res.get_data_on_vertical_plane(\
                 'VELOCITY U', -1, poly_points, poly_number)
        # slice at initial time step (-1) of the velocity v variable
        _, _, vel_v =\
               res.get_data_on_vertical_plane(\
                 'VELOCITY V', -1, poly_points, poly_number)
        # creation of a mesh from the elevation value and curvilinear coordinate of the polyline
        mesh = triangulation_from_data(abs_curv, values_poly_z)

        fig, ax = plt.subplots(1, 1)

        plot2d_vectors(fig, ax, mesh, vel_u.flatten(), vel_v.flatten(),
                       grid_resolution=[100, 20],
                       data_name='velocity',
                       color='k',
                       vmin=(200, -30),
                       vmax=(300, 0))

        fig_name = 'img/vecVelo'
        print(" "*8+"~> Plotting "+fig_name)
        plt.savefig(fig_name)
        plt.close('all')

        # Closing files
        res.close()
        res_vnv_1_t3dhyd.close()
