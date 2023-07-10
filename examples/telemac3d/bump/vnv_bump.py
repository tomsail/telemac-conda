
"""
Validation script for bump
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

        # bump scalar mode
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_bump.cas')


        # bump parallel mode
        cas = TelemacCas('t3d_bump.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_bump_par.cas',
                       cas=cas)

        del cas

        # Post-treatment
        self.add_study('p3d',
                       'postel3d',
                       'p3d_bump.cas')



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_bump.slf',
                            eps=[1.E-4, 1.E-3, 1.E-3, 1.E-3, 1.E-5])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_bump.slf',
                            eps=[1.E-4, 2.E-3, 1.E-3, 1.E-3, 1.E-5])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1.E-4, 1.E-3, 1.E-3, 1.E-3, 1.E-5])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_actions import plot1d
        from postel.plot_vnv import vnv_plot1d_polylines, vnv_plot2d
        import matplotlib.pyplot as plt
                # Getting files
        vnv_1_t3dres = self.get_study_file('vnv_1:T3DRES')
        res_vnv_1_t3dres = TelemacFile(vnv_1_t3dres)
        vnv_1_t3dhyd = self.get_study_file('vnv_1:T3DHYD')
        res_vnv_1_t3dhyd = TelemacFile(vnv_1_t3dhyd)

        # Plotting BOTTOM over polyline over records res_vnv_1_t3dhyd.ntimestep
        vnv_plot1d_polylines(\
                'BOTTOM',
                res_vnv_1_t3dhyd,
                legend_labels='bottom',
                poly=[[0.24, 1.0], [21, 1.0]],
                record=-1,
                fig_size=(12, 2),
                fig_name='img/profil')

        # Plotting profile with analytic solution
        fig, ax = plt.subplots(1, 1, figsize=(12, 5))

        poly = [[0.24, 1], [21, 1]]

        for var_name, plot_name in [('PRIVE 3', 'Analytical solution'),
                                    ('FREE SURFACE', 'TELEMAC-3D'),
                                    ('BOTTOM', 'bottom')]:
            _, abs_curv, data = res_vnv_1_t3dhyd.get_timeseries_on_polyline(\
                                    var_name, poly)

            plot1d(ax, abs_curv, data[:, -1],
                   plot_label=plot_name)

        ax.legend()

        fig_name = 'img/free_surface'
        print(" "*8+"~> Plotting "+fig_name)
        plt.savefig(fig_name)
        plt.close('all')

        # Plotting BOTTOM at 0
        vnv_plot2d('BOTTOM',
                   res_vnv_1_t3dhyd,
                   record=0,
                   filled_contours=True,
                   cbar_label='Bottom elevation (m)',
                   fig_size=(12, 5),
                   fig_name='img/bathy')

        #Plotting mesh
        vnv_plot2d('BOTTOM',
                   res_vnv_1_t3dhyd,
                   plot_mesh=True,
                   fig_size=(12, 5),
                   fig_name='img/Mesh')

        #Plotting vertical mesh
        vnv_plot2d('ELEVATION Z',
                   res_vnv_1_t3dres,
                   poly=[[0.24, 1], [21, 1]],
                   record=0,
                   y_label='z (m)',
                   plot_mesh=True,
                   fig_size=(12, 5),
                   fig_name='img/MeshV')

        # Plotting horizontal slice of vectors velocity
        vnv_plot2d('VELOCITY',
                   res_vnv_1_t3dres,
                   plane=res_vnv_1_t3dres.nplan-1,
                   record=-1,
                   cbar_label='Velocity (m/s)',
                   filled_contours=True,
                   vectors=True,
                   vectors_scale=40,
                   fig_size=(12, 5),
                   fig_name='img/veloH')

        # Plotting vertical slice of vectors velocity
        vnv_plot2d('VELOCITY',
                   res_vnv_1_t3dres,
                   poly=[[0.24, 1], [21, 1]],
                   y_label='z (m)',
                   record=-1,
                   cbar_label='Velocity (m/s)',
                   filled_contours=True,
                   vectors=True,
                   vectors_scale=40,
                   fig_size=(12, 5),
                   fig_name='img/veloV')

        res_vnv_1_t3dres.close()
        res_vnv_1_t3dhyd.close()
