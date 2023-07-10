
"""
Validation script for delwaq
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 3
        self.tags = ['telemac2d']

    def _pre(self):
        """
        Defining the studies
        """

        # delwaq scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_delwaq.cas')


        # delwaq parallel mode
        cas = TelemacCas('t2d_delwaq.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_delwaq_par.cas',
                       cas=cas)

        del cas


    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_delwaq.slf',
                            eps=[0.2, 0.14, 0.013, 0.013, 1.E-9, 0.08])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_delwaq.slf',
                            eps=[0.83, 0.25, 0.058, 0.058, 1.E-9, 0.58])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[0.83, 0.36, 0.058, 0.058, 1.E-9, 0.58])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d
        # Getting files
        res_vnv_1_t2dgeo, _ = self.get_study_res('vnv_1:T2DGEO', load_bnd=True)
        res_vnv_1_t2dres, _ = self.get_study_res('vnv_1:T2DRES')

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_t2dgeo,
                   plot_mesh=True,
                   fig_size=(12, 8),
                   fig_name='img/Mesh',
                   annotate_bnd=True)

        # Plotting BOTTOM at 0
        vnv_plot2d('BOTTOM',
                   res_vnv_1_t2dres,
                   record=0,
                   cbar_label='Bottom elevation (m)',
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/Bottom')

        # Plotting VELOCITY at final time step
        vnv_plot2d('VELOCITY',
                   res_vnv_1_t2dres,
                   record=-1,
                   cbar_label='Velocity (m/s)',
                   filled_contours=True,
                   vectors=True,
                   vectors_scale=35,
                   fig_size=(12, 7),
                   fig_name='img/Velocity_tf')

        # Plotting FREE SURFACE at final time step
        vnv_plot2d('FREE SURFACE',
                   res_vnv_1_t2dres,
                   record=-1,
                   cbar_label='Free surface (m)',
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/FreeSurface')

        # Plotting SALINITY at 1
        vnv_plot2d('SALINITY',
                   res_vnv_1_t2dres,
                   record=1,
                   cbar_label='Salinity (g/L)',
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/Salinity_t1')

        # Plotting SALINITY at final time step
        vnv_plot2d('SALINITY',
                   res_vnv_1_t2dres,
                   record=-1,
                   cbar_label='Salinity (g/L)',
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/Salinity')

        # Plotting VELOCITY at 1
        vnv_plot2d('VELOCITY',
                   res_vnv_1_t2dres,
                   record=1,
                   cbar_label='Velocity (m/s)',
                   filled_contours=True,
                   fig_size=(10, 6),
                   fig_name='img/Velocity_map_t1')

        # Plotting VELOCITY at 15
        vnv_plot2d('VELOCITY',
                   res_vnv_1_t2dres,
                   record=15,
                   cbar_label='Velocity (m/s)',
                   filled_contours=True,
                   fig_size=(10, 6),
                   fig_name='img/Velocity_map_t15')

        # Plotting VECTORS at 15
        vnv_plot2d('VELOCITY',
                   res_vnv_1_t2dres,
                   record=15,
                   plot_mesh=True,
                   cbar_label='Velocity (m/s)',
                   cbar_priority='vector',
                   colored_vectors=True,
                   vectors_scale=35,
                   grid_resolution=[30, 30],
                   fig_size=(10, 6),
                   fig_name='img/Velocity_arrows')

        # Closing files
        res_vnv_1_t2dgeo.close()
        res_vnv_1_t2dres.close()
