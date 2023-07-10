
"""
Validation script for culm
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
        self.rank = 3
        self.tags = ['telemac2d']

    def _pre(self):
        """
        Defining the studies
        """

        # culm scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_culm.cas')


        # culm parallel mode
        cas = TelemacCas('t2d_culm.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_culm_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_culm.slf',
                            eps=[0.042, 0.057, 0.012, 0.012, 1.E-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_culm.slf',
                            eps=[0.059, 0.075, 0.014, 0.014, 1.E-8])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                             eps=[0.044, 0.048, 0.012, 0.012, 1.E-8])

    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_history
        # Getting files
 #        vnv_1_t2dgeo = self.get_study_file('vnv_1:T2DGEO')
 #        res_vnv_1_t2dgeo = TelemacFile(vnv_1_t2dgeo)
        res_vnv_1_t2dgeo, _ = self.get_study_res('vnv_1:T2DGEO', load_bnd=True)
        vnv_1_t2dres = self.get_study_file('vnv_1:T2DRES')
        res_vnv_1_t2dres = TelemacFile(vnv_1_t2dres)

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_t2dgeo,
                   plot_mesh=True,
                   annotate_bnd=True,
                   fig_size=(6, 5),
                   fig_name='img/Mesh')


        # Plotting BOTTOM at 0
        vnv_plot2d('BOTTOM',
                   res_vnv_1_t2dres,
                   record=0,
                   filled_contours=True,
                   cbar_label='Bottom elevation (m)',
                   fig_size=(7, 5),
                   fig_name='img/Bathy')


        # Plotting WATER DEPTH at -1
        vnv_plot2d('WATER DEPTH',
                   res_vnv_1_t2dres,
                   record=-1,
                   filled_contours=True,
                   cbar_label='Water depth (m)',
                   fig_size=(7, 5),
                   fig_name='img/Water_depth')

        # Plotting VELOCITY at -1
        vnv_plot2d('VELOCITY',
                   res_vnv_1_t2dres,
                   record=-1,
                   filled_contours=True,
                   cbar_label='Velocity (m/s)',
                   fig_size=(7, 5),
                   fig_name='img/Velocity')

        # Plotting WATER DEPTH at 0 s
        vnv_plot2d('WATER DEPTH',
                   res_vnv_1_t2dres,
                   record=0,
                   filled_contours=True,
                   cbar_label='Water depth (m)',
                   plot_mesh=True,
                   plot_only_dry_mesh=True,
                   mask_tidal_flats=True,
                   tidal_flats_threshold=0.05,
                   vmax=4.05,
                   vmin=0.05,
                   fig_title='Water depth at t = 0 s',
                   fig_size=(7, 5),
                   fig_name='img/WaterDepth_0s')

        # Plotting WATER DEPTH at 10,000 s
        vnv_plot2d('WATER DEPTH',
                   res_vnv_1_t2dres,
                   time=10000,
                   filled_contours=True,
                   cbar_label='Water depth (m)',
                   plot_mesh=True,
                   plot_only_dry_mesh=True,
                   mask_tidal_flats=True,
                   vmax=4.05,
                   vmin=0.05,
                   tidal_flats_threshold=0.05,
                   fig_title='Water depth at t = 10,000 s',
                   fig_size=(7, 5),
                   fig_name='img/WaterDepth_10000s')

        # Plotting WATER DEPTH at 20,000 s
        vnv_plot2d('WATER DEPTH',
                   res_vnv_1_t2dres,
                   time=20000,
                   filled_contours=True,
                   cbar_label='Water depth (m)',
                   plot_mesh=True,
                   plot_only_dry_mesh=True,
                   mask_tidal_flats=True,
                   vmax=4.05,
                   vmin=0.05,
                   tidal_flats_threshold=0.05,
                   fig_title='Water depth at t = 20,000 s',
                   fig_size=(7, 5),
                   fig_name='img/WaterDepth_20000s')

        # Plotting WATER DEPTH at 40,000 s
        vnv_plot2d('WATER DEPTH',
                   res_vnv_1_t2dres,
                   time=40000,
                   filled_contours=True,
                   cbar_label='Water depth (m)',
                   plot_mesh=True,
                   plot_only_dry_mesh=True,
                   mask_tidal_flats=True,
                   vmax=4.05,
                   vmin=0.05,
                   tidal_flats_threshold=0.05,
                   fig_title='Water depth at t = 40,000 s',
                   fig_size=(7, 5),
                   fig_name='img/WaterDepth_40000s')

        # Plotting at point (7841.22;6842.13)
        vnv_plot1d_history(\
                'FREE SURFACE',
                res_vnv_1_t2dres,
                'FREE SURFACE',
                points=[[7841.22, 6842.13]],
                x_label='t (s)',
                y_label='z (m)',
                fig_size=(7, 5),
                fig_name='img/timeserie',
                xlim=[0., 60000],
                ylim=[23.7, 24.3])

        # Closing files
        res_vnv_1_t2dgeo.close()
        res_vnv_1_t2dres.close()
