
"""
Validation script for border_ice
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
        self.rank = 2
        self.tags = ['telemac2d', 'khione']

    def _pre(self):
        """
        Defining the studies
        """
        # serial
        self.add_study('vnv_seq',
                       'telemac2d',
                       't2d_border-ice.cas')

        # parallel
        cas = TelemacCas('t2d_border-ice.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('vnv_par',
                       'telemac2d',
                       't2d_border-ice_par.cas',
                       cas=cas)

        del cas


    def _check_results(self):
        """
        Post-treatment processes
        """
        # TODO: set eps

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_seq:T2DRES', 'f2d_border-ice.slf', eps=[1.E-3])
        self.check_epsilons('vnv_seq:ICERES', 'fce_border-ice.slf', eps=[1.E-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_par:T2DRES', 'f2d_border-ice.slf', eps=[1.])
        self.check_epsilons('vnv_par:ICERES', 'fce_border-ice.slf', eps=[1.])

        # Comparison between serial and parallel run.
        self.check_epsilons('vnv_seq:T2DRES', 'vnv_par:T2DRES', eps=[1.])
        self.check_epsilons('vnv_seq:ICERES', 'vnv_par:ICERES', eps=[1.])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d

        geo, _ = self.get_study_res('vnv_seq:T2DGEO', load_bnd=True)
        #res_ini = TelemacFile(self.get_study_file('vnv_ini:T2DRES'))
        res_seq = TelemacFile(self.get_study_file('vnv_seq:T2DRES'))
        res_ice_seq = TelemacFile(self.get_study_file('vnv_par:ICERES'))

        #======================================================================
        # DESCRIPTION PLOTS:
        #
        #Plotting mesh
        vnv_plot2d(\
            'BOTTOM',
            geo,
            record=0,
            plot_mesh=True,
            annotate_bnd=True,
            filled_contours=False,
            fig_size=(14, 3.2),
            fig_name='img/mesh',
            x_label='$x$ $(m)$',
            y_label='$y$ $(m)$',)

        # Plotting bottom
        vnv_plot2d(\
            'BOTTOM',
            geo,
            record=0,
            filled_contours=True,
            fig_size=(15, 3.),
            fig_name='img/bottom',
            x_label='$x$ $(m)$',
            y_label='$y$ $(m)$',
            cbar_label='Bottom (m)')

        #======================================================================
        # FIRST OBSERVATION:
        #
        fig_size=(15, 3)
        times = [0., 900, 1800.]
        minutes = [0, 15, 30]

        for idx, time in enumerate(times):
            # Plotting velocity
            vnv_plot2d(\
                'VELOCITY',
                res_seq,
                time=time,
                filled_contours=True,
                vectors=True,
                vectors_scale=15, vectors_normalize=False,
                grid_resolution=[100, 20],
                fig_size=fig_size,
                fig_name='img/U-2d_scalarmap_{}min'.format(minutes[idx]),
                x_label='$x$ $(m)$',
                y_label='$y$ $(m)$',
                cbar_label='$|U|$ $(m/s)$')

            # Plotting temperature
            vnv_plot2d(\
                'TEMPERATURE',
                res_seq,
                time=time,
                filled_contours=True,
                fig_size=fig_size,
                fig_name='img/T-2d_scalarmap_{}min'.format(minutes[idx]),
                x_label='$x$ $(m)$',
                y_label='$y$ $(m)$',
                vmin=-0.02,
                vmax=0.0,
                cbar_extend="both",
                cbar_label=r'$T$ $(^\circ {C})$')

            # Plotting frazil
            vnv_plot2d(\
                'FRAZIL',
                res_ice_seq,
                var_factor=1000.0,
                time=time,
                filled_contours=True,
                fig_size=fig_size,
                fig_name='img/Cf-2d_scalarmap_{}min'.format(minutes[idx]),
                cmap_name='Blues_r',
                x_label='$x$ $(m)$',
                y_label='$y$ $(m)$',
                cbar_label='$10^3$ $C_f$ (volume fraction)')

            # Plotting ice cover
            vnv_plot2d(\
                'SOLID ICE CONC.',
                res_ice_seq,
                time=time,
                filled_contours=True,
                fig_size=fig_size,
                fig_name='img/Ci-2d_scalarmap_{}min'.format(minutes[idx]),
                cmap_name='coolwarm',
                x_label='$x$ $(m)$',
                y_label='$y$ $(m)$',
                cbar_label='$C_i$ (surface fraction)')

            vnv_plot2d(\
                'SOLID ICE THICK.',
                res_ice_seq,
                time=time,
                var_factor=1000.0,
                filled_contours=True,
                fig_size=fig_size,
                fig_name='img/ti-2d_scalarmap_{}min'.format(minutes[idx]),
                cmap_name='coolwarm',
                x_label='$x$ $(m)$',
                y_label='$y$ $(m)$',
                cbar_label='$t_i$ (mm)')

            vnv_plot2d(\
                'TOTAL ICE THICK.',
                res_ice_seq,
                time=time,
                var_factor=1000.0,
                filled_contours=True,
                fig_size=fig_size,
                fig_name='img/titot-2d_scalarmap_{}min'.format(minutes[idx]),
                cmap_name='coolwarm',
                x_label='$x$ $(m)$',
                y_label='$y$ $(m)$',
                cbar_label='$t_{itot}$ (mm)')

            # Plotting ice cover type
            vnv_plot2d(\
                'CHARACTERISTICS ',
                res_ice_seq,
                time=time,
                filled_contours=True,
                fig_size=fig_size,
                fig_name='img/Ice-2d_scalarmap_{}min'.format(minutes[idx]),
                cmap_name='Blues_r',
                x_label='$x$ $(m)$',
                y_label='$y$ $(m)$',
                vmin=0.5, vmax=2.5, nv=3,
                cbar_ticks=[1., 2.],
                cbar_label='Ice cover type')

        # Closing files
        geo.close()

        #del res_ini
        res_seq.close()
        res_ice_seq.close()
