
"""
Validation script for ice_cover
"""
import matplotlib.pyplot as plt
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
        self.rank = 4
        self.tags = ['telemac2d', 'khione']

    def _pre(self):
        """
        Defining the studies
        """
        #----------------------------------------------------------------------
        # sequentiel
        self.add_study('vnv1_seq', 'telemac2d', 't2d_stlawrence.cas')
        # parallel TODO: problem with partitioning
#        cas = TelemacCas('t2d_stlawrence.cas', get_dico('telemac2d'))
#        cas.set('PARALLEL PROCESSORS', 4)
#        self.add_study('vnv1_par', 'telemac2d', 't2d_stlawrence.cas', cas=cas)
#        del cas

        #----------------------------------------------------------------------
        # sequentiel
        self.add_study('vnv2_seq', 'telemac2d', 't2d_stlawrence_dynice.cas')
        # parallel TODO: problem with partitioning
#        cas = TelemacCas('t2d_stlawrence_dynice.cas', get_dico('telemac2d'))
#        cas.set('PARALLEL PROCESSORS', 4)
#        self.add_study('vnv2_par', 'telemac2d', 't2d_stlawrence_dynice.cas', cas=cas)
#        del cas


    def _check_results(self):
        """
        Post-treatment processes
        """
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv1_seq:T2DRES', 'f2d_stlawrence.slf', eps=[1.E-2])
        self.check_epsilons('vnv1_seq:ICERES', 'fce_stlawrence.slf', eps=[1.E-1])
        self.check_epsilons('vnv2_seq:T2DRES', 'f2d_stlawrence_dynice.slf', eps=[1.E-2])
        self.check_epsilons('vnv2_seq:ICERES', 'fce_stlawrence_dynice.slf', eps=[1.E-1])

    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot1d, vnv_plot2d,\
            vnv_plot1d_history, vnv_plot1d_polylines

        cases = [1, 2]

        for case in cases:
            #==================================================================
            # GET TELEMAC RESULT FILES:
            #
            geo, _ = self.get_study_res('vnv{}_seq:T2DGEO'.format(case), load_bnd=True)
            res, _ = self.get_study_res('vnv{}_seq:T2DRES'.format(case))

            res_ice = TelemacFile(self.get_study_file('vnv{}_seq:ICERES'.format(case)))

            #==================================================================
            # DESCRIPTION PLOTS:
            #
            # Plot mesh
            vnv_plot2d('BOTTOM', geo, record=0,
                plot_mesh=True,
                filled_contours=False,
                annotate_bnd=True,
                fig_size=(14, 9),
                fig_name='img/mesh',
                x_label='$x$ $(m)$',
                y_label='$y$ $(m)$',
                aspect_ratio='equal')

            vnv_plot2d('BOTTOM', geo, record=0,
                plot_mesh=True,
                filled_contours=False,
                annotate_bnd=True,
                fig_size=(14, 9),
                fig_name='img/mesh2',
                x_label='$x$ $(m)$',
                y_label='$y$ $(m)$',
                aspect_ratio='equal')

            # Plot bottom
            vnv_plot2d('BOTTOM', geo, record=0,
                plot_mesh=False,
                filled_contours=True,
                fig_size=(14, 8),
                fig_name='img/bottom',
                x_label='$x$ $(m)$',
                y_label='$y$ $(m)$',
                aspect_ratio='equal')

            # Plot strikler zones
            vnv_plot2d('ZONES', geo, record=0,
                plot_mesh=False,
                filled_contours=True,
                fig_size=(14, 8),
                fig_name='img/zones',
                x_label='$x$ $(m)$',
                y_label='$y$ $(m)$',
                aspect_ratio='equal')

            del geo

            #==================================================================
            # HYDRO & TRACERS
            #
            # Plot water depth
            vnv_plot2d('WATER DEPTH', res, record=-1,
                plot_mesh=False,
                plot_only_dry_mesh=False,
                mask_tidal_flats=False,
                tidal_flats_threshold=0.01,
                filled_contours=True,
                fig_size=(14, 8),
                fig_name='img/water_depth_{}'.format(case),
                x_label='$x$ $(m)$',
                y_label='$y$ $(m)$',
                aspect_ratio='equal')

            # Plot temperature
            vnv_plot2d('TEMPERATURE', res, record=-1,
                filled_contours=True,
                fig_size=(14, 8),
                fig_name='img/temperature_{}'.format(case),
                x_label='$x$ $(m)$',
                y_label='$y$ $(m)$',
                vmin=-0.0005,
                vmax=0.0,
                cbar_extend="both",
                aspect_ratio='equal')

            # Plot frazil
            vnv_plot2d('FRAZIL', res_ice, record=-1,
                filled_contours=True,
                fig_size=(14, 8),
                fig_name='img/frazil_{}'.format(case),
                cmap_name='Blues_r',
                x_label='$x$ $(m)$',
                y_label='$y$ $(m)$',
                aspect_ratio='equal')

            # Plot velocity
            vnv_plot2d('VELOCITY', res, record=-1,
                filled_contours=True,
                vectors=True,
                vectors_scale=40,
                fig_size=(14, 8),
                grid_resolution=[75, 75],
                fig_name='img/velocity_{}'.format(case),
                x_label='$x$ $(m)$',
                y_label='$y$ $(m)$',
                aspect_ratio='equal')

            del res

            #==================================================================
            # ICE COVER
            #
            # Plot ice charac
            vnv_plot2d('CHARACTERISTICS ', res_ice, record=-1,
                filled_contours=True,
                fig_size=(14, 8),
                fig_name='img/ice_type_{}'.format(case),
                cmap_name='coolwarm',
                x_label='$x$ $(m)$',
                y_label='$y$ $(m)$',
                aspect_ratio='equal')

            # Plot ice thickness
            vnv_plot2d('TOTAL ICE THICK.', res_ice, record=-1,
                filled_contours=True,
                fig_size=(14, 8),
                fig_name='img/ice_titot_{}'.format(case),
                cmap_name='coolwarm',
                x_label='$x$ $(m)$',
                y_label='$y$ $(m)$',
                aspect_ratio='equal')

            if case == 2:
                # Plot ice charac
                vnv_plot2d('ICE COVER FRAC.', res_ice, record=-1,
                    filled_contours=True,
                    fig_size=(14, 8),
                    fig_name='img/ice_Ci_{}'.format(case),
                    cmap_name='coolwarm',
                    x_label='$x$ $(m)$',
                    y_label='$y$ $(m)$',
                    aspect_ratio='equal')

                # Plot ice thickness
                vnv_plot2d('ICE COVER THICK.', res_ice, record=-1,
                    filled_contours=True,
                    fig_size=(14, 8),
                    fig_name='img/ice_ti_{}'.format(case),
                    cmap_name='coolwarm',
                    x_label='$x$ $(m)$',
                    y_label='$y$ $(m)$',
                    aspect_ratio='equal')

            del res_ice

