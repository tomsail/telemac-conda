
"""
Validation script for tide
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
        self.rank = 1
        self.tags = ['telemac2d']

    def _pre(self):
        """
        Defining the studies
        """

        # tide scalar mode type
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_tide-jmj_type.cas')


        # tide parallel mode type
        cas = TelemacCas('t2d_tide-jmj_type.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_tide-jmj_type_par.cas',
                       cas=cas)

        del cas


        # tide scalar mode real NEA prior
        self.add_study('vnv_3',
                       'telemac2d',
                       't2d_tide-NEA_prior_real.cas')


        # tide parallel mode real NEA prior
        cas = TelemacCas('t2d_tide-NEA_prior_real.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'telemac2d',
                       't2d_tide-NEA_prior_real_par.cas',
                       cas=cas)

        del cas


        # tide scalar mode type NEA prior
        self.add_study('vnv_5',
                       'telemac2d',
                       't2d_tide-NEA_prior_type.cas')


        # tide parallel mode type NEA prior
        cas = TelemacCas('t2d_tide-NEA_prior_type.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_6',
                       'telemac2d',
                       't2d_tide-NEA_prior_type_par.cas',
                       cas=cas)

        del cas


        # tide scalar mode real gen
        self.add_study('vnv_7',
                       'telemac2d',
                       't2d_tide-jmj_real_gen.cas')


        # tide scalar mode type gen
        self.add_study('vnv_8',
                       'telemac2d',
                       't2d_tide-jmj_type_gen.cas')


        # tide on translated mesh scalar mode real gen
        self.add_study('vnv_9',
                       'telemac2d',
                       't2d_tide_local-jmj_real_gen.cas')


        # tide on translated mesh scalar mode type gen
        self.add_study('vnv_10',
                       'telemac2d',
                       't2d_tide_local-jmj_type_gen.cas')



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_tide-jmj_type.slf',
                            eps=[4.E-3, 5.E-3, 4.E-4, 4.E-4, 3.E-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_tide-jmj_type.slf',
                            eps=[5.E-3, 4.E-3, 5.E-4, 5.E-4, 3.E-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[5.E-3, 4.E-3, 5.E-4, 5.E-4, 4.E-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:T2DRES',
                            'f2d_tide-NEA_prior_real.slf',
                            eps=[3.E-3, 2.E-3, 2.E-2, 2.E-2, 3.E-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_4:T2DRES',
                            'f2d_tide-NEA_prior_real.slf',
                            eps=[3.E-3, 2.E-3, 2.E-2, 2.E-2, 3E-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:T2DRES',
                            'vnv_4:T2DRES',
                            eps=[3.E-3, 2.E-3, 2.E-2, 2.E-2, 4.E-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_5:T2DRES',
                            'f2d_tide-NEA_prior_type.slf',
                            eps=[5.E-1, 4.E-1, 3.E-1, 3.E-1, 4.E-1])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_6:T2DRES',
                            'f2d_tide-NEA_prior_type.slf',
                            eps=[3.E-1, 4.E-1, 3.E-1, 3.E-1, 3.E-1])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_5:T2DRES',
                            'vnv_6:T2DRES',
                            eps=[5.E-1, 4.E-1, 3.E-1, 3.E-1, 5.E-1])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_7:T2DRES',
                            'f2d_tide-jmj_real_gen.slf',
                            eps=[1.3E-3, 1.2E-3, 2.E-4, 2.E-4, 1.7E-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_8:T2DRES',
                            'f2d_tide-jmj_type_gen.slf',
                            eps=[4.E-3, 3.E-3, 4.E-4, 4.E-4, 3.E-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_9:T2DRES',
                            'f2d_tide_local-jmj_real_gen.slf',
                            eps=[7.E-4, 8.E-4, 7.E-4, 7.E-4, 1.E-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_10:T2DRES',
                            'f2d_tide_local-jmj_type_gen.slf',
                            eps=[4.E-3, 4.E-3, 4.E-4, 4.E-4, 3.E-3])


    def _post(self):
        """
        Post-treatment processes
        """

        from postel.plot_vnv import vnv_plot1d_polylines, vnv_plot2d

        # Getting files
        geom, _ = self.get_study_res('vnv_1:T2DGEO', load_bnd=True)
        res, _ = self.get_study_res('vnv_1:T2DRES')

        # Plot mesh
        vnv_plot2d('',
            geom,
            fig_size=(7.5, 7),
            fig_name='img/tide_mesh',
            annotate_bnd=True,
            plot_mesh=True)

        # Plot Water depth:
        vnv_plot2d('WATER DEPTH', res,
            record=-1,
            fig_size=(7.5, 7),
            fig_name='img/tide_water_depth',
            cbar_label='H (m)',
            x_label='x (m)', y_label='y (m)',
            filled_contours=True,
            mask_tidal_flats=True)

        # Plot free surface elevation:
        vnv_plot2d('FREE SURFACE', res,
            record=-1,
            fig_size=(7.5, 7),
            fig_name='img/tide_elevation',
            cbar_label='Z (m CD)',
            x_label='x (m)', y_label='y (m)',
            filled_contours=True,
            mask_tidal_flats=True)

        # Plot velocity:
        vnv_plot2d('VELOCITY', res,
            record=-1,
            fig_size=(7.5, 7),
            fig_name='img/tide_velocity',
            cbar_label='Velocity (m/s)',
            x_label='x (m)', y_label='y (m)',
            filled_contours=True,
            mask_tidal_flats=True)

        # Plot velocity vectors:
        vnv_plot2d('VELOCITY', res,
            record=-1,
            fig_size=(7.5, 7),
            fig_name='img/tide_velocity_vectors',
            cbar_label='Velocity (m/s)',
            x_label='x (m)', y_label='y (m)',
            filled_contours=True,
            mask_tidal_flats=True,
            vectors=True, vectors_scale=20,
            grid_resolution=[15, 15])

        # Plot velocity streamlines:
        vnv_plot2d('VELOCITY', res,
            record=-1,
            fig_size=(7.5, 7),
            fig_name='img/tide_velocity_streamlines',
            cbar_label='Velocity (m/s)',
            x_label='x (m)', y_label='y (m)',
            filled_contours=True,
            mask_tidal_flats=True,
            streamlines=True, streamlines_density=3,
            grid_resolution=[50, 50])

        # Closing files
        geom.close()
        res.close()
