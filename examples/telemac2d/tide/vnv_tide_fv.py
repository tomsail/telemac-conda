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
        self.tags = ['telemac2d','fv']

    def _pre(self):
        """
        Defining the studies
        """

        # tide scalar mode type
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_tide-jmj_type_fv.cas')


        # tide parallel mode type
        cas = TelemacCas('t2d_tide-jmj_type_fv.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_tide-jmj_type_fv_par.cas',
                       cas=cas)

        del cas

    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_tide-jmj_type_fv.slf',
                            eps=[1.E-7])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1.E-7])

    def _post(self):
        """
        Post-treatment processes
        """

        from postel.plot_vnv import vnv_plot1d_polylines, vnv_plot2d, vnv_plot1d_history

        # Getting files
        geom, _ = self.get_study_res('vnv_1:T2DGEO', load_bnd=True)
        res, _ = self.get_study_res('vnv_1:T2DRES')

        # Plot mesh
        vnv_plot2d('',
            geom,
            fig_size=(7, 7),
            fig_name='img/tide_mesh',
            annotate_bnd=True,
            plot_mesh=True)

        # Plot Water depth:
        vnv_plot2d('WATER DEPTH', res,
            record=-1,
            fig_size=(7.5, 7),
            fig_name='img/tide_water_depth_fv',
            cbar_label='H (m)',
            x_label='x (m)', y_label='y (m)',
            filled_contours=True,
            mask_tidal_flats=True)

        # Plot free surface elevation:
        vnv_plot2d('FREE SURFACE', res,
            record=-1,
            fig_size=(7.5, 7),
            fig_name='img/tide_elevation_fv',
            cbar_label='Z (m CD)',
#            for similar min/max values of data to plot + number of sample for colorbar range as Finite Elements computation
#            vmin=9.0,vmax=11.4,nv=9,
            x_label='x (m)', y_label='y (m)',
            filled_contours=True,
            mask_tidal_flats=True)

        # Plot velocity:
        vnv_plot2d('VELOCITY', res,
            record=90,
            fig_size=(7.5, 7),
            fig_name='img/tide_velocity_fv',
            cbar_label='Velocity (m/s)',
#            for similar max values of data to plot + number of sample for colorbar range as Finite Elements computation
#            vmax=1.2,nv=9,
            x_label='x (m)', y_label='y (m)',
            filled_contours=True,
            mask_tidal_flats=True)

        # Plot velocity vectors:
        vnv_plot2d('VELOCITY', res,
            record=90,
            fig_size=(7.5, 7),
            fig_name='img/tide_velocity_vectors_fv',
            cbar_label='Velocity (m/s)',
#            for similar max values of data to plot + number of sample for colorbar range as Finite Elements computation
#            vmax=1.2,nv=9,
            x_label='x (m)', y_label='y (m)',
            filled_contours=True,
            mask_tidal_flats=True,
            vectors=True, vectors_scale=20,
            grid_resolution=[15, 15])

        # Plot velocity streamlines:
        vnv_plot2d('VELOCITY', res,
            record=90,
            fig_size=(7.5, 7),
            fig_name='img/tide_velocity_streamlines_fv',
            cbar_label='Velocity (m/s)',
#            for similar max values of data to plot + number of sample for colorbar range as Finite Elements computation
#                   vmax=1.2,nv=9,
            x_label='x (m)', y_label='y (m)',
            filled_contours=True,
            mask_tidal_flats=True,
            streamlines=True, streamlines_density=3,
            grid_resolution=[50, 50])

        # Plot free surface elevation over time
        vnv_plot1d_history('FREE SURFACE',res,
                points=[(195000,150000)],
                fig_name='img/tide_time_fv',
                x_label='Time (s)', y_label='Free surface (m CD)')

        # Closing files
        geom.close()
        res.close()
