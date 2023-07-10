
"""
Validation script for wesel
"""
from vvytel.vnv_study import AbstractVnvStudy
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
        self.listing = True

    def _pre(self):
        """
        Defining the studies
        """

        # wesel scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_wesel.cas')


        # wesel scalar mode
        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_wesel_pos.cas')


    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_wesel.slf',
                            eps=[0.5, 0.6, 0.05, 0.05, 1.E-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_wesel_pos.slf',
                            eps=[0.3, 0.21, 0.06, 0.06, 1.E-8])

        # Comparison between sequential and parallel run.
#        self.check_epsilons('vnv_1:T2DRES',
#                            'vnv_par:T2DRES',
#                            eps=[6e-3])

    def _post(self):
        """
        Post-treatment processes
        """
        import re
        import numpy as np
        import matplotlib.pyplot as plt
        from postel.plot1d import plot1d
        from postel.plot_vnv import vnv_plot2d
        from postel.parser_output import get_latest_output_files
        from postel.parser_output import OutputFileData

        # Getting files
        vnv_1_t2dgeo = self.get_study_file('vnv_1:T2DGEO')
        res_vnv_1_t2dgeo = TelemacFile(vnv_1_t2dgeo)
        vnv_1_t2dres = self.get_study_file('vnv_1:T2DRES')
        res_vnv_1_t2dres = TelemacFile(vnv_1_t2dres)

        vnv_2_t2dres = self.get_study_file('vnv_2:T2DRES')
        res_vnv_2_t2dres = TelemacFile(vnv_2_t2dres)

        geom, _ = self.get_study_res('vnv_1:T2DGEO', load_bnd=True)

       # extract mass fluxes from the listing
        liste = ['1', '2']
        mass1 = []
        mass2 = []
        times = []

        for i in liste:
            t_form1 = re.compile(r'\s*FLUX BOUNDARY\s+1', re.I)
            t_form2 = re.compile(r'\s*FLUX BOUNDARY\s+2', re.I)
            t_form3 = re.compile(r'\s*FLUX BOUNDARY\s+[0-9]+\s*:\s+(?P<val>[0-9.-]+)', re.I)
            cas_file = self.studies['vnv_'+i].steering_file
            file_name = get_latest_output_files(cas_file)
            file_name = file_name[0]
            out_file = OutputFileData(file_name)

            my_grep_t1 = out_file.get_user_defined_output(t_form1)
            my_grep_t2 = out_file.get_user_defined_output(t_form2)
            my_grep = out_file.get_time_profile()
            mass_flux1 = []
            mass_flux2 = []
            mass_flux1.append(0.)
            mass_flux2.append(0.)
            for j in my_grep_t1:
                res = t_form3.match(j)
                mass_flux1.append(float(res.group("val")))
            for j in my_grep_t2:
                res = t_form3.match(j)
                mass_flux2.append(-float(res.group("val")))
            mass1.append(mass_flux1)
            mass2.append(mass_flux2)
            times.append(my_grep[1][1])

        # plot fluxes
        _, ax = plt.subplots(figsize=(10, 5))
        plot1d(ax, times[0], mass1[0], plot_label='inlet flux')
        plot1d(ax, times[0], mass2[0],
               x_label='time (s)',
               y_label='fluxes m\u00B3/s/s',
               #y_lim=[1050,1100],
               plot_label='outlet flux')
        ax.legend()
        fig_name = 'img/fluxes'
        print(" "*8+'~> Plotting '+fig_name)
        plt.savefig(fig_name)
        plt.close('all')

        _, ax = plt.subplots(figsize=(10, 5))
        plot1d(ax, times[1], mass1[1], plot_label='inlet flux')
        plot1d(ax, times[1], mass2[1],
               x_label='time (s)',
               y_label='fluxes m\u00B3/s/s',
               plot_label='outlet flux_pos')
        ax.legend()
        fig_name = 'img/fluxes_pos'
        print(" "*8+'~> Plotting '+fig_name)
        plt.savefig(fig_name)
        plt.close('all')

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_t2dgeo,
                   plot_mesh=True,
                   fig_size=(10, 6),
                   fig_name='img/wesel_mesh0')

       # Plot mesh with boundaries
        vnv_plot2d(\
            '',
            geom,
            aspect_ratio='equal',
            fig_size=(10, 5),
            fig_name='img/wesel_mesh',
            annotate_bnd=True,
            plot_mesh=True)


        # Plotting BOTTOM at 0
        vnv_plot2d('BOTTOM',
                   res_vnv_1_t2dres,
                   record=0,
                   filled_contours=True,
                   cbar_label='Bottom elevation (m+NN)',
                   fig_size=(10, 5),
                   aspect_ratio='equal',
                   fig_name='img/Bathy')

        # Plotting FREE SURFACE at 0
        vnv_plot2d('FREE SURFACE',
                   res_vnv_1_t2dres,
                   record=0,
                   filled_contours=True,
                   cbar_label='Free Surface (m+NN)',
                   aspect_ratio='equal',
                   fig_size=(10, 5),
                   fig_name='img/FreeSurface0')

        # Plotting FREE SURFACE at -1
        vnv_plot2d('FREE SURFACE',
                   res_vnv_1_t2dres,
                   record=-1,
                   filled_contours=True,
                   aspect_ratio='equal',
                   cbar_label='Free Surface (m+NN)',
                   fig_size=(10, 5),
                   fig_name='img/FreeSurface_tf')

        # Plotting VELOCITY at -1
        vnv_plot2d('VELOCITY',
                   res_vnv_1_t2dres,
                   record=-1,
                   filled_contours=True,
                   aspect_ratio='equal',
                   cbar_label='Scalar velocity (m/s)',
                   fig_size=(10, 5),
                   fig_name='img/Velocity_tf')


        # Plotting FREE SURFACE at -1
        vnv_plot2d('FREE SURFACE',
                   res_vnv_2_t2dres,
                   record=-1,
                   filled_contours=True,
                   aspect_ratio='equal',
                   cbar_label='Free Surface (m+NN)',
                   fig_size=(10, 5),
                   fig_name='img/FreeSurface_tf_pos')


        # Plotting VELOCITY at -1
        vnv_plot2d('VELOCITY',
                   res_vnv_2_t2dres,
                   record=-1,
                   filled_contours=True,
                   aspect_ratio='equal',
                   cbar_label='Scalar velocity (m/s)',
                   fig_size=(10, 5),
                   fig_name='img/Velocity_tf_pos')


        #----------------------------------------------------------------------
        # Comparison of free surface (1D slice):

      # read measured water levels (xleft, yleft, xright, yright,
      #   water levels,Rh-km)
        measured_data = np.genfromtxt(
            "fo1_wesel.txt", skip_header=2,
            names=["xl", "yl", "xr", "yr", "wl", "Rhkm"])
        meas_xl = measured_data["xl"]
        meas_yl = measured_data["yl"]
        meas_xr = measured_data["xr"]
        meas_yr = measured_data["yr"]
        meas_wl = measured_data["wl"]
        meas_rhkm = measured_data["Rhkm"]
        # calculation of mid point between left and right points
        meas_x = np.asarray((meas_xl+meas_xr)/2.0)
        meas_y = np.asarray((meas_yl+meas_yr)/2.0)
        polyread = np.column_stack((meas_x, meas_y))

        #getting water levels for each point of the polyline
        _, abs_curv1, values_polylines1 = \
            res_vnv_1_t2dres.get_timeseries_on_polyline('FREE SURFACE',
                                                        polyread)
        _, abs_curv2, values_polylines2 = \
            res_vnv_2_t2dres.get_timeseries_on_polyline('FREE SURFACE',
                                                        polyread)
        #initialising figure
        _, ax = plt.subplots(figsize=(10, 5))

        plot1d(ax, meas_rhkm, meas_wl, marker='o',
               plot_label='measurements')
        plot1d(ax, abs_curv2/1000.+812.5, values_polylines2[:, -1],
               plot_label='wave equation')
        plot1d(ax, abs_curv1/1000.+812.5, values_polylines1[:, -1],
               plot_label='classic',
               x_label='Rh-km',
               y_label='Free surface (m+NN)')
        ax.legend()
        fig_name = 'img/diff_free_surface'
        print(" "*8+'~> Plotting '+fig_name)
        plt.savefig(fig_name)
        plt.close('all')

        # Closing files
        res_vnv_1_t2dgeo.close()
        res_vnv_1_t2dres.close()
        res_vnv_2_t2dres.close()
