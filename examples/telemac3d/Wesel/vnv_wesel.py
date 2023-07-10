
"""
Validation script for Wesel
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
        self.tags = ['telemac3d']
        self.listing = True

    def _pre(self):
        """
        Defining the studies
        """

        # Wesel scalar mode
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_wesel.cas')


        # Wesel parallel mode
        cas = TelemacCas('t3d_wesel.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_wesel_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_wesel.slf',
                            eps=[2.E-1, 2.5E0, 1.2E0, 5.E-1])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_wesel.slf',
                            eps=[2.1E-1, 2.3E0, 1.2E0, 5.E-1])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[2.E-1, 2.5E0, 1.6E0, 6.E-1])


    def _post(self):
        """
        Post-treatment processes
        """
        import re
        import matplotlib.pyplot as plt
        import numpy as np
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_polylines
        from postel.plot1d import plot1d
        from postel.parser_output import get_latest_output_files
        from postel.parser_output import OutputFileData

                # Getting files
        vnv_1_t3dres = self.get_study_file('vnv_1:T3DRES')
        res_vnv_1_t3dres = TelemacFile(vnv_1_t3dres)

        vnv_1_t3dhyd = self.get_study_file('vnv_1:T3DHYD')
        res_vnv_1_t3dhyd = TelemacFile(vnv_1_t3dhyd)

       # extract mass fluxes from the listing
        liste = ['1']
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
#        print(" "*8+'~> Plotting '+fig_name)
        plt.savefig(fig_name)
        plt.close('all')


        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_t3dres,
                   plot_mesh=True,
                   fig_size=(12, 7),
                   fig_name='img/wesel_mesh0')

        # Plotting vertical grid
        #Plotting vertical mesh
        vnv_plot2d('ELEVATION Z',
                   res_vnv_1_t3dres,
                   poly=[[3677, 3234], [3767, 2954]],
                   record=0,
                   y_label='z (m)',
                   plot_mesh=True,
                   fig_size=(12, 5),
                   fig_name='img/vertical_grid')

        # Plotting BOTTOM at 0
        vnv_plot2d('BOTTOM',
                   res_vnv_1_t3dhyd,
                   record=0,
                   filled_contours=True,
                   cbar_label='Bottom elevation (m+NN)',
                   fig_size=(10, 5),
                   aspect_ratio='equal',
                   fig_name='img/Bathy')

        # Plotting FREE SURFACE at 0
        vnv_plot2d('FREE SURFACE',
                   res_vnv_1_t3dhyd,
                   record=0,
                   filled_contours=True,
                   cbar_label='Free Surface (m+NN)',
                   aspect_ratio='equal',
                   fig_size=(10, 5),
#                   xlim=[0,8000],
#                   ylim=[0,4000],
                   fig_name='img/FreeSurface0')

        # Plotting FREE SURFACE at -1
        vnv_plot2d('FREE SURFACE',
                   res_vnv_1_t3dhyd,
                   record=-1,
                   filled_contours=True,
                   aspect_ratio='equal',
                   cbar_label='Free Surface (m+NN)',
                   fig_size=(10, 5),
                   fig_name='img/FreeSurface')

        # Plotting vertical slice of vectors velocity
        vnv_plot2d('VELOCITY',
                   res_vnv_1_t3dres,
                   poly=[[3677, 3234], [3767, 2954]],
                   record=-1,
                   y_label='z (m)',
                   filled_contours=True,
                   vectors=True,
                   vectors_scale=40,
                   cbar_label='Scalar velocity (m/s)',
                   fig_size=(12, 5),
                   fig_name='img/veloV')

        #----------------------------------------------------------------------
        # Comparison of free surface (1D slice):

        # read measured water levels (xleft, yleft, xright, yright,
        # water levels, Rh-km)
        measured_data = np.genfromtxt(
            "fo1_wesel.txt",
            skip_header=2,
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
            res_vnv_1_t3dhyd.get_timeseries_on_polyline('FREE SURFACE',
                                                        polyread)
        #initialising figure
        _, ax = plt.subplots(figsize=(10, 5))

        plot1d(ax, meas_rhkm, meas_wl, marker='o',
               plot_label='measurements')
        plot1d(ax, abs_curv1/1000.+812.5, values_polylines1[:, -1],
               plot_label='simulation',
               x_label='Rh-km',
               y_label='Free surface (m+NN)')
        ax.legend()
        fig_name = 'img/diff_free_surface'
        print(" "*8+'~> Plotting '+fig_name)
        plt.savefig(fig_name)
        plt.close('all')

        # Closing files
        res_vnv_1_t3dres.close()
        res_vnv_1_t3dhyd.close()
