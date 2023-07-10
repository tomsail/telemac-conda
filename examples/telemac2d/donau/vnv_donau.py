
"""
Validation script for donau
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
        self.listing = True

    def _pre(self):
        """
        Defining the studies
        """

        # donau scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_donau.cas')


        # donau parallel mode
        cas = TelemacCas('t2d_donau.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_donau_par.cas',
                       cas=cas)

        del cas


    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_donau.slf',
                            eps=[4.E-2, 6.E-2, 4.E-3, 4.E-3, 1.E-8, 4.E-2, 1.E-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_donau.slf',
                            eps=[5.E-2, 7.E-2, 6.E-3, 6.E-3, 1.E-8, 6.E-2, 1.E-8])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[3.E-2, 4.1E-2, 5.E-3, 5.E-3, 1.E-8, 3.2E-2, 1.E-8])

    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_polylines
        import numpy as np
        from data_manip.computation.polyline_integrals import flux_2d
        from postel.plot1d import plot1d
        import matplotlib.pyplot as plt
        from postel.parser_output import get_latest_output_files
        from postel.parser_output import OutputFileData
        import re

        vnv_1_t2dgeo = self.get_study_file('vnv_1:T2DGEO')
        res_vnv_1_t2dgeo = TelemacFile(vnv_1_t2dgeo)
        vnv_1_t2dres = self.get_study_file('vnv_1:T2DRES')
        res_vnv_1_t2dres = TelemacFile(vnv_1_t2dres)
        vnv_2_t2dres = self.get_study_file('vnv_2:T2DRES')
        res_vnv_2_t2dres = TelemacFile(vnv_2_t2dres)
        geom, _ = self.get_study_res('vnv_1:T2DGEO', load_bnd=True)

        # Load all results as a list:
        res_list, res_labels = self.get_study_res(module='T2D')


       # extract mass fluxes from the listing
        liste = ['1','2']
        mass1 = []
        mass2 = []
        times = []

        for i in liste:
            t_form1 = re.compile(r'\s*FLUX BOUNDARY    1', re.I)
            t_form2 = re.compile(r'\s*FLUX BOUNDARY    2', re.I)
            cas_file = self.studies['vnv_'+i].steering_file
            file_name = get_latest_output_files(cas_file)
            print('listing: ',file_name)
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
                mass_flux1.append(float(j.split()[3]))
            for j in my_grep_t2:
                mass_flux2.append(-float(j.split()[3]))
            mass1.append(mass_flux1)
            mass2.append(mass_flux2)
            times.append(my_grep[1][1])

        # plot fluxes
        fig, ax = plt.subplots(figsize=(10,5))
        plot1d(ax,times[0],mass1[0],plot_label='inlet flux')
        plot1d(ax,times[0],mass2[0],
        x_label='time (s)',
        y_label='fluxes (m\u00B3/s)',
        plot_label='outlet flux')
        ax.set_ylim(bottom=-218.1,top=-217.9)
#        ax.set_xlim(left=352100)
        ax.legend()
        fig_name='img/fluxes'
#        print(" "*8+'~> Plotting '+fig_name)
        plt.savefig(fig_name)
        plt.close('all')

       # Plot mesh with boundaries
        vnv_plot2d(\
            '',
        geom,
        aspect_ratio='equal',
        fig_size=(10, 5),
        fig_name='img/donau_mesh',
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

        # Plotting grid part with bridge piers
        vnv_plot2d(\
                   '',
                   geom,
                   aspect_ratio='equal',
                   xlim=[69500,70000],
                   ylim=[10300,10800],
                   fig_size=(10, 10),
                   fig_name='img/gridpiers',
                   plot_mesh=True)

        vnv_plot2d('BOTTOM',
                   res_vnv_1_t2dres,
                   record=0,
                   filled_contours=True,
                   cbar_label='Bottom elevation (m+NN)',
                   fig_size=(10, 5),
                   aspect_ratio='equal',
                   fig_name='img/Bathy')


        # Plotting BOTTOM FRICTION
        vnv_plot2d('BOTTOM FRICTION',
                   res_vnv_1_t2dres,
                   record=0,
                   filled_contours=True,
                   cbar_label='Nikuradse coefficient (m)',
                   cbar_ticks=(0.025,0.1,0.15,0.3,0.5,1.0),
                   vmax=1.0,
                   cbar_extend='both',
                   fig_size=(10, 5),
                   aspect_ratio='equal',
                   fig_name='img/friction')

        # Plotting FREE SURFACE at 0
        vnv_plot2d('FREE SURFACE',
                   res_vnv_1_t2dres,
                   record=0,
                   filled_contours=True,
                   cbar_label='Free Surface (m+NN)',
                   aspect_ratio='equal',
                   fig_size=(10, 5),
                   fig_name='img/FreeSurface0')

        # Plotting SCALAR VELOCITIES at 0
        vnv_plot2d('SCALAR VELOCITY',
                   res_vnv_1_t2dres,
                   record=0,
                   filled_contours=True,
                   cbar_label='Scalar velocities (m/s)',
                   aspect_ratio='equal',
                   fig_size=(10, 5),
                   fig_name='img/ini_flow')

        # Plotting SCALAR VELOCITIES at final state
        vnv_plot2d('SCALAR VELOCITY',
                   res_vnv_1_t2dres,
                   record=-1,
                   filled_contours=True,
                   cbar_label='Scalar velocities (m/s)',
                   xlim=[69500,70000],
                   ylim=[10300,10800],
                   vmin=0.0,
                   vmax=0.5,
                   cbar_extend='both',
                   aspect_ratio='equal',
                   fig_size=(10, 10),
                   fig_name='img/final_flow')
       #----------------------------------------------------------------------
        # Comparison of free surface (1D slice):

      # read measured water levels (Do-km, xleft, yleft,xright, yright)
        measured_data = np.genfromtxt("donau_hek.dat",skip_header=2, names=["Dokm", "xl", "yl", "xr", "yr"])
        meas_Dokm = measured_data["Dokm"]
        meas_xl = measured_data["xl"]
        meas_yl = measured_data["yl"]
        meas_xr = measured_data["xr"]
        meas_yr = measured_data["yr"]

      # read measured water levels (Do-km, water level)
        measured_data = np.genfromtxt("donau_fix.dat",skip_header=0, names=["Dokm", "wl"])
        meas_Dokm = measured_data["Dokm"]
        meas_wl = measured_data["wl"]


        # calculation of mid point between left and right points
        meas_x = np.asarray((meas_xl+meas_xr)/2.0)
        meas_y = np.asarray((meas_yl+meas_yr)/2.0)
        polyread=np.column_stack((meas_x,meas_y))

        #getting water levels for each point of the polyline
        poly_coord1, abs_curv1,values_polylines1=res_vnv_1_t2dres.get_timeseries_on_polyline('FREE SURFACE', polyread)
#        poly_coord2, abs_curv2,values_polylines2=res_vnv_2_t2dres.get_timeseries_on_polyline('FREE SURFACE', polyread)
        #initialising figure
        fig, ax = plt.subplots(figsize=(10,5))

        plot1d(ax,meas_Dokm,meas_wl,marker='o',
        plot_label='measurements')
        plot1d(ax,2291.4-abs_curv1/1000,values_polylines1[:,-1],
#        plot1d(ax,abs_curv2/1000.+812.5,values_polylines2[:,-1],
        plot_label='simulation',
        x_label='Do-km',
        y_label='Free surface (m+NN)')
        ax.set_xlim(2292,2283)
        ax.set_ylim(308,310)
        ax.legend()
        fig_name = 'img/diff_free_surface'
        print(" "*8+'~> Plotting '+fig_name)
        plt.savefig(fig_name)
        plt.close()

