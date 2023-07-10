
"""
Validation script for cvsm
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
        self.tags = ['telemac2d', 'gaia']
        # Forcing listing
        self.listing = True

    def _pre(self):
        """
        Defining the studies
        """

        # cvsm T2D+GAI scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_cvsm.cas')


        # cvsm T2D+GAI parallel mode
        cas = TelemacCas('t2d_cvsm.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_cvsm_par.cas',
                       cas=cas)

        # cvsm T2D+GAI ALT formula 1
        self.add_study('vnv_alt_1',
                       'telemac2d',
                       't2d_cvsm1.cas')

        # cvsm T2D+GAI ALT formula 2
        self.add_study('vnv_alt_2',
                       'telemac2d',
                       't2d_cvsm2.cas')

        # cvsm T2D+GAI ALT formula 3
        self.add_study('vnv_alt_3',
                       'telemac2d',
                       't2d_cvsm3.cas')

        # cvsm T2D+GAI ALT formula 4
        self.add_study('vnv_alt_4',
                       'telemac2d',
                       't2d_cvsm4.cas')

        # cvsm T2D+GAI ALT formula 5
        self.add_study('vnv_alt_5',
                       'telemac2d',
                       't2d_cvsm5.cas')

        # cvsm T2D+GAI ALT formula 6
        self.add_study('vnv_alt_6',
                       'telemac2d',
                       't2d_cvsm6.cas')


#        del cas

    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:GAIRES',
                            'gai_ref_cvsm.slf',
                            eps=[1.e-1])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:GAIRES',
                            'gai_ref_cvsm.slf',
                            eps=[1.e-1])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:GAIRES',
                            'vnv_2:GAIRES',
                            eps=[1.e-1])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_cvsm.slf',
                            eps=[1.e-1])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_cvsm.slf',
                            eps=[1.e-1])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1.e-1])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.parser_output import get_latest_output_files
        from postel.parser_output import OutputFileData
        from postel.plot_vnv import vnv_plot1d
        import matplotlib.pyplot as plt
        import numpy as np

        # mass balance for vnv_1 and all classes
        cas_file = self.studies['vnv_1'].steering_file

        file_name = get_latest_output_files(cas_file)
        file_name = file_name[0]
        out_file = OutputFileData(file_name)
        mass = out_file.get_sediment_mass_profile()
        tmp_iterations, tmp_times = out_file.get_time_profile()
        _, iterations = tmp_iterations
        _, times = tmp_times

        for sed_class in mass:
           for ttype in ['Total', 'Lost']:

               title = "{}_mass_{:02d}".format(ttype.lower(), sed_class)

               vnv_plot1d(times[2::2], mass[sed_class][ttype.lower()],
                          [title],
                          x_label="time (s)", y_label="{} mass (kg)".format(ttype),
                          fig_name="img/"+title+".png")

        # comparison with measurements
        def yen(file_name, in_title):
            """
            Custom plot

            @param file_name (str) Name of the result file
            @parma in_title (str) Title of the fig and part of name of file
            """
            # factor is used for scaling (evolution/h0_factor = evolutionCut)
            # See the paper of Yen et al. 1995
            import matplotlib.tri as tri
            from postel.plot1d import plot1d

            slf = TelemacFile(file_name)
            mesh = np.array(slf.ikle3)
            triang = tri.Triangulation(slf.meshx, slf.meshy, mesh)

            evolution = slf.get_data_value('CUMUL BED EVOL', -1)
            evolution_interpolator = \
                    tri.LinearTriInterpolator(triang, evolution)

            # Read the reference file
            h0_factor = 0.0544

            #load profile 90
            data_profile_90 = np.genfromtxt("data/yen_90profilexy-koor.dat",
                                            names=["x", "y", "z"])
            profile_ref_90x = data_profile_90["x"]
            profile_ref_90y = data_profile_90["y"]
            profile_ref_90z = data_profile_90["z"]
            evolution_cut_90 = \
              evolution_interpolator.__call__(profile_ref_90x,
                                              profile_ref_90y)/h0_factor

            distance_profile_90 = profile_ref_90y - 46.1371

            #load profile 180
            data_profile_180 = np.genfromtxt("data/yen_180profilexy-koor.dat",
                                             names=["x", "y", "z"])
            profile_ref_180x = data_profile_180["x"]
            profile_ref_180y = data_profile_180["y"]
            profile_ref_180z = data_profile_180["z"]
            evolution_cut_180 = \
               evolution_interpolator.__call__(profile_ref_180x,
                                               profile_ref_180y)/h0_factor

            distance_profile_180 = profile_ref_180x - 104.682

            #load profile 90 grain size
            # simulation must be multiplied by 1000 for mm

            data_profile_90_dm =  np.genfromtxt("data/yen_90profilexy-dm.dat",names=["x","y","z"])
            profileRef90Xdm = data_profile_90_dm["x"]
            profileRef90Ydm = data_profile_90_dm["y"]
            profileRef90Zdm = data_profile_90_dm["z"]
            meandiameter = slf.get_data_value('MEAN DIAMETER M', -1)
            meandiameter_interpolator = \
                    tri.LinearTriInterpolator(triang, meandiameter)
            meandiameter_90dm = meandiameter_interpolator.__call__(profileRef90Xdm,profileRef90Ydm)*1000

            distance_profile_90dm = profileRef90Ydm - 46.1371


            ###################################################################
            #
            # Plot with matplotlib
            #
            ###################################################################

            #plot 90
            fig, ax = plt.subplots(figsize=(6, 4))
            plot1d(ax, distance_profile_90, profile_ref_90z,
               plot_label="Reference",
               marker="o",
               ls='-')
            plot1d(ax, distance_profile_90, evolution_cut_90,
               x_label='Distance [m]',
               y_label='Normalised evolution [-]',
               plot_label="Simulation",
               marker=">",
               ls='--')

            ax.legend()
            plt.xlim([0.0,1.0])
            title = in_title +"_90"
            ax.set_title(title)
            print(" "*8+"~> Plotting "+title)
            plt.savefig('img/' + title + ".png")
            plt.close('all')

            #plot 180
            fig, ax = plt.subplots(figsize=(6, 4))
            plot1d(ax, distance_profile_180, profile_ref_180z,
               plot_label="Reference",
               marker="o",
               ls='-')
            plot1d(ax, distance_profile_180, evolution_cut_180,
               x_label='Distance [m]',
               y_label='Normalised evolution [-]',
               plot_label="Simulation",
               marker=">",
               ls='--')

            ax.legend()
            plt.xlim([0.0,1.0])
            title = in_title +"_180"
            ax.set_title(title)
            print(" "*8+"~> Plotting "+title)
            plt.savefig('img/' + title + ".png")
            plt.close('all')


            #plot 90 grain size
            fig, ax = plt.subplots(figsize=(6, 4))
            plot1d(ax, distance_profile_90dm, profileRef90Zdm,
                   plot_label="Reference",
                   marker="o",
                   ls='-')
            plot1d(ax, distance_profile_90dm, meandiameter_90dm,
               x_label='Distance [m]',
                   y_label='Mean diameter [mm]',
               plot_label="Simulation",
                          marker=">",
               ls='--')

            ax.legend()
            plt.xlim([0.0,1.0])
            title = in_title +"_90_dm"
            ax.set_title(title)
            print(" "*8+"~> Plotting "+title)
            plt.savefig('img/' + title + ".png")
            plt.close('all')


            slf.close()

        def yen_contour(file_name, in_title):
            """
            Custom contour

            @param file_name (str) Name of the result file
            @parma in_title (str) Title of the fig and part of name of file
            """

            import matplotlib.image as image
            from postel.plot2d import plot2d_scalar_filled_contour,plot2d_image

            slf = TelemacFile(file_name)

            evolution = slf.get_data_value('CUMUL BED EVOL', -1)

            #scale evolution
            h0_factor = 0.0544
            evolution = evolution/h0_factor

            data_xmin = 96.6819 -0.12
            data_xmax = 105.6819 + 0.4
            data_ymin = 42.6371 -1
            data_ymax = 42.6371

            title = in_title + "_EvolutionR05"

            xmin = data_xmin -0.2
            xmax = data_xmax -0.055

            ymin = data_ymin -1.3
            ymax = data_ymax + 4.83

            fig, ax = plt.subplots(1, 1, figsize=(12, 12))
            plot2d_scalar_filled_contour(fig, ax,
                    slf.tri, evolution,
                    nv=7,
                    vmin=-0.75,
                    vmax=0.75,
                    data_name='Normalised evolution (-)')
            plot2d_image(ax, image_file='data/PaperDataRun5.png',
                         extent=[xmin, xmax, ymin, ymax],
                         zorder=+1)
            plt.ylim([40,48])
            print(" "*8+"~> Plotting "+title)
            plt.savefig('img/' + title + ".png")
            plt.close('all')

            slf.close()



        yen(self.get_study_file('vnv_1:GAIRES'), 'gaia_cvsm')
        yen_contour(self.get_study_file('vnv_1:GAIRES'), 'gaia_cvsm')

        for ALT_form in [1, 2, 3, 4, 5, 6]:
            res_file = self.get_study_file('vnv_alt_{}:GAIRES'.format(ALT_form))
            title = 'gaia_cvsm_{}'.format(ALT_form)
            yen(res_file, title)
            yen_contour(res_file, title)

        #yen(self.get_study_file('vnv_3:GAIRES'), 'gaia_cvsm1')
        #yen_contour(self.get_study_file('vnv_1_vf:GAIRES'), 'gaia_cvsm1')

        # mass balance for vnv_alt_*
        for ALT_form in [1, 2, 3, 4, 5, 6]:
            cas_file = self.studies['vnv_alt_{}'.format(ALT_form)].steering_file

            file_name = get_latest_output_files(cas_file)
            file_name = file_name[0]
            out_file = OutputFileData(file_name)

            mass = out_file.get_sediment_mass_profile()

            tmp_iterations, tmp_times = out_file.get_time_profile()
            _, iterations = tmp_iterations
            _, times = tmp_times
            sed_class = 0
            ttype = 'Lost'

            data = np.zeros(len(mass[sed_class][ttype.lower()]))

            for i in range(0, len(data)):
                data[i] += data[-1] + mass[sed_class][ttype.lower()][i]

            title = "{}_{:02d}".format(ttype.lower(), sed_class)

            vnv_plot1d(times[2::2], data,
                       [title],
                       x_label="time (s)", y_label="Cumulated lost mass",
                       fig_name="img/multi{}_{}.png".format(ALT_form, title))
