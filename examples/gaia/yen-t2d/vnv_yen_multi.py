
"""
Validation script for yen_multi
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
        self.tags = ['telemac2d', 'gaia']
        # Forcing listing
        self.listing = True

    def _pre(self):
        """
        Defining the studies
        """

        # yen-exp_multi T2D+GAI scalar mode
        self.add_study('vnv_multi1',
                       'telemac2d',
                       't2d_yen-exp_multi1.cas')


        # yen-exp_multi T2D+GAI scalar mode
        for bed_form in [2, 3, 4, 5, 6, 7, 10, 30, 9]:
            t2d_steering_file = 't2d_yen-exp_multi{}.cas'.format(bed_form)
            gai_steering_file = 'gai_yen-exp_multi{}.cas'.format(bed_form)

            t2d_cas = TelemacCas('t2d_yen-exp_multi1.cas', get_dico('telemac2d'))
            t2d_cas.set('GAIA STEERING FILE', gai_steering_file)

            gai_cas = TelemacCas('gai_yen-exp_multi1.cas', get_dico('gaia'))
            gai_cas.set('BED-LOAD TRANSPORT FORMULA FOR ALL SANDS', bed_form)
            if bed_form == 9:
                gai_cas.set('LAYERS NON COHESIVE BED POROSITY',
                            [0.37500, 0.375, 0.375, 0.375])
                gai_cas.set('ACTIVE LAYER THICKNESS', 0.01)
                gai_cas.set('NUMBER OF LAYERS FOR INITIAL STRATIFICATION', 4)
            if bed_form == 3:
                gai_cas.set('MAXIMUM NUMBER OF ITERATIONS FOR POSITIVE THICKNESS', 50)

            if bed_form == 2 or bed_form == 3 or bed_form == 30 or bed_form == 4:
                gai_cas.set('FORMULA FOR SLOPE EFFECT',1)

            gai_cas.write(gai_steering_file)

            self.add_study('vnv_multi{}'.format(bed_form),
                           'telemac2d',
                           t2d_steering_file,
                           cas=t2d_cas)
            del t2d_cas
            del gai_cas


        for hid_form in [1, 2, 4]:
            t2d_steering_file = 't2d_yen-exp_multi1_hid{}.cas'.format(hid_form)
            gai_steering_file = 'gai_yen-exp_multi1_hid{}.cas'.format(hid_form)

            t2d_cas = TelemacCas('t2d_yen-exp_multi1.cas', get_dico('telemac2d'))
            t2d_cas.set('GAIA STEERING FILE', gai_steering_file)

            gai_cas = TelemacCas('gai_yen-exp_multi1.cas', get_dico('gaia'))
            gai_cas.set('HIDING FACTOR FORMULA', hid_form)
            gai_cas.write(gai_steering_file)

            self.add_study('vnv_multi1_hid{}'.format(hid_form),
                           'telemac2d',
                           t2d_steering_file,
                           cas=t2d_cas)
            del t2d_cas
            del gai_cas

    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi1:GAIRES',
                            'gai_ref_multi1.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi1:T2DRES',
                            'f2d_multi1.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi2:GAIRES',
                            'gai_ref_multi2.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi2:T2DRES',
                            'f2d_multi2.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi3:GAIRES',
                            'gai_ref_multi3.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi3:T2DRES',
                            'f2d_multi3.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi4:GAIRES',
                            'gai_ref_multi4.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi4:T2DRES',
                            'f2d_multi4.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi5:GAIRES',
                            'gai_ref_multi5.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi5:T2DRES',
                            'f2d_multi5.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi6:GAIRES',
                            'gai_ref_multi6.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi6:T2DRES',
                            'f2d_multi6.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi7:GAIRES',
                            'gai_ref_multi7.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi7:T2DRES',
                            'f2d_multi7.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi10:GAIRES',
                            'gai_ref_multi10.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi10:T2DRES',
                            'f2d_multi10.slf',
                            eps=[1e-3])
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi30:GAIRES',
                            'gai_ref_multi30.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi30:T2DRES',
                            'f2d_multi30.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi9:GAIRES',
                            'gai_ref_multi9.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi9:T2DRES',
                            'f2d_multi9.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi1_hid1:GAIRES',
                            'gai_ref_multi1_hid1.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi1_hid1:T2DRES',
                            'f2d_multi1_hid1.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi1_hid2:GAIRES',
                            'gai_ref_multi1_hid2.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi1_hid2:T2DRES',
                            'f2d_multi1_hid2.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi1_hid4:GAIRES',
                            'gai_ref_multi1_hid4.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi1_hid4:T2DRES',
                            'f2d_multi1_hid4.slf',
                            eps=[1e-3])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.parser_output import get_latest_output_files
        from postel.parser_output import OutputFileData
        from postel.plot_vnv import vnv_plot1d
        import matplotlib.pyplot as plt
        import numpy as np

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
               y_label='Normalised evolution [m]',
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
               y_label='Normalised evolution [m]',
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

            data_xmin = 96.6819 -0.08
            data_xmax = 105.6819 + 0.37
            data_ymin = 42.6371 + 0.95
            data_ymax = 42.6371 + 0.95

            title = in_title + "_EvolutionR04"

            xmin = data_xmin -0.2
            xmax = data_xmax -0.055

            ymin = data_ymin -3.3
            ymax = data_ymax + 3.83

            fig, ax = plt.subplots(1, 1, figsize=(12, 12))
            plot2d_scalar_filled_contour(fig, ax,
                    slf.tri, evolution,
                    nv=7,
                    vmin=-0.75,
                    vmax=0.75,
                    data_name='Normalised evolution (m)')
            plot2d_image(ax, image_file='data/PaperDataRun4.png',
                         extent=[xmin, xmax, ymin, ymax],
                         zorder=+1)
            plt.ylim([40,48])
            print(" "*8+"~> Plotting "+title)
            plt.savefig('img/' + title + ".png")
            plt.close('all')

            slf.close()

        for bed_form in [1, 2, 3, 4, 5, 6, 7, 10, 30, 9]:
            res_file = self.get_study_file('vnv_multi{}:GAIRES'.format(bed_form))
            title = 'gaia_yen-exp_multi{}'.format(bed_form)
            yen(res_file, title)
            yen_contour(res_file, title)

        for hid_form in [1, 2, 4]:
            res_file = self.get_study_file('vnv_multi1_hid{}:GAIRES'.format(hid_form))
            title = 'gaia_yen-exp_multi1_hid{}'.format(hid_form)
            yen(res_file, title)
            yen_contour(res_file, title)

        for bed_form in [1, 2, 3, 4, 5, 6, 7, 10, 30, 9]:
            cas_file = self.studies['vnv_multi{}'.format(bed_form)].steering_file

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
                       fig_name="img/multi{}_{}.png".format(bed_form, title))

