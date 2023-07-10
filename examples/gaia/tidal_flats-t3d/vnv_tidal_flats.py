
"""
Validation script for tidal_flats
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
        self.tags = ['telemac3d', 'gaia']
        # Forcing listing
        self.listing = True

    def _pre(self):
        """
        Defining the studies
        """

        # Scalar run
        self.add_study('vnv_scalar',
                       'telemac3d',
                       't3d_tidal_flats.cas')


        # Parallel run
        cas = TelemacCas('t3d_tidal_flats.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_parall',
                       'telemac3d',
                       't3d_tidal_flats_par.cas',
                       cas=cas)

        # Parallel run with SETDEP=0
        new_t3d_cas_filename = 't3d_tidal_flats_par_setdep0.cas'
        new_gaia_cas_filename = 'gai_tidal_flats_setdep0.cas'


        # load gaia file and change the advection-diffusion scheme for settling
        casgai = TelemacCas('gai_tidal_flats.cas', get_dico('gaia'))
        casgai.set('ADVECTION-DIFFUSION SCHEME WITH SETTLING VELOCITY', 0)

        # write the gaia cas file to new file
        casgai.write(new_gaia_cas_filename)

        # change gaia steering filename in telemac3d cas file
        cas.set('GAIA STEERING FILE', new_gaia_cas_filename)

        # write the new t3d cas file
        cas.write(new_t3d_cas_filename)

        # run the SETDEP=0 scenario
        self.add_study('vnv_parall_setdep0',
	               'telemac3d',
	               new_t3d_cas_filename)

        del cas
        del casgai

    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_scalar:GAIRES',
                            'gai_ref_tidal_flats.slf',
                            eps=[1e-10])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_parall:GAIRES',
                            'gai_ref_tidal_flats.slf',
                            eps=[1e-10])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_scalar:GAIRES',
                            'vnv_parall:GAIRES',
                            eps=[1e-10])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_scalar:T3DRES',
                            'f3d_tidal_flats.slf',
                            eps=[1e-10])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_parall:T3DRES',
                            'f3d_tidal_flats.slf',
                            eps=[1e-10])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_scalar:T3DRES',
                            'vnv_parall:T3DRES',
                            eps=[1e-10])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.parser_output import get_latest_output_files
        from postel.parser_output import OutputFileData
        from postel.plot_vnv import vnv_plot2d
        from postel.plot1d import plot1d
        import matplotlib.pyplot as plt
        import numpy as np

        # Get files
        vnv_parall_t3dres = self.get_study_file('vnv_parall:T3DRES')
        res_vnv_parall_t3dres = TelemacFile(vnv_parall_t3dres)
        vnv_parall_t3dhyd = self.get_study_file('vnv_parall:T3DHYD')
        res_vnv_parall_t3dhyd = TelemacFile(vnv_parall_t3dhyd)

        plotrec=89

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_parall_t3dres,
                   plot_mesh=True,
                   fig_size=(10,1),
                   fig_name='img/mesh')


        # Plotting COH SEDIMENT1 at -1
        vnv_plot2d('COH SEDIMENT1',
                   res_vnv_parall_t3dhyd,
                   record=plotrec,
                   filled_contours=True,
                   cmap_name='viridis',
                   fig_size=(15, 3),
                   fig_name='img/sediment_plan_view')


        # TODO: change cmap
        # Plotting vertical split
        vnv_plot2d(\
                'COH SEDIMENT1',
                res_vnv_parall_t3dres,
                poly=[[19500, 0], [24999, 0]],
                record=plotrec,
                filled_contours=True,
                fig_size=(12, 7),
                x_label='X (m)',
                y_label='Z elevation (m)',
                fig_name='img/sediment_section')

        # TODO: change cmap
        # Plotting vertical split
        vnv_plot2d(\
                'VELOCITY U',
                res_vnv_parall_t3dres,
                poly=[[19500, 0], [24999, 0]],
                record=plotrec,
                filled_contours=True,
                fig_size=(12, 7),
                x_label='X (m)',
                y_label='Z elevation (m)',
                fig_name='img/velocityU_section')



        # Closing files
        res_vnv_parall_t3dres.close()
        res_vnv_parall_t3dhyd.close()

        #=================================================
        # Get the bed sediment mass balance for each model
        #=================================================

        cas_file1 = self.studies['vnv_parall'].steering_file
        cas_file2 = self.studies['vnv_parall_setdep0'].steering_file

        file_name1 = get_latest_output_files(cas_file1)
        file_name2 = get_latest_output_files(cas_file2)

        file_name1 = file_name1[0]
        out_file1 = OutputFileData(file_name1)
        file_name2 = file_name2[0]
        out_file2 = OutputFileData(file_name2)

        mass1 = out_file1.get_sediment_mass_profile()
        mass2 = out_file2.get_sediment_mass_profile()

        tmp_iterations1, tmp_times1 = out_file1.get_time_profile()
        _, iterations1 = tmp_iterations1
        _, times1 = tmp_times1

        tmp_iterations2, tmp_times2 = out_file2.get_time_profile()
        _, iterations2 = tmp_iterations2
        _, times2 = tmp_times2

        sed_class = 1
        ttype = 'lost'

        beddata1 = np.zeros(len(mass1[sed_class][ttype.lower()]))
        beddata2 = np.zeros(len(mass2[sed_class][ttype.lower()]))

        for i in range(1, len(beddata1)):
            beddata1[i] += beddata1[i-1] + mass1[sed_class][ttype.lower()][i]

        for i in range(1, len(beddata2)):
            beddata2[i] += beddata2[i-1] + mass2[sed_class][ttype.lower()][i]

        title = "{}_{:02d}".format(ttype.lower(), sed_class)


        # Plot bed mass error time series
        fig, ax = plt.subplots(figsize=(12, 7))
        plot1d(ax, times1[2::2], beddata1,
               plot_label='Cumulative bed mass error using SETDEP = 1')
        plot1d(ax, times2[2::2], beddata2,
               x_label='Time (s)',
               y_label='Mass error (kg)',
               plot_label='Cumulative bed mass error using SETDEP = 0')
        ax.legend()

        # Save figure
        fig_name = 'img/bed_mass_error_series'
        print(" "*8+'~> Plotting '+fig_name)
        plt.savefig(fig_name)
        plt.close('all')

        #===================================================
	# Get the suspended sediment mass balance for tracer
        #===================================================

        mass1 = out_file1.get_tracer_mass_profile()
        mass2 = out_file2.get_tracer_mass_profile()

        tracer_class = 1
        ttype = 'lost'

        susdata1 = np.zeros(len(mass1[tracer_class][ttype.lower()]))
        susdata2 = np.zeros(len(mass2[tracer_class][ttype.lower()]))

        for i in range(1, len(susdata1)):
            susdata1[i] += susdata1[i-1] + mass1[tracer_class][ttype.lower()][i]

        for i in range(1, len(susdata2)):
            susdata2[i] += susdata2[i-1] + mass2[tracer_class][ttype.lower()][i]

        # Plot suspended mass error timeseries
        # WARNING!!! Use of [:-1], not to take into account the last element of
        # the array as the error is not written in the listing file for the last
        # time step due to fix #766 (Final GAIA balance not written if not a
        # multiple of LISTING PRINTOUT PERIOD)
        fig, ax = plt.subplots(figsize=(12, 7))
        plot1d(ax, times1[2::2][:-1], susdata1,
               plot_label='Cumulative suspended mass error using SETDEP = 1')
        plot1d(ax, times2[2::2][:-1], susdata2,
               x_label='Time (s)',
               y_label='Mass error (kg)',
               plot_label='Cumulative suspended mass error using SETDEP = 0')
        ax.legend()

        # Save figure
        fig_name = 'img/sus_mass_error_series'
        print(" "*8+'~> Plotting '+fig_name)
        plt.savefig(fig_name)
        plt.close('all')

        #===================================================
	# Total mass error (suspended and bed mass)
        #===================================================

        totdata1 = np.zeros(len(mass1[sed_class][ttype.lower()]))
        totdata2 = np.zeros(len(mass2[sed_class][ttype.lower()]))

        for i in range(0, len(totdata1)):
            totdata1[i] += beddata1[i] + susdata1[i]

        for i in range(0, len(totdata2)):
            totdata2[i] += beddata2[i] + susdata2[i]

        # Plot total mass error timeseries
        fig, ax = plt.subplots(figsize=(12, 7))
        plot1d(ax, times1[2::2][:-1], totdata1,
               plot_label='Cumulative combined mass error using SETDEP = 1')
        plot1d(ax, times2[2::2][:-1], totdata2,
               x_label='Time (s)',
               y_label='Mass error (kg)',
               plot_label='Cumulative combined mass error using SETDEP = 0')
        ax.legend()

        # Save figure
        fig_name = 'img/tot_mass_error_series'
        print(" "*8+'~> Plotting '+fig_name)
        plt.savefig(fig_name)
        plt.close('all')
