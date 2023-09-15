
"""
Validation script for Malpasset
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation of Malpasset
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 4
        self.tags = ['telemac2d','fv']

    def _pre(self):
        """
        Defining the studies
        """
        # malpasset fine
        cas = TelemacCas('t2d_malpasset-fine.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('fine',
                       'telemac2d',
                       't2d_malpasset-fine_par.cas',
                       cas=cas)
        del cas

    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('fine:T2DRES',
                            'f2d_malpasset-fine.slf',
                            eps=[1.E-8])

    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d, vnv_plotbar
        from os import path

        # Getting files
        geo, _ = self.get_study_res('fine:T2DGEO', load_bnd=True)
        res, _ = self.get_study_res('fine:T2DRES')

        # figure limits
        xlim = [4500., 5500.]
        ylim = [3900., 4500.]

        #----------------------------------------------------------------------
        # Plotting mesh
        #
        vnv_plot2d(\
            '',
            geo,
            plot_mesh=True,
            fig_size=(13, 7),
            fig_name='img/Mesh_large',
            annotate_bnd=True)

        vnv_plot2d(
            '',
            geo,
            plot_mesh=True,
            fig_size=(14, 8),
            xlim=xlim, ylim=ylim,
            fig_name='img/Mesh_large_dam',
            annotate_bnd=True)

        #----------------------------------------------------------------------
        # FIRST OBSERVATION:
        #
        times = [0., 200., 400., 600.]
        for itm, record in enumerate([0, 2, 4, 6]):
            time_str = int(times[itm])
            fig_name = 'img/WD{}_{}_firstobs'.format(time_str, 'fine')
            vnv_plot2d(\
                'WATER DEPTH', res,
                record=record,
                fig_size=(10, 3.5),
                fig_title='$t = {}s$'.format(time_str),
                xlim=[3000., 12500.],
                ylim=[2000., 5500.],
                fig_name=fig_name,
                cbar_label='Water depth (m)',
                filled_contours=True,
                mask_tidal_flats=True,
                tidal_flats_threshold=0.01,
                plot_mesh=True,
                plot_only_dry_mesh=True)

        #----------------------------------------------------------------------
        # Plotting VELOCITY and WATER DEPTH maps:
        #
        fig_name = 'img/Velocity100_fine'
        vnv_plot2d(\
            'VELOCITY',
            res,
            var_type='vector',
            record=1,
            fig_size=(6.5, 4),
            fig_title='FINE',
            xlim=xlim, ylim=ylim,
            fig_name=fig_name,
            cbar_label='Velocity norm (m/s)',
            filled_contours=True,
            mask_tidal_flats=True,
            tidal_flats_threshold=0.01,
            plot_mesh=True,
            plot_only_dry_mesh=True,
            vectors=True,
            vectors_normalize=True,
            vectors_scale=30)

        fig_name = 'img/WDVelocity100_fine'
        vnv_plot2d(\
            'WATER DEPTH',
            res,
            var_type='scalar',
            record=1,
            fig_size=(6.5, 4),
            fig_title='FINE',
            xlim=xlim, ylim=ylim,
            fig_name=fig_name,
            cbar_label='Water depth (m)',
            filled_contours=True,
            mask_tidal_flats=True,
            tidal_flats_threshold=0.01,
            plot_mesh=True,
            plot_only_dry_mesh=True,
            vectors=True,
            vectors_normalize=True,
            vectors_scale=30)

        times = [0., 200., 400., 600.]
        for itm, record in enumerate([0, 2, 4, 6]):
            fig_name = 'img/WD{}_fine'.format(int(times[itm]))
            vnv_plot2d(\
                'WATER DEPTH',
                res,
                record=record,
                fig_size=(10, 3.5),
                fig_title='FINE',
                xlim=[3000., 12500.],
                ylim=[2000., 5500.],
                fig_name=fig_name,
                cbar_label='Water depth (m)',
                filled_contours=True,
                mask_tidal_flats=True,
                tidal_flats_threshold=0.01,
                plot_mesh=True,
                plot_only_dry_mesh=True)

            fig_name2 = 'img/WD_ZOOM{}_fine'.format(int(times[itm]))
            vnv_plot2d(\
                'WATER DEPTH',
                res,
                record=record,
                fig_size=(10, 6),
                fig_title='FINE',
                cbar_label='Water depth (m)',
                xlim=[4000., 10000.],
                ylim=[2000., 5500.],
                fig_name=fig_name2,
                filled_contours=True,
                mask_tidal_flats=True,
                tidal_flats_threshold=0.01,
                plot_mesh=True,
                plot_only_dry_mesh=True)

        #----------------------------------------------------------------------
        # Accuracy of the water depth:
        #
        # Observation of water depth on points P6 to P14
        point_labels = ['A', 'B', 'C', 'P6', 'P7', 'P8', 'P9', 'P10', \
                        'P11', 'P12', 'P13', 'P14']
        wd_obs = [84.2-43.9, 49.1-34.5, 54.0-30.0, 40.2-27.4, 34.9-23.1,\
                  27.4-19.1, 21.5-11.4, 16.1-9.3, 12.9-7.5]

        # find case rfo file
        rfo_file = path.join(
            self.get_vnv_working_dir('fine'),
            'rfo_malpasset-fine.txt')

        # read rfo file
        rfof = open(rfo_file, "r")
        lines = rfof.readlines()
        wd_line = False
        wd_i = 0
        wd = [] # list of water depth on points P6 to P14
        for idx, line in enumerate(lines):
            if line.startswith("MEASUREMENT POINT"):
                wd_line = True
            if wd_line:
                wd_i += 1
                if wd_i <= 9:
                    wd.append(float(line.split(' ')[-2]))

        # compute relative error (percentage):
        abs_err = [abs(wd[i] - wd_obs[i]) for i in range(len(wd))]
        rel_err = [(wd[i] - wd_obs[i])/wd_obs[i]*100 for i in range(len(wd))]

        # plot
        vnv_plotbar(\
            [wd_obs, wd, abs_err],
            fig_size=(6, 4),
            legend_labels=['Observed', 'Computed', 'Absolute error'],
            x_labels=point_labels[3::],
            ylim=[0, 120],
            fig_title='FINE',
            fig_name="img/WD_vs_observations_err_fine",
            annotate_format='f',
            annotate=True)

        vnv_plotbar(\
            [wd_obs, wd],
            fig_size=(6, 4),
            legend_labels=['Observed', 'Computed'],
            x_labels=point_labels[3::],
            ylim=[0, 120],
            fig_title='FINE',
            fig_name="img/WD_vs_observations_fine",
            annotate_format='f',
            annotate=True)

        #======================================================================
        # Delete results
        geo.close()
        res.close()
