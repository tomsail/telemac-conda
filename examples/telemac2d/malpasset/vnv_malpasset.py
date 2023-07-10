
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
        self.rank = 2
        self.tags = ['telemac2d','fv']

    def _pre(self):
        """
        Defining the studies
        """
        #======================================================================
        # eria run
        self.add_study('eria_seq',
                       'telemac2d',
                       't2d_malpasset-eria.cas')
        # parallel mode
        cas = TelemacCas('t2d_malpasset-eria.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('eria_par',
                       'telemac2d',
                       't2d_malpasset-eria_par.cas',
                       cas=cas)
        del cas

        #======================================================================
        # nerd run
        self.add_study('nerd_seq',
                       'telemac2d',
                       't2d_malpasset-nerd.cas')
        # parallel mode
        cas = TelemacCas('t2d_malpasset-nerd.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('nerd_par',
                       'telemac2d',
                       't2d_malpasset-nerd_par.cas',
                       cas=cas)
        del cas

        #======================================================================
        # charac run
        self.add_study('char_seq',
                       'telemac2d',
                       't2d_malpasset-char.cas')
        # charac run in parallel
        cas = TelemacCas('t2d_malpasset-char.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('char_par',
                       'telemac2d',
                       't2d_malpasset-char_par.cas',
                       cas=cas)
        del cas

        #======================================================================
        # prim run
        self.add_study('prim_seq',
                       'telemac2d',
                       't2d_malpasset-prim.cas')
        # parallel mode
        cas = TelemacCas('t2d_malpasset-prim.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('prim_par',
                       'telemac2d',
                       't2d_malpasset-prim_par.cas',
                       cas=cas)
        del cas

        #======================================================================
        # kin1 run
        self.add_study('kin1_seq',
                       'telemac2d',
                       't2d_malpasset-kin1.cas')
        # parallel mode
        cas = TelemacCas('t2d_malpasset-kin1.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('kin1_par',
                       'telemac2d',
                       't2d_malpasset-kin1_par.cas',
                       cas=cas)
        del cas

        #======================================================================
        # hllc run
        self.add_study('hllc_seq',
                       'telemac2d',
                       't2d_malpasset-hllc.cas')
        # parallel mode
        cas = TelemacCas('t2d_malpasset-hllc.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('hllc_par',
                       'telemac2d',
                       't2d_malpasset-hllc_par.cas',
                       cas=cas)
        del cas


    def _check_results(self):
        """
        Post-treatment processes
        """
        #======================================================================
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('nerd_seq:T2DRES',
                            'f2d_malpasset-nerd.slf',
                            eps=[3.0, 2.2, 1.9, 1.9, 1.E-15])
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('nerd_par:T2DRES',
                            'f2d_malpasset-nerd.slf',
                            eps=[2.6, 2.7, 2.8, 2.8, 1.E-15])
        # Comparison between sequential and parallel run.
        self.check_epsilons('nerd_seq:T2DRES',
                            'nerd_par:T2DRES',
                            eps=[5.1, 2.9, 2.8, 2.8, 1.E-15])

        #======================================================================
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('eria_seq:T2DRES',
                            'f2d_malpasset-eria.slf',
                            eps=[6.7, 5.2, 5.2, 5.2, 1.E-15])
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('eria_par:T2DRES',
                            'f2d_malpasset-eria.slf',
                            eps=[6.8, 5.2, 5.2, 5.2, 1.E-15])
        # Comparison between sequential and parallel run.
        self.check_epsilons('eria_seq:T2DRES',
                            'eria_par:T2DRES',
                            eps=[8.8, 3.8, 5.3, 5.3, 1.E-15])

        #======================================================================
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('char_seq:T2DRES',
                            'f2d_malpasset-char.slf',
                            eps=[0.020, 7.E-3, 0.015, 0.015, 1.E-15])
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('char_par:T2DRES',
                            'f2d_malpasset-char.slf',
                            eps=[0.017, 9.E-3, 0.020, 0.020, 1.E-15])
        # Comparison between sequential and parallel run.
        self.check_epsilons('char_seq:T2DRES',
                            'char_par:T2DRES',
                            eps=[0.016, 0.010, 0.020, 0.020, 1.E-15])

        #======================================================================
        self.check_epsilons('prim_seq:T2DRES',
                            'f2d_malpasset-prim.slf',
                            eps=[3.9, 2.9, 4.1, 4.1, 1.E-15])
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('prim_par:T2DRES',
                            'f2d_malpasset-prim.slf',
                            eps=[3.8, 2.9, 4.1, 4.1, 1.E-15])
        # Comparison between sequential and parallel run.
        self.check_epsilons('prim_seq:T2DRES',
                            'prim_par:T2DRES',
                            eps=[4.1, 3.0, 4.4, 4.4, 1.E-15])

        #======================================================================
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('kin1_seq:T2DRES',
                            'f2d_malpasset-kin1.slf',
                            eps=[1.E-8])
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('kin1_par:T2DRES',
                            'f2d_malpasset-kin1.slf',
                            eps=[1.E-8])
        # Comparison between sequential and parallel run.
        self.check_epsilons('kin1_seq:T2DRES',
                            'kin1_par:T2DRES',
                            eps=[1.E-8])

        #======================================================================
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('hllc_seq:T2DRES',
                            'f2d_malpasset-hllc.slf',
                            eps=[1.E-5])
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('hllc_par:T2DRES',
                            'f2d_malpasset-hllc.slf',
                            eps=[1.E-5])
        # Comparison between sequential and parallel run.
        self.check_epsilons('hllc_seq:T2DRES',
                            'hllc_par:T2DRES',
                            eps=[1.E-5])

    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d, vnv_plotbar, \
            vnv_plot1d_polylines, vnv_plot1d_history, vnv_plotbar_cpu_times
        from postel.plot2d import plot2d_triangle_mesh, plot2d_annotate_bnd, \
            plot2d_scalar_filled_contour
        from postel.deco_vnv import decoVNV
        from os import path
        import matplotlib.pyplot as plt
        import numpy as np

        # Getting files
        geo, _ = self.get_study_res('nerd_seq:T2DGEO', load_bnd=True)
        kin1_res, _ = self.get_study_res('kin1_seq:T2DRES', load_bnd=True)

        # Load all results as a list:
        res_list, res_labels = self.get_study_res(module='T2D', whitelist=['seq'])

        #======================================================================
        # DESCRIPTION PLOTS:
        #
        # figure limits
        xlim = [4500., 5500.]
        ylim = [3900., 4500.]

        # Plotting mesh
        vnv_plot2d(\
            '',
            geo,
            plot_mesh=True,
            fig_size=(13, 7),
            fig_name='img/Mesh_small',
            annotate_bnd=True)

        vnv_plot2d(\
            '',
            geo,
            plot_mesh=True,
            fig_size=(14, 8),
            xlim=xlim, ylim=ylim,
            fig_name='img/Mesh_small_dam',
            annotate_bnd=True)

        #----------------------------------------------------------------------
        # Plotting BOTTOM at 0
        #
        # Physical model points locations
        P6 = [4947, 4289]
        P7 = [5717, 4407]
        P8 = [6775, 3869]
        P9 = [7128, 3162]
        P10 = [8585, 3443]
        P11 = [9674, 3085]
        P12 = [10939, 3044]
        P13 = [11724, 2810]
        P14 = [12723, 2485]

        # Transformers locations
        A = [5550, 4400]
        B = [11900, 3250]
        C = [13000, 2700]
        point_list = [A, B, C, P6, P7, P8, P9, P10, P11, P12, P13, P14]
        point_labels = ['A', 'B', 'C', 'P6', 'P7', 'P8', 'P9', 'P10', \
                        'P11', 'P12', 'P13', 'P14']

        # Plot bottom with point locations
        plt.style.use('default')
        plt.rcParams.update(decoVNV)
        fig, ax = plt.subplots(1, 1, figsize=(14, 6))
        mesh = res_list[0].tri
        bottom = res_list[0].get_data_value('BOTTOM', 0)
        plot2d_triangle_mesh(ax, mesh, color='k', linewidth=0.1, alpha=.5)
        plot2d_scalar_filled_contour(\
            fig, ax, mesh, bottom,
            vmin=-20, vmax=100, nv=13,
            extend='both',
            data_name='bottom (m)', colorbar=True)
        for i, pt in enumerate(point_list):
            lab = point_labels[i]
            if i > 2:
                plt.plot(pt[0], pt[1], marker='x', markersize=6, color='k')
                plt.annotate(lab, xy=pt, xytext=(pt[0]+100, pt[1]-200))
            else:
                plt.plot(pt[0], pt[1], marker='d', markersize=8, color='r')
                plt.annotate(lab, xy=pt, xytext=(pt[0]-200, pt[1]+100))
        fig_name = "img/bathymetry"
        print(" "*8+"~> Plotting {}".format(fig_name))
        fig.savefig(fig_name)
        plt.close('all')

        #======================================================================
        # FIRST OBSERVATION:
        #
        times = [0., 200., 400., 600.]
        for itm, record in enumerate([0, 2, 4, 6]):
            time_str = int(times[itm])
            fig_name = 'img/WD{}_{}_firstobs'.format(time_str, 'kin1')
            vnv_plot2d(\
                'WATER DEPTH', kin1_res,
                record=record,
                fig_size=(10, 3.5),
                fig_title='$t = {}$ s'.format(time_str),
                xlim=[3000., 12500.],
                ylim=[2000., 5500.],
                fig_name=fig_name,
                cbar_label='Water depth (m)',
                filled_contours=True,
                mask_tidal_flats=True,
                tidal_flats_threshold=0.01,
                plot_mesh=True,
                plot_only_dry_mesh=True)

        #======================================================================
        # COMPARISON OF NUMERICAL SCHEMES:
        #
        #----------------------------------------------------------------------
        # Computation time:.
        vnv_plotbar_cpu_times(\
             self.action_time,
             fig_size=(7, 3),
             fig_name='img/cpu_times')

        #----------------------------------------------------------------------
        # Maximum principle and positivity:
        #
        max_vars = []
        min_vars = []

        for idx, res in enumerate(res_list):

            # computing min and max for all time steps:
            min_dat = 10.
            max_dat = 0.

            for rec in range(res.ntimestep):
                data = res.get_data_value('WATER DEPTH', rec)

                aux1 = np.max(data)
                aux2 = np.min(data)

                if aux1 > max_dat:
                    max_dat = aux1
                if aux2 < min_dat:
                    min_dat = aux2

            max_vars.append(max_dat)
            min_vars.append(min_dat)

        vnv_plotbar(\
            [min_vars],
            fig_size=(10, 3),
            legend_labels=['min(T)'],
            x_labels=res_labels,
            y_scale='linear',
            fig_title='Minimum values of water depth at $t=T$',
            fig_name="img/minT",
            bar_width=.5,
            annotate=True,
            annotate_threshold=-1.e-12)

        vnv_plotbar(\
            [max_vars],
            fig_size=(10, 3),
            legend_labels=['max(T)'],
            x_labels=res_labels,
            y_scale='linear',
            fig_title='Maximum values of water depth at $t=T$',
            fig_name="img/maxT",
            bar_width=.5,
            annotate=True)

        #----------------------------------------------------------------------
        # Plotting VELOCITY and WATER DEPTH maps:
        for idx, res in enumerate(res_list):
            fig_name = 'img/Velocity100_{}'.format(res_labels[idx].lower())
            vnv_plot2d(\
                'VELOCITY',
                res,
                var_type='vector',
                record=1,
                fig_size=(6.5, 4),
                fig_title=res_labels[idx],
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

            fig_name = 'img/WDVelocity100_{}'.format(res_labels[idx].lower())
            vnv_plot2d(\
                'WATER DEPTH',
                res,
                var_type='scalar',
                record=1,
                fig_size=(6.5, 4),
                fig_title=res_labels[idx],
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
                fig_name = 'img/WD{}_{}'.format(int(times[itm]),
                                                res_labels[idx].lower())
                vnv_plot2d(\
                    'WATER DEPTH',
                    res,
                    record=record,
                    fig_size=(10, 3.5),
                    fig_title=res_labels[idx],
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
        # Plot 1d along polylines:
        fig_size = (9, 3)
        vnv_plot1d_polylines(\
            'FREE SURFACE',
            res_list,
            legend_labels=res_labels,
            fig_size=fig_size,
            record=-1,
            poly=[[4701.18, 4143.10], [4655.5, 4392.10]],
            poly_number=[100],
            fig_name='img/WaterDepth_Dam',
            plot_bottom=True)

        vnv_plot1d_polylines(\
            'FREE SURFACE',
            res_list,
            legend_labels=res_labels,
            fig_size=fig_size,
            record=-1,
            poly=[[4634.0, 4132.84], [4589.81, 4393.22]],
            poly_number=[100],
            fig_name='img/WaterDepth_Dam_amont',
            plot_bottom=True)

        vnv_plot1d_polylines(\
            'FREE SURFACE',
            res_list,
            legend_labels=res_labels,
            fig_size=fig_size,
            record=-1,
            poly=[[4884.95, 4161.82], [4846.39, 4362.44]],
            poly_number=[100],
            fig_name='img/WaterDepth_Dam_aval',
            plot_bottom=True)

#        #----------------------------------------------------------------------
#        # Plot 1d along polylines vs time:
#        for idx, res in enumerate(res_list):
#            vnv_plot1d_polylines(\
#                'FREE SURFACE',
#                res,
#                legend_labels=res_labels[idx],
#                fig_size=fig_size,
#                fig_title=res_labels[idx],
#                record=[0, 1, 2, 3, -1],
#                poly=[[4701.18, 4143.10], [4655.5, 4392.10]],
#                poly_number=[100],
#                fig_name='img/WaterDepth_Dam_time'+res_labels[idx],
#                plot_bottom=True)

#            vnv_plot1d_polylines(\
#                'FREE SURFACE',
#                res,
#                legend_labels=res_labels[idx],
#                fig_size=fig_size,
#                fig_title=res_labels[idx],
#                record=[0, 1, 2, 3, -1],
#                poly=[[4634.0, 4132.84], [4589.81, 4393.22]],
#                poly_number=[100],
#                fig_name='img/WaterDepth_Dam_amont_time'+res_labels[idx],
#                plot_bottom=True)

#            vnv_plot1d_polylines(\
#                'FREE SURFACE',
#                res,
#                legend_labels=res_labels[idx],
#                fig_size=fig_size,
#                fig_title=res_labels[idx],
#                record=[0, 1, 2, 3, -1],
#                poly=[[4884.95, 4161.82], [4846.39, 4362.44]],
#                poly_number=[100],
#                fig_name='img/WaterDepth_Dam_aval_time'+res_labels[idx],
#                plot_bottom=True)

        #----------------------------------------------------------------------
        # Accuracy of the water depth:
        #
        # Observation of water depth on points P6 to P14
        wd_obs = [84.2-43.9, 49.1-34.5, 54.0-30.0, 40.2-27.4, 34.9-23.1,\
                  27.4-19.1, 21.5-11.4, 16.1-9.3, 12.9-7.5]

        # loop over cases
        for name, study in self.studies.items():
            sc = name.split('_')[0].upper()
            # read rfo file content:
            if 'seq' in name:
                # find case rfo file
                rfo_file = path.join(
                    self.get_vnv_working_dir(name), \
                    'rfo_malpasset-{}.txt'.format(name.split('_')[0]))
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
                ylim=[0, 100],
                fig_title=sc,
                fig_name="img/WD_vs_observations_err_{}".format(sc),
                annotate_format='f',
                annotate=True)

            vnv_plotbar(\
                [wd_obs, wd],
                fig_size=(6, 4),
                legend_labels=['Observed', 'Computed'],
                x_labels=point_labels[3::],
                ylim=[0, 100],
                fig_title=sc,
                fig_name="img/WD_vs_observations_{}".format(sc),
                annotate_format='f',
                annotate=True)

        #----------------------------------------------------------------------
        # Accuracy of the wave propagation:
        #
        time_obs = [1140.0, 1320.0]

        AtoB_obs = [time_obs[0] for i in range(len(res_labels))]
        AtoC_obs = [time_obs[1] for i in range(len(res_labels))]
        times_AtoB = []
        times_AtoC = []

        # loop over cases
        for name, study in self.studies.items():
            sc = name.split('_')[0].upper()

            # read rfo file content:
            if 'seq' in name:
                # find case rfo file
                rfo_file = path.join(
                    self.get_vnv_working_dir(name), \
                    'rfo_malpasset-{}.txt'.format(name.split('_')[0]))

                # read rfo file
                rfof = open(rfo_file, "r")
                lines = rfof.readlines()
                for idx, line in enumerate(lines):
                    if line.startswith("TIME FROM A TO B"):
                        times_AtoB.append(float(line.split(' ')[-2]))
                    if line.startswith("TIME FROM A TO C"):
                        times_AtoC.append(float(line.split(' ')[-2]))

        # compute relative error (percentage):
        AtoB_abs_err = [abs(times_AtoB[i] -AtoB_obs[i]) \
                        for i in range(len(res_labels))]
        AtoB_rel_err = [(times_AtoB[i] - AtoB_obs[i])/AtoB_obs[i]*100 \
                        for i in range(len(res_labels))]
        AtoC_abs_err = [abs(times_AtoC[i] -AtoC_obs[i])\
                        for i in range(len(res_labels))]
        AtoC_rel_err = [(times_AtoC[i] - AtoC_obs[i])/AtoC_obs[i]*100 \
                        for i in range(len(res_labels))]

        # plot
        vnv_plotbar(\
            [AtoB_obs, times_AtoB, AtoB_abs_err],
            fig_size=(8, 4),
            legend_labels=['Observed', 'Computed', 'Absolute error'],
            x_labels=res_labels,
            ylim=[0, 2000],
            fig_name="img/propagation_AtoB",
            fig_title='A to B',
            annotate_format='f1',
            annotate=True)

        vnv_plotbar(\
            [AtoC_obs, times_AtoC, AtoC_abs_err],
            fig_size=(8, 4),
            legend_labels=['Observed', 'Computed', 'Absolute error'],
            x_labels=res_labels,
            ylim=[0, 2000],
            fig_name="img/propagation_AtoC",
            fig_title='A to C',
            annotate_format='f1',
            annotate=True)

        #----------------------------------------------------------------------
        # Mass conservation:
        #
        lost_volume_seq = []
        lost_volume_par = []
        lost_volume_relerr_seq = []
        lost_volume_relerr_par = []

        # loop over cases
        for name, study in self.studies.items():
            sc = name.split('_')[0].upper()
            # read rfo file content:
            if 'seq' in name:
                # find case rfo file
                rfo_file = path.join(
                    self.get_vnv_working_dir(name), \
                    'rfo_malpasset-{}.txt'.format(name.split('_')[0]))
                # read rfo file
                rfof = open(rfo_file, "r")
                lines = rfof.readlines()
                for idx, line in enumerate(lines):
                    if line.startswith(" VOLUME LOST"):
                        lost_volume_seq.append(float(line.split()[-1]))
                    if line.startswith(" RELATIVE ERROR"):
                        lost_volume_relerr_seq.append(float(line.split()[-1]))
            # read rfo file content:
            if 'par' in name:
                # find case rfo file
                rfo_file = path.join(
                    self.get_vnv_working_dir(name), \
                    'rfo_malpasset-{}.txt'.format(name.split('_')[0]))
                # read rfo file
                rfof = open(rfo_file, "r")
                lines = rfof.readlines()
                for idx, line in enumerate(lines):
                    if line.startswith(" VOLUME LOST"):
                        lost_volume_par.append(float(line.split()[-1]))
                    if line.startswith(" RELATIVE ERROR"):
                        lost_volume_relerr_par.append(float(line.split()[-1]))

        # plot
        vnv_plotbar(\
            [lost_volume_seq, lost_volume_par],
            fig_size=(8, 3),
            legend_labels=['SEQ', 'PAR'],
            x_labels=res_labels,
            ylim=[0, 1],
            fig_name="img/lost_volume",
            fig_title='Lost volume (m$^3$)',
            annotate_format='f',
            annotate=True)

        vnv_plotbar(\
            [lost_volume_relerr_seq, lost_volume_relerr_par],
            fig_size=(8, 3),
            legend_labels=['SEQ', 'PAR'],
            x_labels=res_labels,
            ylim=[0, 0.001],
            fig_name="img/lost_volume_error",
            fig_title='Lost volume relative error',
            annotate_format='f',
            annotate=True)

        #======================================================================
        # Closing files
        geo.close()
        kin1_res.close()
        for res in res_list:
            res.close()
