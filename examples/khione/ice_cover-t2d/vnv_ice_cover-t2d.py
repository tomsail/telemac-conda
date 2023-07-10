
"""
Validation script for ice_cover
"""
import datetime
import matplotlib.dates as mdates
import matplotlib.pyplot as plt
from postel.deco_vnv import decoVNV, decoVNV_1d
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
from data_manip.extraction.telemac_file import TelemacFile
from data_manip.computation.datetimes import compute_datetimes

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 2
        self.tags = ['telemac2d', 'khione']

    def _pre(self):
        """
        Defining the studies
        """
        #----------------------------------------------------------------------
        # cas 1: totally covered channel
        cas = TelemacCas('t2d_cover.cas', get_dico('telemac2d'))
        cas.set('RESULTS FILE', 'r2d_hydro_v01.slf')
        cas.set('KHIONE STEERING FILE', 'ice_cover_v01.cas')
        self.add_study('vnv1_seq', 'telemac2d', 't2d_cover.cas', cas)
        # cas 1 parallel
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('vnv1_par', 'telemac2d', 't2d_cover.cas', cas=cas)
        del cas
        #----------------------------------------------------------------------
        # cas 1: totally covered channel
        cas = TelemacCas('t2d_cover.cas', get_dico('telemac2d'))
        cask = TelemacCas('ice_cover_v01.cas', get_dico('khione'))
        cask.set('MODEL FOR UNDER COVER FRICTION', 2)
        cask.write('ice_tmp.cas')
        cas.set('RESULTS FILE', 'r2d_hydro_v01.slf')
        cas.set('KHIONE STEERING FILE', 'ice_tmp.cas')
        self.add_study('vnv1_2_seq', 'telemac2d', 't2d_cover.cas', cas)
        # cas 1 parallel
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('vnv1_2_par', 'telemac2d', 't2d_cover.cas', cas=cas)
        del cas

        #----------------------------------------------------------------------
        # cas 2: partially covered channel (downstream)
        cas = TelemacCas('t2d_cover.cas', get_dico('telemac2d'))
        cas.set('RESULTS FILE', 'r2d_hydro_v02.slf')
        cas.set('KHIONE STEERING FILE', 'ice_cover_v02.cas')
        self.add_study('vnv2_seq', 'telemac2d', 't2d_cover.cas', cas)
        # cas 2 parallel
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('vnv2_par', 'telemac2d', 't2d_cover.cas', cas=cas)
        del cas
        #----------------------------------------------------------------------
        # cas 2: partially covered channel (downstream)
        cas = TelemacCas('t2d_cover.cas', get_dico('telemac2d'))
        cask = TelemacCas('ice_cover_v02.cas', get_dico('khione'))
        cask.set('MODEL FOR UNDER COVER FRICTION', 2)
        cask.write('ice_tmp.cas')
        cas.set('RESULTS FILE', 'r2d_hydro_v02.slf')
        cas.set('KHIONE STEERING FILE', 'ice_tmp.cas')
        self.add_study('vnv2_2_seq', 'telemac2d', 't2d_cover.cas', cas)
        # cas 2 parallel
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('vnv2_2_par', 'telemac2d', 't2d_cover.cas', cas=cas)
        del cas

        #----------------------------------------------------------------------
        # cas 3: partially covered channel (upstream)
        cas = TelemacCas('t2d_cover.cas', get_dico('telemac2d'))
        cas.set('RESULTS FILE', 'r2d_hydro_v03.slf')
        cas.set('KHIONE STEERING FILE', 'ice_cover_v03.cas')
        cas.set('PRESCRIBED ELEVATIONS', [6.6265, 0.0])
        self.add_study('vnv3_seq', 'telemac2d', 't2d_cover.cas', cas)
        # cas 3 parallel
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('vnv3_par', 'telemac2d', 't2d_cover.cas', cas=cas)
        del cas
        #----------------------------------------------------------------------
        # cas 3: partially covered channel (upstream)
        cas = TelemacCas('t2d_cover.cas', get_dico('telemac2d'))
        cask = TelemacCas('ice_cover_v03.cas', get_dico('khione'))
        cask.set('MODEL FOR UNDER COVER FRICTION', 2)
        cask.write('ice_tmp.cas')
        cas.set('RESULTS FILE', 'r2d_hydro_v03.slf')
        cas.set('KHIONE STEERING FILE', 'ice_tmp.cas')
        self.add_study('vnv3_2_seq', 'telemac2d', 't2d_cover.cas', cas)
        # cas 3 parallel
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('vnv3_2_par', 'telemac2d', 't2d_cover.cas', cas=cas)
        del cas

        #----------------------------------------------------------------------
        # cas 4: partially covered channel with icejam (upstream)
        cas = TelemacCas('t2d_cover.cas', get_dico('telemac2d'))
        cas.set('RESULTS FILE', 'r2d_hydro_v04.slf')
        cas.set('KHIONE STEERING FILE', 'ice_cover_v04.cas')
        cas.set('INITIAL DEPTH', 2.6265)
        cas.set('PRESCRIBED ELEVATIONS', [6.6265, 0.0])
        self.add_study('vnv4_seq', 'telemac2d', 't2d_cover.cas', cas)
        # cas 4 parallel
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('vnv4_par', 'telemac2d', 't2d_cover.cas', cas=cas)
        del cas
        #----------------------------------------------------------------------
        # cas 4: partially covered channel with icejam (upstream)
        cas = TelemacCas('t2d_cover.cas', get_dico('telemac2d'))
        cask = TelemacCas('ice_cover_v04.cas', get_dico('khione'))
        cask.set('MODEL FOR UNDER COVER FRICTION', 2)
        cask.write('ice_tmp.cas')
        cas.set('RESULTS FILE', 'r2d_hydro_v04.slf')
        cas.set('KHIONE STEERING FILE', 'ice_tmp.cas')
        self.add_study('vnv4_2_seq', 'telemac2d', 't2d_cover.cas', cas)
        # cas 4 parallel
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('vnv4_2_par', 'telemac2d', 't2d_cover.cas', cas=cas)
        del cas

    def _check_results(self):
        """
        Post-treatment processes
        """
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv1_seq:T2DRES', 'f2d_hydro_v01.slf', eps=[1.e-6])
        self.check_epsilons('vnv1_seq:ICERES', 'fce_cover_v01.slf', eps=[1.e-6])
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv1_par:T2DRES', 'f2d_hydro_v01.slf', eps=[1.e-6])
        self.check_epsilons('vnv1_par:ICERES', 'fce_cover_v01.slf', eps=[1.e-6])
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv1_seq:T2DRES', 'vnv1_par:T2DRES', eps=[1.e-6])
        self.check_epsilons('vnv1_seq:ICERES', 'vnv1_par:ICERES', eps=[1.e-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv1_2_seq:T2DRES', 'f2d_hydro_v01_2.slf', eps=[1.e-6])
        self.check_epsilons('vnv1_2_seq:ICERES', 'fce_cover_v01_2.slf', eps=[1.e-6])
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv1_2_par:T2DRES', 'f2d_hydro_v01_2.slf', eps=[1.e-6])
        self.check_epsilons('vnv1_2_par:ICERES', 'fce_cover_v01_2.slf', eps=[1.e-6])
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv1_2_seq:T2DRES', 'vnv1_2_par:T2DRES', eps=[1.e-6])
        self.check_epsilons('vnv1_2_seq:ICERES', 'vnv1_2_par:ICERES', eps=[1.e-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv2_seq:T2DRES', 'f2d_hydro_v02.slf', eps=[1.e-6])
        self.check_epsilons('vnv2_seq:ICERES', 'fce_cover_v02.slf', eps=[1.e-6])
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv2_par:T2DRES', 'f2d_hydro_v02.slf', eps=[1.e-6])
        self.check_epsilons('vnv2_par:ICERES', 'fce_cover_v02.slf', eps=[1.e-6])
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv2_seq:T2DRES', 'vnv2_par:T2DRES', eps=[1.e-6])
        self.check_epsilons('vnv2_seq:ICERES', 'vnv2_par:ICERES', eps=[1.e-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv2_2_seq:T2DRES', 'f2d_hydro_v02_2.slf', eps=[1.e-6])
        self.check_epsilons('vnv2_2_seq:ICERES', 'fce_cover_v02_2.slf', eps=[1.e-6])
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv2_2_par:T2DRES', 'f2d_hydro_v02_2.slf', eps=[1.e-6])
        self.check_epsilons('vnv2_2_par:ICERES', 'fce_cover_v02_2.slf', eps=[1.e-6])
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv2_2_seq:T2DRES', 'vnv2_2_par:T2DRES', eps=[1.e-6])
        self.check_epsilons('vnv2_2_seq:ICERES', 'vnv2_2_par:ICERES', eps=[1.e-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv3_seq:T2DRES', 'f2d_hydro_v03.slf', eps=[1.e-6])
        self.check_epsilons('vnv3_seq:ICERES', 'fce_cover_v03.slf', eps=[1.e-6])
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv3_par:T2DRES', 'f2d_hydro_v03.slf', eps=[1.e-6])
        self.check_epsilons('vnv3_par:ICERES', 'fce_cover_v03.slf', eps=[1.e-6])
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv3_seq:T2DRES', 'vnv3_par:T2DRES', eps=[1.e-6])
        self.check_epsilons('vnv3_seq:ICERES', 'vnv3_par:ICERES', eps=[1.e-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv3_2_seq:T2DRES', 'f2d_hydro_v03_2.slf', eps=[1.e-6])
        self.check_epsilons('vnv3_2_seq:ICERES', 'fce_cover_v03_2.slf', eps=[1.e-6])
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv3_2_par:T2DRES', 'f2d_hydro_v03_2.slf', eps=[1.e-6])
        self.check_epsilons('vnv3_2_par:ICERES', 'fce_cover_v03_2.slf', eps=[1.e-6])
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv3_2_seq:T2DRES', 'vnv3_2_par:T2DRES', eps=[1.e-6])
        self.check_epsilons('vnv3_2_seq:ICERES', 'vnv3_2_par:ICERES', eps=[1.e-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv4_seq:T2DRES', 'f2d_hydro_v04.slf', eps=[1.e-6])
        self.check_epsilons('vnv4_seq:ICERES', 'fce_cover_v04.slf', eps=[1.e-6])
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv4_par:T2DRES', 'f2d_hydro_v04.slf', eps=[1.e-6])
        self.check_epsilons('vnv4_par:ICERES', 'fce_cover_v04.slf', eps=[1.e-6])
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv4_seq:T2DRES', 'vnv4_par:T2DRES', eps=[1.e-6])
        self.check_epsilons('vnv4_seq:ICERES', 'vnv4_par:ICERES', eps=[1.e-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv4_2_seq:T2DRES', 'f2d_hydro_v04_2.slf', eps=[1.e-6])
        self.check_epsilons('vnv4_2_seq:ICERES', 'fce_cover_v04_2.slf', eps=[1.e-6])
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv4_2_par:T2DRES', 'f2d_hydro_v04_2.slf', eps=[1.e-6])
        self.check_epsilons('vnv4_2_par:ICERES', 'fce_cover_v04_2.slf', eps=[1.e-6])
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv4_2_seq:T2DRES', 'vnv4_2_par:T2DRES', eps=[1.e-6])
        self.check_epsilons('vnv4_2_seq:ICERES', 'vnv4_2_par:ICERES', eps=[1.e-6])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot1d, vnv_plot2d,\
            vnv_plot1d_history, vnv_plot1d_polylines


        #======================================================================
        # GET TELEMAC RESULT FILES:
        #
        geom, _ = self.get_study_res('vnv1_seq:T2DGEO', load_bnd=True)
        res, _ = self.get_study_res('vnv1_seq:T2DRES')

        # Load all results as a list:
        res_list, res_labels = self.get_study_res(module='T2D', whitelist=['seq'])
        ice_list, ice_labels = self.get_study_res(module='ICE', whitelist=['seq'])

        #======================================================================
        # DESCRIPTION PLOTS:
        #
        #Plotting mesh
        vnv_plot2d(\
            'BOTTOM',
            geom,
            record=0,
            plot_mesh=True,
            annotate_bnd=True,
            filled_contours=False,
            fig_size=(14, 2.5),
            fig_name='img/mesh',
            x_label='$x$ $(m)$',
            y_label='$y$ $(m)$',)

        # Plotting bottom
        vnv_plot2d(\
            'BOTTOM',
            geom,
            record=0,
            filled_contours=True,
            fig_size=(14, 2.5),
            fig_name='img/bottom',
            x_label='$x$ $(m)$',
            y_label='$y$ $(m)$',
            cbar_label='Bottom (m)')

        # Plotting bottom and elevation profile
        vnv_plot1d_polylines(\
            'FREE SURFACE',
            res_list[0],
            legend_labels='free surface',
            record=-1,
            poly=[[0., 75.], [10000., 75.]],
            poly_number=[50],
            fig_size=(10, 3),
            y_label='$z$ $(m)$',
            x_label='$x$ $(m)$',
            fig_name='img/profile_elevation',
            plot_bottom=True)

        #======================================================================
        # FIRST OBSERVATION:
        #
        # Plotting ice cover thickness
#        vnv_plot2d(\
#            'TOTAL ICE THICK.',
#            ice_list[0],
#            record=0,
#            var_factor=1000.0,
#            filled_contours=True,
#            fig_size=(10, 3),
#            fig_name='img/ice_thickness_cas0_0',
#            cmap_name='coolwarm',
#            x_label='$x$ $(m)$',
#            y_label='$y$ $(m)$',
#            cbar_label='$h_i$ $(mm)$')

        # 1D profiles:
        var_plt = ['FREE SURFACE', 'EQUIV. SURFACE',\
                    'TOP ICE COVER', 'BOTTOM ICE COVER']
        lab_plt = ['water free surface', 'eq. free surface',\
                    'top ice cover', 'bottom ice cover']

        records = [0, -1]
        n_cases = 8

        for record in records:
            # Plotting ice profiles:
            for i in range(n_cases):
                res_plt1 = [res_list[i], ice_list[i], ice_list[i], ice_list[i]]
                vnv_plot1d_polylines(\
                    var_plt, res_plt1,
                    legend_labels=lab_plt,
                    record=record,
                    poly=[[0., 75.], [10000., 75.]],
                    poly_number=[50],
                    fig_size=(10, 3),
                    y_label='$z$ $(m)$',
                    x_label='$x$ $(m)$',
                    fig_name='img/profile_ice_cas{}_{}'.format(i, record),
                    plot_bottom=True)

            for i in range(n_cases):
                # Plotting velocity profiles:
                vnv_plot1d_polylines(\
                    'VELOCITY U', res_list[i],
                    legend_labels='velocity',
                    record=record,
                    poly=[[0., 75.], [10000., 75.]],
                    poly_number=[50],
                    fig_size=(10, 3),
                    y_label='$U_x$ $(m/s)$',
                    x_label='$x$ $(m)$',
                    fig_name='img/profile_vel_cas{}_{}'.format(i, record))

                # Plotting temperature profiles:
#                vnv_plot1d_polylines(\
#                    'TEMPERATURE', res_list[i],
#                    legend_labels='',
#                    record=record,
#                    poly=[[0., 75.], [10000., 75.]],
#                    poly_number=[150],
#                    fig_size=(10, 4),
#                    y_label='$T$ $(\degree C)$',
#                    x_label='$x$ $(m)$',
#                    fig_name='img/profile_T_cas{}_{}'.format(i, record))

                # Plotting frazil profiles:
#                vnv_plot1d_polylines(\
#                    'FRAZIL', res_list[i],
#                    legend_labels='',
#                    record=record,
#                    poly=[[0., 75.], [10000., 75.]],
#                    poly_number=[150],
#                    fig_size=(10, 4),
#                    y_label='$C_f$ (volume fraction)',
#                    x_label='$x$ $(m)$',
#                    fig_name='img/profile_Cf_cas{}_{}'.format(i, record))

