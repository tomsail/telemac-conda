
"""
Validation script for bumpsub
"""
import numpy as np
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
        self.tags = ['telemac2d', 'fv']

    def _pre(self):
        """
        Defining the studies
        """
        casname='t2d_flume_FV.cas'

        #======================================================================
        # ROE run
        cas = TelemacCas(casname, get_dico('telemac2d'))
        cas.set('FINITE VOLUME SCHEME', 0)
        self.add_study('roe_seq', 'telemac2d', casname, cas=cas)
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('roe_par', 'telemac2d', casname, cas=cas)
        del cas

        #======================================================================
        # ZOKAGOA run
        cas = TelemacCas(casname, get_dico('telemac2d'))
        cas.set('FINITE VOLUME SCHEME', 3)
        self.add_study('zoka_seq', 'telemac2d', casname, cas=cas)
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('zoka_par', 'telemac2d', casname, cas=cas)
        del cas

        #======================================================================
        # TCHAMEN run
        cas = TelemacCas(casname, get_dico('telemac2d'))
        cas.set('FINITE VOLUME SCHEME', 4)
        self.add_study('tcha_seq', 'telemac2d', casname, cas=cas)
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('tcha_par', 'telemac2d', casname, cas=cas)
        del cas

        #======================================================================
        # WAF run
        cas = TelemacCas(casname, get_dico('telemac2d'))
        cas.set('FINITE VOLUME SCHEME', 6)
        self.add_study('waf_seq', 'telemac2d', casname, cas=cas)
        del cas

        #======================================================================
        # KIN1 run
        cas = TelemacCas(casname, get_dico('telemac2d'))
        cas.set('FINITE VOLUME SCHEME', 1)
        self.add_study('kin1_seq', 'telemac2d', casname, cas=cas)
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('kin1_par', 'telemac2d', casname, cas=cas)
        del cas

        #======================================================================
        # HLLC run
        cas = TelemacCas(casname, get_dico('telemac2d'))
        cas.set('FINITE VOLUME SCHEME', 5)
        self.add_study('hllc1_seq', 'telemac2d', casname, cas=cas)
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('hllc1_par', 'telemac2d', casname, cas=cas)
        del cas

        #======================================================================
        # KIN2 MINMOD limitor run
        cas = TelemacCas(casname, get_dico('telemac2d'))
        cas.set('FINITE VOLUME SCHEME', 1)
        cas.set('FINITE VOLUME SCHEME SPACE ORDER', 2)
        cas.set('FINITE VOLUME SCHEME TIME ORDER', 1)
        cas.set('FLUX LIMITOR FOR TRACERS', 1)
        self.add_study('kin2-mm_seq', 'telemac2d', casname, cas=cas)
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('kin2-mm_par', 'telemac2d', casname, cas=cas)
        del cas

        #======================================================================
        # KIN2 VAN ALBALA run
        cas = TelemacCas(casname, get_dico('telemac2d'))
        cas.set('FINITE VOLUME SCHEME', 1)
        cas.set('FINITE VOLUME SCHEME SPACE ORDER', 2)
        cas.set('FINITE VOLUME SCHEME TIME ORDER', 1)
        cas.set('FLUX LIMITOR FOR TRACERS', 2)
        self.add_study('kin2-va_seq', 'telemac2d', casname, cas=cas)
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('kin2-va_par', 'telemac2d', casname, cas=cas)
        del cas

        #======================================================================
        # KIN2 MC limitor run
        cas = TelemacCas(casname, get_dico('telemac2d'))
        cas.set('FINITE VOLUME SCHEME', 1)
        cas.set('FINITE VOLUME SCHEME SPACE ORDER', 2)
        cas.set('FINITE VOLUME SCHEME TIME ORDER', 1)
        cas.set('FLUX LIMITOR FOR TRACERS', 3)
        self.add_study('kin2-mc_seq', 'telemac2d', casname, cas=cas)
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('kin2-mc_par', 'telemac2d', casname, cas=cas)
        del cas

        #======================================================================
        # HLLC MC limitor run
        cas = TelemacCas(casname, get_dico('telemac2d'))
        cas.set('FINITE VOLUME SCHEME', 5)
        cas.set('FINITE VOLUME SCHEME SPACE ORDER', 2)
        cas.set('FINITE VOLUME SCHEME TIME ORDER', 1)
        cas.set('FLUX LIMITOR FOR TRACERS', 3)
        self.add_study('hllc2-mc_seq', 'telemac2d', casname, cas=cas)
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('hllc2-mc_par', 'telemac2d', casname, cas=cas)
        del cas


    def _check_results(self):
        """
        Post-treatment processes
        """
        self.check_epsilons('roe_seq:T2DRES', 'f2d_flume_roe.slf', eps=[1e-6])
        self.check_epsilons('roe_par:T2DRES', 'f2d_flume_roe.slf', eps=[1e-6])
        self.check_epsilons('roe_seq:T2DRES', 'roe_par:T2DRES', eps=[1e-6])

        self.check_epsilons('zoka_seq:T2DRES', 'f2d_flume_zoka.slf', eps=[1e-6])
        self.check_epsilons('zoka_par:T2DRES', 'f2d_flume_zoka.slf', eps=[1e-6])
        self.check_epsilons('zoka_seq:T2DRES', 'zoka_par:T2DRES', eps=[1e-6])

        self.check_epsilons('tcha_seq:T2DRES', 'f2d_flume_tcha.slf', eps=[1e-6])
        self.check_epsilons('tcha_par:T2DRES', 'f2d_flume_tcha.slf', eps=[1e-6])
        self.check_epsilons('tcha_seq:T2DRES', 'tcha_par:T2DRES', eps=[1e-6])

        self.check_epsilons('waf_seq:T2DRES', 'f2d_flume_waf.slf', eps=[1e-6])

        self.check_epsilons('kin1_seq:T2DRES', 'f2d_flume_kin1.slf', eps=[1e-6])
        self.check_epsilons('kin1_par:T2DRES', 'f2d_flume_kin1.slf', eps=[1e-6])
        self.check_epsilons('kin1_seq:T2DRES', 'kin1_par:T2DRES', eps=[1e-6])

        self.check_epsilons('hllc1_seq:T2DRES', 'f2d_flume_hllc1.slf', eps=[1e-6])
        self.check_epsilons('hllc1_par:T2DRES', 'f2d_flume_hllc1.slf', eps=[1e-6])
        self.check_epsilons('hllc1_seq:T2DRES', 'hllc1_par:T2DRES', eps=[1e-6])

        self.check_epsilons('kin2-mc_seq:T2DRES', 'f2d_flume_kin2-mc.slf', eps=[1.])
        self.check_epsilons('kin2-mc_par:T2DRES', 'f2d_flume_kin2-mc.slf', eps=[1.])
        self.check_epsilons('kin2-mc_seq:T2DRES', 'kin2-mc_par:T2DRES', eps=[1.])

        self.check_epsilons('kin2-va_seq:T2DRES', 'f2d_flume_kin2-va.slf', eps=[1.])
        self.check_epsilons('kin2-va_par:T2DRES', 'f2d_flume_kin2-va.slf', eps=[1.])
        self.check_epsilons('kin2-va_seq:T2DRES', 'kin2-va_par:T2DRES', eps=[1.])

        self.check_epsilons('kin2-mm_seq:T2DRES', 'f2d_flume_kin2-mm.slf', eps=[1.])
        self.check_epsilons('kin2-mm_par:T2DRES', 'f2d_flume_kin2-mm.slf', eps=[1.])
        self.check_epsilons('kin2-mm_seq:T2DRES', 'kin2-mm_par:T2DRES', eps=[1.])

        self.check_epsilons('hllc2-mc_seq:T2DRES', 'f2d_flume_hllc2-mc.slf', eps=[1.])
        self.check_epsilons('hllc2-mc_par:T2DRES', 'f2d_flume_hllc2-mc.slf', eps=[1.])
        self.check_epsilons('hllc2-mc_seq:T2DRES', 'hllc2-mc_par:T2DRES', eps=[1.])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot3d, vnv_plot2d, vnv_plot1d, vnv_plotbar, \
                vnv_plot1d_polylines, vnv_plot1d_history, vnv_plotbar_cpu_times
        from os import path
        import numpy as np
        #======================================================================
        # GET TELEMAC RESULT FILES:
        #
        # Load all results as a list:
        res_list, res_labels = self.get_study_res(module='T2D', whitelist=['seq'])
        geom, _ = self.get_study_res('hllc1_seq:T2DGEO', load_bnd=True)
        res,  _ = self.get_study_res('hllc1_seq:T2DRES')

        #======================================================================
        # DESCRIPTION PLOTS:
        #
        # Plot mesh
        vnv_plot2d(\
            '',
            geom,
            fig_size=(10, 2),
            fig_name='img/flumetracer_mesh0',
            annotate_bnd=True,
            plot_mesh=True)

        #======================================================================
        # PLOT 1D
        records = [0, -1]

        for idx, record in enumerate(records):
            vnv_plot1d_polylines(\
                'FREE SURFACE',
                res_list,
                res_labels,
                record=record,
                fig_size=(8, 5),
                y_label='z',
                x_label='x',
                fig_name='img/t2d_flumetracer_vf_h_{}'.format(record),
                plot_bottom=True)

            vnv_plot1d_polylines(\
                'VELOCITY U',
                res_list,
                res_labels,
                record=record,
                fig_size=(8, 5),
                y_label='U',
                x_label='x',
                fig_name='img/t2d_flumetracer_vf_u_{}'.format(record),
                plot_bottom=True)

            vnv_plot1d_polylines(\
                'TRACER 1',
                res_list,
                res_labels,
                record=record,
                ref_name='SOL T1',
                fig_size=(8, 5),
                y_label='tracer',
                x_label='x',
                fig_name='img/t2d_flumetracer_vf_T1_{}'.format(record),
                plot_bottom=True,
                markers=True,
                markevery=15)

            vnv_plot1d_polylines(\
                'TRACER 2',
                res_list,
                res_labels,
                record=record,
                ref_name='SOL T2',
                fig_size=(8, 5),
                y_label='tracer',
                x_label='x',
                fig_name='img/t2d_flumetracer_vf_T2_{}'.format(record),
                plot_bottom=True,
                markers=True,
                markevery=15)

        #======================================================================
        # COMPARISON OF NUMERICAL SCHEMES:
        #
        #----------------------------------------------------------------------
        # Computation time:
        vnv_plotbar_cpu_times(\
            self.action_time,
            fig_size=(10, 2.5),
            fig_name='img/flumetracer_cpu_times')


