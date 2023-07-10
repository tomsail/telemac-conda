
"""
Validation script for ritter
"""
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
        self.rank = 0
        self.tags = ['telemac2d','fv']

    def _pre(self):
        """
        Defining the studies
        """
        #======================================================================
        # ROE run
        cas = TelemacCas('t2d_ritter-hllc.cas', get_dico('telemac2d'))
        cas.set('FINITE VOLUME SCHEME', 0)
        self.add_study('roe_seq', 'telemac2d', 't2d_ritter_FV.cas', cas=cas)
        # ROE parallel mode
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('roe_par', 'telemac2d', 't2d_ritter_FV_par.cas', cas=cas)
        del cas

        #======================================================================
        # KIN1 run
        cas = TelemacCas('t2d_ritter-hllc.cas', get_dico('telemac2d'))
        cas.set('FINITE VOLUME SCHEME', 1)
        self.add_study('kin1_seq', 'telemac2d', 't2d_ritter_FV.cas', cas=cas)
        # KIN1 parallel mode
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('kin1_par', 'telemac2d', 't2d_ritter_FV_par.cas', cas=cas)
        del cas

        #======================================================================
        # KIN2 run
        cas = TelemacCas('t2d_ritter-hllc.cas', get_dico('telemac2d'))
        cas.set('FINITE VOLUME SCHEME', 1)
        cas.set('FINITE VOLUME SCHEME SPACE ORDER', 2)
        self.add_study('kin2_seq', 'telemac2d', 't2d_ritter_FV.cas', cas=cas)
        # KIN2 parallel mode
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('kin2_par', 'telemac2d', 't2d_ritter_FV_par.cas', cas=cas)
        del cas

        #======================================================================
        # ZOKAGOA run
        cas = TelemacCas('t2d_ritter-hllc.cas', get_dico('telemac2d'))
        cas.set('FINITE VOLUME SCHEME', 3)
        self.add_study('zoka_seq', 'telemac2d', 't2d_ritter_FV.cas', cas=cas)
        # ZOKAGOA parallel mode
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('zoka_par', 'telemac2d', 't2d_ritter_FV_par.cas', cas=cas)
        del cas

        #======================================================================
        # TCHAMEN run
        cas = TelemacCas('t2d_ritter-hllc.cas', get_dico('telemac2d'))
        cas.set('FINITE VOLUME SCHEME', 4)
        self.add_study('tcha_seq', 'telemac2d', 't2d_ritter_FV.cas', cas=cas)
        # TCHAMEN parallel mode
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('tcha_par', 'telemac2d', 't2d_ritter_FV_par.cas', cas=cas)
        del cas


        #======================================================================
        # HLLC run
        cas = TelemacCas('t2d_ritter-hllc.cas', get_dico('telemac2d'))
        cas.set('FINITE VOLUME SCHEME', 5)
        self.add_study('hllc_seq', 'telemac2d', 't2d_ritter_FV.cas', cas=cas)
        # HLLC parallel mode
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('hllc_par', 'telemac2d', 't2d_ritter_FV_par.cas', cas=cas)
        del cas

        #======================================================================
        # WAF run
        cas = TelemacCas('t2d_ritter-hllc.cas', get_dico('telemac2d'))
        cas.set('FINITE VOLUME SCHEME', 6)
        self.add_study('waf_seq', 'telemac2d', 't2d_ritter_FV.cas', cas=cas)
        del cas


    def _check_results(self):
        """
        Post-treatment processes
        """
        self.check_epsilons('roe_seq:T2DRES', 'f2d_ritter-roe.slf', eps=[1e-8])
        self.check_epsilons('roe_par:T2DRES', 'f2d_ritter-roe.slf', eps=[1e-8])
        self.check_epsilons('roe_seq:T2DRES', 'roe_par:T2DRES', eps=[1e-8])

        self.check_epsilons('zoka_seq:T2DRES', 'f2d_ritter-zoka.slf', eps=[1e-8])
        self.check_epsilons('zoka_par:T2DRES', 'f2d_ritter-zoka.slf', eps=[1e-8])
        self.check_epsilons('zoka_seq:T2DRES', 'zoka_par:T2DRES', eps=[1e-8])

        self.check_epsilons('tcha_seq:T2DRES', 'f2d_ritter-tcha.slf', eps=[1e-8])
        self.check_epsilons('tcha_par:T2DRES', 'f2d_ritter-tcha.slf', eps=[1e-8])
        self.check_epsilons('tcha_seq:T2DRES', 'tcha_par:T2DRES', eps=[1e-8])

        self.check_epsilons('waf_seq:T2DRES', 'f2d_ritter-waf.slf', eps=[1e-4])

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

        #======================================================================
        # PLOT 1D
        records = [0, -1]

        for idx, record in enumerate(records):
            vnv_plot1d_polylines(\
                'WATER DEPTH',
                res_list,
                res_labels,
                record=record,
                fig_size=(6, 5),
                y_label='z',
                x_label='x',
                fig_name='img/t2d_ritter_fvschemes_comparison_{}'.format(record))

