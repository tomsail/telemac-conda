
"""
Validation script for stoker
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
        cas = TelemacCas('t2d_stoker-hllc.cas', get_dico('telemac2d'))
        cas.set('FINITE VOLUME SCHEME', 0)
        self.add_study('roe_seq', 'telemac2d', 't2d_stoker_FV.cas', cas=cas)
        # ROE parallel mode
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('roe_par', 'telemac2d', 't2d_stoker_FV_par.cas', cas=cas)
        del cas

        #======================================================================
        # ZOKAGOA run
        cas = TelemacCas('t2d_stoker-hllc.cas', get_dico('telemac2d'))
        cas.set('FINITE VOLUME SCHEME', 3)
        self.add_study('zoka_seq', 'telemac2d', 't2d_stoker_FV.cas', cas=cas)
        # ZOKAGOA parallel mode
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('zoka_par', 'telemac2d', 't2d_stoker_FV_par.cas', cas=cas)
        del cas

        #======================================================================
        # TCHAMEN run
        cas = TelemacCas('t2d_stoker-hllc.cas', get_dico('telemac2d'))
        cas.set('FINITE VOLUME SCHEME', 4)
        self.add_study('tcha_seq', 'telemac2d', 't2d_stoker_FV.cas', cas=cas)
        # TCHAMEN parallel mode
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('tcha_par', 'telemac2d', 't2d_stoker_FV_par.cas', cas=cas)
        del cas

        #======================================================================
        # WAF run
        cas = TelemacCas('t2d_stoker-hllc.cas', get_dico('telemac2d'))
        cas.set('FINITE VOLUME SCHEME', 6)
        self.add_study('waf_seq', 'telemac2d', 't2d_stoker_FV.cas', cas=cas)
        del cas


    def _check_results(self):
        """
        Post-treatment processes
        """
        self.check_epsilons('roe_seq:T2DRES', 'f2d_stoker-roe.slf', eps=[1e-8])
        self.check_epsilons('roe_par:T2DRES', 'f2d_stoker-roe.slf', eps=[1e-8])
        self.check_epsilons('roe_seq:T2DRES', 'roe_seq:T2DRES', eps=[1e-8])

        self.check_epsilons('zoka_seq:T2DRES', 'f2d_stoker-zoka.slf', eps=[1e-8])
        self.check_epsilons('zoka_par:T2DRES', 'f2d_stoker-zoka.slf', eps=[1e-8])
        self.check_epsilons('zoka_seq:T2DRES', 'zoka_seq:T2DRES', eps=[1e-8])

        self.check_epsilons('tcha_seq:T2DRES', 'f2d_stoker-tcha.slf', eps=[1e-8])
        self.check_epsilons('tcha_par:T2DRES', 'f2d_stoker-tcha.slf', eps=[1e-8])
        self.check_epsilons('tcha_seq:T2DRES', 'tcha_seq:T2DRES', eps=[1e-8])

        self.check_epsilons('waf_seq:T2DRES', 'f2d_stoker-waf.slf', eps=[1e-3])

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
                fig_name='img/t2d_stoker_fvschemes_comparison_{}'.format(record))

