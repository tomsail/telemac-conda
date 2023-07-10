"""
Validation script for thacker (planar surface in a paraboloid)
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico

class VnvStudy(AbstractVnvStudy):
    """
    Class for verification of thacker (planar surface in a paraboloid)

    """
    def _init(self):
        """
        Defining general parameters
        """
        self.rank = 2
        self.tags = ['telemac2d','fv']

    def _pre(self):
        """
        Defining the studies
        """
        #======================================================================
        # KIN2 run
        cas = TelemacCas('t2d_thacker-fv.cas', get_dico('telemac2d'))
        cas.set('FINITE VOLUME SCHEME', 1)
        cas.set('FINITE VOLUME SCHEME SPACE ORDER', 2)
        self.add_study('kin2_seq', 'telemac2d', 't2d_thacker-fv.cas', cas=cas)
        # KIN2 parallel mode
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('kin2_par', 'telemac2d', 't2d_thacker-fv_par.cas', cas=cas)
        del cas

        #======================================================================
        # HLLC2 run
        cas = TelemacCas('t2d_thacker-fv.cas', get_dico('telemac2d'))
        cas.set('FINITE VOLUME SCHEME', 5)
        cas.set('FINITE VOLUME SCHEME SPACE ORDER', 2)
        self.add_study('hllc2_seq', 'telemac2d', 't2d_thacker-fv.cas', cas=cas)
        # HLLC2 parallel mode
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('hllc2_par', 'telemac2d', 't2d_thacker-fv_par.cas', cas=cas)
        del cas

        #======================================================================
        # WAF run
        cas = TelemacCas('t2d_thacker-fv.cas', get_dico('telemac2d'))
        cas.set('FINITE VOLUME SCHEME', 6)
        self.add_study('waf_seq', 'telemac2d', 't2d_thacker-fv.cas', cas=cas)
        del cas

    def _check_results(self):
        """
        Post-treatment processes
        """
        self.check_epsilons('kin2_seq:T2DRES', 'f2d_thacker-kin2.slf', eps=[1.])
        self.check_epsilons('kin2_par:T2DRES', 'f2d_thacker-kin2.slf', eps=[1.])
        self.check_epsilons('kin2_seq:T2DRES', 'kin2_par:T2DRES', eps=[1.])

        self.check_epsilons('hllc2_seq:T2DRES', 'f2d_thacker-hllc2.slf', eps=[1.])
        self.check_epsilons('hllc2_par:T2DRES', 'f2d_thacker-hllc2.slf', eps=[1.])
        self.check_epsilons('hllc2_seq:T2DRES', 'hllc2_par:T2DRES', eps=[1.])

        self.check_epsilons('waf_seq:T2DRES', 'f2d_thacker-waf.slf', eps=[1e-8])

    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot3d, vnv_plot2d, vnv_plot1d, vnv_plotbar, \
                vnv_plot1d_polylines, vnv_plot1d_history, vnv_plotbar_cpu_times
        from os import path
        import numpy as np
        #======================================================================
        # GENERAL PARAMETERS (used for adim):
        #
        RHO = 1000.
        G = 9.81
        H0 = 0.1
        A = 1.0
        ETA = 0.5
        OMEGA = np.sqrt(2*G*H0)/A
        T = 2.*np.pi/OMEGA
        U0 = ETA*OMEGA

        #======================================================================
        # GET TELEMAC RESULT FILES:
        #
        # Load all results as a list:
        res_list, res_labels = self.get_study_res(module='T2D', whitelist=['seq'])

        #======================================================================
        # PLOT 1D
        # One record every 0.5s i.e. record=9 ~ t=T
        records = [0, 1, 3, 6, 9]
        time_labels = ['0', 'T/9', 'T/3', '2T/3', 'T']

        for idx, record in enumerate(records):
            time_label = 't='+ time_labels[idx]
            vnv_plot1d_polylines(\
                'FREE SURFACE',
                res_list,
                res_labels,
                record=record,
                fig_size=(6, 5),
                ref_name='ANALYTIC SOL E',
                y_factor=1./H0,
                x_factor=1./A,
                y_label='z/H0',
                x_label='x/a',
                fig_name='img/t2d_thacker_fvschemes_comparison_{}'.format(record),
                fig_title=time_label,
                plot_bottom=True)

