
"""
Validation script for bumpcri
"""
from os import path, remove
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
from analytic_sol import BumpAnalyticSol

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 1
        self.tags = ['telemac2d','fv']

        # Boundary conditions:
        self.q_in = 0.3
        self.h_out = 0.5 # not used when flow is critical

        # Analytical solution:
        self.sol = BumpAnalyticSol(\
            flow='cri', Q=self.q_in, hl=self.h_out, length=20.,\
            bottom_function='exponential', N=801)
        self.sol()
        self.sol_file = "ANALYTIC_SOL_BUMPCRI_FV.txt"
        if path.exists(self.sol_file):
            remove(self.sol_file)

        self.sol.savetxt(self.sol_file)

    def set_bumpcri_values(self, cas):
        """
        Setting bumcri specific values to TelemacCas
        """
        cas.set('BOUNDARY CONDITIONS FILE', "geo_bump_tor.cli", convert=True)
        cas.set('PRESCRIBED FLOWRATES', [0., self.q_in])
        cas.set('FORMATTED DATA FILE 1', self.sol_file)
        cas.remove('PRESCRIBED ELEVATIONS')

    def _pre(self):
        """
        Defining the studies
        """
        #======================================================================
        # ROE run
        cas = TelemacCas('t2d_bump_FV.cas', get_dico('telemac2d'))
        self.set_bumpcri_values(cas)
        cas.set('FINITE VOLUME SCHEME', 0)
        self.add_study('roe_seq', 'telemac2d', 't2d_bump_FV.cas', cas=cas)
        # ROE parallel mode
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('roe_par', 'telemac2d', 't2d_bump_FV_par.cas', cas=cas)
        del cas

        #======================================================================
        # ZOKAGOA run
        cas = TelemacCas('t2d_bump_FV.cas', get_dico('telemac2d'))
        self.set_bumpcri_values(cas)
        cas.set('FINITE VOLUME SCHEME', 3)
        self.add_study('zoka_seq', 'telemac2d', 't2d_bump_FV.cas', cas=cas)
        # ZOKAGOA parallel mode
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('zoka_par', 'telemac2d', 't2d_bump_FV_par.cas', cas=cas)
        del cas

        #======================================================================
        # TCHAMEN run
        cas = TelemacCas('t2d_bump_FV.cas', get_dico('telemac2d'))
        self.set_bumpcri_values(cas)
        cas.set('FINITE VOLUME SCHEME', 4)
        self.add_study('tcha_seq', 'telemac2d', 't2d_bump_FV.cas', cas=cas)
        # TCHAMEN parallel mode
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('tcha_par', 'telemac2d', 't2d_bump_FV_par.cas', cas=cas)
        del cas

        #======================================================================
        # HLLC2 run
        cas = TelemacCas('t2d_bump_FV.cas', get_dico('telemac2d'))
        self.set_bumpcri_values(cas)
        cas.set('FINITE VOLUME SCHEME', 5)
        cas.set('FINITE VOLUME SCHEME SPACE ORDER', 2)
        self.add_study('hllc2_seq', 'telemac2d', 't2d_bump_FV.cas', cas=cas)
        # HLLC2 parallel mode
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('hllc2_par', 'telemac2d', 't2d_bump_FV_par.cas', cas=cas)
        del cas

        #======================================================================
        # WAF run
        cas = TelemacCas('t2d_bump_FV.cas', get_dico('telemac2d'))
        self.set_bumpcri_values(cas)
        cas.set('FINITE VOLUME SCHEME', 6)
        self.add_study('waf_seq', 'telemac2d', 't2d_bump_FV.cas', cas=cas)
        del cas

    def _check_results(self):
        """
        Post-treatment processes
        """
        self.check_epsilons('roe_seq:T2DRES', 'f2d_bumpcri-roe.slf', eps=[1e-8])
        self.check_epsilons('roe_par:T2DRES', 'f2d_bumpcri-roe.slf', eps=[1e-8])
        self.check_epsilons('roe_seq:T2DRES', 'roe_par:T2DRES', eps=[1e-8])

        self.check_epsilons('zoka_seq:T2DRES', 'f2d_bumpcri-zoka.slf', eps=[1e-8])
        self.check_epsilons('zoka_par:T2DRES', 'f2d_bumpcri-zoka.slf', eps=[1e-8])
        self.check_epsilons('zoka_seq:T2DRES', 'zoka_par:T2DRES', eps=[1e-8])

        self.check_epsilons('tcha_seq:T2DRES', 'f2d_bumpcri-tcha.slf', eps=[1e-8])
        self.check_epsilons('tcha_par:T2DRES', 'f2d_bumpcri-tcha.slf', eps=[1e-8])
        self.check_epsilons('tcha_seq:T2DRES', 'tcha_par:T2DRES', eps=[1e-8])

        self.check_epsilons('hllc2_seq:T2DRES', 'f2d_bumpcri-hllc2.slf', eps=[1.])
        self.check_epsilons('hllc2_par:T2DRES', 'f2d_bumpcri-hllc2.slf', eps=[1.])
        self.check_epsilons('hllc2_seq:T2DRES', 'hllc2_par:T2DRES', eps=[1.])

        self.check_epsilons('waf_seq:T2DRES', 'f2d_bumpcri-waf.slf', eps=[1e-3])

    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot1d_polylines
        #======================================================================
        # GET TELEMAC RESULT FILES:
        #
        # Load all results as a list:
        res_list, res_labels = self.get_study_res(module='T2D', whitelist=['seq'])

        #======================================================================
        # PLOT 1D
        records = [0, -1]

        for record in records:
            vnv_plot1d_polylines(\
                'FREE SURFACE',
                res_list,
                res_labels,
                record=record,
                fig_size=(6, 5),
                y_label='z',
                x_label='x',
                fig_name='img/t2d_bumpcri_fvschemes_comparison_{}'.format(record),
                plot_bottom=True)
