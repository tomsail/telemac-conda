
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
        self.walltime = '10:00:00'

    def _pre(self):
        """
        Defining the studies
        """

        #======================================================================
        # KIN2 run
        cas = TelemacCas('t2d_malpasset-hllc.cas', get_dico('telemac2d'))
        cas.set('FINITE VOLUME SCHEME', 1)
        cas.set('FINITE VOLUME SCHEME SPACE ORDER', 2)
        self.add_study('kin2_seq', 'telemac2d', 't2d_malpasset_FV.cas', cas=cas)
        # KIN2 parallel mode
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('kin2_par', 'telemac2d', 't2d_malpasset_FV_par.cas', cas=cas)
        del cas

        #======================================================================
        # WAF run
        cas = TelemacCas('t2d_malpasset-hllc.cas', get_dico('telemac2d'))
        cas.set('FINITE VOLUME SCHEME', 6)
        self.add_study('waf_seq', 'telemac2d', 't2d_malpasset_FV.cas', cas=cas)
        del cas

    def _check_results(self):
        """
        Post-treatment processes
        """
        #======================================================================

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('kin2_seq:T2DRES',
                            'f2d_malpasset-kin2.slf',
                            eps=[1.])
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('kin2_par:T2DRES',
                            'f2d_malpasset-kin2.slf',
                            eps=[1.])
        # Comparison between sequential and parallel run.
        self.check_epsilons('kin2_seq:T2DRES',
                            'kin2_par:T2DRES',
                            eps=[1.])

        #======================================================================
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('waf_seq:T2DRES',
                            'f2d_malpasset-waf.slf',
                            eps=[3.E-1])


    def _post(self):
        """
        Post-treatment processes
        """

