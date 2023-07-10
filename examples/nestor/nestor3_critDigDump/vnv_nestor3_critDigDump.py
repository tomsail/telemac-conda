
"""
Validation script for nestor3_critDigDump
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
        self.rank = 3
        self.tags = ['telemac2d', 'sisyphe', 'gaia', 'nestor']

    def _pre(self):
        """
        Defining the studies
        """
        #============ nestor3 + sis ====================================
        # scalar mode
        self.add_study('vnv_sis1', 'telemac2d', 't2d_nestor3_sis.cas')

        # parallel mode
        cas = TelemacCas('t2d_nestor3_sis.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('vnv_sis2', 'telemac2d', 't2d_nestor3_sis_par.cas', cas=cas)

        del cas

        #============ nestor3 + gai ====================================
        # scalar mode
        self.add_study('vnv_gai1', 'telemac2d', 't2d_nestor3_gai.cas')

        # parallel mode
        cas = TelemacCas('t2d_nestor3_gai.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('vnv_gai2', 'telemac2d', 't2d_nestor3_gai_par.cas', cas=cas)

        del cas
        #================================================



    def _check_results(self):
        """
        Post-treatment processes
        """
        #============ nestor3 + sis ====================================
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_sis1:SISRES',
                            'sis_ref_nestor3.slf',  eps=[1.e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_sis2:SISRES',
                            'sis_ref_nestor3.slf',  eps=[1.e-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_sis1:SISRES',
                            'vnv_sis2:SISRES',  eps=[1.e-3])

        #============ nestor3 + gai ====================================
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_gai1:GAIRES',
                            'gai_ref_nestor3.slf',  eps=[1.e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_gai2:GAIRES',
                            'gai_ref_nestor3.slf',  eps=[1.e-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_gai1:GAIRES',
                            'vnv_gai2:GAIRES',  eps=[1.e-3])





        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_gai1:T2DRES',
                            't2d_ref_nestor3_gai.slf',
                            eps=[1.e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_gai2:T2DRES',
                            't2d_ref_nestor3_gai.slf',
                            eps=[1.e-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_gai1:T2DRES',
                            'vnv_gai2:T2DRES',
                            eps=[1.e-3])


    def _post(self):
        """
        Post-treatment processes
        """

