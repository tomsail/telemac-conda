
"""
Validation script for nestorExample1
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
        self.rank = 1
        self.tags = ['telemac2d', 'sisyphe', 'gaia', 'nestor']

    def _pre(self):
        """
        Defining the studies
        """
        #============ nestor2 + sis ====================================
        # scalar mode
        self.add_study('vnv_sis1', 'telemac2d', 't2d_nestor2_sis.cas')

        # parallel mode
        cas = TelemacCas('t2d_nestor2_sis.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('vnv_sis2', 'telemac2d', 't2d_nestor2_sis_par.cas', cas=cas)

        del cas

        #============ nestor2 + gai ====================================
        # scalar mode
        self.add_study('vnv_gai1', 'telemac2d', 't2d_nestor2_gai.cas')

        # parallel mode
        cas = TelemacCas('t2d_nestor2_gai.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('vnv_gai2', 'telemac2d', 't2d_nestor2_gai_par.cas', cas=cas)

        del cas
        #================================================



    def _check_results(self):
        """
        Post-treatment processes
        """
        #============ nestor2 + sis ====================================
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_sis1:SISRES',
                            'sis_ref_nestor2.slf',  eps=[1.e-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_sis2:SISRES',
                            'sis_ref_nestor2.slf',  eps=[1.e-6])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_sis1:SISRES',
                            'vnv_sis2:SISRES',  eps=[1.e-6])

        #============ nestor2 + gai ====================================
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_gai1:GAIRES',
                            'gai_ref_nestor2.slf',  eps=[1.e-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_gai2:GAIRES',
                            'gai_ref_nestor2.slf',  eps=[1.e-6])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_gai1:GAIRES',
                            'vnv_gai2:GAIRES',  eps=[1.e-6])





        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_gai1:T2DRES',
                            't2d_ref_nestor2.slf',
                            eps=[1.e-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_gai2:T2DRES',
                            't2d_ref_nestor2.slf',
                            eps=[1.e-6])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_gai1:T2DRES',
                            'vnv_gai2:T2DRES',
                            eps=[1.e-6])


    def _post(self):
        """
        Post-treatment processes
        """

