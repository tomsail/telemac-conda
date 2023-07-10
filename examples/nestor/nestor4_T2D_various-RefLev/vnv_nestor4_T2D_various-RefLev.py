
"""
Validation script for nestor4_T2D_various-RefLev
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
        self.tags = ['telemac2d', 'sisyphe', 'nestor']

    def _pre(self):
        """
        Defining the studies
        """

        # Test example nestor scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_nestor4.cas')


        # Test example nestor parallel mode
        cas = TelemacCas('t2d_nestor4.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_nestor2_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            't2d_ref_nestor4.slf',
                            eps=[1.e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            't2d_ref_nestor4.slf',
                            eps=[1.e-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1.e-3])


    def _post(self):
        """
        Post-treatment processes
        """

