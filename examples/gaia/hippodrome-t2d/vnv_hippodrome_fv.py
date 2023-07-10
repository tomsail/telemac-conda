
"""
Validation script for hippodrome
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
        self.rank = 2
        self.tags = ['telemac2d', 'gaia','fv']

    def _pre(self):
        """
        Defining the studies
        """

        # hippodrome scalar mode T2D+GAI
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_1COs_vf.cas')


        # hippodrome parallel mode T2D+GAI
        cas = TelemacCas('t2d_1COs_vf.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_1COs_par.cas',
                       cas=cas)

        del cas


    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:GAIRES',
                            'gai_ref_1COs_vf.slf',
                            eps=[1.e-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:GAIRES',
                            'gai_ref_1COs_vf.slf',
                            eps=[1.e-6])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:GAIRES',
                            'vnv_2:GAIRES',
                            eps=[1.e-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_1COs_vf.slf',
                            eps=[1.e-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_1COs_vf.slf',
                            eps=[1.e-6])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1.e-6])

    def _post(self):
        """
        Post-treatment processes
        """

