
"""
Validation script for bars
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
        self.tags = ['telemac2d', 'sisyphe']

    def _pre(self):
        """
        Defining the studies
        """

        # bars T2D+SIS scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_Bar_Test_Random.cas')


        # bars T2D+SIS parallel mode
        cas = TelemacCas('t2d_Bar_Test_Random.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_Bar_Test_Random_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:SISRES',
                            'fis_Bar_Test_Random.slf',
                            eps=[0.02, 0.03, 0.008, 0.007, 0.004, 0.2, 0.009])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:SISRES',
                            'fis_Bar_Test_Random.slf',
                            eps=[1.8e-0])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:SISRES',
                            'vnv_2:SISRES',
                            eps=[1.8e-0])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_Bar_Test_Random.slf',
                            eps=[0.02, 0.03, 0.008, 0.004, 0.009])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_Bar_Test_Random.slf',
                            eps=[1.5e-0])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1.8e-0])


    def _post(self):
        """
        Post-treatment processes
        """

