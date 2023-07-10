
"""
Validation script for westcoast
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
        self.tags = ['artemis']

    def _pre(self):
        """
        Defining the studies
        """

        # West coast of Ireland, scalar mode
        self.add_study('vnv_1',
                       'artemis',
                       'art_westcoast.cas')


        # West coast of Ireland, parallel mode
        cas = TelemacCas('art_westcoast.cas', get_dico('artemis'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('SOLVER', 9)


        self.add_study('vnv_2',
                       'artemis',
                       'art_westcoast_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the reference file.
        self.check_epsilons('vnv_1:ARTRES',
                            'f2d_westcoast.12689.slf',
                            eps=[1.e-8])

        # Comparison with the reference file.
        self.check_epsilons('vnv_2:ARTRES',
                            'f2d_westcoast.12689.slf',
                            eps=[1.e-8, 1.e-8, 1.e-8, 1.e-8, 360, 1.e-8])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:ARTRES',
                            'vnv_2:ARTRES',
                            eps=[1.e-8, 1.e-8, 1.e-8, 1.e-8, 360, 1.e-8])


    def _post(self):
        """
        Post-treatment processes
        """

