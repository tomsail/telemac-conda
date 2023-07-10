
"""
Validation script for beach
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

        # idealised beach, scalar mode
        self.add_study('vnv_1',
                       'artemis',
                       'art_plage.cas')


        # idealised beach, parallel mode
        cas = TelemacCas('art_plage.cas', get_dico('artemis'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('SOLVER', 9)


        self.add_study('vnv_2',
                       'artemis',
                       'art_plage_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the reference file.
        self.check_epsilons('vnv_1:ARTRES',
                            'ref_plage.slf',
                            eps=[1.e-8])

        # Comparison with the reference file.
        self.check_epsilons('vnv_2:ARTRES',
                            'ref_plage.slf',
                            eps=[1.e-8, 1.e-8, 1.e-8, 1.e-8, 2., 1.e-8])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:ARTRES',
                            'vnv_2:ARTRES',
                            eps=[1.e-8, 1.e-8, 1.e-8, 1.e-8, 2., 1.e-8])


    def _post(self):
        """
        Post-treatment processes
        """
