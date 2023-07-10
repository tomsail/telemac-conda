
"""
Validation script for creocean_animated
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
        self.tags = ['artemis']
        self.walltime = '05:00:00'

    def _pre(self):
        """
        Defining the studies
        """

        # animated creocean parallel mode
        cas = TelemacCas('art_creocean200-anim.cas', get_dico('artemis'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('SOLVER', 9)


        self.add_study('vnv_1_anim',
                       'artemis',
                       'art_creocean200-anim_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the reference file.
        self.check_epsilons('vnv_1_anim:ARTRES',
                            'f2d_creocean200.slf',
                            eps=[1.e-8, 1.e-8, 1.e-8, 360., 1.e-8])

        # Comparison with the reference file (first period).
        self.check_epsilons('vnv_1_anim:ARTAMP',
                            'famp_creocean200.slf',
                            eps=[3.e-7])

        # Comparison with the reference file (10th period).
        self.check_epsilons('vnv_1_anim:ARTAMP',
                            'famp_creocean200.slf',
                            eps=[3.e-7])

        # Comparison with the reference file (last period).
        self.check_epsilons('vnv_1_anim:ARTAMP',
                            'famp_creocean200.slf',
                            eps=[3.e-7])


    def _post(self):
        """
        Post-treatment processes
        """

