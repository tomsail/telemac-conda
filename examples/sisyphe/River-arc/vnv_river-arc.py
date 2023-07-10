
"""
Validation script for river-arc
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
        self.tags = ['telemac2d', 'sisyphe']

    def _pre(self):
        """
        Defining the studies
        """

        # Run river-arc with coupled sisyphe telemac
        self.add_study('vnv_scal',
                       'telemac2d',
                       't2d_river-arc.cas')


        # Run river-arc with coupled sisyphe telemac
        cas = TelemacCas('t2d_river-arc.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_para',
                       'telemac2d',
                       't2d_river-arc_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_scal:SISRES',
                            'fis_river-arc.slf',
                            eps=[666])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_para:SISRES',
                            'fis_river-arc.slf',
                            eps=[666])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_scal:SISRES',
                            'vnv_para:SISRES',
                            eps=[666])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_scal:T2DRES',
                            'f2d_river-arc.slf',
                            eps=[1e-1])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_para:T2DRES',
                            'f2d_river-arc.slf',
                            eps=[666])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_scal:T2DRES',
                            'vnv_para:T2DRES',
                            eps=[666])


    def _post(self):
        """
        Post-treatment processes
        """

