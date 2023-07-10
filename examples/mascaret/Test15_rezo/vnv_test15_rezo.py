
"""
Validation script for test15_rezo
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
        self.tags = ['mascaret']

    def _pre(self):
        """
        Defining the studies
        """

        # Test15 unsteady subcritical kernel
        self.add_study('vnv_1',
                       'mascaret',
                       'rezo.xcas')



    def _check_results(self):
        """
        Post-treatment processes
        """


    def _post(self):
        """
        Post-treatment processes
        """

