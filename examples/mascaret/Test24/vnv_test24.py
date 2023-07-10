
"""
Validation script for test24
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
        self.tags = ['mascaret']

    def _pre(self):
        """
        Defining the studies
        """

        # Test24 transcritical implicit kernel
        self.add_study('vnv_1',
                       'mascaret',
                       'mascaret_imp.xcas')


        # Test24 transcritical explicit kernel
        self.add_study('vnv_2',
                       'mascaret',
                       'mascaret_exp.xcas')



    def _check_results(self):
        """
        Post-treatment processes
        """


    def _post(self):
        """
        Post-treatment processes
        """

