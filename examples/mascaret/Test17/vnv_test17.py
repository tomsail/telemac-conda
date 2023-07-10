
"""
Validation script for test17
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

        # Test17 permanent kernel
        self.add_study('vnv_1',
                       'mascaret',
                       'sarap.xcas')


        # Test17 transcritical kernel
        self.add_study('vnv_2',
                       'mascaret',
                       'mascaret.xcas')



    def _check_results(self):
        """
        Post-treatment processes
        """


    def _post(self):
        """
        Post-treatment processes
        """

