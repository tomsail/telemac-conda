
"""
Validation script for partel-gretel
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
        self.tags = ['python3']

    def _pre(self):
        """
        Defining the studies
        """

        # Using partel.py to split a mesh
        self.add_command('vnv_partel',
                         'partel.py --mpi --file geo_gouttedo.slf --ncsize=4 --file-format=SERAFIN --bnd-file geo_gouttedo.cli')


        # Using partel.py to split a mesh with an input file
        self.add_command('vnv_partel_input',
                         'partel.py --mpi --input-file partel.par')


        # Using gretel.py to split a mesh
        self.add_command('vnv_gretel',
                         'gretel.py --geo-file geo_gouttedo.slf --geo-file-format SERAFIN --res-file mesh --res-file-format SERAFIN --ncsize 4 --bnd-file geo_gouttedo.cli --method=1')


        # Using gretel.py to split a mesh with an input file
        self.add_command('vnv_gretel_input',
                         'gretel.py --input-file gretel.par')



    def _check_results(self):
        """
        Post-treatment processes
        """


    def _post(self):
        """
        Post-treatment processes
        """

