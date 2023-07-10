
"""
Validation script for api
"""
from sys import platform

from vvytel.vnv_study import AbstractVnvStudy


class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 0
        self.tags = ['api',
                     'python3',
                     'telemac2d',
                     'telemac3d',
                     'sisyphe',
                     'gaia',
                     'waqtel',
                     'tomawac',
                     'artemis']

    def _pre(self):
        """
        Defining the studies
        """

        python = 'python' if platform == 'win32' else 'python3'

        # Telemac2d api run of gouttedo
        self.add_command('vnv_t2d',
                         f'{python} t2d.py && {python} test_recompile.py',
                         hpc=True)

        # Telemac2d api run of gouttedo in parallel
        self.add_command('vnv_t2d-par',
                         f'mpiexec -n 4 {python} t2d.py && '
                         f'mpirun -n 4 {python} test_recompile.py',
                         hpc=True)

        # Telemac2d api run of gouttedo Finite Volume
        self.add_command('vnv_t2d_fv',
                         f'{python} t2d_cin.py',
                         hpc=True)

        # Telemac2d api run of gouttedo Finite Volume in parallel
        self.add_command('vnv_t2d_fv-par',
                         f'mpiexec -n 4 {python} t2d_cin.py',
                         hpc=True)

        # Tomawac api run of dean
        self.add_command('vnv_wac',
                         f'{python} wac.py',
                         hpc=True)

        # Tomawac api run of dean in parallel
        self.add_command('vnv_wac-par',
                         f'mpiexec -n 4 {python} wac.py',
                         hpc=True)

        # Telemac3d api run of gouttedo3d
        self.add_command('vnv_t3d',
                         f'{python} t3d.py',
                         hpc=True)

        # Telemac3d api run of gouttedo3d in parallel
        self.add_command('vnv_t3d-par',
                         f'mpiexec -n 4 {python} t3d.py',
                         hpc=True)

        # Telemac3d-Waqtel api run of heat_exchange
        self.add_command('vnv_t3d-waq',
                         f'{python} t3d_waq.py',
                         hpc=True)

        # Telemac3d-Waqtel api run of heat_exchange in parallel
        self.add_command('vnv_t3d-waq-par',
                         f'mpiexec -n 4 {python} t3d_waq.py',
                         hpc=True)

        # Telemac2d-gaia api run of hippodrome-t2d
        self.add_command('vnv_t2d-gai',
                         f'{python} t2d_gai.py',
                         hpc=True)

        # Telemac2d-gaia api run of hippodrome-t2d in parallel
        self.add_command('vnv_t2d-gai-par',
                         f'mpiexec -n 4 {python} t2d_gai.py',
                         hpc=True)

        # Telemac3d-gaia api run of hippodrome-t3d
        self.add_command('vnv_t3d-gai',
                         f'{python} t3d_gai.py',
                         hpc=True)

        # Telemac3d-gaia api run of hippodrome-t3d in parallel
        self.add_command('vnv_t3d-gai-par',
                         f'mpiexec -n 4 {python} t3d_gai.py',
                         hpc=True)

        # Artemis api run of bj78
        self.add_command('vnv_art',
                         f'{python} art.py',
                         hpc=True)

        # Telemac2d-sisyphe api run of conservation
        self.add_command('vnv_t2d-sis',
                         f'{python} t2d_sis_cpl.py',
                         hpc=True)

    def _check_results(self):
        """
        Post-treatment processes
        """

    def _post(self):
        """
        Post-treatment processes
        """
