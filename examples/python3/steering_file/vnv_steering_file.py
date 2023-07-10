
"""
Validation script for steering_file
"""
from os import path
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas
from utils.exceptions import TelemacException


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
        from execution.telemac_dico import TelemacDico
        from config import CFGS
        from os import chdir, getcwd, remove

        list_modules = ['telemac2d',
                        'telemac3d',
                        'artemis',
                        'sisyphe',
                        'waqtel',
                        'khione',
                        'tomawac',
                        'stbtel',
                        'postel3d',
                        'gaia',
                        'mascaret']

        #####
        # Testing dictionaries
        for module in list_modules:
            print("    ~> Testing dico for ", module)
            dico_name = path.join(CFGS.get_root(), "sources",
                                  module, module+".dico")
            dico = TelemacDico(dico_name)
            print(dico)
            del dico

        #####
        # Testing reading an english file
        test_dir = 'examples/telemac2d/gouttedo'
        file_name = 't2d_gouttedo.cas'
        pwd = getcwd()
        full_test_dir = path.join(CFGS.get_root(), test_dir)
        dico_name = path.join(CFGS.get_root(), "sources",
                              "telemac2d", "telemac2d.dico")
        chdir(full_test_dir)
        print("    ~> Testing steering file: ", file_name)
        cas = TelemacCas(file_name, dico_name)
        print(cas)
        chdir(pwd)

        #####
        # Testing translation of a steering file
        test_dir = 'examples/telemac2d/gouttedo'
        file_name = 't2d_gouttedo.cas'
        pwd = getcwd()
        full_test_dir = path.join(CFGS.get_root(), test_dir)
        dico_name = path.join(CFGS.get_root(), "sources",
                              "telemac2d", "telemac2d.dico")
        chdir(full_test_dir)
        print("    ~> Testing translattion of steering file: ", file_name)
        cas = TelemacCas(file_name, dico_name)
        cas.write_fr_gb()
        remove('t2d_gouttedo_fr.cas')
        remove('t2d_gouttedo_en.cas')
        chdir(pwd)

        #####
        # Testing get/set
        test_dir = 'examples/telemac2d/gouttedo'
        file_name = 't2d_gouttedo.cas'
        pwd = getcwd()
        full_test_dir = path.join(CFGS.get_root(), test_dir)
        dico_name = path.join(CFGS.get_root(), "sources",
                              "telemac2d", "telemac2d.dico")
        chdir(full_test_dir)
        print("    ~> Testing get set in steering file: ", file_name)
        cas = TelemacCas(file_name, dico_name)

        if not cas.get('PARALLEL PROCESSORS') == 0:
            raise TelemacException("Error in get/set test")

        if not cas.get('TITLE') == cas.get('TITRE'):
            raise TelemacException("Error in get/set test")

        cas.set('TITLE', 'none')
        if not cas.get('TITLE') == 'none':
            raise TelemacException("Error in get/set test")

        cas.set('TYPE OF ADVECTION', [3, 3])
        if not cas.get('TYPE OF ADVECTION') == [3, 3]:
            raise TelemacException("Error in get/set test")

        chdir(pwd)

        # Translate test case
        self.add_command('vnv_manip_cas',
                         'manip_cas.py translate telemac2d t2d_gouttedo.cas')

    def _check_results(self):
        """
        Post-treatment processes
        """


    def _post(self):
        """
        Post-treatment processes
        """
