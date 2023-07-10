"""
Validation script for MascaretCas
On three test-cases (Mascaret, Courlis, Tracer) :
    Conversion xcas => cas
    Copyfiles with xcas and cas files
    Execution of the test-cases with cas files
"""
import os
import shutil
from glob import glob
from pathlib import Path

from execution.mascaret_cas import MascaretCas
from vvytel.vnv_study import AbstractVnvStudy


class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 1
        self.tags = ['mascaret', 'python3', 'courlis']

    def _pre(self):
        """
        Defining the studies
        """
        from config import CFGS

        xcas_files = []
        xcas_files.append(os.path.join(CFGS.get_root(), "examples",
                                       "courlis", "Garonne", "hydro_Torr.xcas"))
        xcas_files.append(os.path.join(CFGS.get_root(), "examples",
                                       "mascaret", "Test23", "mascaret.xcas"))
        xcas_files.append(os.path.join(CFGS.get_root(), "examples",
                                       "mascaret", "Test_Tracer", "thermic.xcas"))

        current_folder = os.getcwd()

        # Translate test case
        vnv_folder = os.path.join(current_folder, "vnv_mascaret_xcas_cas_exe")
        if os.path.exists(vnv_folder):
            shutil.rmtree(vnv_folder)

        os.mkdir(vnv_folder)

        for xcas_file in xcas_files:
            case = MascaretCas(
                xcas_file,
                check_files=False,
                convert_xcas=True)

            xcas_path = Path(xcas_file)
            test_case = f'{xcas_path.parent.name}_{xcas_path.stem}'

            folder = os.path.join(vnv_folder, test_case)

            if os.path.exists(folder):
                shutil.rmtree(folder)

            os.mkdir(folder)

            case_name = xcas_path.stem
            en_filename = os.path.join(folder, f'{case_name}_en.cas')
            case.write_case_file(cas_filename=en_filename, lang='en')

            case.copy_cas_files(folder, verbose=True)
            print("\n")

            self.add_study('vnv_all_xcas_cas_exe_en_' + test_case,
                           'mascaret',
                           en_filename)

            folder_alter = os.path.join(vnv_folder,  f'{test_case}_alter')

            if os.path.exists(folder_alter):
                shutil.rmtree(folder_alter)

            os.mkdir(folder_alter)

            case = MascaretCas(
                en_filename,
                check_files=False)

            en_filename_alter = os.path.join(
                folder_alter, f'{case_name}_en.xcas')
            case.write_xcas_file(xcas_filename=en_filename_alter)

            case.copy_cas_files(folder_alter, verbose=True)
            print("\n")

            self.add_study('vnv_all_xcas_cas_exe_en_alter_' + test_case,
                           'mascaret',
                           en_filename_alter)

    def _check_results(self):
        """
        Post-treatment processes
        """

    def _post(self):
        """
        Post-treatment processes
        """
