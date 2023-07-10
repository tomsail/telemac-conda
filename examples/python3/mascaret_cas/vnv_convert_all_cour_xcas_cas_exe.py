"""
Validation script for MascaretCas
On all Courlis test-cases :
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
        self.rank = 3
        self.tags = ['mascaret', 'python3']

    def _pre(self):
        """
        Defining the studies
        """
        from config import CFGS

        current_folder = os.getcwd()
        vnv_folder = os.path.join(current_folder,
                                  "vnv_convert_all_cour_xcas_cas_exe")
        if os.path.exists(vnv_folder):
            shutil.rmtree(vnv_folder)

        os.mkdir(vnv_folder)

        example_mascaret_folder = os.path.join(
            CFGS.get_root(),
            "examples",
            "courlis")

        xcas_files = glob(os.path.join(example_mascaret_folder, "*", "*.xcas"))
        dico_name = os.path.join(CFGS.get_root(), "sources",
                                 "mascaret", "mascaret.dico")

        for xcas_file in xcas_files:
            print("xcas file: ", xcas_file)
            case = MascaretCas(
                xcas_file,
                dico_name,
                check_files=False,
                convert_xcas=True)

            xcas_path = Path(xcas_file)
            test_case = f'{xcas_path.parent.name}_{xcas_path.stem}'

            folder = os.path.join(vnv_folder, test_case)

            if os.path.exists(folder):
                shutil.rmtree(folder)

            os.mkdir(folder)

            case_name = Path(case.file_name).stem
            fr_filename = os.path.join(folder, f'{case_name}_fr.cas')
            case.write_case_file(cas_filename=fr_filename, lang='fr')
            print("French case filename: ", fr_filename)
            en_filename = os.path.join(folder, f'{case_name}_en.cas')
            case.write_case_file(cas_filename=en_filename, lang='en')
            print("English case filename: ", en_filename)

            # Execution of all
            case.copy_cas_files(folder, verbose=False, copy_cas_file=True)

            cas_file = fr_filename

            case = MascaretCas(cas_file, dico_name, check_files=False)

            xcas_filename = os.path.join(folder, f'{case_name}.xcas')
            case.write_xcas_file(xcas_filename=xcas_filename)

            print("\n")

            self.add_study('vnv_all_xcas_cas_exe_fr_' + test_case,
                           'mascaret',
                           en_filename)

            self.add_study('vnv_all_xcas_cas_exe_en_' + test_case,
                           'mascaret',
                           fr_filename)

    def _check_results(self):
        """
        Post-treatment processes
        """

    def _post(self):
        """
        Post-treatment processes
        """
