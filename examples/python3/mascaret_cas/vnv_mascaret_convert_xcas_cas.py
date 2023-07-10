"""
Validation script of MascaretCas
Conversion of all Mascaret xcas files
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
        self.rank = 0
        self.tags = ['python3', 'mascaret']

    def _pre(self):
        """
        Defining the studies
        """
        from config import CFGS

        current_folder = os.getcwd()
        vnv_folder = os.path.join(current_folder, "vnv_mascaret_xcas_cas")
        if os.path.exists(vnv_folder):
            shutil.rmtree(vnv_folder)

        os.mkdir(vnv_folder)

        example_mascaret_folder = os.path.join(
            CFGS.get_root(),
            "examples",
            "mascaret")

        xcas_files = glob(os.path.join(example_mascaret_folder, "*", "*.xcas"))
        dico_name = os.path.join(CFGS.get_root(), "sources",
                                 "mascaret", "mascaret.dico")

        # Translate test case
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

    def _check_results(self):
        """
        Post-treatment processes
        """

    def _post(self):
        """
        Post-treatment processes
        """
