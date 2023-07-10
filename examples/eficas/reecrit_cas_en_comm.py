#!/usr/bin/env python
# -*- coding: utf-8 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
# THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
# IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
# THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
# (AT YOUR OPTION) ANY LATER VERSION.
#
# THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
# WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
# MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
# GENERAL PUBLIC LICENSE FOR MORE DETAILS.
#
# YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
# ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
#    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
#
#
# ======================================================================

"""
Script to test reading and writting a test case with eficas
then comparing the two runs
"""
# Modules Python
from __future__ import absolute_import
from __future__ import print_function

import sys
from os import path, listdir, system, chdir, environ, remove
from argparse import ArgumentParser
import shutil
import re

# Modules Eficas
try:
    import Telemac.prefs
except ImportError as excp:
    print("Add the path to eficas to PYTHONPATH")
if hasattr(Telemac.prefs, 'encoding'):
    # Hack pour changer le codage par defaut des strings
    import sys
    reload(sys)
    sys.setdefaultencoding(prefs.encoding)
    del sys.setdefaultencoding
    # Fin hack

from PyQt5.QtWidgets import QApplication

HEADER = '\033[95m'
OKBLUE = '\033[94m'
OKGREEN = '\033[92m'
WARNING = '\033[93m'
FAIL = '\033[91m'
ENDC = '\033[0m'
BOLD = '\033[1m'
UNDERLINE = '\033[4m'

PREFIX = {'telemac2d':'t2d',
          'telemac3d':'t3d',
          'tomawac':'tom',
          'sisyphe':'sis',
          'artemis':'art',
          'waqtel':'waq'}

def clean_up_diff(cas_file, eficas_cas_file, tmp_diff):
    """
    Remove obsolete lines from diff
    """
    new_diff = []
    with open(tmp_diff, 'r') as fle:
        for line in fle:
            if cas_file in line or \
               eficas_cas_file in line or\
               'mpirun' in line or \
               '---' in line or \
               re.match('[0-9]+(,[0-9]*)?[ca][0-9]+(,[0-9]*)?', line):
                continue
            new_diff.append(line)

    return new_diff

def read_write_eficas(module, steering_file, eficas_steering):
    """
    Import a steergin file in Eficas and save it back

    @param module Name of the telemac-mascaret module
    @param steering_file Name of the steering file
    @param eficas_file Name of the steering file written by Eficas
    """
    from InterfaceQT4.eficas_go import getEficasSsIhm
    code = 'TELEMAC'
    my_eficas = getEficasSsIhm(code=code, versionCode=module)

    handler = my_eficas.fileOpen(steering_file)
    if not handler:
        raise Exception(steering_file, "Eficas crashed")
    if not handler.isJdcValid():
        report = handler.getJdcRapport()
        raise Exception(steering_file, report)

    handler.fileSaveAs(eficas_steering)

def validate_catalog(module, val_folder, root_dir, light, lng=''):
    """
    Validate a given Catalog for a given module

    @param module Name of the module
    @param val_folder Path for the folder containing the examples (Telemac tree)
    @param root_dir Telemac root path
    @param light Specify eficas output if true light output
    """
    print(" "*2, "~> For module", val_folder)
    examples_dir = path.join(root_dir, 'examples', val_folder)
    output_dir = path.join(root_dir, 'examples', 'eficas')

    crashed = []
    different = []
    for example in sorted(listdir(examples_dir)):
    #for example in ['Negretti2D']:
        example_dir = path.join(examples_dir, example)
        chdir(example_dir)
        for case in sorted(listdir(example_dir)):
            if case.endswith('.cas') and \
               "_reecrit" not in case and \
               case[0:3] == PREFIX[module] and \
               "_reecrit" not in case and \
               "_ori" not in case:
                # Adding lang extension (.fr for translated french case)
                ext_lang = '.fr' if lng == 'fr' else ''
                mycase = case + ext_lang
                print(" "*6, "~> For test case ", mycase)

                root, _ = path.splitext(case)
                if lng == 'fr':
                    lang = 'fr'
                    ori = "_ori_fr"
                else:
                    lang = ''
                    ori = "_ori"
                ori_case = root + ori + ".cas"
                if light:
                    eficas_case = root + ori +"_reecrit.Lcas"
                else:
                    eficas_case = root + ori +"_reecrit.cas"

                run_log = ori_case+".log"
                run_eficas_log = eficas_case+".log"
                tmp_diff_file = path.join(output_dir, "tmp_"+root+'.log')

                # Creating a temporary case file with &ETA at the end
                shutil.copyfile(mycase, ori_case)
                with open(ori_case, 'a') as fle:
                    fle.write('\n&ETA\n')

                # Import and export in eficas
                try:
                    read_write_eficas(module, ori_case, eficas_case)
                except Exception as e:
                    print(e)
                    crashed.append(mycase)
                    print(" "*8+FAIL+"FAILED"+ENDC)
                    print(" "*8+"Crashed in eficas")
                    continue

                # Running original case
                cmd = "%s.py %s --use-link -w %s > %s"%(module,
                                                        ori_case,
                                                        ori_case+"_dir",
                                                        run_log)
                print(cmd)
                system(cmd)

                # Running eficas case
                cmd = "%s.py %s --use-link -w %s > %s"%(module,
                                                        eficas_case,
                                                        eficas_case+"_dir",
                                                        run_eficas_log)
                print(cmd)
                system(cmd)

                # Creating diff file
                cmd = "diff %s %s > %s"%(run_log, run_eficas_log, tmp_diff_file)
                system(cmd)

                # Cleanup of diff file
                new_diff = clean_up_diff(ori_case, eficas_case, tmp_diff_file)
                if new_diff != []:
                    different.append(case)
                    print(" "*8+FAIL+"FAILED"+ENDC)
                    print(" "*8+"Diff in steering case")
                    continue

                # Clean up of files
                remove(ori_case)
                remove(eficas_case)
                remove(run_log)
                remove(run_eficas_log)
                remove(tmp_diff_file)
                if path.exists("out_user_fortran"):
                    remove("out_user_fortran")
                shutil.rmtree(ori_case+"_dir")
                shutil.rmtree(eficas_case+"_dir")

                # Passed the test case
                print(" "*8+OKGREEN+"PASSED"+ENDC)

    if crashed != []:
        print("The following test in", val_folder,
              " crashed in eficas:", crashed)
    if different != []:
        print("The following test in", val_folder,
              " have a difference with normal run:", different)

def main():
    """
    main function
    """
    parser = ArgumentParser()
    parser.add_argument(\
       "-m", "--module",
       dest="module",
       help="specify on which module to run the eficas validation ")
    parser.add_argument(\
       "-f", "--folder",
       dest="folder",
       default='',
       help="specify in which example folder to run the eficas \
             validation (Default the module one)")
    parser.add_argument(\
       "-l", "--light",
       action='store_false',
       dest="light",
       help="Have eficas export steering file in light format")
    parser.add_argument(\
       "-r", "--root_dir", dest="root_dir", default='',
       help="specify the root, default is taken from config file")
    parser.add_argument(\
       "--lng", dest="lng", default='ang',
       help="specify the language in which the test cases are")

    args = parser.parse_args()
    if args.root_dir == '':
        root_dir = environ['HOMETEL']
    else:
        root_dir = args.root_dir
    if args.folder == '':
        args.folder = args.module

    validate_catalog(args.module,
                     args.folder,
                     root_dir,
                     light=args.light,
                     lng=args.lng)

if __name__ == "__main__":
    main()
