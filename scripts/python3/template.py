#!/usr/bin/env python3
r"""
    @author TELEMAC-MASCARET Consortium
    @brief Function to run a steering file using the api
"""
from argparse import ArgumentParser
import string
from os import path, environ

from mpi4py import MPI

from execution.telemac_cas import TelemacCas
from execution.mascaret_cas import MascaretCas
from telapy.api.t2d import Telemac2d
from telapy.api.t3d import Telemac3d
from telapy.api.art import Artemis
from telapy.api.wac import Tomawac
from telapy.api.masc import Mascaret, build_api_input


SCRIPT_TEMPLATE = """\
#!/usr/bin/env python3
# Class {module} import
from mpi4py import MPI
from telapy.api.{short} import {module}

def main():
    # Creation of the instance {module}
    steering_file = "{steering_file}"
    user_fortran = "{fortran_file}"
    comm = MPI.COMM_WORLD
    # Initialising {module} instance
    study = {module}(steering_file, user_fortran=user_fortran, comm=comm)
    # Reading steering file
    study.set_case()
    # Doing initialisation
    study.init_state_default()
    # Time step loop
    study.run_all_time_steps()
    # Ending computation
    study.finalize()
    # Deleting {module} instance
    del study

if __name__ == '__main__':
    main()
"""

SCRIPT_TEMPLATE_MASCARET = """\
#!/usr/bin/env python3
from telapy.api.masc import Mascaret, build_api_input
from execution.mascaret_cas import MascaretCas

def main():
    steering_file = "{steering_file}"
    cas = MascaretCas(steering_file)
    files_name, files_type = build_api_input(steering_file)

    # Creation of the instance
    masc = Mascaret()
    masc.create_mascaret(iprint=1)

    #  Mascaret files & import
    masc.import_model(files_name, files_type)

    time_step = masc.get("Model.DT")
    t_0 = masc.get("Model.InitTime")
    t_end = masc.get("Model.MaxCompTime")
    ntime_step = masc.get("Model.MaxNbTimeStep")
    t_end = t_0+time_step*ntime_step

    # Initialization
    val = cas.get("parametresConditionsInitiales/ligneEau/fichLigEau")
    if val is not None:
        masc.init_hydro_from_file(val)
    else:
        npoin = masc.get_var_size('Model.X')[0]

        masc.init_hydro([0.]*npoin, [0.]*npoin)

    # Steady state computation with one step
    masc.compute(t_0, t_end, time_step)

    # Delete Mascaret
    masc.delete_mascaret()

    del masc

if __name__ == '__main__':
    main()
"""

SHORT = {'telemac2d':'t2d',
         'telemac3d':'t3d',
         'tomawac':'wac',
         'sisyphe':'sis',
         'artemis':'art'}

def get_fortran_file(steering_file, module):
    """
    Get the fortran file from a cas (looks into coupled steering files as well)

    @param steering_file Name of the steering file
    @param module Name of the module
    """
    dico = path.join(environ['HOMETEL'], 'sources', module, module+'.dico')
    cas = TelemacCas(steering_file, dico)
    fortran_file = cas.get('FORTRAN FILE', '')

    if fortran_file == '':
        # Only searching in coupled files for telemac2d and telemac3d
        fortran_file = None
        if module in ["telemac2d", "telemac3d"]:
            cpl_with = cas.get('COUPLING WITH', '')
            if cpl_with == '':
                return None
            cpl_mods = cpl_with.lower().split(';')
            for cpl_mod in cpl_mods:
                cpl_dico = path.join(environ['HOMETEL'],
                                     'sources',
                                     cpl_mod,
                                     cpl_mod+'.dico')
                cpl_steering_file = cas.get(cpl_mod.upper()+' STEERING FILE')
                # Some coupled module do not have a dictionary (nestor, waqtel)
                if path.exists(cpl_dico):
                    cpl_cas = TelemacCas(cpl_steering_file, cpl_dico)
                    fortran_file = cpl_cas.get('FORTRAN FILE', '')
                    del cpl_cas
                    if fortran_file != '':
                        return fortran_file
            return None


    return fortran_file


def run(module, steering_file, stdout, log):
    """
    Running a full study

    @param module (str) Name of the module
    @param steering_file (str) Name of the steering file
    @param stdout (int) Output for TelApy (6 normal listing -1 into file)
    @param log (str) Logging level for TelApy (INFO, DEBUG)
    """
    comm = MPI.COMM_WORLD
    # mascaret does not have a user Fortran
    if module != 'mascaret':
        fortran = get_fortran_file(steering_file, module)

    if module == "mascaret":

        cas = MascaretCas(steering_file)
        files_name, files_type = build_api_input(steering_file)

        masc = Mascaret()
        masc.create_mascaret(iprint=1)

        #  Mascaret files & import
        masc.import_model(files_name, files_type)

        time_step = masc.get("Model.DT")
        t_0 = masc.get("Model.InitTime")
        t_end = masc.get("Model.MaxCompTime")
        ntime_step = masc.get("Model.MaxNbTimeStep")
        # TODO: Is not the right way to estimate end of computation
        t_end = t_0+time_step*ntime_step

        # Initialization
        val = cas.get("parametresConditionsInitiales/ligneEau/fichLigEau")
        if val is not None:
            masc.init_hydro_from_file(val)
        else:
            npoin = masc.get_var_size('Model.X')[0]

            masc.init_hydro([0.]*npoin, [0.]*npoin)

        # Steady state computation with one step
        masc.compute(t_0, t_end, time_step)

        # Delete Mascaret
        masc.delete_mascaret()

        del masc

        return

    if module == "telemac2d":
        study = Telemac2d(steering_file, user_fortran=fortran,
                          comm=comm, stdout=stdout, log_lvl=log)
    elif module == "telemac3d":
        study = Telemac3d(steering_file, user_fortran=fortran,
                          comm=comm, stdout=stdout, log_lvl=log)
    elif module == "artemis":
        study = Artemis(steering_file, user_fortran=fortran,
                        comm=comm, stdout=stdout, log_lvl=log)
    elif module == "tomawac":
        study = Tomawac(steering_file, user_fortran=fortran,
                        comm=comm, stdout=stdout, log_lvl=log)

    # Running telemac
    study.set_case()
    study.init_state_default()
    study.run_all_time_steps()
    comm.Barrier()
    study.finalize()
    # Instance delete
    del study

def dump_script(module, steering_file, script_file):
    """
    dump a api run into a file

    @param module (string) Name of the module
    @param steering_file (string) Name of the steering file
    @param fortran_file (string) Name of the fortran file
    @param script_file (string) Name of file in which we write the script
    """

    if module == 'mascaret':
        script = SCRIPT_TEMPLATE_MASCARET.format(\
                steering_file=steering_file)
    else:
        fortran_file = get_fortran_file(steering_file, module)
        script = SCRIPT_TEMPLATE.format(\
                steering_file=steering_file,
                fortran_file=None if fortran_file == '' else fortran_file,
                module=string.capwords(module),
                short=SHORT[module])

    with open(script_file, 'w') as fobj:
        fobj.write(script)

def main():
    """ main function """
    # Define a parser for the program options
    parser = ArgumentParser()
    parser.add_argument(\
             "module",
             choices=['mascaret', 'telemac2d', 'telemac3d', 'artemis',
                      'tomawac'],
             help="name of the steering file")
    parser.add_argument(\
             "steering_file",
             help="name of the steering file")
    parser.add_argument(\
             "--double-run",
             dest="double_run",
             action="store_true",
             help="Running main computation twice")
    parser.add_argument(\
             "-v", "--verbose",
             dest="verbose",
             action="store_true",
             help="Display Telemac listing")
    parser.add_argument(\
             "--log",
             dest="log",
             default='INFO',
             choices=['INFO', 'DEBUG'],
             help="TelApy log level")
    parser.add_argument(\
             "-o", "--output-script",
             dest="output_script",
             default="",
             help="Will generate a python script running the case")

    # reading the options
    args = parser.parse_args()

    if args.output_script != '':
        dump_script(args.module, args.steering_file,
                    args.output_script)
    else:
        stdout = 6 if args.verbose else 0
        run(args.module, args.steering_file, stdout, args.log)
        print("First run passed")
        if args.double_run:
            run(args.module, args.steering_file, stdout, args.log)
            print("Second run passed")

    print("My work is done")

if __name__ == "__main__":
    main()
