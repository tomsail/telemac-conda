#!/usr/bin/env python3
"""
Example of a telapy telemac2d run (test case pildepon)
"""
import sys
from os import path, chdir, environ, getcwd, makedirs
if(not path.exists(path.join(environ.get('HOMETEL', ''),
                             'builds',
                             environ.get('USETELCFG', ''),
                             'wrap_api', 'lib', 'api.pyf'))):
    print("  -> telapy not available doing nothing")
    sys.exit(0)

from telapy.api.wac import Tomawac
from execution.telemac_cas import TelemacCas
from mpi4py import MPI
import numpy as np

def vnv_copy_files(module, cas_file, par):
    """
    Copying input files into validation folder if HOMETEL and USETEL other wise
    running in cas folder

    @param module (str) Name of the module
    @param cas_file (str) steering file
    @param dist (str) Folder in which to copy (creates it if it does not exist)
    """

    if 'USETELCFG' in environ and 'HOMETEL' in environ:
        vnv_working_dir = path.join(getcwd(),
                                    'vnv_api',
                                    path.basename(__file__[:-3])+par,
                                    environ['USETELCFG'])

        # Creating folders if they do not exists
        if MPI.COMM_WORLD.Get_rank() == 0:
            if not path.exists(vnv_working_dir):
                print("  ~> Creating: ", vnv_working_dir)
                makedirs(vnv_working_dir)
            chdir(path.dirname(cas_file))
            dico_file = path.join(environ['HOMETEL'], 'sources', module,
                                  module+'.dico')
            cas = TelemacCas(cas_file, dico_file)

            cas.copy_cas_files(vnv_working_dir, copy_cas_file=True)

            del cas
        MPI.COMM_WORLD.barrier()
    else:
        vnv_working_dir = path.dirname(cas_file)

    return vnv_working_dir


def main(recompile=True):
    """
    Main function of script

    @param recompile (Boolean) If True recompiling user fortran

    @retuns Value of ... at the end of the simulation
    """
    comm = MPI.COMM_WORLD

    root = environ.get('HOMETEL', path.join('..', '..', '..'))

    pwd = getcwd()

    cas_file = path.join(root, 'examples', 'tomawac', 'bottom_friction',
                         'tom_friction.cas')

    ncsize = comm.Get_size()

    par = '-par' if ncsize > 1 else "-seq"

    vnv_working_dir = vnv_copy_files('tomawac', cas_file, par)

    chdir(vnv_working_dir)

    # Creation of the instance Tomawac
    study = Tomawac('tom_friction.cas',
                    comm=comm, stdout=0, recompile=recompile)
    # Testing construction of variable list
    _ = study.variables

    study.set_case()
    res_name = study.get('MODEL.RESULTFILE')
    study.set('MODEL.RESULTFILE', 'test_api.slf')
    res_name2 = study.get('MODEL.RESULTFILE')
    assert res_name2 == 'test_api.slf'
    study.set('MODEL.RESULTFILE', res_name)
    # Initalization
    study.init_state_default()
    # Run all time steps
    ntimesteps = study.get("MODEL.NTIMESTEPS")
    for _ in range(10):
        study.run_one_time_step()

        tmp = study.get_array("MODEL.IKLE")
        study.set_array("MODEL.IKLE", tmp)
        tmp2 = study.get_array("MODEL.IKLE")
        diff = abs(tmp2 - tmp)
        assert np.amax(diff) == 0

        tmp = study.mpi_get_array("MODEL.X")
        study.mpi_set_array("MODEL.X", tmp)
        tmp2 = study.mpi_get_array("MODEL.X")
        diff = abs(tmp2 - tmp)
        assert np.amax(diff) < 1e-8

    val = study.get_array("MODEL.BOTTOM")
    # Running gretel
    comm.Barrier()
    # Ending the run
    study.finalize()
    # Instance delete
    del study
    chdir(pwd)

    return val

if __name__ == "__main__":
    VAL1 = main()
    print("First run passed")
    VAL2 = main(recompile=False)
    print("Second run passed")
    assert np.array_equal(VAL1, VAL2)
    print("My work is done")
