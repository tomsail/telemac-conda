#!/usr/bin/env python3
"""
Example of a read and write of a file
"""
import sys
from os import path, remove
try:
    import _hermes
except ImportError:
    print("Hermes api not available doing nothing")
    sys.exit(0)
from telapy.api.hermes import HermesFile
import filecmp

def read_write(fformat, mesh_file, bnd_file=None, compare=True):
    """
    Read a couple mesh, boundary and rewrite it
    And check that the dump are identical
    And the binaries as well

    @param fformat Format for the files
    @param mesh_file Name of the mesh file
    @param bnd_file Name of the boundary file
    """

    print("Opening Serafin file {}".format(mesh_file))
    myfile = HermesFile(mesh_file, fformat, 'r', bnd_file)

    mesh_file2 = 'hermes-'+mesh_file
    if bnd_file is not None:
        bnd_file2 = 'hermes-'+bnd_file
    else:
        bnd_file2 = None

    if path.exists(mesh_file2):
        remove(mesh_file2)
    if path.exists(bnd_file2):
        remove(bnd_file2)
    myfile2 = HermesFile(mesh_file2, fformat, 'w', bnd_file2)

    myfile2.import_from(myfile)
    print('import passed')

    myfile.close()
    myfile2.close()

    # Binary files should be identical
    if compare:
        assert filecmp.cmp(mesh_file, mesh_file2)

if __name__ == "__main__":
    print("Testing r2d_gouttedo.slf")
    read_write('SERAFIN', 'r2d_gouttedo.slf', 'geo_gouttedo.cli')
    print("Testing r3d_gouttedo.slf")
    read_write('SERAFIN', 'r3d_gouttedo.slf', 'geo_gouttedo.cli')
    print("Testing geo_gouttedo.med")
    read_write('MED', 'geo_gouttedo.med', 'geo_gouttedo.clm', False)
