#!/usr/bin/env python3
"""
Example of a read-write using hermes api with different formats
"""

import sys
from os import path, remove
try:
    import _hermes
except ImportError:
    print("Hermes api not available doing nothing")
    sys.exit(0)
from telapy.api.hermes import HermesFile
from os import path

def read_write(mesh_file, fformat, outformat, compare=True):
    """
    Open file mesh_file and rewrite it in outformat
    And check that they contain the same data

    @param mesh_file Name of the input file
    @param fformat Format of mesh_file
    @param outformat Output format
    """
    print("Opening MED file {}".format(mesh_file))
    myfile = HermesFile(mesh_file, fformat, 'r')

    root, ext = path.splitext(mesh_file)
    mesh_file2 = 'hermes-'+root+'-from-'+fformat.lower()+ext
    if path.exists(mesh_file2):
        remove(mesh_file2)
    print("Opening Serafin file {}".format(mesh_file2))
    myfile2 = HermesFile(mesh_file2, outformat, 'w')

    print("Importing data")
    myfile2.import_from(myfile)

    str_file = str(myfile)

    myfile.close()
    myfile2.close()

    myfile2 = HermesFile(mesh_file2, outformat, 'r')
    str_file2 = str(myfile2)

    # Both file should contain the same data
    if compare:
        assert str_file == str_file2

    myfile2.close()


if __name__ == "__main__":
    print("Testing MED -> SRF in 2D")
    read_write('geo_gouttedo.med', 'MED', 'SERAFIN')
    print("Testing SRF -> MED in 2D")
    read_write('r2d_gouttedo.slf', 'SERAFIN', 'MED')
    print("Testing MED -> SRF in 3D")
    read_write('r3d_gouttedo.med', 'MED', 'SERAFIN', False)
    print("Testing SRF -> MED in 3D")
    read_write('r3d_gouttedo.slf', 'SERAFIN', 'MED')
