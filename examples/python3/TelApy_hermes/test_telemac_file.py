"""
Test for TelemacFile
"""
from os import path, remove
import shutil
import numpy as np
from data_manip.extraction.telemac_file import TelemacFile

def create_file_from_scratch(file_name, bnd_file=None, var=False):
    """
    Example of creation of mesh from stracth

    @param (str) bnd_file If not None creating boundary info
    @param (bool) var If true creating a var in the file
    """
    print("Creating from scratch {}".format(file_name))

    points = np.array(\
            [[0., 0.],
             [1., 0.],
             [2., 0.],
             [0.5, 1.],
             [1.5, 1.],
             [1., 2.]])
    ikle = [[0, 1, 3],
            [1, 4, 3],
            [1, 2, 4],
            [3, 4, 5],
            ]

    if path.exists(file_name):
        remove(file_name)
    if bnd_file is not None and path.exists(bnd_file):
        remove(bnd_file)

    res = TelemacFile(file_name, bnd_file=bnd_file, access='w')
    res.add_header('Zelda triforce', date=[1986, 2, 21, 0, 0, 0])
    res.add_mesh(points[:, 0], points[:, 1], ikle)
    if var:
        res.add_variable('dummy', '')

    if bnd_file is not None:
        if file_name.endswith("med"):
            ikle_bnd = [[0, 3], [3, 5], [5, 4], [4, 2], [2, 1], [1, 0]]
            lihbor = [2, 2, 2, 2, 4, 4]
            liubor = [2, 2, 2, 2, 5, 5]
            livbor = [2, 2, 2, 2, 5, 5]
        else:
            ikle_bnd = [0, 3, 5, 4, 2, 1]
            lihbor = [4, 2, 2, 2, 4, 4]
            liubor = [5, 2, 2, 2, 5, 5]
            livbor = [5, 2, 2, 2, 5, 5]

        res.add_bnd(ikle_bnd, lihbor=lihbor, liubor=liubor, livbor=livbor)

    res.write()
    res.close()

def read_write_add_var(file_name):
    """
    Adding variable in existing file
    """

    print("Adding new variable in {}".format(file_name))
    res = TelemacFile(file_name, access='rw')

    # Loading data from file
    res.read()

    # Adding new variable
    res.add_variable('stuff', 'M')

    bathy = np.ones((res.npoin3), dtype=np.float64)

    # Setting variable value for each timestep for new variable
    for record in range(res.ntimestep):
        res.add_data_value('stuff', record, bathy)

    # Writting class data
    res.write()

    res.close()

def read_write_add_time(file_name):
    """
    Adding new time step in existing file
    """

    print("Adding new time step in {}".format(file_name))
    res = TelemacFile(file_name, access='rw')

    res.read()

    # Adding new time step
    res.add_time_step(666.0)

    # Setting value for each variable for new time step
    bathy = np.ones((res.npoin3), dtype=np.float64)
    for var in res.varnames:
        res.add_data_value(var, -1, bathy)

    # Writting class data
    res.write()

    res.close()

def read_write(file_name1, file_name2):
    """
    Writting info from file1 into file2
    """

    print("Reading content and writting it in another file:\n{} -> {}"
          .format(file_name1, file_name2))
    res1 = TelemacFile(file_name1, access='r')
    if path.exists(file_name2):
        remove(file_name2)
    res2 = TelemacFile(file_name2, access='w')

    res2.read(res1)

    res2.write()

    res1.close()
    res2.close()

def read_write_bnd(file_name1, bnd_name1, file_name2, bnd_name2):
    """
    Same as read_write but with a boundary file
    """

    print("Reading content and writting it in another file:\n{} -> {}"
          .format(file_name1, file_name2))
    res1 = TelemacFile(file_name1, bnd_file=bnd_name1, access='r')
    if path.exists(file_name2):
        remove(file_name2)
    if path.exists(bnd_name2):
        remove(bnd_name2)
    res2 = TelemacFile(file_name2, bnd_file=bnd_name2, access='w')

    res2.read(res1)

    res2.write()

    res1.close()
    res2.close()


def main():
    """ Main function """
    # Serafin format
    shutil.copy('geo_triangular_shelf.slf', 'geo_triangular_shelf1.slf')
    shutil.copy('geo_triangular_shelf.cli', 'geo_triangular_shelf1.cli')

    shutil.copy('geo_gouttedo.slf', 'geo_gouttedo1.slf')
    shutil.copy('geo_gouttedo.cli', 'geo_gouttedo1.cli')

    read_write('geo_gouttedo1.slf', 'geo_gouttedo2.slf')

    read_write_bnd('geo_triangular_shelf1.slf', 'geo_triangular_shelf1.cli',
                   'geo_triangular_shelf3.slf', 'geo_triangular_shelf3.cli')
    read_write_bnd('geo_gouttedo1.slf', 'geo_gouttedo1.cli',
                   'geo_gouttedo3.slf', 'geo_gouttedo3.cli')

    read_write_add_var('geo_gouttedo1.slf')

    read_write_add_time('geo_gouttedo1.slf')

    create_file_from_scratch('triforce_no_var.slf')
    create_file_from_scratch('triforce.slf', var=True)
    create_file_from_scratch('triforce2.slf', 'triforce2.cli', var=True)

    # Med format
    try:
        import _hermes
        has_hermes = True
    except ImportError:
        has_hermes = False

    if has_hermes:
        shutil.copy('geo_gouttedo.med', 'geo_gouttedo1.med')
        shutil.copy('geo_gouttedo.clm', 'geo_gouttedo1.clm')

        read_write('geo_gouttedo1.med', 'geo_gouttedo2.med')

        read_write_bnd('geo_triangular_shelf1.slf', 'geo_triangular_shelf1.cli',
                       'geo_triangular_shelf3.med', 'geo_triangular_shelf3.clm')

        read_write_bnd('geo_gouttedo1.med', 'geo_gouttedo1.clm',
                       'geo_gouttedo3.med', 'geo_gouttedo3.clm')

        read_write_add_var('geo_gouttedo1.med')

        read_write_add_time('geo_gouttedo1.med')

        create_file_from_scratch('triforce_no_var.med')
        create_file_from_scratch('triforce.med', var=True)
        create_file_from_scratch('triforce2.med', bnd_file='triforce2.bnd', var=True)

if __name__ == "__main__":
    main()
