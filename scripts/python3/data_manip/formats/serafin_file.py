"""
Contains the class Selafin
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
from os import path
import logging
from struct import unpack, pack, error

import numpy as np

from data_manip.extraction.parser_selafin import get_endian_from_char, \
                                   get_float_type_from_float

from utils.exceptions import TelemacException


class SerafinFile():
    """
    Class Selafin

    @brief
        Read and create Selafin files with python

    @details
        The idea is to be able to set float_type and float_type_size from
        outside when we start to write a selafinfiles.
        This will make it possible to do some kind of converters
    """
    logger = logging.getLogger(__name__)

    def __init__(self, file_name, fformat, boundary_file=None,
                 access='r', log_lvl='INFO'):
        """
        Intialisation of the class

        @param file_name (string) Name of the file
        """
        if log_lvl == 'INFO':
            i_log = logging.INFO
        elif log_lvl == 'DEBUG':
            i_log = logging.DEBUG
        else:
            i_log = logging.CRITICAL
        logging.basicConfig(level=i_log)
        self.logger.setLevel(i_log)

        self._fformat = fformat.encode('utf-8') + b' '*(8 - len(fformat))
        self.file_name = file_name
        self.boundary_file = boundary_file

        if 'r' in access:
            if not path.exists(self.file_name):
                raise TelemacException(
                    "Could not find {}".format(self.file_name))
        elif access == 'w':
            if path.exists(self.file_name):
                raise TelemacException(
                    "File already exist remove it first:{}"
                    .format(self.file_name))
        elif 'r' not in access and 'w' not in access:
            raise TelemacException(
                "Error in access string '%s' \
                should contain r and/or w " % access)

        if self.boundary_file is not None:
            if access == 'r':
                if not path.exists(self.boundary_file):
                    raise TelemacException(
                        "Could not find {}".format(self.boundary_file))
            elif access == 'w':
                if path.exists(self.boundary_file):
                    raise TelemacException(
                        "File already exist remove it first:{}"
                        .format(self.boundary_file))

        self.__access = access
        # "<" means little-endian, ">" means big-endian
        self.__endian = ">"
        if access == 'w':
            if fformat == "SERAFIND":
                self.__float = ('d', 8)
            else:
                self.__float = ('f', 4)
        else:
            self.__float = None

        self.__file = None
        self.__file_bnd = None
        self.__title = None
        self.__nvar = None
        self.__varnames = None
        self.__varunits = None
        self.__ndim = None
        self.__ndp3 = None
        self.__nptfr = None
        self.__nptir = None
        self.__nelem3 = None
        self.__npoin3 = None
        self.__ikle3 = None
        self.__ipob3 = None
        self.__knolg = None
        self.__meshx = None
        self.__meshy = None
        self.__nplan = None
        self.__datetime = None
        self.__x_orig = None
        self.__y_orig = None

        self.__values = None
        self.__times = None
        self.__ntimestep = None

        self.__lihbor = None
        self.__liubor = None
        self.__livbor = None
        self.__hbor = None
        self.__ubor = None
        self.__vbor = None
        self.__chbord = None
        self.__litbor = None
        self.__tbor = None
        self.__atbor = None
        self.__btbor = None
        self.__color = None
        self.__nbor = None

        self._typ_elem = None
        self._typ_bnd_elem = None

        if 'r' in self.__access:
            self.__read()
            if self.boundary_file is not None:
                self.__read_bnd()


    def __read(self):
        """
        read data from file
        """
        if self.__access == 'rw':
            access = 'rb'
        else:
            access = self.__access + 'b'
        self.__file = open(self.file_name, access)
        self.__endian = get_endian_from_char(self.__file, 80)
        # Reading headers
        f = self.__file
        endian = self.__endian
        # ~~ Read title ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        _, tmp, _ = unpack(endian+'i80si', f.read(4+80+4))
        self.__title = tmp.decode('utf-8')
        # ~~ Read nbv(1) and nbv(2) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        _, nbv1, nbv2, _ = unpack(endian+'iiii', f.read(4+8+4))
        self.__nvar = nbv1 + nbv2
        # ~~ Read variable names and units ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        self.__varnames = []
        self.__varunits = []
        for _ in range(self.__nvar):
            _, name, unit, _ = unpack(endian+'i16s16si', f.read(4+16+16+4))
            self.__varnames.append(name.decode('utf8').strip())
            self.__varunits.append(unit.decode('utf8').strip())
        # ~~ Read iparam array ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        dummy = unpack(endian+'12i', f.read(4+40+4))
        iparam = np.asarray(dummy[1:11])
        self.__x_orig = iparam[2]
        self.__y_orig = iparam[3]
        self.__nplan = max(iparam[6], 1)
        self.__nptfr = iparam[7]
        self.__nptir = iparam[8]
        # ~~ Read DATE/TIME array ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        self.__datetime = [1900, 1, 1, 0, 0, 0]
        if iparam[9] == 1:
            dummy = unpack(endian+'8i', f.read(4+24+4))
            self.__datetime = list(dummy[1:7])
        # ~~ Read nelem3, npoin3, ndp3, nplan ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        _, self.__nelem3, self.__npoin3, self.__ndp3, _, _ = \
            unpack(endian+'6i', f.read(4+16+4))
        # ~~ Read the ikle array ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        f.seek(4, 1)
        self.__ikle3 = \
                np.array(unpack(endian+str(self.__nelem3*self.__ndp3)+'I',
                                f.read(4*self.__nelem3*self.__ndp3))) - 1
        f.seek(4, 1)
        self.__ikle3 = self.__ikle3.reshape((self.__nelem3, self.__ndp3))
        self.__ndim = 3 if self.__nplan > 1 else 2
        # ~~ Read the ipobo array ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        f.seek(4, 1)
        self.__ipob3 = np.asarray(unpack(endian+str(self.__npoin3)+'i',
                                         f.read(4*self.__npoin3))) -1
        f.seek(4, 1)
        self.__nbor = self.__ipob3[np.where(self.__ipob3 != -1)]
        # ~~> checks float encoding
        self.__float = get_float_type_from_float(
            self.__file, self.__endian, self.__npoin3)
        # ~~ Read the x-coordinates of the nodes ~~~~~~~~~~~~~~~~~~
        ftype, fsize = self.__float
        # Correction of format from size of float
        self._fformat = b'SERAFIN ' if ftype == 'f' else b'SERAFIND'
        f.seek(4, 1)
        self.__meshx = np.asarray(
            unpack(endian+str(self.__npoin3)+ftype,
                   f.read(fsize*self.__npoin3)))
        f.seek(4, 1)
        # ~~ Read the y-coordinates of the nodes ~~~~~~~~~~~~~~~~~~
        f.seek(4, 1)
        self.__meshy = np.asarray(
            unpack(endian+str(self.__npoin3)+ftype,
                   f.read(fsize*self.__npoin3)))
        f.seek(4, 1)
        # ~~> time series
        ats = []
        att = []
        while True:
            try:
                att.append(f.tell())
                # ~~ Read AT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                f.seek(4, 1)
                ats.append(unpack(endian+ftype, f.read(fsize))[0])
                f.seek(4, 1)
                # ~~ Skip Values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                f.seek(self.__nvar*(4+fsize*self.__npoin3+4), 1)
            except error:
                att.pop(len(att)-1)   # since the last record failed the try
                break
        self.__time_pos = att
        self.__times = np.asarray(ats)
        self.__ntimestep = len(self.__times)

        self.__values = np.zeros((self.__ntimestep, self.__nvar, self.__npoin3),
                                 dtype=np.float64)
        if fsize == 4:
            dtype = np.float32
        else:
            dtype = np.float64
        tmp = np.zeros((self.__npoin3), dtype=dtype)

        for itime, _ in enumerate(self.__times):
            f.seek(self.__time_pos[itime])
            # Skipping time
            f.seek(4+fsize+4, 1)
            for ivar, _ in enumerate(self.__varnames):
                f.seek(4, 1)
                tmp = unpack(endian+str(self.__npoin3)+ftype,
                             f.read(fsize*self.__npoin3))
                self.__values[itime, ivar, :] = tmp
                f.seek(4, 1)

        if self.__ndim == 3:
            self.__meshz = self.__values[0, 0, :]
        else:
            self.__meshz = None

    def __read_bnd(self):
        """
        Read boundary file
        """
        self.__file_bnd = open(self.boundary_file, self.__access)
        f = self.__file_bnd
        lines = f.readlines()
        self.__nptfr = len(lines)
        self.__lihbor = np.zeros((self.__nptfr), dtype=np.int)
        self.__liubor = np.zeros((self.__nptfr), dtype=np.int)
        self.__livbor = np.zeros((self.__nptfr), dtype=np.int)
        self.__hbor = np.zeros((self.__nptfr), dtype=np.float64)
        self.__ubor = np.zeros((self.__nptfr), dtype=np.float64)
        self.__vbor = np.zeros((self.__nptfr), dtype=np.float64)
        self.__chbord = np.zeros((self.__nptfr), dtype=np.float64)
        self.__litbor = np.zeros((self.__nptfr), dtype=np.int)
        self.__tbor = np.zeros((self.__nptfr), dtype=np.float64)
        self.__atbor = np.zeros((self.__nptfr), dtype=np.float64)
        self.__btbor = np.zeros((self.__nptfr), dtype=np.float64)
        self.__nbor = np.zeros((self.__nptfr), dtype=np.int)
        self.__color = np.zeros((self.__nptfr), dtype=np.int)

        for i, line in enumerate(lines):
            if line == '\n':
                continue
            values = line.strip('\n').split()
            self.__lihbor[i] = int(values[0])
            self.__liubor[i] = int(values[1])
            self.__livbor[i] = int(values[2])
            self.__hbor[i] = float(values[3])
            self.__ubor[i] = float(values[4])
            self.__vbor[i] = float(values[5])
            self.__chbord[i] = float(values[6])
            self.__litbor[i] = int(values[7])
            self.__tbor[i] = float(values[8])
            self.__atbor[i] = float(values[9])
            self.__btbor[i] = float(values[10])
            self.__nbor[i] = int(values[11]) -1
            self.__color[i] = int(values[12])


    def __write(self):
        """
        write data in file
        """
        f = self.__file
        endian = self.__endian
        ftype, fsize = self.__float
        if self.__access == 'rw':
            f.close()
            self.__file = open(self.file_name, 'wb')
            f = self.__file
            f.seek(0)
        # ~~ Write title ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if len(self.__title) > 72:
            tmp_title = self.__title[:72].encode('utf-8') + self._fformat
        else:
            tmp_title = self.__title.encode('utf-8') \
                        + b' '*(72-len(self.__title)) + self._fformat
        f.write(pack(endian+'i80si', 80, tmp_title, 80))
        # ~~ Write nbv(1) and nbv(2) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        f.write(pack(endian+'iiii', 4+4, self.__nvar, 0, 4+4))
        # ~~ Write variable names and units ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        for name, unit in zip(self.__varnames, self.__varunits):
            tmp_name = name + ' '*(16-len(name))
            tmp_unit = unit + ' '*(16-len(unit))
            f.write(pack(endian+'i', 32))
            f.write(pack(endian+'16s', tmp_name.encode('utf8')))
            f.write(pack(endian+'16s', tmp_unit.encode('utf8')))
            f.write(pack(endian+'i', 32))
        # ~~ Write iparam array ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        f.write(pack(endian+'i', 4*10))
        iparam = [0]*10
        iparam[2] = self.__x_orig
        iparam[3] = self.__y_orig
        iparam[6] = self.__nplan
        iparam[7] = self.__nptfr
        iparam[8] = self.__nptir
        if self.__datetime is not None:
            iparam[9] = 1

        for val in iparam:
            f.write(pack(endian+'i', val))
        f.write(pack(endian+'i', 4*10))
        # ~~ Write DATE/TIME array ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if iparam[9] == 1:
            f.write(pack(endian+'i', 4*6))
            for val in self.__datetime:
                f.write(pack(endian+'i', val))
            f.write(pack(endian+'i', 4*6))
        # ~~ Write nelem3, npoin3, ndp3, nplan ~~~~~~~~~~~~~~~~~~~~~~~~~~~
        f.write(pack(endian+'6i', 4*4, self.__nelem3, self.__npoin3,
                     self.__ndp3, 1, 4*4))
        # ~~ Write the ikle array ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        f.write(pack(endian+'I', 4*self.__nelem3*self.__ndp3))
        f.write(pack(endian+str(self.__nelem3*self.__ndp3)+'I',
                     *(self.__ikle3.ravel()+1)))
        f.write(pack(endian+'I', 4*self.__nelem3*self.__ndp3))
        # ~~ Write the ipobo array ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        f.write(pack(endian+'i', 4*self.__npoin3))
        f.write(pack(endian+str(self.__npoin3)+'i', *(self.__ipob3+1)))
        f.write(pack(endian+'i', 4*self.__npoin3))
        # ~~ Write the x-coordinates of the nodes ~~~~~~~~~~~~~~~~~~~~~~~
        f.write(pack(endian+'i', fsize*self.__npoin3))
        f.write(pack(endian+str(self.__npoin3)+ftype, *(self.__meshx)))
        f.write(pack(endian+'i', fsize*self.__npoin3))
        # ~~ Write the y-coordinates of the nodes ~~~~~~~~~~~~~~~~~~~~~~~
        f.write(pack(endian+'i', fsize*self.__npoin3))
        f.write(pack(endian+str(self.__npoin3)+ftype, *(self.__meshy)))
        f.write(pack(endian+'i', fsize*self.__npoin3))

        for itime, time in enumerate(self.__times):
            f.write(pack(endian+'i'+ftype+'i', fsize, time, fsize))
            for ivar, var in enumerate(self.__varnames):
                f.write(pack(endian+'i', fsize*self.__npoin3))
                f.write(pack(endian+str(self.__npoin3)+ftype,
                             *(self.__values[itime, ivar, :])))
                f.write(pack(endian+'i', fsize*self.__npoin3))

    def __write_bnd(self):
        """ Write boundary file """
        f = self.__file_bnd
        for i in range(self.__nptfr):
            f.write("{} {} {} {} {} {} {} {} {} {} {} {} {}\n".format(
                self.__lihbor[i],
                self.__liubor[i],
                self.__livbor[i],
                self.__hbor[i],
                self.__ubor[i],
                self.__vbor[i],
                self.__chbord[i],
                self.__litbor[i],
                self.__tbor[i],
                self.__atbor[i],
                self.__btbor[i],
                self.__nbor[i]+1,
                self.__color[i]
                ))


    def close(self):
        """
        Closing file
        """
        # If write before closing writting the file
        if self.__access == 'w':
            self.__file = open(self.file_name, self.__access+'b')
            if self.boundary_file is not None:
                self.__file_bnd = open(self.boundary_file, self.__access)
        if 'w' in self.__access:
            self.__write()
            if self.boundary_file is not None:
                self.__write_bnd()
        self.__file.close()
        if self.boundary_file is not None:
            self.__file_bnd.close()

    def get_endianess(self):
        """
        Return the endiness of the file

        @returns the endianess
        """
        if self.__endian == "<":
            return "LITTLE_ENDIAN"
        if self.__endian == ">":
            return "BIG_ENDIAN"

        return "UNKNOWN_ENDIAN"

    def set_endianess(self, endianess):
        """
        Return the endiness of the file

        @returns the endianess
        """
        if endianess == "LITTLE_ENDIAN":
            self.__endian = "<"
        if endianess == "BIG_ENDIAN":
            self.__endian = ">"

    def get_mesh_title(self):
        """
        Retuns the title of the file

        @returns The title
        """
        return self.__title

    def get_mesh_date(self):
        """
        Retuns the date of the file

        @returns The date (6-integer-array)
        """
        return np.asarray(self.__datetime, dtype=np.int32)

    def get_mesh_nelem(self):
        """
        Retuns the number of element in the file

        @returns The number of elements
        """
        return self.__nelem3

    def get_mesh_npoin_per_element(self):
        """
        Retuns the number of points per element in the file

        @returns The number of points per element
        """
        return self.__ndp3

    def get_mesh_connectivity(self):
        """
        Retuns the connectivity for the given type

        @returns An 2d array of shape (nelem, ndp)
        """
        return self.__ikle3

    def get_mesh_npoin(self):
        """
        Retuns the number of points

        @returns The number of points
        """
        return self.__npoin3

    def get_mesh_nplan(self):
        """
        Retuns the number of planes

        @returns The number of planes
        """
        return self.__nplan

    def get_mesh_dimension(self):
        """
        Retuns the number of dimensions

        @returns The number of dimensions
        """
        return self.__ndim

    def get_mesh_orig(self):
        """
        Retuns the number of planes

        @returns The number of planes
        """
        return (self.__x_orig, self.__y_orig)

    def get_mesh_coord(self, jdim):
        """
        Retuns the coordinates of each points for a given dimension

        @param jdim Index of dimension [1-ndim]

        @returns A numpy array of size npoin
        """
        if jdim == 1:
            return self.__meshx
        if jdim == 2:
            return self.__meshy
        if jdim == 3 and self.__ndim == 3:
            return self.__meshz

        raise TelemacException("jdim must be within [1,{}]"\
                               .format(self.__ndim))

    def get_mesh_l2g_numbering(self):
        """
        Retuns the local to global numbering

        @returns The local to global numbering
        """
        return self.__ipob3

    def get_mesh_nptir(self):
        """
        Retuns the number of interface points

        @returns The number of interface points
        """
        return self.__nptir

    def get_bnd_ipobo(self):
        """
        Retuns the ipobo array

        @returns The ipobo array
        """
        return self.__ipob3

    def get_bnd_numbering(self):
        """
        Retuns the boundary to general numbering

        @returns The boundary to general numbering
        """
        return self.__nbor

    def get_bnd_connectivity(self):
        """
        Retuns the connectivity array for the boundary elements

        @returns The connectivity array for the boundary elements
        """
        ikle_bnd = np.zeros((self.__nptfr, 1), dtype=np.int)
        ikle_bnd[:, 0] = self.__nbor
        return ikle_bnd

    def get_bnd_npoin(self):
        """
        Retuns the number of boundary points

        @returns The number of boundary points
        """
        return self.__nptfr

    def get_bnd_nelem(self):
        """
        Retuns the number of boundary elements

        @returns The number of boundary elements
        """
        return self.__nptfr

    def get_bnd_value(self):
        """
        Retuns the information on the boundary values

        @returns lihbor, liubor, livbor, hbor, ubor, vbor, chbord,
                 litbor, tbor, atbor, btbor
        """
        return self.__lihbor, self.__liubor, self.__livbor, self.__hbor,\
             self.__ubor, self.__vbor, self.__chbord, self.__litbor,\
             self.__tbor, self.__atbor, self.__btbor, self.__color

    def get_data_nvar(self):
        """
        Returns the number of variables

        @returns The number of variables
        """
        return self.__nvar

    def get_data_var_list(self):
        """
        Retuns the list of the variables name and units

        @returns Two arrays of size nvar first for name second for units
        """
        return self.__varnames, self.__varunits

    def get_data_ntimestep(self):
        """
        Retuns the number of time steps

        @returns The number of time steps
        """
        return len(self.__times)

    def get_data_time(self, record):
        """
        Retuns the time of a given record

        @param record Number of the record (starts from 0)

        @returns The time
        """
        return self.__times[record]

    def get_data_value(self, var_name, record):
        """
        Retuns the value for each point for a given variable and a given record

        @param var_name Name of the variable
        @param record Number of the record (starts from 0 if a -1 is given will
                                   give the last time step)

        @returns A numpy array of size npoin
        """
        idx_var = self.__varnames.index(var_name.strip())
        return self.__values[record, idx_var, :]

    def set_header(self, title, nvar, var_name, var_unit):
        """
        Write header of the file

        @param title Title of the file
        @param nvar Number of variables
        @param var_name Name for each variable
        @param var_unit Unit for each variable
        """
        self.__title = title
        self.__nvar = nvar
        self.__varnames = var_name
        self.__varunits = var_unit

    def set_mesh(self, mesh_dim, typ_elem, ndp, nptfr, nptir, nelem, npoin,
                 ikles, ipobo, knolg, coordx, coordy, nplan, date,
                 time, x_orig, y_orig, coordz=None):
        """
        Write the mesh information into the file

        @param mesh_dim
        @param mesh_dim Dimension of the mesh
        @param typ_elem TYPE OF THE MESH ELEMENTS
        @param ndp Number of points per element
        @param nptfr Number of boundary point
        @param nptir Number of interface point
        @param nelem Number of element in the mesh
        @param npoin Number of points in the mesh
        @param ikles Connectivity array for the main element
        @param ipobo Is a boundary point ? array
        @param knolg Local to global numbering array
        @param coordx X coordinates of the mesh points
        @param coordy Y coordinates of the mesh points
        @param nplan Number of planes
        @param date Date of the creation of the mesh
        @param time Time of the creation of the mesh
        @param x_orig Origin of x coordinates
        @param y_orig Origin of y coordinates
        @param coordz Z coordinates of the mesh points
        """
        self.__ndim = mesh_dim
        self.__ndp3 = ndp
        self.__nptfr = nptfr
        self.__nptir = nptir
        self.__nelem3 = nelem
        self.__npoin3 = npoin
        self.__ikle3 = ikles
        self.__ipob3 = ipobo
        self.__knolg = knolg
        self.__meshx = coordx
        self.__meshy = coordy
        self.__nplan = nplan
        self.__datetime = []
        self.__datetime[0:3] = date
        self.__datetime[3:6] = time
        self.__x_orig = x_orig
        self.__y_orig = y_orig


    def add_data(self, var_name, var_unit, time, record, first_var, values):
        """
        Write information for a given variable and a given timestep

        @param var_name Name of the variable
        @param var_unit Unit of the variable
        @param time Time of the data
        @param record Time step of the data (starts from 0)
        @param first_var True if it is the first variable of the dataset
        @param values The value for each point of the mesh
        """
        if first_var:
            if self.__values is None:
                self.__values = np.zeros((1, self.__nvar, self.__npoin3),
                                         dtype=np.float64)
                self.__ntimestep = 0
            else:
                # New time step
                if record >= self.__ntimestep:
                    # Increasing size of values
                    data = np.zeros((1, self.__nvar, self.__npoin3),
                                    dtype=np.float64)
                    self.__values = np.append(self.__values, data, axis=0)

            if self.__times is not None:
                if record >= self.__ntimestep:
                    self.__times = np.append(self.__times, time)
                    self.__ntimestep += 1
            else:
                self.__times = np.zeros((1), dtype=np.float64)
                self.__times[0] = time
                self.__ntimestep = 1

        idx_var = self.__varnames.index(var_name.strip())

        # Increase size of values in case a variable was added
        _, nvar, _ = self.__values.shape
        if idx_var >= nvar:
            data = np.zeros((self.__ntimestep, 1, self.__npoin3),
                            dtype=np.float64)
            self.__values = np.append(self.__values, data, axis=1)

        self.__values[record, idx_var, :] = values


    def set_bnd(self, typ_bnd_elem, nelebd, ikle, lihbor, liubor,
                livbor, hbor, ubor, vbor, chbord, litbor, tbor, atbor,
                btbor, color):
        """
        Write boundary information

        @param typ_bnd_elem (int) Type of boundary element
        @param nelebd Number of boundary elements
        @param ikle Connectivity array for the boundary elements
        @param lihbor Type of boundary conditions on depth
        @param liubor Type of boundary conditions on u
        @param livbor Type of boundary conditions on v
        @param hbor Prescribed boundary condition on depth
        @param ubor Prescribed boundary condition on velocity u
        @param vbor Prescribed boundary condition on velocity v
        @param chbord Friction coefficient at boundary
        @param litbor Physical boundary conditions for tracers
        @param tbor Prescribed boundary condition on tracer
        @param atbor Thermal exchange coefficients
        @param btbor Thermal exchange coefficients
        @param color Boundary color of the boundary element
        """
        self.__nptfr = nelebd
        self.__lihbor = lihbor
        self.__liubor = liubor
        self.__livbor = livbor
        self.__hbor = hbor
        self.__ubor = ubor
        self.__vbor = vbor
        self.__chbord = chbord
        self.__litbor = litbor
        self.__tbor = tbor
        self.__atbor = atbor
        self.__btbor = btbor
        self.__color = color
        self.__nbor = ikle
