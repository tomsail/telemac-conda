"""
tel2tom and tom2tel
"""
from os import path, remove
import numpy as np
import matplotlib.tri as mtri
from data_manip.extraction.telemac_file import TelemacFile
from utils.polygon import points_in_poly, import_poly_from_i2sfile
from scipy.spatial import cKDTree

def interp_triangle_area(xtri, ytri, x, y):
    """
    performs barycentrix interpolation (similar to bilinear)
    xTri: x-coordinates of triangle (Nx3)
    yTri: y-coordinates of triangle (Nx3)
     x = x coordinates for interpolation (Nx1)
     y = y coordinates for interpolation (Nx1)
    """
    a_1 = 0.5*np.abs((x-xtri[:, 2])*(ytri[:, 1]-y) -
                     (x-xtri[:, 1]) * (ytri[:, 2]-y))
    a_2 = 0.5*np.abs((xtri[:, 0]-xtri[:, 2])*(y-ytri[:, 0]) -
                     (xtri[:, 0]-x) * (ytri[:, 2]-ytri[:, 0]))
    a_3 = 0.5*np.abs((xtri[:, 0]-x)*(ytri[:, 1]-ytri[:, 0]) -
                     (xtri[:, 0]-xtri[:, 1]) * (y-ytri[:, 0]))
    a_a = a_1 + a_2 + a_3

    return np.vstack((a_1/a_a, a_2/a_a, a_3/a_a)).T


def interp_triangle_prepare(connection, xtri, ytri, x, y,
                            maskzone, extrap=False):
    """
     This function determines all preprocessing for trainagle interpolation

     sctInterp = interpTrianglePrepare(connection,xTri,yTri,x,y)

    INPUT
       - xTri: x-coordinates of triangle (Mx1)
       - yTri: y-coordinates of triangle (Mx1)
       - connection: coordinate number for interpolation (Kx3)
       - x = x coordinates for interpolation (Nx1)
       - y = y coordinates for interpolation (Nx1)
       - extrap: logical to determine whether nearest neighbour interpolation
                 is done (optional)

    OUTPUT
     CoordIndex of the points on the triangle (Nx3) needed for interpolation
     interpCoef (coefficient) used to multiply
     mask: logical value of points
    """
    # determine in which triangle the points are

    my_tri = mtri.Triangulation(xtri, ytri, connection)
    tri_finder = my_tri.get_trifinder()

    intri = tri_finder(x, y)

    # Initialising
    coordx = np.empty((len(x), 3), dtype=np.float64)
    coordy = np.empty((len(x), 3), dtype=np.float64)
    coord_index = np.empty((len(x), 3), dtype=np.float64)

    coordx[:] = np.nan
    coordy[:] = np.nan
    coord_index[:] = np.nan

    # Excluding out of mesh points
    masktri = intri != -1
    nomask = np.logical_not(masktri)

    for i in range(3):
        coord_index[masktri, i] = connection[intri[masktri], i]
        coordx[masktri, i] = xtri[connection[intri[masktri], i]]
        coordy[masktri, i] = ytri[connection[intri[masktri], i]]

    interp_coef = interp_triangle_area(coordx, coordy, x, y)

    if extrap:
        tree = cKDTree(np.column_stack((xtri, ytri)))
        extrap_x = np.array([[xx, yy] for xx, yy in zip(x, y)])
        _, index = tree.query(extrap_x[nomask, :])
        coord_index[nomask, :] = [0, -1, -1]
        coord_index[nomask, 0] = index
        interp_coef[nomask, :] = [1., 0., 0.]

    coord_index[maskzone, :] = [-1, -1, -1]
    interp_coef[maskzone, :] = [1., 0., 0.]
        
    return coord_index, interp_coef


def connect_tel2tom(tel_file, tom_file,
                    tel_bnd=None, tom_bnd=None,
                    contour_tom=None, contour_tel=None):
    """
    connectTelTom(telFile,tomFile,contourTom,contourTel)

    INPUT:
    - telFile, tomFile: selafin files with the meshes of telemac
    and tomawac
    - contourTel, contourTom: i2s files with contours that should
    not be taken into account
    """
    print(tel_bnd, tom_bnd)
    t2d = TelemacFile(tel_file, bnd_file=tel_bnd)
    tom = TelemacFile(tom_file, bnd_file=tom_bnd)

    x_t2d = t2d.meshx
    y_t2d = t2d.meshy
    x_tom = tom.meshx
    y_tom = tom.meshy
    xy_t2d = np.vstack((x_t2d, y_t2d)).T
    xy_tom = np.vstack((x_tom, y_tom)).T

    # tom2tel
    print("  ~> Building tom2tel")
    if contour_tom is not None:
        # Select points within contour
        poly_tom = import_poly_from_i2sfile(contour_tom)
        maskzone = np.logical_not(points_in_poly(xy_tom, poly_tom))
    else:
        maskzone = np.zeros(len(x_tom), dtype=np.bool)
        maskzone[:] = False
    root, ext = path.splitext(tom_file)
    bnd_ext = ".cli" if ext == ".slf" else ".bnd"

    res_file = root+'-tom2tel'+ext
    res_bnd_file = root+'-tom2tel'+bnd_ext

    if path.exists(res_file):
        remove(res_file)
    if path.exists(res_bnd_file):
        remove(res_bnd_file)

    if tel_bnd is None or tom_bnd is None:
        res_bnd_file = None

    print(res_file, res_bnd_file)
    res = TelemacFile(res_file, bnd_file=res_bnd_file, access='w')
    connect_send_recv('TEL2TOM', maskzone, t2d, tom, res)
    res.close()
    print("  - Created {} and {}".format(res_file, res_bnd_file))

    # tel2tom
    print("  ~> Building tel2tom")
    if contour_tel is not None:
        # Select points within contour
        poly_tel = import_poly_from_i2sfile(contour_tel)
        maskzone = points_in_poly(xy_t2d, poly_tel)
    else:
        maskzone = np.zeros(len(x_t2d), dtype=np.bool)
        maskzone[:] = False
    root, ext = path.splitext(tel_file)
    bnd_ext = ".cli" if ext == ".slf" else ".bnd"

    res_file = root+'-tel2tom'+ext
    res_bnd_file = root+'-tel2tom'+bnd_ext

    if path.exists(res_file):
        remove(res_file)
    if path.exists(res_bnd_file):
        remove(res_bnd_file)

    if tel_bnd is None or tom_bnd is None:
        res_bnd_file = None

    res = TelemacFile(res_file, bnd_file=res_bnd_file, access='w')
    connect_send_recv('TOM2TEL', maskzone, tom, t2d, res)
    res.close()
    print("  - Created {} and {}".format(res_file, res_bnd_file))

    t2d.close()
    tom.close()


def connect_send_recv(var_name, mask, send, recv, out):
    """
    computed connectivity list between telemac and tomowac

    connectSendRecv(varName,mask,sctSend,sctRecv,outFile)

     INPUT:
     - varName: name of variables to create. can be either TEL2TOm
     or TOM2TEL
     - mask: list of points not to include
     - sctSend: - strcuture of mesh for sender using telheadr
     - sctRecv: - strcuture of mesh for receiver using telheadr
     - outFile: name of file to make

     IT COMPUTES THE CONNECTIVITY LIST BETWEEN
     TELEMAC2D AND TOMAWAC SELAFIN FILES

     --------------------------------------------------------------
     The new TOMAWAC selafin file TOM_interp.slf created, includes:
     --------------------------------------------------------------
     ->TEL2TOM:
     CLOSEST INDEX OF THE TELEMAC2D MESH ONTO
     THE TOMAWAC GRID NODES
     ->TEL2TOM01,TEL2TOM02,TEL2TOM03:
     NEAREST NEIGHBOR INDEX (THE OTHER TWO VARIABLES ARE ZERO)
     ->TEL2TOMWTS01,TEL2TOMWTS02,TEL2TOMWTS03:
     LINEAR INTERPOLATION COEFFICIENTS
    """
    send.set_kd_tree()
    coord_index, interp_coef = interp_triangle_prepare(\
            send.ikle2, send.meshx, send.meshy,\
            recv.meshx, recv.meshy, mask,\
            extrap=True)
    out.read(recv)

    for i in range(1, 4):
        varname = "{}{:02d}".format(var_name, i)
        out.add_variable(varname, '')
        out.add_data_value(varname, 0, coord_index[:, i-1] + 1)

        varname = "{}WTS{:02d}".format(var_name, i)
        out.add_variable(varname, '')
        out.add_data_value(varname, 0, interp_coef[:, i-1])

    out.write()
