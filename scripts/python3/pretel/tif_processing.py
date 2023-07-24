import rasterio
from rasterio.transform import from_gcps
from rasterio.control import GroundControlPoint
import numpy as np
from data_manip.extraction.telemac_file import TelemacFile

def georeferencing_tif_file(tif_file,
        nb_pixels_x=1024, nb_pixels_y=1024,
        tl_xy=[0., 1.], bl_xy=[0., 0.],
        br_xy=[1., 0.], tr_xy=[1., 1.],
        crs='epsg:2154'):
    """
    Georeferencing unreference tif file

    @param tif_file (str) tif file
    @param nb_pixels_x (int) number of pixels in x direction
    @param nb_pixels_y (int) number of pixels in y direction
    @param tl_xy (float(2)) top left point coordonate in crs system
    @param bl_xy (float(2)) bottom left point coordonate in crs system
    @param br_xy (float(2)) bottom right point coordonate in crs system
    @param tr_xy (float(2)) top right point coordonate in crs system
    @param crs (str) EPSG code of the coordonate system (default: 'epsg:2154')
    """
    tl = GroundControlPoint(0, 0, tl_xy[0], tl_xy[1])
    bl = GroundControlPoint(nb_pixels_y, 0, bl_xy[0], bl_xy[1])
    br = GroundControlPoint(nb_pixels_y, nb_pixels_x, br_xy[0], br_xy[1])
    tr = GroundControlPoint(0, nb_pixels_x, tr_xy[0], tr_xy[1])
    gcps = [tl, bl, br, tr]

    transform = from_gcps(gcps)

    with rasterio.open(tif_file, 'r+') as ds:
        ds.crs = crs
        ds.transform = transform

def get_tif_data_on_triangular_mesh(res, tif, translation_xy=[0, 0]):
    """
    Write tif raster data on telemac result mesh (in slf format).
    Values on mesh nodes are taken form the closest raster point.

    @param res (TelemacFile) telemac result
    @param tif (str) tif file
    @param translation_xy (float(2)) translation vector
    """
    # project tif data on mesh node
    meshx2d = res.meshx[0:res.npoin2]
    meshy2d = res.meshy[0:res.npoin2]
    src = rasterio.open(tif)
    ndvi = src.read(1)
    tif_data = np.zeros(res.npoin2, dtype=np.float)

    for j, (x_coord, y_coord) in enumerate(zip(meshx2d, meshy2d)):
        # Checking that mesh node is within tif boundary if not setting to zero
        x_coord += translation_xy[0]
        y_coord += translation_xy[1]
        if ((x_coord>src.bounds.left) and (x_coord<src.bounds.right) and
            (y_coord>src.bounds.bottom) and (y_coord<src.bounds.top)):
            rr, cc = src.index(x_coord, y_coord)
            tif_data[j] = ndvi[rr, cc]
        else:
            tif_data[j] = 0.

    return tif_data

def tif_to_slf(geo_file, tif_file, tif_slf_file,
               var_name='TIR', var_unit='', translation_xy=[0, 0]):
    """
    Write tif raster data on telemac result mesh (in slf format).
    Values on mesh nodes are taken form the closest raster point.

    @param geo_file (str) telemac 2d geometry file
    @param tif_file (str) tif file
    @param tif_slf_file (str) output slf file created from tif data projected on result mesh
    @param var_name (str) tif variable name in output slf file
    @param var_unit (str) tif variable unit in output slf file
    @param translation_xy (float(2)) translation vector
    """
    # loading telemac file
    res = TelemacFile(geo_file)

    # loading tif file
    src = rasterio.open(tif_file)
    ndvi = src.read(1)

    # project tif data on mesh node
    tif_data = np.zeros(res.npoin2, dtype=np.float)

    for j, (x_coord, y_coord) in enumerate(zip(res.meshx, res.meshy)):
        x_coord += translation_xy[0]
        y_coord += translation_xy[1]
        # Checking that mesh node is within tif boundary if not setting to zero
        if ((x_coord>src.bounds.left) and (x_coord<src.bounds.right) and
            (y_coord>src.bounds.bottom) and (y_coord<src.bounds.top)):
            rr, cc = src.index(x_coord, y_coord)
            tif_data[j] = ndvi[rr, cc]
        else:
            tif_data[j] = 0.

    # defining output from res file
    output = TelemacFile(tif_slf_file, access='w')
    output.read(res)

    # Adding new variable
    output._varnames.append(var_name)
    output._varunits.append(var_unit)
    output._nvar += 1

    data = np.ones((output.ntimestep, 1, output.npoin3), dtype=np.float64)
    data[:,0,:] = tif_data
    output._values = np.append(output._values, data, axis=1)

    # writting output file
    output.write()
