from telapy.tools.study_t2d_driven import Telemac2DStudy
import testcases_t2d_driven as tc
import sys
import os
"""
Validation script for running two consecutive simulations of Telemac2d code
from data file or python specifications.
"""

# Parallelism based on script argument
try:
    nproc1 = int(sys.argv[1])  # number of processors
    nproc2 = nproc1
except:
    nproc1 = 1
    from configuration.cfg import Config
    CFGS = Config()
    python_dir = os.path.dirname(os.path.realpath(__file__))
    root_dir = os.environ.get('HOMETEL')
    if root_dir == '':
        root_dir = os.path.dirname(os.path.dirname(python_dir))
    # Defining user configuration name and file
    cfg_name = os.environ.get('USETELCFG')
    cfg_file = os.environ.get('SYSTELCFG')
    if cfg_file == '':
        cfg_file = os.path.join(root_dir, 'configs', 'systel.cfg')

    CFGS.parse_cfg_file(cfg_file, cfg_name, root_dir, python_dir)
    if 'mpi' in CFGS.configs[CFGS.cfgname]['options'].split():
        try:
            from cpu_cores import CPUCoresCounter
            instance = CPUCoresCounter.factory()
            nproc2 = instance.get_physical_cores_count()
        except:
            nproc2 = os.cpu_count()
    else:
        nproc2 = 1

# Telemac2DStudy documentation
# help(Telemac2DStudy)

# OPTION 1 : use data in the configuration file.

# Create an instance of the test case Garonne.
steering_file = 'input_data/t2d_garonne_zonal.cas'           # t2d test case (zonally defined)
user_fortran = 'input_data/Princi.f'                         # t2d fortran file
ks_area = 'input_data/ks_area.txt'                           # manning regions
cas_path = os.path.dirname(tc.__file__)
cas_garonne = tc.Garonne(steering_file, user_fortran, ks_area, cas_path)
print("Loading config: ", cas_garonne.name, cas_garonne.config)

# Create an instance of Telemac2DStudy from this Garonne.
# Coordinates of the points of interest (Marmande)
points = [(425697., 246914.), (425697., 246916.)]
results_file = 'sortie_gar.slf'  # results file
study = Telemac2DStudy(points, cas_garonne, results_file, "./study_test_run", 666, nproc1)
print("Running case: ", study.test_case.name)

# Run the instance of Telemac2d.
study(out_dir=os.path.join("results", "1"))

# Post-Treatment
print("[RESULTS][POINTS OF INTEREST]")
print("x = x-coordinate # y = y-coordinate # z = Water elevation    # zn = at closest node")
for i, _ in enumerate(study.z_points):
    print("x = {}     # y = {}     # z = {} # zn ={}".format(points[i][0],
                                                             points[i][1],
                                                             study.z_points[i],
                                                             study.z_node[i]))
# Plot the water elevation without mesh
study.plot_final_state(out_dir=os.path.join("results", "1"),
                       filename=os.path.join("results", "1", "water_level.pdf"))

# Clean up
del study


# OPTION 2 : use python defined input data.

# Create an instance of the test case Garonne.
steering_file = 'input_data/t2d_garonne_zonal.cas'           # t2d test case (zonally defined)
user_fortran = 'input_data/Princi.f'                         # t2d fortran file
ks_area = 'input_data/ks_area.txt'                           # manning regions
cas_path = os.path.dirname(tc.__file__)
cas_garonne = tc.Garonne(steering_file, user_fortran, ks_area, cas_path)
print("Loading config: ", cas_garonne.name, cas_garonne.config)

# Create an instance of Telemac2DStudy from this Garonne.
# Coordinates of the points of interest (Marmande)
points = [(425697., 246914.), (425697., 246916.)]
results_file = 'sortie_gar.slf'                    # serafin file
study = Telemac2DStudy(points, cas_garonne, results_file,
                       "./study_test_run", 666, nproc2, mesh_state=True)
print("Running case: ", study.test_case.name)

# Run the instance of Telemac2d.
Ks_major = 16
Ks_minor_1 = 45
Ks_minor_2 = 38
Ks_minor_3 = 40
Q = 5750
x = [Ks_major, Ks_minor_1, Ks_minor_2, Ks_minor_3, Q]
study(x, out_dir=os.path.join("results", "2"))

# Post-Treatment

print("[RESULTS][POINTS OF INTEREST]")
print("x = x-coordinate # y = y-coordinate # z = Water elevation    # zn = at closest node")
for i, _ in enumerate(study.z_points):
    print("x = {}     # y = {}     # z = {} # zn ={}".format(points[i][0],
                                                             points[i][1],
                                                             study.z_points[i],
                                                             study.z_node[i]))

idx = 29515
print("Results from mesh_water_depth at node {}".format(idx))
print("x = {} # y = {} # z = {}".format(study.mesh_water_depth['x-coord'][idx],
                                        study.mesh_water_depth['y-coord'][idx],
                                        study.mesh_water_depth['waterDepth'][idx]))

# Plot the water elevation with mesh
study.plot_final_state(out_dir=os.path.join("results", "2"),
                       filename=os.path.join("results", "2", "water_level.pdf"),
                       plot_mesh=True)

# Film the water elevation (not ported to python3)
#study.film_state(out_dir=os.path.join("results", "2"),
#                 filename=os.path.join("results", "2","water_level.mp4"),
#                 plot_mesh=True)
del study
