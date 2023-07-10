from telapy.tools.study_t2d_driven import Telemac2DStudy
import testcases_t2d_driven as tc
import sys
import os
"""
This script is used to run N simulations of Telemac2d code.
"""

# Parallelism based on script argument and run configuration
try:
    nproc = int(sys.argv[1])  # number of processors
except:
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
            nproc = instance.get_physical_cores_count()
        except:
            nproc = os.cpu_count()
    else:
        nproc = 1

# Telemac2DStudy documentation
# help(Telemac2DStudy)

# Create an instance of the test case Garonne.
steering_file = 'input_data/t2d_garonne_zonal.cas'  # t2d test case (zonally defined)
user_fortran = 'input_data/Princi.f'                # t2d fortran file
ks_area = 'input_data/ks_area.txt'                  # manning regions
cas_path = os.path.dirname(tc.__file__)
cas_garonne = tc.Garonne(steering_file, user_fortran, ks_area, cas_path)
# Print information concerning this test case.
print("Loading config: ", cas_garonne.name, cas_garonne.config)

# Create an instance of Telemac2DStudy from this Garonne.
# Coordinates of the points of interest (Marmande)
points = [(425697., 246914.), (425697., 246916.)]
results_file = 'sortie_gar.slf'  # results file
study = Telemac2DStudy(points, cas_garonne, results_file,
                       "./study_test_ens", 666, nproc)
print("Running case: ", study.test_case.name)

# Input space

Nl = 3 # learning sample size

# In this example we use a hardcodedi distribution it can be generated from Python modules

X = [[17.801835003673503, 39.678349288296914, 43.85325728871733, 43.154116466916335, 8807.641199604444],
     [15.770017846237016, 46.18793175247774, 47.29677432494225, 39.79764479361369, 7964.614999654199],
     [16.81169614504843, 49.00699476848045, 33.57394423190668, 32.97479348254673, 8590.39177800137],
     [13.827268388107711, 47.09993316423509, 34.97273216646893, 36.29226881695051, 7820.360948314167],
     [16.54389042587234, 43.157932671164794, 31.414902608785713, 31.80101676184016, 8262.714461329919],
     [18.907677116493147, 42.63213466923376, 41.73644820627276, 44.87226274032118, 7323.662785986174],
     [15.027734694272377, 37.503955784389895, 29.957947966422985, 48.11508230630467, 8329.110577876105],
     [17.389173749595862, 54.316874667270916, 45.61166132958741, 40.396262789494386, 8994.66227580087],
     [13.078302503255832, 36.96857049817972, 39.53734990659866, 47.359842141845725, 9638.316252690382],
     [14.415515798172505, 52.7659489674364, 37.94996785274626, 35.68655027801387, 9239.983619724755]]

X = [list(X[i]) for i in range(Nl)]

Y = []
ix = 0
for ix, x in enumerate(X):
    print("-- Simu #"+str(ix))
    study(x, out_dir=os.path.join("results", str(ix)))
    Y.append(study.z_points.data.tolist())
    # Plotting a state could be costly on some platforms. To be used when strictly necessary.
    study.plot_final_state(out_dir=os.path.join("results", str(ix)),
                           filename=os.path.join("results", str(ix), "water_level.pdf"))

print('Y', Y)
del study
