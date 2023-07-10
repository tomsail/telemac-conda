#!/usr/bin/env python3
"""
telapy mascaret-tracer example:
    - unsteady case for the tracers
    - comparison of a direct run with a step-by-step computation
Author(s): Fabrice Zaoui
Copyright (c) EDF 2022 
"""
import sys
import numpy as np
from os import path, chdir, environ, getcwd
from telapy.api.masc import Mascaret
if(not path.exists(path.join(environ.get('HOMETEL', ''),
                             'builds',
                             environ.get('USETELCFG', ''),
                             'wrap_api', 'lib', 'api.pyf'))):
    print("  -> telapy not available doing nothing")
    sys.exit(0)

root = environ.get('HOMETEL', path.join('..', '..', '..'))
pwd = getcwd()
chdir(path.join(root, 'examples', 'mascaret', 'Test_Tracer'))
    
# create two models for comparison
masc1 = Mascaret()
masc2 = Mascaret()
masc1.create_mascaret(iprint=0)
masc2.create_mascaret(iprint=0)

#  list Tracer files & import
masc1.import_model_onefile('mascaret.files')
masc2.import_model_onefile('mascaret.files')

# hydraulic initialisation
masc1.init_hydro_from_file('mascaret0.lig')
masc2.init_hydro_from_file('mascaret0.lig')

# tracer initialisation
nb_nodes, _, _ = masc1.get_var_size('Model.X')
tracer0 = np.zeros((nb_nodes, 1), dtype=np.float64)
tracer0[:, :] = 10.
masc1.init_tracer(tracer0)
masc2.init_tracer(tracer0)

# time info
tini = 0.  # initial time (s)
tend = 14400  # end of computation (s)
dt = 3  # time step (s)

# computations
masc1.compute(tini, tend, dt)
t0 = tini
dt0 = 3 * dt
for i in range(int(tend / dt0)):
    masc2.compute(t0, t0+dt0, dt0)
    t0 += dt0

tracer1 = masc1.get_tracer()
tracer2 = masc2.get_tracer()

# delete Mascaret
masc1.delete_mascaret()
masc2.delete_mascaret()

# check max difference
print('max difference = %f' % np.max(np.abs(tracer1 - tracer2)))

# go back
chdir(pwd)
