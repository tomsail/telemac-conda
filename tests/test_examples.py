import subprocess
import sys, os 

# test if we can import well libraries 
HOMETEL = os.getenv('HOMETEL')

SOLVERS = {     # solver used   N of tests passed
    'artemis'   :  ['artemis',  []],     
    'courlis'   :  ['mascaret', []],     
    'gaia'      :  ['telemac',  []],        
    'khione'    :  ['telemac',  []],      
    'mascaret'  :  ['mascaret', []],    
    'nestor'    :  ['telemac',  []],    
    'telemac2d' :  ['telemac2d',[]],   
    'telemac3d' :  ['telemac3d',[]],   
    'tomawac'   :  ['tomawac',  []],     
    'waqtel'    :  ['waqtel',   []],      
}

# test if can run some tests
RESULTS = {}
for solver in  SOLVERS.keys():
    # FOR EVERY TELEMAC SOLVER
    RESULTS[solver] = {}
    module_folder_full = os.path.join(HOMETEL,'examples',solver)
    print('   => testing',solver)
    os.chdir(module_folder_full)
    list_tests = os.listdir(module_folder_full)
    # 
    for n_test,test in enumerate(list_tests):
        # FOR EVERY TEST 
        try: 
            test_folder_full = os.path.join(module_folder_full,test)
            os.chdir(test_folder_full)
            list_files = os.listdir(test_folder_full)
            # init coupling
            RESULTS[solver]['coupled'] = 0
            RESULTS[solver][test] = 0
            for f in list_files:
                if f.endswith('cas'):
                    # ARTEMIS
                    if solver == 'artemis':
                        if f.startswith('art'):
                            os.system('artemis.py '+f) 
                    # COURLIS or MASCARET
                    if solver in ['courlis', 'mascaret']:
                        if f.endswith('xcas'):
                            os.system('mascaret.py '+f)
                    # GAIA, KHIONE, SISYPHE, NESTOR or WAQTEL
                    if solver in ['gaia','khione', 'sisyphe','nestor','waqtel','telemac2d','telemac3d']:
                        if f.startswith('t2d'):
                            os.system('telemac2d.py '+f) 
                        elif f.startswith('t3d'):
                            os.system('telemac3d.py '+f) 
                    # TOMAWAC
                    if solver == 'tomawac':
                        if f.startswith('t3d'):
                            RESULTS[solver]['coupled'] = 1
                            os.system('telemac3d.py '+f) 
                        elif f.startswith('t2d'):
                            os.system('telemac2d.py '+f) 
                            RESULTS[solver]['coupled'] = 1
                        if f.startswith('tom'):
                            # print('h')
                            if not RESULTS[solver]['coupled']:
                                # print('here')
                                os.system('tomawac.py '+f)
                    RESULTS[solver][test] = 1
                    print('     -> test',test,'passed')

        except Exception as e: 
            print(e)
# 
for solver in  SOLVERS.keys():
    print('    +> '+ solver +': ('+str(sum(RESULTS[solver].values())+1)+'/'+str(len(RESULTS[solver]))+') passed')
