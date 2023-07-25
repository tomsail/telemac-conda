import subprocess
import sys, os 

# test if we can import well libraries 
HOMETEL = os.getenv('HOMETEL')

SOLVERS = {     # solver used   N of tests passed
    'artemis'   :  ['artemis',  0],     
    'courlis'   :  ['mascaret', 0],     
    'gaia'      :  ['telemac',  0],        
    'khione'    :  ['telemac',  0],      
    'mascaret'  :  ['mascaret', 0],    
    'nestor'    :  ['telemac',  0],    
    'telemac2d' :  ['telemac2d',0],   
    'telemac3d' :  ['telemac3d',0],   
    'tomawac'   :  ['tomawac',  0],     
    'waqtel'    :  ['waqtel',   0],      
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
                    # COURLIS
                    if solver in ['courlis']:
                        if f.endswith('xcas'):
                            os.system('mascaret.py '+f)
                   
                    RESULTS[solver][test] = 1
                    print('     -> test',test,'passed')
                    
        except Exception as e: 
            print(e)
# 
for solver in  SOLVERS.keys():
    print('    +> '+ solver +': ('+str(sum(RESULTS[solver].values())+1)+'/'+str(len(RESULTS[solver]))+') passed')
