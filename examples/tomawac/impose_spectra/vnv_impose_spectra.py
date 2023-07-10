
"""
Validation script for impose_spectra
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
from utils.exceptions import TelemacException
from data_manip.extraction.telemac_file import TelemacFile

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 2
        self.tags = ['tomawac']

        self.res_errs = []
        self.spe_errs = []

    def _pre(self):
        """
        Defining the studies
        """

        # Oceanic mesh
        self.add_study('vnv_1',
                       'tomawac',
                       'tom_oceanic.cas')


        # Oceanic mesh parallel mode
        cas = TelemacCas('tom_oceanic.cas', get_dico('tomawac'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'tomawac',
                       'tom_oceanic_par.cas',
                       cas=cas)

        del cas


        # Coastal mesh
        self.add_study('vnv_3',
                       'tomawac',
                       'tom_coastal.cas')


        # Coastal mesh parallel mode
        cas = TelemacCas('tom_coastal.cas', get_dico('tomawac'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'tomawac',
                       'tom_coastal_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """
        import numpy as np

        # Check oceanic result at the last time step.
        errs = self.check_epsilons(\
                'vnv_1:WACRES',
                'f2d_OceanicResults.slf',
                eps=[1e-8, 1e-5, 1e-5, 1e-6, 1e-5])
        self.res_errs.append(errs)

        # Check oceanic result from paral at the last time step.
        errs = self.check_epsilons(\
                'vnv_2:WACRES',
                'f2d_OceanicResults.slf',
                eps=[1e-8, 1e-5, 1e-5, 1e-6, 1e-5])
        self.res_errs.append(errs)

        # Check coastal result at the last time step.
        errs = self.check_epsilons(\
                'vnv_3:WACRES',
                'f2d_CoastalResults.slf',
                eps=[1e-8, 1e-5, 1e-5, 1e-6, 1e-5])
        self.res_errs.append(errs)

        # Check coastal result from paral at the last time step.
        errs = self.check_epsilons(\
                'vnv_4:WACRES',
                'f2d_CoastalResults.slf',
                eps=[1e-8, 1e-5, 1e-5, 1e-6, 1e-5])
        self.res_errs.append(errs)

        # Check coastal result from paral at the last time step.
        self.check_epsilons('vnv_4:WACRES',
                            'f2d_CoastalResults.slf',
                            eps=[1e-8, 1e-5, 1e-5, 1e-6, 1e-5])

        # Check coastal result from paral at the last time step.
        self.check_epsilons('vnv_4:WACRES',
                            'f2d_CoastalResults.slf',
                            eps=[1e-8, 1e-5, 1e-5, 1e-6, 1e-5])

        def compare_diff_mesh(f1,f2,t_read,eps):
            # read file names
            print('')
            print(" "*8+"+> checking epsilon between files {} and {}:"\
                  .format(f1.replace(self.case_dir, '.'),
                          f2.replace(self.case_dir, '.')))

            SerafinFile1 = TelemacFile(f1)
            SerafinFile2 = TelemacFile(f2)
            #Read mesh data 1
            X1 = SerafinFile1.meshx
            Y1 = SerafinFile1.meshy
            NELEM1 = SerafinFile1.nelem2
            IKLE1 = SerafinFile1.ikle2
            VarName1 = SerafinFile1.varnames
            t_series1 = SerafinFile1.times
            #Read mesh data 2
            NPOIN2 = SerafinFile2.npoin2
            X2 = SerafinFile2.meshx
            Y2 = SerafinFile2.meshy
#            t_series2 = SerafinFile2.times
            # Get time
            t_read1 = np.searchsorted(t_series1,t_series1[t_read])
#            t_read2 = np.searchsorted(t_series2,t_series2[t_read])
            # Map Mesh 2 on Mesh 1 (we assume Mesh 2 is completely inside Mesh 1)
            elem21 = np.ones(NPOIN2,dtype=int)*-1
            lambda21 = np.ones([NPOIN2,3],dtype=float)*np.nan
            for ip in range(NPOIN2) :
                for i1 in range(NELEM1) :
                    n1 = IKLE1[i1,0]
                    n2 = IKLE1[i1,1]
                    n3 = IKLE1[i1,2]
                    det1 = (X1[n3]-X1[n2])*(Y2[ip]-Y1[n2])-(Y1[n3]-Y1[n2])*(X2[ip]-X1[n2])
                    det2 = (X1[n1]-X1[n3])*(Y2[ip]-Y1[n3])-(Y1[n1]-Y1[n3])*(X2[ip]-X1[n3])
                    det3 = (X1[n2]-X1[n1])*(Y2[ip]-Y1[n1])-(Y1[n2]-Y1[n1])*(X2[ip]-X1[n1])
                    if ((det1>-1.e-7) and
                        (det2>-1.e-7) and
                        (det3>-1.e-7)) :
                        elem21[ip] = i1
                        detJ = (X1[n2]-X1[n1])*(Y1[n3]-Y1[n1])-(Y1[n2]-Y1[n1])*(X1[n3]-X1[n1])
                        lambda21[ip,0] = det1/detJ
                        lambda21[ip,1] = det2/detJ
                        lambda21[ip,2] = det3/detJ
                        break
            # compare result files
            all_err = []
            for ivar, var_name in enumerate(VarName1):
                var1 = SerafinFile1.get_data_value(var_name, t_read1)
                var2 = SerafinFile2.get_data_value(var_name, t_read1)
                diff = np.ones(NPOIN2,dtype=int)*np.nan
                for ip in range(NPOIN2) :
                    if elem21[ip] != -1 :
                        n1 = IKLE1[elem21[ip],0]
                        n2 = IKLE1[elem21[ip],1]
                        n3 = IKLE1[elem21[ip],2]
                        diff[ip] = (lambda21[ip,0]*var1[n1] +
                          lambda21[ip,1]*var1[n2] +
                          lambda21[ip,2]*var1[n3])
                diff -= var2

                err = np.mean(diff)
                all_err.append(err)
                print(" " * 10 + "- Difference for variables {}: {} (eps={})"\
                      .format(var_name, err, eps[ivar]))

                passed = (err < eps[ivar])

                # Check epsilon
                if not passed:
                    raise TelemacException(\
                          "Epsilon reached in {} vs {}".format(f1, f2))

            SerafinFile1.close()
            SerafinFile2.close()
            return all_err

        errs = compare_diff_mesh(\
                self.get_study_file('vnv_1:WACRES'),
                self.get_study_file('vnv_3:WACRES'),
                -1,
                [1e-2, 1e-2, 2e-2, 1e-2, 1e-2])
        self.res_errs.append(errs)

        errs = compare_diff_mesh(\
                self.get_study_file('vnv_2:WACRES'),
                self.get_study_file('vnv_4:WACRES'),
                -1,
                [1e-2, 1e-2, 2e-2, 1e-2, 1e-2])
        self.res_errs.append(errs)

        # Check coastal result from paral at the last time step.
        errs = self.check_epsilons(\
                'vnv_1:WACLEO',
                'fsp_OceanicResults.slf',
                norm='l1',
                eps=[1e-7])
        self.spe_errs.append(errs)

        # Check coastal result from paral at the last time step.
        errs = self.check_epsilons(\
                'vnv_2:WACLEO',
                'fsp_OceanicResults.slf',
                norm='l1',
                eps=[1e-7])
        self.spe_errs.append(errs)


        # Check coastal result from paral at the last time step.
        errs = self.check_epsilons(\
                'vnv_3:WACLEO',
                'fsp_CoastalResults.slf',
                eps=[1e-7])
        self.spe_errs.append(errs)

        # Check coastal result from paral at the last time step.
        errs = self.check_epsilons(\
                'vnv_4:WACLEO',
                'fsp_CoastalResults.slf',
                norm='l1',
                eps=[1e-7])
        self.spe_errs.append(errs)

        # Check coastal result from paral at the last time step.
        errs = self.check_epsilons(\
                'vnv_1:WACLEO',
                'vnv_3:WACLEO',
                eps=[1e-7],
                norm='l1',
                check_name=False)
        self.spe_errs.append(errs)

        # Check coastal result from paral at the last time step.
        errs = self.check_epsilons(\
                'vnv_2:WACLEO',
                'vnv_4:WACLEO',
                eps=[1e-7],
                norm='l1',
                check_name=False)
        self.spe_errs.append(errs)

    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_actions import plot1d
        import numpy as np
        import matplotlib.pyplot as plt

        def write_table(TabName, HeaderNames, TabNames, var_diff, eps) :
            from os import path, stat, mkdir, getcwd
            # Define table tex file
            try:
                stat(path.join(getcwd(),'table'))
            except:
                mkdir(path.join(getcwd(),'table'))
            tab_name = path.join(getcwd(),'table',TabName)
            # Extract data
            n_var = len(var_diff)
            n_names = len(TabNames)
            if len(eps) != n_names :
                print("/!\ eps is not of the correct size")
            #Fill table array
            TabArray = np.ones([n_var, n_names])*np.nan
            TabCol = 'c'
            for i_var in range(n_var) :
                TabArray[i_var,:] = var_diff[i_var]
                TabCol += 'c'
            TabCol += 'c'
            # Write table Header
            w_file = open(tab_name,'w')
            w_file.write('\\begin{tabular*}{\\linewidth}{@{\\extracolsep{\\fill}}%s}\n' % (TabCol))
            w_file.write('\\toprule\n')
            w_file.write('\\toprule\n')
            for i_h in range(len(HeaderNames)) :
                w_file.write(' & \\multicolumn{2}{c}{\\textbf{ %s }}' % (HeaderNames[i_h]))
            w_file.write(' & \\multirow{2}{*}{\\textbf{ eps }}')
            w_file.write(' \\\\\n')
            for i_h in range(len(HeaderNames)) :
                w_file.write(' & \\textbf{scalar} & \\textbf{2 proc}')
            w_file.write(' & \\\\\n')
            w_file.write('\\midrule\n')
            # Write data
            for i_name in range(n_names) :
                w_file.write(TabNames[i_name])
                for i_var in range(n_var) :
                    if abs(TabArray[i_var,i_name]) > eps[i_name] :
                        w_file.write(' & {\\color{PantoneRed} %.3g}' % (TabArray[i_var,i_name]))
                    else :
                        w_file.write(' & %.3g' % (TabArray[i_var,i_name]))
                w_file.write(' & %.3g' % (eps[i_name]))
                w_file.write(' \\\\\n')
            #Finish Table
            w_file.write('\\bottomrule\n')
            w_file.write('\\bottomrule\n')
            w_file.write('\\end{tabular*}\n')
            w_file.close()

        res = TelemacFile(self.get_study_file('vnv_1:WACRES'))
        write_table('tab_r2d.tex',
                    ['Oceanic - Reference','Coastal - Reference','Oceanic - Coastal'],
                    res.varnames,
                    self.res_errs, [1e-2,1e-2,1e-2,1e-2,1e-2])
        res.close()

        res = TelemacFile(self.get_study_file('vnv_1:WACLEO'))
        write_table('tab_spe.tex',
                    ['Oceanic - Reference','Coastal - Reference','Oceanic - Coastal'],
                    res.varnames,
                    self.spe_errs, [1e-7]*21)
        res.close()

        # Plotting vertical split
        # TODO: Improvment to be done on plot (not nice)
        ref_oce = TelemacFile('f2d_OceanicResults.slf')
        ref_coa = TelemacFile('f2d_CoastalResults.slf')
        res_oce = TelemacFile(self.get_study_file('vnv_1:WACRES'))
        res_coa = TelemacFile(self.get_study_file('vnv_3:WACRES'))

        poly = [[-200., 100.], [200., 100.]]

        record = -1

        for var_name in ['WAVE HEIGHT HM0', 'MEAN PERIOD TMOY', 'WAVE SPREAD']:
            _, abs_curv_coa, values_coa =\
                res_coa.get_timeseries_on_polyline(var_name, poly)

            _, abs_curv_oce, values_oce =\
                res_oce.get_timeseries_on_polyline(var_name, poly)

            _, abs_curv_ref_coa, values_ref_coa =\
                ref_coa.get_timeseries_on_polyline(var_name, poly)

            _, abs_curv_ref_oce, values_ref_oce =\
                ref_oce.get_timeseries_on_polyline(var_name, poly)

            _, axe = plt.subplots()

            plot1d(axe, abs_curv_ref_oce, values_ref_oce[:, record],
                   x_label='y (m)',
                   y_label=var_name.lower(),
                   plot_label='Oceanic Reference',
                   marker='+')

            plot1d(axe, abs_curv_ref_coa, values_ref_coa[:, record],
                   x_label='y (m)',
                   y_label=var_name.lower(),
                   plot_label='Coastal Reference',
                   marker='o')

            plot1d(axe, abs_curv_oce, values_oce[:, record],
                   x_label='y (m)',
                   y_label=var_name.lower(),
                   plot_label='Oceanic Modelled')

            plot1d(axe, abs_curv_coa, values_coa[:, record],
                   x_label='y (m)',
                   y_label=var_name.lower(),
                   plot_label='Coastal Modelled')
            axe.legend()

            fig_name = 'img/{}'.format(var_name.lower().replace(' ', '_'))
            plt.savefig(fig_name)
            plt.close('all')

        ref_oce.close()
        ref_coa.close()
        res_oce.close()
        res_coa.close()
