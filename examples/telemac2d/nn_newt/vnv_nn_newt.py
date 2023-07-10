
"""
Validation script for non-newtonian model
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 2
        self.tags = ['fv','telemac2d']
        self.listing = True

    def _pre(self):
        """
        Defining the studies
        """

        # Bingham option 1
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_nn_newt.cas')


        # Bingham option 1 parallel
        cas = TelemacCas('t2d_nn_newt.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_nn_newt_par.cas',
                       cas=cas)


        # Bingham option 2
        cas = TelemacCas('t2d_nn_newt.cas', get_dico('telemac2d'))
        cas.set('BINGHAM OPTION', 2)

        self.add_study('vnv_3',
                       'telemac2d',
                       't2d_nn_newt_opt2.cas',
                       cas=cas)

        # Bingham option 2 parallel
        cas = TelemacCas('t2d_nn_newt.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('BINGHAM OPTION', 2)

        self.add_study('vnv_4',
                       'telemac2d',
                       't2d_nn_newt_opt2_par.cas',
                       cas=cas)

        # Bingham option 3
        cas = TelemacCas('t2d_nn_newt.cas', get_dico('telemac2d'))
        cas.set('BINGHAM OPTION', 3)

        self.add_study('vnv_5',
                       'telemac2d',
                       't2d_nn_newt_opt3.cas',
                       cas=cas)

        # Bingham option 3 parallel
        cas = TelemacCas('t2d_nn_newt.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('BINGHAM OPTION', 3)

        self.add_study('vnv_6',
                       'telemac2d',
                       't2d_nn_newt_opt3_par.cas',
                       cas=cas)

        # Herschel-Bulkley n=1.5
        cas = TelemacCas('t2d_nn_newt.cas', get_dico('telemac2d'))
        cas.set('NON-NEWTONIAN MODEL', 2)
        cas.set('HERSCHEL-BULKLEY POWER-LAW INDEX', 1.5)

        self.add_study('vnv_7',
                       'telemac2d',
                       't2d_nn_newt_HB.cas',
                       cas=cas)

        # Herschel-Bulkley n=1.5 parallel
        cas = TelemacCas('t2d_nn_newt.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('NON-NEWTONIAN MODEL', 2)
        cas.set('HERSCHEL-BULKLEY POWER-LAW INDEX', 1.5)

        self.add_study('vnv_8',
                       'telemac2d',
                       't2d_nn_newt_HB_par.cas',
                       cas=cas)

        # Herschel-Bulkley n=0.5
        cas = TelemacCas('t2d_nn_newt.cas', get_dico('telemac2d'))
        cas.set('NON-NEWTONIAN MODEL', 2)
        cas.set('HERSCHEL-BULKLEY POWER-LAW INDEX', 0.5)

        self.add_study('vnv_9',
                       'telemac2d',
                       't2d_nn_newt_HB_05.cas',
                       cas=cas)

        # Herschel-Bulkley n=0.5 parallel
        cas = TelemacCas('t2d_nn_newt.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('NON-NEWTONIAN MODEL', 2)
        cas.set('HERSCHEL-BULKLEY POWER-LAW INDEX', 0.5)

        self.add_study('vnv_10',
                       'telemac2d',
                       't2d_nn_newt_HB_05_par.cas',
                       cas=cas)

        del cas

    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with reference file
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_nn_newt.slf',
                            eps=[1.E-15])

        # Comparison seq/par.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1.E-15])

        # Comparison with reference file
        self.check_epsilons('vnv_3:T2DRES',
                            'f2d_nn_newt_opt2.slf',
                            eps=[1.E-15])

        # Comparison seq/par.
        self.check_epsilons('vnv_3:T2DRES',
                            'vnv_4:T2DRES',
                            eps=[1.E-15])

        # Comparison with reference file
        self.check_epsilons('vnv_5:T2DRES',
                            'f2d_nn_newt_opt3.slf',
                            eps=[1.E-15])

        # Comparison seq/par.
        self.check_epsilons('vnv_5:T2DRES',
                            'vnv_6:T2DRES',
                            eps=[1.E-15])

        # Comparison with reference file
        self.check_epsilons('vnv_7:T2DRES',
                            'f2d_nn_newt_HB.slf',
                            eps=[1.E-15])

        # Comparison seq/par.
        self.check_epsilons('vnv_7:T2DRES',
                            'vnv_8:T2DRES',
                            eps=[1.E-15])

        # Comparison with reference file
        self.check_epsilons('vnv_9:T2DRES',
                            'f2d_nn_newt_HB_05.slf',
                            eps=[1.E-15])

        # Comparison seq/par.
        self.check_epsilons('vnv_9:T2DRES',
                            'vnv_10:T2DRES',
                            eps=[1.E-15])

    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d
        from matplotlib import pyplot as plt
        import numpy as np
        from postel.plot1d import plot1d

        #Get T2D results file
        res, _ = self.get_study_res('vnv_1:T2DRES')
        res2, _ = self.get_study_res('vnv_3:T2DRES')
        res3, _ = self.get_study_res('vnv_5:T2DRES')
        res4, _ = self.get_study_res('vnv_7:T2DRES')
        res5, _ = self.get_study_res('vnv_9:T2DRES')

        #Plot the mesh
        vnv_plot2d(\
                '',res,
                plot_mesh=True,
                fig_name='img/mesh',
                fig_size=(10,2),
                x_label='x (m)',y_label='y (m)')

        #Plot the initial state
        vnv_plot2d(\
                'WATER DEPTH',
                res,
                record=0,
                fig_name='img/init',
                fig_size=(10,4),
                cbar_label='Flow depth (m)',
                filled_contours=True,
                x_label='x (m)',y_label='y (m)')

        #Plot the Bingham profile
        csv_file = './data/sol.dat'
        sol = np.loadtxt(csv_file, delimiter=";", comments='#')
        line = [np.array([0.,6.]),np.array([3000.,6.])]
        line_num = [200]
        l, _, h = res.get_timeseries_on_polyline('WATER DEPTH',line,line_num)
        l2, _, h2 = res2.get_timeseries_on_polyline('WATER DEPTH',line,line_num)
        l3, _, h3 = res3.get_timeseries_on_polyline('WATER DEPTH',line,line_num)

        fig, ax = plt.subplots(figsize=(10,5))
        plot1d(ax,l[:,0],h[:,-1],plot_label='Bingham Opt 1')
        plot1d(ax,l2[:,0],h2[:,-1],plot_label='Bingham Opt 2')
        plot1d(ax,l3[:,0],h3[:,-1],plot_label='Bingham Opt 3')
        plot1d(ax,sol[:,0],sol[:,1],plot_label='Theoretical solution',\
                x_label='X (m)',y_label='Flow depth (m)')
        ax.legend()
        fig_name = 'img/profile-bingham'
        print(" "*8+'~> Plotting '+fig_name)
        plt.savefig(fig_name)
        plt.close()


        #Plot the Herschley-Bulkley profile
        l4, _, h4 = res4.get_timeseries_on_polyline('WATER DEPTH',line,line_num)
        l5, _, h5 = res5.get_timeseries_on_polyline('WATER DEPTH',line,line_num)

        fig, ax = plt.subplots(figsize=(10,5))
        plot1d(ax,l[:,0],h[:,-1],plot_label='Bingham Opt 1')
        plot1d(ax,l4[:,0],h4[:,-1],plot_label='Herschley-Bulkley n=1.5')
        plot1d(ax,l5[:,0],h5[:,-1],plot_label='Herschley-Bulkley n=0.5',\
                x_label='X (m)',y_label='Flow depth (m)')
        ax.legend()
        fig_name = 'img/profile-hb'
        print(" "*8+'~> Plotting '+fig_name)
        plt.savefig(fig_name)
        plt.close()

