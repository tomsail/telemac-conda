
"""
Validation script for weirs2
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
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
        self.tags = ['telemac2d']

    def _pre(self):
        """
        Defining the studies
        """

        # weirs2 scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_weirs2.cas')


        # weirs2 parallel mode
        cas = TelemacCas('t2d_weirs2.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_weirs2_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_weirs2.slf',
                            eps=[1.E-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_weirs2.slf',
                            eps=[1.E-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1.E-3])


    def _post(self):
        """
        Post-treatment processes
        """
        import matplotlib.pyplot as plt
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_polylines
        import pandas as pd
        from postel.plot1d import plot1d
        from os import path
        
        # Getting files
        vnv_1_t2dgeo = self.get_study_file('vnv_1:T2DGEO')
        vnv_1_t2dres = self.get_study_file('vnv_1:T2DRES')
        vnv_1_t2dwop = self.get_study_file('vnv_1:T2DWOP')
        res_vnv_1_t2dgeo = TelemacFile(vnv_1_t2dgeo)
        res_vnv_1_t2dres = TelemacFile(vnv_1_t2dres)
        res_vnv_1_t2dwop = pd.read_csv(vnv_1_t2dwop, header = 1)
        
        # Plotting evolution of discharge per weir elements
        weirs = [[' '+str(elem) for elem in range(1,21)],
                 [' '+str(elem) for elem in range(21,61)],
                 [' '+str(elem) for elem in range(95,127)],
                 [' '+str(elem) for elem in range(71,95)],
                 [' '+str(elem) for elem in range(61,71)]]
        time = res_vnv_1_t2dwop['TIME'].to_numpy()
        qweirs = []
        for i, l_elem in enumerate(weirs):
            qweirs.append(abs(res_vnv_1_t2dwop[l_elem].sum(axis=1).iloc[-1]))
            fig, ax = plt.subplots(figsize=(10, 8))
            for elem in l_elem:
                valeurs = res_vnv_1_t2dwop[elem].to_numpy()
                plot1d(ax, time, valeurs,
                    plot_label=str(elem),
                    x_label='Time (s)',
                    y_label='Discharge (m^3/s)',
                    linestyle='-')
            ax.legend(loc='lower center', bbox_to_anchor=(0.5, 1), fancybox=True, ncol=10)
            namef = 'q_weir' + str(i + 1) + '.png'
            plt.savefig(path.join('.', 'img', namef))
        plt.close('all')
        
        #print discharge
        with open(path.join('./img/qweirs.txt'), 'w') as outfile:
            for idx, q in enumerate(qweirs):
                if idx == 0:
                    outfile.write('{0:d} & {1:f}'.format(idx+1, q))
                else:
                    outfile.write('\\\\ {0:d} & {1:f}'.format(idx+1, q))
    
        # Plotting FREE SURFACE over polyline over records
        vnv_plot1d_polylines(\
            'FREE SURFACE',
            res_vnv_1_t2dres, 'Elevation',
            fig_size=(10, 5),
            poly=[[0, 500], [2600., 500.0], [2600., 1550.0], [0, 1550.0]],
            record=[0, 12, 24, 36, 48, 60, -1],
            fig_name='img/weirs2_free_surface')

        # Plotting TRACER 1 over polyline over records
        vnv_plot1d_polylines(\
            'TRACER 1',
            res_vnv_1_t2dres, 'Tracer',
            y_label='Concentration (-)',
            fig_size=(10, 5),
            poly=[[0, 500], [2600., 500.0], [2600., 1550.0], [0, 1550.0]],
            record=[0, 12, 24, 36, 48, 60, -1],
            fig_name='img/weirs2_tracer')

        # Plotting BOTTOM
        vnv_plot2d(\
            '',
            res_vnv_1_t2dgeo,
            record=0,
            plot_mesh=True,
            fig_size=(8, 5),
            fig_name='img/weirs2_geo')

        # Plotting VELOCITY
        records = [2, 12, 24, 36, 48, 60, -1]
        fig_names = ['img/weirs2_figure_velo_00600s', \
                     'img/weirs2_figure_velo_03600s', \
                     'img/weirs2_figure_velo_07200s', \
                     'img/weirs2_figure_velo_10800s', \
                     'img/weirs2_figure_velo_14400s', \
                     'img/weirs2_figure_velo_18000s', \
                     'img/weirs2_figure_velo_43200s']

        for idx, record in enumerate(records):
            vnv_plot2d(\
                'VELOCITY',
                res_vnv_1_t2dres,
                record=record,
                cbar_label='Velocity (m/s)',
                filled_contours=True,
                vectors=True,
                vectors_scale=10,
                fig_size=(8, 5),
                fig_name=fig_names[idx],
                annotate_time=True)

        # Closing files
        res_vnv_1_t2dgeo.close()
        res_vnv_1_t2dres.close()
