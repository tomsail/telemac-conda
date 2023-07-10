
"""
Validation script for bumpsub
"""
import numpy as np
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
        self.rank = 1
        self.tags = ['telemac2d']

    def _pre(self):
        """
        Defining the studies
        """
        casname='t2d_flume_FE.cas'

        #======================================================================
        # CHAR run
        cas = TelemacCas(casname, get_dico('telemac2d'))
        cas.set('SCHEME FOR ADVECTION OF TRACERS', 1)
        self.add_study('char_seq', 'telemac2d', casname, cas=cas)
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('char_par', 'telemac2d', casname, cas=cas)
        del cas

        #======================================================================
        # NERD run
        cas = TelemacCas(casname, get_dico('telemac2d'))
        cas.set('SCHEME FOR ADVECTION OF TRACERS', 14)
        cas.set('SCHEME OPTION FOR ADVECTION OF TRACERS', 1)
        self.add_study('nerd_seq', 'telemac2d', casname, cas=cas)
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('nerd_par', 'telemac2d', casname, cas=cas)
        del cas

        #======================================================================
        # NERD run
        cas = TelemacCas(casname, get_dico('telemac2d'))
        cas.set('SCHEME FOR ADVECTION OF TRACERS', 15)
        cas.set('SCHEME OPTION FOR ADVECTION OF TRACERS', 1)
        cas.set('TREATMENT OF NEGATIVE DEPTHS', 3)
        cas.set('OPTION FOR THE TREATMENT OF TIDAL FLATS', 1)
        self.add_study('eria_seq', 'telemac2d', casname, cas=cas)
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('eria_par', 'telemac2d', casname, cas=cas)
        del cas

        #======================================================================
        # N run
        cas = TelemacCas(casname, get_dico('telemac2d'))
        cas.set('SCHEME FOR ADVECTION OF TRACERS', 4)
        cas.set('SCHEME OPTION FOR ADVECTION OF TRACERS', 1)
        self.add_study('n_seq', 'telemac2d', casname, cas=cas)
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('n_par', 'telemac2d', casname, cas=cas)
        del cas

        #======================================================================
        # PSI run
        cas = TelemacCas(casname, get_dico('telemac2d'))
        cas.set('SCHEME FOR ADVECTION OF TRACERS', 5)
        cas.set('SCHEME OPTION FOR ADVECTION OF TRACERS', 1)
        self.add_study('psi_seq', 'telemac2d', casname, cas=cas)
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('psi_par', 'telemac2d', casname, cas=cas)
        del cas


    def _check_results(self):
        """
        Post-treatment processes
        """

    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot3d, vnv_plot2d, vnv_plot1d, vnv_plotbar, \
                vnv_plot1d_polylines, vnv_plot1d_history, vnv_plotbar_cpu_times
        from os import path
        import numpy as np
        #======================================================================
        # GET TELEMAC RESULT FILES:
        #
        # Load all results as a list:
        res_list, res_labels = self.get_study_res(module='T2D', whitelist=['seq'])

        #======================================================================
        # PLOT 1D
        records = [0, -1]

        for idx, record in enumerate(records):
            vnv_plot1d_polylines(\
                'FREE SURFACE',
                res_list,
                res_labels,
                record=record,
                fig_size=(8, 4),
                y_label='z',
                x_label='x',
                fig_name='img/t2d_flume_ef_h_{}'.format(record),
                plot_bottom=True)

            vnv_plot1d_polylines(\
                'VELOCITY U',
                res_list,
                res_labels,
                record=record,
                fig_size=(8, 4),
                y_label='U',
                x_label='x',
                fig_name='img/t2d_flume_ef_u_{}'.format(record),
                plot_bottom=True)

            vnv_plot1d_polylines(\
                'TRACER 1',
                res_list,
                res_labels,
                record=record,
                ref_name='SOL T1',
                fig_size=(8, 4),
                y_label='tracer',
                x_label='x',
                fig_name='img/t2d_flume_ef_T1_{}'.format(record),
                plot_bottom=True,
                markers=True,
                markevery=15)

            vnv_plot1d_polylines(\
                'TRACER 2',
                res_list,
                res_labels,
                record=record,
                ref_name='SOL T2',
                fig_size=(8, 4),
                y_label='tracer',
                x_label='x',
                fig_name='img/t2d_flume_ef_T2_{}'.format(record),
                plot_bottom=True,
                markers=True,
                markevery=15)
