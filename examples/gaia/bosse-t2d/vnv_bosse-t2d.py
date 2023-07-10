
"""
Validation script for bosse-t2d
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
        self.tags = ['telemac2d', 'gaia']

    def _pre(self):
        """
        Defining the studies
        """

        # bosse-t2d scalar mode T2D+GAI EF
        self.add_study('fe_seq',
                       'telemac2d',
                       't2d_bosse-t2d.cas')


        # bosse-t2d parallel mode T2D+GAI
        cas = TelemacCas('t2d_bosse-t2d.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('fe_parall',
                       'telemac2d',
                       't2d_bosse-t2d_par.cas',
                       cas=cas)

        del cas

        # bosse-t2d scalar mode T2D+GAI FV
        cas = TelemacCas('t2d_bosse-t2d.cas', get_dico('telemac2d'))
        cas.set('GAIA STEERING FILE', 'gai_bosse-vf-t2d.cas')

        self.add_study('fv_seq',
                       'telemac2d',
                       't2d_bosse-t2d_vf.cas',
                       cas=cas)

        del cas

        # bosse-t2d parallel mode T2D+GAI
        cas = TelemacCas('t2d_bosse-t2d.cas', get_dico('telemac2d'))
        cas.set('GAIA STEERING FILE', 'gai_bosse-vf-t2d.cas')
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('fv_parall',
                       'telemac2d',
                       't2d_bosse-t2d_vf_par.cas',
                       cas=cas)

        del cas


    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison between sequential and parallel
        self.check_epsilons('fe_seq:T2DRES','f2d_bosse-t2d_fe.slf',
                            eps=[1.e-4])
        self.check_epsilons('fe_parall:T2DRES','f2d_bosse-t2d_fe.slf',
                            eps=[1.e-4])
        self.check_epsilons('fe_seq:T2DRES','fe_parall:T2DRES',
                            eps=[1.e-4])
        self.check_epsilons('fv_seq:T2DRES','f2d_bosse-t2d_fv.slf',
                            eps=[1.e-4])
        self.check_epsilons('fv_parall:T2DRES','f2d_bosse-t2d_fv.slf',
                            eps=[1.e-4])
        self.check_epsilons('fv_seq:T2DRES','fv_parall:T2DRES',
                            eps=[1.e-4])
        self.check_epsilons('fe_seq:GAIRES','gai_ref_bosse-t2d_fe.slf',
                            eps=[1.e-4])
        self.check_epsilons('fe_parall:GAIRES','gai_ref_bosse-t2d_fe.slf',
                            eps=[1.e-4])
        self.check_epsilons('fe_seq:GAIRES','fe_parall:GAIRES',
                            eps=[1.e-4])
        self.check_epsilons('fv_seq:GAIRES','gai_ref_bosse-t2d_fv.slf',
                            eps=[1.e-4])
        self.check_epsilons('fv_parall:GAIRES','gai_ref_bosse-t2d_fv.slf',
                            eps=[1.e-4])
        self.check_epsilons('fv_seq:GAIRES','fv_parall:GAIRES',
                            eps=[1.e-4])

        # Comparison with exact solution
        self.check_epsilons('fe_seq:GAIRES','fe_seq:GAIRES',
                            var1='BOTTOM', var2='EXACT SOLUTION',
                            eps=[0.09])
        self.check_epsilons('fv_seq:GAIRES','fv_seq:GAIRES',
                            var1='BOTTOM', var2='EXACT SOLUTION',
                            eps=[0.07])

    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_polylines
        import numpy as np
        from os import path

        # Getting the name of the geometry and result file of the sequential run

        geom_res, _ = self.get_study_res('fe_seq:T2DGEO', load_bnd=True)
        res_file = self.get_study_file('fe_seq:GAIRES')
        bnd_file = self.get_study_file('fe_seq:T2DCLI')
        res_seq = TelemacFile(res_file, bnd_file=bnd_file)
        res_vnv_1_gaires, _ = self.get_study_res('fe_seq:GAIRES')

        #======================================================================
        # Load all results as a list:
        res_list, res_labels = self.get_study_res(module='GAI', whitelist=['seq'])
        #======================================================================
        # DESCRIPTION PLOTS:
        #
        # Plotting bed elevation over polyline for time step 0
        vnv_plot1d_polylines(\
            'BOTTOM',
            res_seq,
            legend_labels='initial bed level',
            fig_size=(8, 2),
            record=0,
            poly=[[0., 0.], [16., 0.]],
            poly_number=[50],
            ylim=[0,0.15],
            fig_name='img/InitialBottom')
        #
        # Plotting bed elevation and exact bed elevation at different records
        records=[0,10,35]
        for idx,record in enumerate(records):
            time_label = 't={:.2f}'.format(res_vnv_1_gaires.times[record])
            vnv_plot1d_polylines(\
                'BOTTOM',
                res_list,
                legend_labels=res_labels,
                record=record,
                fig_size=(8, 7),
                ref_name='EXACT SOLUTION',
                y_label='b (m)',
                x_label='x (m)',
                poly=[[0., 0.], [16., 0.]],
                poly_number=[100],
                fig_name='img/bottom{}'.format(record),
                fig_title=time_label
                )

        # Plot 2d mesh and boundaries:

        vnv_plot2d(\
            'BOTTOM',
            geom_res,
            record=0,
            fig_size=(12, 2),
            fig_name="img/gai_bosse-t2d_mesh",
            annotate_bnd=True,
            plot_mesh=True)

