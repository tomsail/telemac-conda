
"""
Validation script for bosse-t3d
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
        self.rank = 3
        self.tags = ['telemac3d', 'gaia']
        # For vnv_1
        self.walltime = "8:00:00"

    def _pre(self):
        """
        Defining the studies
        """

        # bosse-t3d scalar mode T3D+GAI
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_bosse-t3d.cas')

        # bosse-t3d T3D+GAI parallel mode
        cas = TelemacCas('t3d_bosse-t3d.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_bosse-t3d_par.cas',
                       cas=cas)

        del cas

    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:GAIRES',
                            'gai_ref_bosse-t3d.slf',
                            eps=[1.e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:GAIRES',
                            'gai_ref_bosse-t3d.slf',
                            eps=[1.e-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:GAIRES',
                            'vnv_2:GAIRES',
                            eps=[1.e-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_bosse-t3d.slf',
                            eps=[1.e-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_bosse-t3d.slf',
                            eps=[1.e-6])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1.e-6])

    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_actions import plot1d
        from postel.plot_vnv import vnv_plot1d_polylines, vnv_plot2d
        import matplotlib.pyplot as plt
        # Getting files
        vnv_1_gaires = self.get_study_file('vnv_1:GAIRES')
        res_vnv_1_gaires = TelemacFile(vnv_1_gaires)
        vnv_1_t3dres = self.get_study_file('vnv_1:T3DRES')
        res_vnv_1_t3dres = TelemacFile(vnv_1_t3dres)

        # DESCRIPTION PLOTS:
        #
        # Plotting bed elevation over polyline for time step 0
        vnv_plot1d_polylines(
            'BOTTOM',
            res_vnv_1_gaires,
            legend_labels='initial bed level',
            fig_size=(8, 2),
            record=0,
            poly=[[0., 0.], [16., 0.]],
            poly_number=[50],
            ylim=[0, 0.15],
            fig_name='img/InitialBottom')
        #
        # Plotting bed evolution
        records = [0, 3, 6, 9, 12]
        for idx, record in enumerate(records):
            time_label = 't={:.2f}'.format(res_vnv_1_gaires.times[record])
            vnv_plot1d_polylines(
                'BOTTOM',
                res_vnv_1_gaires,
                legend_labels='bottom',
                record=record,
                y_label='b (m)',
                x_label='x (m)',
                poly=[[0., 0.], [16., 0.]],
                poly_number=[100],
                fig_name='img/bottom{}'.format(record),
                fig_title=time_label
            )

        # Plotting BOTTOM at 0
        vnv_plot2d('BOTTOM',
                   res_vnv_1_gaires,
                   record=0,
                   filled_contours=True,
                   fig_size=(12, 2),
                   plot_mesh=True,
                   fig_name='img/bathy_mesh')

        res_vnv_1_gaires.close()
        res_vnv_1_t3dres.close()
