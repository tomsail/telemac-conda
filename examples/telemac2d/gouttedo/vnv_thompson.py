"""
Validation script for thompson
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
        Defining general parameters
        """
        self.rank = 1
        self.tags = ['telemac2d']

    def _pre(self):
        """
        Defining the studies
        """
        self.add_study('seq',
                       'telemac2d',
                       't2d_thompson.cas')

        cas = TelemacCas('t2d_thompson.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('par',
                       'telemac2d',
                       't2d_thompson_par.cas',
                       cas=cas)

        del cas

    def _check_results(self):
        """
        Check on run results
        """
        self.check_epsilons('seq:T2DRES',
                            'f2d_thompson.slf',
                            eps=[1e-6])
        self.check_epsilons('par:T2DRES',
                            'f2d_thompson.slf',
                            eps=[1e-6])
        self.check_epsilons('seq:T2DRES',
                            'par:T2DRES',
                            eps=[1e-6])

    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot1d_polylines, vnv_plot2d, vnv_plot3d
        # Getting the name of the result file of the sequential run of
        # vnv_gouttedo
        res_file = self.get_study_file('seq:T2DRES')
        bnd_file = self.get_study_file('seq:T2DCLI')
        res_seq = TelemacFile(res_file, bnd_file=bnd_file)

        # Plotting water depth over polyline for time step 0
        vnv_plot1d_polylines(\
            'WATER DEPTH',
            res_seq,
            legend_labels='initial elevation',
            fig_size=(12, 5),
            record=0,
            poly=[[0., 10.], [20., 10.]],
            poly_number=[50],
            fig_name='img/WaterDepth1d_0')

        # Plotting water depth over polyline for time step 0
        vnv_plot1d_polylines(\
            'WATER DEPTH',
            res_seq,
            legend_labels='final elevation',
            fig_size=(6, 5),
            record=-1,
            poly=[[0., 10.], [20., 10.]],
            poly_number=[50],
            fig_name='img/WaterDepth_Y10')

        # plotting mesh
        vnv_plot2d(\
            '',
            res_seq,
            record=0,
            fig_size=(5, 5),
            fig_name="img/Mesh",
            plot_mesh=True)

        for record in [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]:
            # Plotting sclar map of water depth for some time steps
            fig_name = "img/WaterDepth_t{}".format(record)
            vnv_plot2d(\
                'WATER DEPTH', res_seq,
                record=record,
                fig_size=(7, 6),
                fig_name=fig_name,
                cbar_label='H',
                contours=True,
                annotate_time=True,
                filled_contours=True)

            # Plotting 3d sclar map of water depth for some time steps
            fig_name = "img/WaterDepth_t{}_3d".format(record)
            vnv_plot3d(\
                'WATER DEPTH', res_seq,
                record=record,
                fig_size=(8, 6),
                fig_name=fig_name,
                zlim=[0., 4.5],
                annotate_time=True,
                cbar_label='H')

        res_seq.close()
