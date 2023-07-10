
"""
Validation script for seiche
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
        self.rank = 4
        self.tags = ['telemac2d', 'fv']

    def _pre(self):
        """
        Defining the studies
        """

        # seiche scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_seiche.cas')


        # seiche scalar mode
        cas = TelemacCas('t2d_seiche.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_seiche_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_seiche.slf',
                            eps=[1.E-4])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_seiche.slf',
                            eps=[1.E-4])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1.E-4])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_polylines
        import matplotlib.pyplot as plt
        from postel.plot1d import plot1d

        
        # Getting files
        vnv_1_t2dres = self.get_study_file('vnv_1:T2DRES')
        res_vnv_1_t2dres = TelemacFile(vnv_1_t2dres)
        vnv_1_t2dgeo = self.get_study_file('vnv_1:T2DGEO')
        bnd_file = self.get_study_file('vnv_1:T2DCLI')
        res_vnv_1_t2dgeo = TelemacFile(vnv_1_t2dgeo, bnd_file)
        #Plotting mesh
        times=res_vnv_1_t2dres.times
        vnv_plot2d('',
                   res_vnv_1_t2dgeo,
                   plot_mesh=True,
                   annotate_bnd=True,
                   fig_size=(20, 2),
                   fig_name='img/Mesh')
        poly=[[0.,1.],[199.75, 1.]]
        polygone_discretized_points, absci, waterdep= \
            res_vnv_1_t2dres.get_data_on_polyline('FREE SURFACE', -1, poly)
        _, ax = plt.subplots(figsize=(10, 5))
        plot1d(ax, absci, waterdep, plot_label='telemac')
        fig_name = 'img/free'
        print(" "*8+'~> Plotting '+fig_name)
        plt.savefig(fig_name)
        plt.close('all')
