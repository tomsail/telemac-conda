
"""
Validation script for amr
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
        self.tags = ['telemac3d', 'postel3d']

    def _pre(self):
        """
        Defining the studies
        """

        # amr scalar mode
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_amr.cas')


        # amr parallel mode
        cas = TelemacCas('t3d_amr.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_amr_par.cas',
                       cas=cas)

        del cas
        
        # sigma scalar mode
        self.add_study('vnv_3',
                       'telemac3d',
                       't3d_sigma.cas')


        # sigma parallel mode
        cas = TelemacCas('t3d_sigma.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'telemac3d',
                       't3d_sigma_par.cas',
                       cas=cas)

        del cas


        
        # amr scalar mode
        self.add_study('p3d',
                       'postel3d',
                       'p3d_amr.cas')



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_amr.slf',
                            eps=[1.E-5])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_amr.slf',
                            eps=[1.E-5])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1.E-5])
        
        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_3:T3DRES',
                            'vnv_4:T3DRES',
                            eps=[2.E-5])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d
        vnv_1_t3dcli = self.get_study_file('vnv_1:T3DCLI')
        vnv_1_t3dres = self.get_study_file('vnv_1:T3DRES')
        res_vnv_1_t3dres = TelemacFile(vnv_1_t3dres,bnd_file=vnv_1_t3dcli)

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_t3dres,
                   plot_mesh=True,
                   annotate_bnd=True,
                   fig_name='img/mesh')

        points1=[[0,50],[1000,50]]
        points2=[[250, 0],[250,100]]
        poly_number1=[100]
        poly_number2=[10]
        
        vnv_plot2d('ELEVATION Z',res_vnv_1_t3dres,
                   plot_mesh=True,
                   y_label='z (m)',
                   poly=points1,
                   poly_number=poly_number1,
                   fig_name='img/vert_mesh')

        cbar_ticks=[1,3,5,10,20,50,100,200]
        vnv_plot2d('TEMPERATURE',res_vnv_1_t3dres,
                   plot_mesh=True,
                   filled_contours=True,
                   y_label='z (m)',
                   poly=points1,
                   nv=9,
                   y_factor=10.0,
                   fig_size=(30,3),
                   aspect_ratio='equal',
                   cbar_ticks=cbar_ticks,
                   record=-1,
                   poly_number=poly_number1,
                   fig_name='img/coupey')
           
        vnv_plot2d('TEMPERATURE',res_vnv_1_t3dres,
                   plot_mesh=True,
                   x_label='y (m)',
                   filled_contours=True,
                   y_label='z (m)',
                   y_factor=10.0,
                   poly=points2,
                   fig_size=(3,3),
                   aspect_ratio='equal',
                   record=-1,
                   nv=9,
                   cbar_ticks=cbar_ticks,
                   poly_number=poly_number2,
                   fig_name='img/coupex')
           
        vnv_plot2d('TEMPERATURE',res_vnv_1_t3dres,
                   plane=0,
                   plot_mesh=True,
                   x_label='y (m)',
                   filled_contours=True,
                   y_label='z (m)',
                   fig_size=(30,3),
                   aspect_ratio='equal',
                   record=-1,
                   nv=9,
                   cbar_ticks=cbar_ticks,
                   fig_name='img/fond')
           
        vnv_3_t3dcli = self.get_study_file('vnv_3:T3DCLI')
        vnv_3_t3dres = self.get_study_file('vnv_3:T3DRES')
        res_vnv_3_t3dres = TelemacFile(vnv_3_t3dres,bnd_file=vnv_3_t3dcli)

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_3_t3dres,
                   plot_mesh=True,
                   annotate_bnd=True,
                   fig_name='img/meshsigma')

        points1=[[0,50],[1000,50]]
        points2=[[250, 0],[250,100]]
        poly_number1=[100]
        poly_number2=[10]
        
        vnv_plot2d('ELEVATION Z',res_vnv_3_t3dres,
                   plot_mesh=True,
                   y_label='z (m)',
                   poly=points1,
                   poly_number=poly_number1,
                   fig_name='img/vert_meshsigma')

        cbar_ticks=[1,3,5,10,20,50,100,200]
        vnv_plot2d('TEMPERATURE',res_vnv_3_t3dres,
                   plot_mesh=True,
                   filled_contours=True,
                   y_label='z (m)',
                   poly=points1,
                   nv=9,
                   y_factor=10.0,
                   fig_size=(30,3),
                   aspect_ratio='equal',
                   cbar_ticks=cbar_ticks,
                   record=-1,
                   poly_number=poly_number1,
                   fig_name='img/coupeysigma')
           
        vnv_plot2d('TEMPERATURE',res_vnv_3_t3dres,
                   plot_mesh=True,
                   x_label='y (m)',
                   filled_contours=True,
                   y_label='z (m)',
                   y_factor=10.0,
                   poly=points2,
                   fig_size=(3,3),
                   aspect_ratio='equal',
                   record=-1,
                   nv=9,
                   cbar_ticks=cbar_ticks,
                   poly_number=poly_number2,
                   fig_name='img/coupexsigma')
           
        vnv_plot2d('TEMPERATURE',res_vnv_3_t3dres,
                   plane=0,
                   plot_mesh=True,
                   x_label='y (m)',
                   filled_contours=True,
                   y_label='z (m)',
                   fig_size=(30,3),
                   record=-1,
                   aspect_ratio='equal',
                   nv=9,
                   cbar_ticks=cbar_ticks,
                   fig_name='img/fondsigma')

        # Closing files
        res_vnv_1_t3dres.close()
