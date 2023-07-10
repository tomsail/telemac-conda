
"""
Validation script for waq3d_aed2_flume
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
        self.rank = 0
        self.tags = ['telemac3d', 'waqtel', 'aed']

    def _pre(self):
        """
        Defining the studies
        """

        # water quality in 3D - based on AED2
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_waq3d_aed2_flume.cas')


        # water quality in 3D parallel - based on AED2
        cas = TelemacCas('t3d_waq3d_aed2_flume.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_waq3d_aed2_flume_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_waq3d_aed2_flume.slf',
                            eps=[1.e-6, 1.e-5, 1.e-5, 4e-3,
                                 1.e-8, 2.e-5, 1.e-7, 1.e-6,
                                 1.e-7, 1.e-6, 1.e-7, 1.e-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_waq3d_aed2_flume.slf',
                            eps=[1.e-6, 1.e-5, 1.e-5, 4e-3,
                                 1.e-8, 2.e-5, 1.e-7, 1.e-6,
                                 1.e-7, 1.e-6, 1.e-7, 1.e-8])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1.e-6, 1.e-5, 1.e-5, 4e-3,
                                 1.e-8, 2.e-5, 1.e-7, 1.e-6,
                                 1.e-7, 1.e-6, 1.e-7, 1.e-8])


    def _post(self):
        """
        Post-treatment processes
        """

        from postel.plot_vnv import vnv_plot2d
        # Getting files
        vnv_1_t3dres = self.get_study_file('vnv_1:T3DRES')
        res = TelemacFile(vnv_1_t3dres)

        #Plotting mesh
        vnv_plot2d('',
                   res,
                   plot_mesh=True,
                   fig_size=(15, 4),
                   fig_name='img/res_mesh')

        # Plotting 3D mesh section (vertical mesh)
        vnv_plot2d('ELEVATION Z',
                   res,
                   poly=[[0., 50], [500, 50]],
                   record=0,
                   plot_mesh=True,
                   x_label='$x$ (m)', y_label='$z$ (m)',
                   fig_size=(20, 5),
                   fig_name='img/res_mesh_section')

        # Plotting final condition for temperature (at -1)
        vnv_plot2d('TEMPERATURE',
                   res,
                   poly=[[0., 50], [500, 50]],
                   record=-1,
                   filled_contours=True,
                   x_label='$x$ (m)', y_label='$z$ (m)',
                   cbar_label='$T$ ($^{\circ}$C)',
                   fig_size=(20, 5),
                   fig_name='img/res_temp')

        # Plotting final condition for oxygen (OXY_oxy) (at -1)
        vnv_plot2d('OXY_oxy',
                   res,
                   poly=[[0., 50], [500, 50]],
                   record=-1,
                   filled_contours=True,
                   x_label='$x$ (m)', y_label='$z$ (m)',
                   cbar_label='O$_2$ (mmol/m$^3$)',
                   fig_size=(20, 5),
                   fig_name='img/res_oxy')

        # Plotting final condition for ammonium (NIT_amm) (at -1)
        vnv_plot2d('NIT_amm',
                   res,
                   poly=[[0., 50], [500, 50]],
                   record=-1,
                   filled_contours=True,
                   x_label='$x$ (m)', y_label='$z$ (m)',
                   cbar_label='NH$_4$ (mmol/m$^3$)',
                   fig_size=(20, 5),
                   fig_name='img/res_ammonium')

        # Plotting final condition for nitrate (NIT_nit) (at -1)
        vnv_plot2d('NIT_nit',
                   res,
                   poly=[[0., 50], [500, 50]],
                   record=-1,
                   filled_contours=True,
                   x_label='$x$ (m)', y_label='$z$ (m)',
                   cbar_label='NO$_3$ (mmol/m$^3$)',
                   fig_size=(20, 5),
                   fig_name='img/res_nitrate')

        # Plotting final condition for phosphate (PHS_frp) (at -1)
        vnv_plot2d('PHS_frp',
                   res,
                   poly=[[0., 50], [500, 50]],
                   record=-1,
                   filled_contours=True,
                   x_label='$x$ (m)', y_label='$z$ (m)',
                   cbar_label='PO$_4$ (mmol/m$^3$)',
                   fig_size=(20, 5),
                   fig_name='img/res_phosphate')

        # Plotting final condition for DOP (OGM_dop) (at -1)
        vnv_plot2d('OGM_dop',
                   res,
                   poly=[[0., 50], [500, 50]],
                   record=-1,
                   filled_contours=True,
                   x_label='$x$ (m)', y_label='$z$ (m)',
                   cbar_label='DOP (mmol/m$^3$)',
                   fig_size=(20, 5),
                   fig_name='img/res_dop')

        # Plotting final condition for DON (OGM_don) (at -1)
        vnv_plot2d('OGM_don',
                   res,
                   poly=[[0., 50], [500, 50]],
                   record=-1,
                   filled_contours=True,
                   x_label='$x$ (m)', y_label='$z$ (m)',
                   cbar_label='DON (mmol/m$^3$)',
                   fig_size=(20, 5),
                   fig_name='img/res_don')

        # Plotting final condition for POP (OGM_pop) (at -1)
        vnv_plot2d('OGM_pop',
                   res,
                   poly=[[0., 50], [500, 50]],
                   record=-1,
                   filled_contours=True,
                   x_label='$x$ (m)', y_label='$z$ (m)',
                   cbar_label='POP (mmol/m$^3$)',
                   fig_size=(20, 5),
                   fig_name='img/res_pop')

        res.close()
