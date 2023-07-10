
"""
Validation script for flume_frazil
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
from data_manip.extraction.telemac_file import TelemacFile
from data_manip.computation.datetimes import compute_datetimes

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 3
        self.tags = ['telemac3d', 'khione']

    def _pre(self):
        """
        Defining the studies
        """

        # explicit thermal growth
        self.add_study('vnv_2', 'telemac3d', 't3d_frazil_growth.cas')
        cas = TelemacCas('t3d_frazil_growth.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('vnv_3', 'telemac3d', 't3d_frazil_growth.cas', cas=cas)
        del cas

        # implicit thermal growth
        cas = TelemacCas('t3d_frazil_growth.cas', get_dico('telemac3d'))
        cas.set('KHIONE STEERING FILE', 'ice_frazil_growth_implicit.cas')
        self.add_study('vnv_4', 'telemac3d', 't3d_frazil_growth.cas', cas=cas)
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('vnv_5', 'telemac3d', 't3d_frazil_growth.cas', cas=cas)
        del cas

        # thermal growth with salinity
        cas = TelemacCas('t3d_frazil_growth_sal.cas', get_dico('telemac3d'))
        self.add_study('vnv_6', 'telemac3d', 't3d_frazil_growth.cas', cas=cas)
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('vnv_7', 'telemac3d', 't3d_frazil_growth.cas', cas=cas)
        del cas


    def _check_results(self):
        """
        Post-treatment processes
        """
        # Explicit thermal growth
        self.check_epsilons('vnv_2:T3DRES', 'f3d_growth_explicit.slf', eps=[1.E-6])
        self.check_epsilons('vnv_3:T3DRES', 'f3d_growth_explicit.slf', eps=[1.E-6])
        self.check_epsilons('vnv_2:T3DRES', 'vnv_3:T3DRES', eps=[1.E-6])
        # Explicit thermal growth
        self.check_epsilons('vnv_2:ICERES', 'fce_growth_explicit.slf', eps=[1.E-4])
        self.check_epsilons('vnv_3:ICERES', 'fce_growth_explicit.slf', eps=[1.E-4])
        self.check_epsilons('vnv_2:ICERES', 'vnv_3:ICERES', eps=[1.E-4])

        # Implicit thermal growth
        self.check_epsilons('vnv_4:T3DRES', 'f3d_growth_implicit.slf', eps=[1.E-6])
        self.check_epsilons('vnv_5:T3DRES', 'f3d_growth_implicit.slf', eps=[1.E-6])
        self.check_epsilons('vnv_4:T3DRES', 'vnv_5:T3DRES', eps=[1.E-6])
        # Implicit thermal growth
        self.check_epsilons('vnv_4:ICERES', 'fce_growth_implicit.slf', eps=[1.E-4])
        self.check_epsilons('vnv_5:ICERES', 'fce_growth_implicit.slf', eps=[1.E-4])
        self.check_epsilons('vnv_4:ICERES', 'vnv_5:ICERES', eps=[1.E-4])

        # Implicit thermal growth with salinity
        self.check_epsilons('vnv_6:T3DRES', 'f3d_growth_salinity.slf', eps=[1.E-6])
        self.check_epsilons('vnv_7:T3DRES', 'f3d_growth_salinity.slf', eps=[1.E-6])
        self.check_epsilons('vnv_6:T3DRES', 'vnv_7:T3DRES', eps=[1.E-6])
        # Implicit thermal growth with salinity
        self.check_epsilons('vnv_6:ICERES', 'fce_growth_salinity.slf', eps=[1.E-4])
        self.check_epsilons('vnv_7:ICERES', 'fce_growth_salinity.slf', eps=[1.E-4])
        self.check_epsilons('vnv_6:ICERES', 'vnv_7:ICERES', eps=[1.E-4])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot2d import plot2d_scalar_filled_contour
        from postel.plot_vnv import vnv_plot2d,\
            vnv_plot1d_polylines
        import datetime
        import matplotlib.pyplot as plt

        geo, _ = self.get_study_res('vnv_2:T3DGEO', load_bnd=True, module='T3D')
        res_vnv_2 = TelemacFile(self.get_study_file('vnv_2:T3DRES'))
        res_ice_vnv_2 = TelemacFile(self.get_study_file('vnv_2:ICERES'))
        res_vnv_3 = TelemacFile(self.get_study_file('vnv_3:T3DRES'))
        res_ice_vnv_3 = TelemacFile(self.get_study_file('vnv_3:ICERES'))
        res_vnv_6 = TelemacFile(self.get_study_file('vnv_6:T3DRES'))
        res_ice_vnv_6 = TelemacFile(self.get_study_file('vnv_6:ICERES'))

        #======================================================================
        # DESCRIPTION PLOTS:
        #
        #Plotting mesh
        vnv_plot2d(\
            'BOTTOM',
            geo,
            record=0,
            plot_mesh=True,
            annotate_bnd=True,
            filled_contours=False,
            fig_size=(14, 2.5),
            fig_name='img/mesh',
            x_label='$x$ $(m)$',
            y_label='$y$ $(m)$',)

        # Plotting bottom
        vnv_plot2d(\
            'BOTTOM',
            geo,
            record=0,
            filled_contours=True,
            fig_size=(14, 2.5),
            fig_name='img/bottom',
            x_label='$x$ $(m)$',
            y_label='$y$ $(m)$',
            cbar_label='Bottom (m)')

        #======================================================================
        # FIRST OBSERVATION:
        #
        vnv_plot2d(\
            'VELOCITY U',
            res_vnv_2,
            record=-1,
            filled_contours=True,
            fig_size=(14, 2.5),
            fig_name='img/U-2d_scalarmap',
            x_label='$x$ $(m)$',
            y_label='$y$ $(m)$',
            cbar_label='$U$ $(m/s)$',
            plane=4)

        # Plotting frazil map
        vnv_plot2d(\
            'FRAZIL',
            res_ice_vnv_2,
            record=-1,
            filled_contours=True,
            fig_size=(14, 2.5),
            fig_name='img/Cf-2d_scalarmap',
            x_label='$x$ $(m)$',
            y_label='$y$ $(m)$',
            cbar_label='$C_f$ (volume fraction)',
            plane=4)

        # Plotting temperature map
        vnv_plot2d(\
            'TEMPERATURE',
            res_ice_vnv_2,
            record=-1,
            filled_contours=True,
            fig_size=(14, 2.5),
            fig_name='img/T-2d_scalarmap',
            x_label='$x$ $(m)$',
            y_label='$y$ $(m)$',
            cbar_label='$T$ $(^\circ {C})$',
            plane=4)

        # Plotting frazil map
        vnv_plot2d(\
            'FRAZIL S',
            res_ice_vnv_2,
            record=-1,
            filled_contours=True,
            fig_size=(14, 2.5),
            fig_name='img/Cf-surface_scalarmap',
            x_label='$x$ $(m)$',
            y_label='$y$ $(m)$',
            cbar_label='$C_f$ (volume fraction)',
            plane=4)

        # Plotting temperature map
        vnv_plot2d(\
            'TEMPERATURE S',
            res_ice_vnv_2,
            record=-1,
            filled_contours=True,
            fig_size=(14, 2.5),
            fig_name='img/T-surface_scalarmap',
            x_label='$x$ $(m)$',
            y_label='$y$ $(m)$',
            cbar_label='$T$ $(^\circ {C})$',
            plane=4)

        vnv_plot1d_polylines(\
                'TEMPERATURE S',
                [res_ice_vnv_2,res_ice_vnv_6],
                record=-1,
                fig_name='img/temperature_salinity',
                legend_labels=['Pure water','Saline water'])



        # Closing files
        geo.close()
        res_vnv_2.close()
        res_vnv_3.close()
        res_vnv_6.close()

