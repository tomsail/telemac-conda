
"""
Validation script for bump2d
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
from data_manip.extraction.telemac_file import TelemacFile
from math import atan, degrees

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

        # Evolution of a bump in 2D T2D+GAI scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_bump2d-t2d.cas')


        # Evolution of a bump in 2D T2D+GAI parallel mode
        cas = TelemacCas('t2d_bump2d-t2d.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_bump2d-t2d_par.cas',
                       cas=cas)

        del cas

    def _check_results(self):
        """
        Post-treatment processes
        """

        self.check_epsilons('vnv_1:GAIRES',
                            'gai_ref_bump2d-t2d.slf',
                            eps=[5e-4])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:GAIRES',
                            'gai_ref_bump2d-t2d.slf',
                            eps=[1e-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:GAIRES',
                            'vnv_2:GAIRES',
                            eps=[1.e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_bump2d-t2d.slf',
                            eps=[1.e-4])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_bump2d-t2d.slf',
                            eps=[1.e-4])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1.e-4])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d
        from postel.plot2d import plot2d_scalar_filled_contour,plot2d_scalar_contour
        import matplotlib.pyplot as plt
        import numpy as np
        vnv1_t2dres = self.get_study_file('vnv_1:GAIRES')
        res_vnv_1_t2dres = TelemacFile(vnv1_t2dres)

        vnv1_gaires = self.get_study_file('vnv_1:GAIRES')
        res_vnv_1_gaires = TelemacFile(vnv1_gaires)

        # Plotting bottom at record 0 with mesh
        vnv_plot2d('BOTTOM',
                   res_vnv_1_t2dres,
                   record=0,
                   filled_contours=True,
                   plot_mesh=True,
                   fig_size=(9, 7),
                   fig_name='img/bottom')

        #########################################################################
        #
        # factor is used for scaling (evolution/h0_factor = evolutionCut)
        # See the paper of Yen et al. 1995
        #########################################################################

        slf = res_vnv_1_gaires
        mesh = np.array(slf.ikle3)
        meshx = slf.meshx
        meshy = slf.meshy

        #last time step
        timePos = -1
        bottomEnd = slf.get_data_value('BOTTOM', timePos)
        t_end = slf.times[timePos]
        t_end = "t = %1.2f h" %(t_end/3600)

        #near the end
        near_last_index = 3
        timePos = slf.ntimestep - near_last_index
        bottom = slf.get_data_value('BOTTOM', timePos)
        t_near_end = slf.times[timePos]
        t_near_end = "t = %1.2f h" %(t_near_end/3600)

        #Plot the bottom elevation at the end
        fig,ax = plt.subplots()
        fig2,ax2 = plt.subplots()
        plot2d_scalar_filled_contour(fig2, ax2,
                slf.tri, bottomEnd,
                data_name='Bottom (m)')

        #Get and plot the isolines for bottom = 0.1
        levels = np.asarray([-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9])
        triplotcontour = ax.tricontour(meshx,meshy,mesh,bottomEnd,levels,extend='both',alpha=0.0)
        triplotcontourf = ax.tricontourf(meshx,meshy,mesh,bottomEnd,levels,extend='both')
        p = triplotcontour.collections[4].get_paths()[0]
        v = p.vertices
        ax2.plot(v[:,0],v[:,1],"k-.",label=t_end)

        #Get the point of the isoline with the minimal y-coordinate value and plot it
        minXYEnd = [10.0E+20,10.0E+20]
        for xy in v:
          if (xy[1]<minXYEnd[1]):
             minXYEnd[0] = xy[0]
             minXYEnd[1] = xy[1]
        triplotcontour = ax.tricontour(meshx,meshy,mesh,bottom,levels,extend='both',alpha=0.0)
        p = triplotcontour.collections[4].get_paths()[0]
        v = p.vertices
        ax2.plot(v[:,0],v[:,1],"k--",label=t_near_end)

        #Same thing for the time frame near the end
        minXY = [10.0E+20,10.0E+20]
        for xy in v:
          if minXY[1] > xy[1]:
             minXY[0] = xy[0]
             minXY[1] = xy[1]
        ax2.plot(minXY[0],minXY[1],"ko")
        ax2.plot(minXYEnd[0],minXYEnd[1],"ko")
        ax2.plot([0.0,1000.0],[0.0,0.0],"k-")

        #Get the line equation given by the two points
        a0 = minXY[0]
        a1 = minXY[1]
        b0 = minXYEnd[0]
        b1 = minXYEnd[1]

        m = (a1-b1)/(a0 - b0)
        b = a1-m*a0

        #Compute spread angle
        alpha = degrees(atan(abs(a1-b1)/abs(a0-b0)))

        #Plot the line
        xLine = [400,500,900]
        yLine = [value *m + b for value in xLine]
        ax2.plot(xLine,yLine,"k-")
        ax2.plot([minXY[0],minXYEnd[0]],[minXY[1],minXYEnd[1]],"g-")
        ax2.set_title(r"spread angle $\alpha$ = %1.2f"%(alpha))

        #Save figure
        ax2.legend()
        fig_name = 'img/post_bump2d'
        print(" "*8+"~> Plotting "+fig_name)
        plt.savefig(fig_name + ".png")
        plt.close('all')
