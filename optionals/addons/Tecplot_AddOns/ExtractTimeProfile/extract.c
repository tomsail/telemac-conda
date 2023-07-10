#include "TECADDON.h"
#include "ADDGLBL.h"

static Boolean_t IsTransientDataSet()
{
      return (TecUtilDataSetGetMaxStrandID()>0);
}

static void Dialog1Init_CB(void)
/*************************************************************************
*************************************************************************/
{
  TecUtilLockStart(AddOnID);
/* <<< Add init code (if necessary) here>>> */

  TecGUISetVisibility(Z_TF_D1,
	           TecUtilFrameGetPlotType() == PlotType_Cartesian3D);
  TecGUISetVisibility(LBL8_LBL_D1,
	           TecUtilFrameGetPlotType() == PlotType_Cartesian3D);

  TecGUISetSensitivity(DoExtraction_BTN_D1,
	           (TecUtilFrameGetPlotType() == PlotType_Cartesian3D ||
	           TecUtilFrameGetPlotType() == PlotType_Cartesian2D) &&
		   IsTransientDataSet());


  TecGUITextFieldSetDouble(X_TF_D1,0.0,"%f");
  TecGUITextFieldSetDouble(Y_TF_D1,0.0,"%f");
  TecGUITextFieldSetDouble(Z_TF_D1,0.0,"%f");
  TecUtilLockFinish(AddOnID);

}


/**
 */
static void DoExtraction_BTN_D1_CB(void)
/*************************************************************************
 * Function : Get the coordinates of the points to extract, create a
 * new zone, do the extraction and fill the data in this new zone
 * created.
*************************************************************************/
{
  Boolean_t    IsOk   ;
  double X ;
  double Y ;
  double Z ;

  Boolean_t extraction1D();

/************************************************************************/

  TecUtilLockStart(AddOnID);

  TRACE("Extract Button Pushed\n");

    IsOk = TecGUITextFieldGetDouble(X_TF_D1,&X);
    IsOk = TecGUITextFieldGetDouble(Y_TF_D1,&Y);
    IsOk = TecGUITextFieldGetDouble(Z_TF_D1,&Z);

    if( !IsTransientDataSet())
    {
        TecUtilDialogMessageBox("Frame contains no transient data",
		MessageBox_Information);
        return;
    }
    if(!(TecUtilFrameGetPlotType() == PlotType_Cartesian3D ||
	 TecUtilFrameGetPlotType() == PlotType_Cartesian2D))
    {
        TecUtilDialogMessageBox("Plot type should be Cartesian 2D or 3D",
		MessageBox_Information);
        return;
    }

    IsOk = extraction1D(X,Y,Z);

  // remove box ....


  TecUtilLockFinish(AddOnID);
}


#include "guibld.c"
