#include "TECADDON.h"
#include "ADDGLBL.h"
#if !defined (MSWIN)
#include <unistd.h>
#endif
#include "GUIDEFS.h"

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

  TecGUISetSensitivity(Z1_TF_T1_2,
	           TecUtilFrameGetPlotType() == PlotType_Cartesian3D);
  TecGUISetSensitivity(LZ1_LBL_T1_2,
	           TecUtilFrameGetPlotType() == PlotType_Cartesian3D);
  TecGUISetSensitivity(Z2_TF_T1_2,
	           TecUtilFrameGetPlotType() == PlotType_Cartesian3D);
  TecGUISetSensitivity(LZ2_LBL_T1_2,
	           TecUtilFrameGetPlotType() == PlotType_Cartesian3D);
  TecGUISetSensitivity(Extract1D_BTN_T2_2,
	           (TecUtilFrameGetPlotType() == PlotType_Cartesian3D ||
	           TecUtilFrameGetPlotType() == PlotType_Cartesian2D) &&
		   IsTransientDataSet());

  TecGUISetSensitivity(Z_TF_T2_2,
	           TecUtilFrameGetPlotType() == PlotType_Cartesian3D);
  TecGUISetSensitivity(LZ_LBL_T2_2,
	           TecUtilFrameGetPlotType() == PlotType_Cartesian3D);
  TecGUISetSensitivity(Extract1D_BTN_T2_2,
	           (TecUtilFrameGetPlotType() == PlotType_Cartesian3D ||
	           TecUtilFrameGetPlotType() == PlotType_Cartesian2D) &&
		   IsTransientDataSet());

  TecGUITextFieldSetDouble(X_TF_T2_2,0.0,"%f");
  TecGUITextFieldSetDouble(Y_TF_T2_2,0.0,"%f");
  TecGUITextFieldSetDouble(Z_TF_T2_2,0.0,"%f");
  TecGUITextFieldSetDouble(X1_TF_T1_2,0.0,"%f");
  TecGUITextFieldSetDouble(Y1_TF_T1_2,0.0,"%f");
  TecGUITextFieldSetDouble(Z1_TF_T1_2,0.0,"%f");
  TecGUITextFieldSetDouble(X2_TF_T1_2,0.0,"%f");
  TecGUITextFieldSetDouble(Y2_TF_T1_2,0.0,"%f");
  TecGUITextFieldSetDouble(Z2_TF_T1_2,0.0,"%f");
  TecGUITextFieldSetLgIndex(NBPT_TF_T1_2,10,FALSE);
  TecGUIToggleSet(ResIsTrans_TOG_T1_2,FALSE);
  TecUtilLockFinish(AddOnID);

}


/**
 */
static void Extract2D_BTN_T1_2_CB(void)
/*************************************************************************
 * Function : Get the coordinates of the points to extract, create a
 * new zone, do the extraction and fill the data in this new zone
 * created.
*************************************************************************/
{
  Boolean_t    IsOk   ;
  double X[2] ;
  double Y[2];
  double Z[2] ;
  int NbPoints;
  int tralala;
  Boolean_t ResIsTransient ;

  Boolean_t extraction();

  char str[50];
  char Command[200];

/************************************************************************/

  TecUtilLockStart(AddOnID);

  TRACE("Extract Button Pushed\n");


  TRACE("Extract Button Pushed\n");

    IsOk = TecGUITextFieldGetDouble(X1_TF_T1_2,&X[0]);
    IsOk = TecGUITextFieldGetDouble(Y1_TF_T1_2,&Y[0]);
    IsOk = TecGUITextFieldGetDouble(Z1_TF_T1_2,&Z[0]);
    IsOk = TecGUITextFieldGetDouble(X2_TF_T1_2,&X[1]);
    IsOk = TecGUITextFieldGetDouble(Y2_TF_T1_2,&Y[1]);
    IsOk = TecGUITextFieldGetDouble(Z2_TF_T1_2,&Z[1]);
    IsOk = TecGUITextFieldGetLgIndex(NBPT_TF_T1_2,&NbPoints);
    ResIsTransient = TecGUIToggleGet(ResIsTrans_TOG_T1_2);

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
    if(NbPoints < 2) 
    {
        TecUtilDialogMessageBox(
		"You should extract at least 2 points on the line.",
		MessageBox_Information);
	return;
    }
    sprintf(str," Number of points : %d\n",NbPoints);
    TRACE(str);

    if(X[0] == X[1] && Y[0] == Y[1] && Z[0] == Z[1]) 
    {
        TecUtilDialogMessageBox(
		"The start point should be different from the end point",
		MessageBox_Information);
	return;
    }
    sprintf(str," Number of points : %d\n",NbPoints);
    TRACE(str);



    IsOk = extraction(X,Y,Z,NbPoints,ResIsTransient);

  // remove box ....
  //
  TRACE("end of extraction \n");
  if ( IsOk) 
  {
      TRACE("extraction returned OK\n");
      //if ( TecUtilMarcroIsRecordingActive() == TRUE)
      if ( TecUtilMacroIsRecordingActive() )
      {
	  if(ResIsTransient == TRUE) tralala = 1;
	  else tralala = 0;
	  sprintf(Command,"%d %d %lf %lf %lf %lf %lf %lf",
		         tralala,NbPoints,
			 X[0],Y[0],Z[0],
			 X[1],Y[1],Z[1]);
	  TecUtilMacroRecordAddOnCommand("ExtractTimeProfileFromLine",Command);
      }
  }



  TecUtilLockFinish(AddOnID);
}

static void Extract1D_BTN_T2_2_CB(void)
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
  char Command[200];

  Boolean_t extraction();

/************************************************************************/

  TecUtilLockStart(AddOnID);

  TRACE("Extract Button Pushed\n");

    IsOk = TecGUITextFieldGetDouble(X_TF_T2_2,&X);
    IsOk = TecGUITextFieldGetDouble(Y_TF_T2_2,&Y);
    IsOk = TecGUITextFieldGetDouble(Z_TF_T2_2,&Z);

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

    IsOk = extraction(&X,&Y,&Z,1,FALSE);
    TRACE("fin appel extraction\n");
  if ( IsOk) 
  {
      if ( TecUtilMacroIsRecordingActive() )
      {
	  TRACE("recording macro command ...\n");
	  sprintf(Command,"%lf %lf %lf",X,Y,Z);
	  TecUtilMacroRecordAddOnCommand("ExtractTimeProfileFromPoint",Command);
      }
  }

  // remove box ....


  TecUtilLockFinish(AddOnID);
}

/*************************************************************************
 ************************************************************************/


/**
 *  */
static void ResIsTrans_TOG_T1_2_CB(const LgIndex_t *I)
{
  TecUtilLockStart(AddOnID);
  TRACE1("Toggle (ResIsTrans_TOG_T1_2) Value Changed,  New value is: %d\n",*I);
  TecUtilLockFinish(AddOnID);
}



/**
 */
static LgIndex_t  NBPT_TF_T1_2_CB(const char *S)
{
  LgIndex_t IsOk = 1;
  TecUtilLockStart(AddOnID);
  TRACE1("Text field (NBPT_TF_T1_2) Value Changed,  New value is: %s\n",S);
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}


/**
 */
static LgIndex_t  X1_TF_T1_2_CB(const char *S)
{
  LgIndex_t IsOk = 1;
  TecUtilLockStart(AddOnID);
  TRACE1("Text field (X1_TF_T1_2) Value Changed,  New value is: %s\n",S);
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}


/**
 */
static LgIndex_t  Y1_TF_T1_2_CB(const char *S)
{
  LgIndex_t IsOk = 1;
  TecUtilLockStart(AddOnID);
  TRACE1("Text field (Y1_TF_T1_2) Value Changed,  New value is: %s\n",S);
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}


/**
 */
static LgIndex_t  Z1_TF_T1_2_CB(const char *S)
{
  LgIndex_t IsOk = 1;
  TecUtilLockStart(AddOnID);
  TRACE1("Text field (Z1_TF_T1_2) Value Changed,  New value is: %s\n",S);
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}


/**
 */
static LgIndex_t  X2_TF_T1_2_CB(const char *S)
{
  LgIndex_t IsOk = 1;
  TecUtilLockStart(AddOnID);
  TRACE1("Text field (X2_TF_T1_2) Value Changed,  New value is: %s\n",S);
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}


/**
 */
static LgIndex_t  Y2_TF_T1_2_CB(const char *S)
{
  LgIndex_t IsOk = 1;
  TecUtilLockStart(AddOnID);
  TRACE1("Text field (Y2_TF_T1_2) Value Changed,  New value is: %s\n",S);
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}


/**
 */
static LgIndex_t  Z2_TF_T1_2_CB(const char *S)
{
  LgIndex_t IsOk = 1;
  TecUtilLockStart(AddOnID);
  TRACE1("Text field (Z2_TF_T1_2) Value Changed,  New value is: %s\n",S);
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}


/**
 */


/**
 */
static LgIndex_t  X_TF_T2_2_CB(const char *S)
{
  LgIndex_t IsOk = 1;
  TecUtilLockStart(AddOnID);
  TRACE1("Text field (X_TF_T2_2) Value Changed,  New value is: %s\n",S);
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}


/**
 */
static LgIndex_t  Y_TF_T2_2_CB(const char *S)
{
  LgIndex_t IsOk = 1;
  TecUtilLockStart(AddOnID);
  TRACE1("Text field (Y_TF_T2_2) Value Changed,  New value is: %s\n",S);
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}


/**
 */
static LgIndex_t  Z_TF_T2_2_CB(const char *S)
{
  LgIndex_t IsOk = 1;
  TecUtilLockStart(AddOnID);
  TRACE1("Text field (Z_TF_T2_2) Value Changed,  New value is: %s\n",S);
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}


/**
 */
static void Dialog1HelpButton_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecUtilDialogMessageBox("On-line Help not available for this dialog.",
                          MessageBox_Information);
  TecUtilLockFinish(AddOnID);
}


/**
 */
static void Dialog1CloseButton_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecGUIDialogDrop(Dialog1Manager);
  TecUtilLockFinish(AddOnID);
}



/**
 */
static void TAB1_TBA_D1_CB(const LgIndex_t *I)
{
  TecUtilLockStart(AddOnID);
  TRACE0("Activate callback for tab (TAB1_TBA_D1) called\n");
  TecUtilLockFinish(AddOnID);
}


/**
 */
static void TAB1_TBD_D1_CB(const LgIndex_t *I)
{
  TecUtilLockStart(AddOnID);
  TRACE0("Deactivate callback for tab (TAB1_TBD_D1) called\n");
  TecUtilLockFinish(AddOnID);
}




#include "guibld.c"
