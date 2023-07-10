#include "TECADDON.h"
#include "ADDGLBL.h"
#if !defined (MSWIN)
#include <unistd.h>
#endif
#include "GUIDEFS.h"





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
static void Dialog1Init_CB(void)
{
  TecUtilLockStart(AddOnID);
/* <<< Add init code (if necessary) here>>> */
  TecUtilLockFinish(AddOnID);
}


/**
 */
static LgIndex_t  FileName_TF_D1_CB(const char *S)
{
  LgIndex_t IsOk = 1;
  TecUtilLockStart(AddOnID);
  TRACE1("Text field (FileName_TF_D1) Value Changed,  New value is: %s\n",S);
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}


/**
 */
static void Browse_BTN_D1_CB(void)
{
  char *SelectedFileName = NULL;
  char *Type = "My Data";
  char *Filter = "*.map";

  TecUtilLockStart(AddOnID);
  TRACE("Browse Button Pushed\n");
  if ( TecUtilDialogGetFileName(SelectFileOption_ReadSingleFile,
	                        &SelectedFileName,
				Type,
				"",
				Filter))
  {
      TecGUITextFieldSetString(FileName_TF_D1,SelectedFileName);
      TecUtilStringDealloc(&SelectedFileName);
  
  }



  TecUtilLockFinish(AddOnID);

}


/**********************************************************************
 */
static void OKButton_BTN_D1_CB(void)
/**********************************************************************
 */
{
      FILE *f ;
      char *FileName;
      Boolean_t IsOk ;
      Boolean_t add_delwaq_data();
      char *ErrMsg;

      char str1[40];

/**********************************************************************
 */
      TecUtilLockStart(AddOnID);
      TRACE("Load File Button Pushed\n");

      // get the file name from the text field
      FileName = TecGUITextFieldGetString(FileName_TF_D1);
      TRACE(FileName);
      // check the file name 
      if (FileName != NULL && strlen(FileName) > 0 ) 
      {
	  IsOk = TRUE ;
	  TRACE("File name ok.\n");
      }
      else
      {
	  TecUtilDialogErrMsg("Choose a file name.");
	  IsOk = FALSE ;
      }
      // if file name not void, try to open :
      if ( IsOk == TRUE ) 
      {
	 TRACE("Try to open the file :  \n ");
	 f = fopen(FileName,"rb"); 
	 if ( f == NULL ) 
	 {
	     TecUtilDialogErrMsg("Invalid File Name");
	 }
	 else
	 {
	     // try to read the file and add the data
	     // to the current data set.
	     TRACE("Try to read the data : \n");
  	     IsOk = add_delwaq_data(f,ErrMsg);
	     TRACE("End of data read \n");
	     if(!IsOk)
	     {
         	  TecUtilDialogErrMsg(*ErrMsg);
	     }
	     else
                 TecGUIDialogDrop(Dialog1Manager);

	 }
      }

      TecUtilLockFinish(AddOnID);
}

/**********************************************************************
 */


#include "guibld.c"
