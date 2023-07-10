#include "TECADDON.h"
#include "ADDGLBL.h"
#if !defined (MSWIN)
#include <unistd.h>
#endif
#include "GUIDEFS.h"
#include "ENGINE.h"


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
static void Dialog1CancelButton_CB(void)
{
  TecGUIDialogDrop(Dialog1Manager);
/* Modal Dialogs must call TecUtilLockStart prior to coming */
/* up and then call TecUtilLockFinish when the Ok or Cancel */
/* button is pressed.  Only TecUtilLockFinish is supplied here. */
  TecUtilLockFinish(AddOnID);
}


/**
 */
static void Dialog1OkButton_CB(void)
{
    char *FileName;
    Boolean_t IsOk;

    FileName = TecGUITextFieldGetString(FileName_TF_D1);
    TRACE("Try to read file : ");
    TRACE(FileName);
    TRACE("\n");
    if (FileName != NULL  && strlen(FileName) > 0 ) 
    {
	IsOk = TRUE ;
    }
    else
    {
	TecUtilDialogErrMsg("Please select a file ");
	IsOk = FALSE;
    }
    
    if ( IsOk == TRUE ) 
    {
        StringList_pa Instructions;
	Instructions = TecUtilStringListAlloc();
	TecUtilStringListAppendString(Instructions,"STANDARDSYNTAX");
	TecUtilStringListAppendString(Instructions,"1.0");
	TecUtilStringListAppendString(Instructions,"FILENAME");
	TecUtilStringListAppendString(Instructions,FileName);
	TRACE("Call to loadercallback from dialog1okbutton\n");
        IsOk = LoaderCallback(Instructions);
        TecUtilStringDealloc(&FileName);
	TecUtilStringListDealloc(&Instructions);
	if ( IsOk == TRUE ) 
	{
            TecGUIDialogDrop(Dialog1Manager);
	}
	else
	{
            TecUtilDialogErrMsg("Error loading the file");
	}
        TecUtilLockFinish(AddOnID);
    }

}


/**
 */
static void Dialog1Init_CB(void)
{
/* Modal Dialogs must call TecUtilLockStart prior to coming */
/* up and then call TecUtilLockFinish when the Ok or Cancel */
/* button is pressed. */
  TecUtilLockStart(AddOnID);
/* <<< Add init code (if necessary) here>>> */
}


/**
 */
static void XYPLOT_TOG_D1_CB(const LgIndex_t *I)
{
  TecUtilLockStart(AddOnID);
  TRACE1("Toggle (XYPLOT_TOG_D1) Value Changed,  New value is: %d\n",*I);
  TecUtilLockFinish(AddOnID);
}


/**
 */
static void BrowseButton_BTN_D1_CB(void)
{
    char *SelectedFileName = NULL;
    char *Type = "My dtat";
    char *Filter = "*";

    TecUtilLockStart(AddOnID);
    TRACE("Browse Button Pushed\n");
    if (TecUtilDialogGetFileName(SelectFileOption_ReadSingleFile, 
		                 &SelectedFileName,Type,"",Filter))
    {
	TecGUITextFieldSetString(FileName_TF_D1,SelectedFileName);
	TecUtilStringDealloc(&SelectedFileName);
    }

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




#include "guibld.c"
