#include "TECADDON.h"
#include "ADDGLBL.h"
#if !defined (MSWIN)
#include <unistd.h>
#endif
#include "GUIDEFS.h"



LgIndex_t LoadMode = 1;
StringList_pa FileNames;
Boolean_t DoneWithDialog2;


/**
 */
static void Dialog2HelpButton_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecUtilDialogMessageBox("On-line Help not available for this dialog.",
                          MessageBox_Information);
  TecUtilLockFinish(AddOnID);
}


/**
 */
static void Dialog2CancelButton_CB(void)
{
  TecGUIDialogDrop(Dialog2Manager);
/* Modal Dialogs must call TecUtilLockStart prior to coming */
/* up and then call TecUtilLockFinish when the Ok or Cancel */
/* button is pressed.  Only TecUtilLockFinish is supplied here. */
  DoneWithDialog2 = TRUE;

  TecUtilLockFinish(AddOnID);
}


/**
 */
static void Dialog2OkButton_CB(void)
{
    Boolean_t IsOk;
  TecGUIDialogDrop(Dialog2Manager);
/* Modal Dialogs must call TecUtilLockStart prior to coming */
/* up and then call TecUtilLockFinish when the Ok or Cancel */
/* button is pressed.  Only TecUtilLockFinish is supplied here. */
  LoadMode = TecGUIRadioBoxGetToggle(LoadMode_RADIO_D2);
  TecGUIDialogDrop(Dialog2Manager);
  DoneWithDialog2 = TRUE;

  /*
  IsOk = TecUtilDialogGetFileNames(SelectFileOption_AllowMultiFileRead,
                            &FileNames, "Data Files.", NULL,"*"); 
			    */

  TecUtilLockFinish(AddOnID);
}


/**
 */
static void Dialog2Init_CB(void)
{
/* Modal Dialogs must call TecUtilLockStart prior to coming */
/* up and then call TecUtilLockFinish when the Ok or Cancel */
/* button is pressed. */
  TecUtilLockStart(AddOnID);
/* <<< Add init code (if necessary) here>>> */
}


/**
 */
static void LoadMode_RADIO_D2_CB(const LgIndex_t *I)
{
  TecUtilLockStart(AddOnID);
  TRACE1("RadioBox (LoadMode_RADIO_D2) Value Changed,  New value is: %d\n",*I);
  TecUtilLockFinish(AddOnID);
}




#include "guibld.c"
