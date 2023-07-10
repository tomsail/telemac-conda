#include "TECADDON.h"
#include "ADDGLBL.h"
#if !defined (MSWIN)
#include <unistd.h>
#endif
#include "GUIDEFS.h"




LgIndex_t     DataSetLoadMode = 1;
LgIndex_t     FileLoadMode    = 1;

StringList_pa FileNames;
StringList_pa DelFileNames;

Boolean_t     DoneWithDialog1 = FALSE;
Boolean_t     DoneWithDialog2 = FALSE;
Boolean_t     DoneWithDialog3 = FALSE;
Boolean_t     IsOkDialog3 = FALSE;

/*******************************************************************
 * DIALOG3 
 * If the user wishes to load telemac + delwaq data, this will
 * be the dialog for choosing the telemac and delwaq files.
 *******************************************************************/
static void Dialog3HelpButton_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecUtilDialogMessageBox("On-line Help not available for this dialog.",
                          MessageBox_Information);
  TecUtilLockFinish(AddOnID);
}


/*******************************************************************/
static void Dialog3CancelButton_CB(void)
/*******************************************************************/
{
  DoneWithDialog3 = TRUE;
  IsOkDialog3 = FALSE;
  TecGUIDialogDrop(Dialog3Manager);
/* Modal Dialogs must call TecUtilLockStart prior to coming */
/* up and then call TecUtilLockFinish when the Ok or Cancel */
/* button is pressed.  Only TecUtilLockFinish is supplied here. */
  TecUtilLockFinish(AddOnID);
/*******************************************************************/
}
/*******************************************************************/


/******************************************************************* 
 * OK button : get the file name lists for Delwaq and Telemac.
 *******************************************************************/
static void Dialog3OkButton_CB(void)
{
/*******************************************************************/
  char *FNames;

/*******************************************************************/
  TecGUIDialogDrop(Dialog3Manager);

  // get the Telemac file list as a string list :
  FNames = TecGUITextGetString(FTelList_T_D3);
  FileNames = TecUtilStringListFromNLString(FNames);
  TecUtilStringDealloc(&FNames);

  // get the Delwaq file list as a string list :
  FNames = TecGUITextGetString(FDelList_T_D3);
  DelFileNames = TecUtilStringListFromNLString(FNames);
  TecUtilStringDealloc(&FNames);

  DoneWithDialog3 = TRUE;
  IsOkDialog3 = TRUE;
  TecUtilLockFinish(AddOnID);
/*******************************************************************/
}
/*******************************************************************/


/*******************************************************************/
static void Dialog3Init_CB(void)
{
/*******************************************************************/
/* Modal Dialogs must call TecUtilLockStart prior to coming */
/* up and then call TecUtilLockFinish when the Ok or Cancel */
/* button is pressed. */
  TecUtilLockStart(AddOnID);
/* <<< Add init code (if necessary) here>>> */
  TecGUITextSetString(FTelList_T_D3,"");
  TecGUITextSetString(FDelList_T_D3,"");
/*******************************************************************/
}


/*******************************************************************
 * Function : Getting the names of the telemac data files via the
 * Browse button.
 * Put the file names into the associated the multiple line Text
 * Field.
*******************************************************************/
static void BrowsTel_BTN_D3_CB(void)
{
/*******************************************************************/
  Boolean_t IsOk ;
  char *FNames;
  StringList_pa SLFileNames;
/*******************************************************************/
  TecUtilLockStart(AddOnID);
  // call the file selector
  IsOk = TecUtilDialogGetFileNames(
                   SelectFileOption_AllowMultiFileRead,
                   &SLFileNames, "Telemac Data Files.", NULL,"*");

  // Put the file name list into the user text field.
  TecGUITextSetString(FTelList_T_D3,"");
  FNames = TecUtilStringListToNLString(SLFileNames);
  TecGUITextSetString(FTelList_T_D3,FNames);
  TecUtilStringListDealloc(&SLFileNames);
  TecUtilLockFinish(AddOnID);
  return;
/*******************************************************************/
}
/*******************************************************************/


/******************************************************************* 
 * Function :
 * Browse for Delwaq data files. Put the file names into the
 * associated text field.
 *******************************************************************/
static void BrowseDel_BTN_D3_CB(void)
{
/*******************************************************************/
  Boolean_t IsOk ;
  char *FNames;
  StringList_pa SLFileNames;
/*******************************************************************/
  TecUtilLockStart(AddOnID);
  // get the file list via the browser
  IsOk = TecUtilDialogGetFileNames(
                   SelectFileOption_AllowMultiFileRead,
                   &SLFileNames, "Delwaq Data Files.", NULL,"*");

  // Put the file name list into the user text field.:
  // First, reset the text field.
  TecGUITextSetString(FDelList_T_D3,"");
  // Convert the string list into a NL delimeted text.
  FNames = TecUtilStringListToNLString(SLFileNames);
  // Put this text into the Text Field.
  TecGUITextSetString(FDelList_T_D3,FNames);
  // Deallocate Stringlist and string.
  TecUtilStringDealloc(&FNames);
  TecUtilStringListDealloc(&SLFileNames);
  // the end.
  TecUtilLockFinish(AddOnID);
  return;
/*******************************************************************/
}
/*******************************************************************/


/*******************************************************************/
static LgIndex_t  FTelList_T_D3_CB(const char *S)
{
/*******************************************************************/
  LgIndex_t IsOk = 1;
  TecUtilLockStart(AddOnID);
  TecUtilLockFinish(AddOnID);
  return (IsOk);
/*******************************************************************/
}
/*******************************************************************/


/*******************************************************************/
static LgIndex_t  FDelList_T_D3_CB(const char *S)
/*******************************************************************/
{
  LgIndex_t IsOk = 1;
  TecUtilLockStart(AddOnID);
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}
/*******************************************************************/


/*******************************************************************
 * DIALOG2
 * Choice for loading of the data set :
 *  1 -> New data set, reset plot style
 *  2 -> New data set, retain plot style
 *  3 -> Add data set to existing one.
 */
/*******************************************************************/
static void Dialog2HelpButton_CB(void)
/*******************************************************************/
{
  TecUtilLockStart(AddOnID);
  TecUtilDialogMessageBox(
	   "On-line Help not available for this dialog.",
            MessageBox_Information);
  TecUtilLockFinish(AddOnID);
}
/*******************************************************************/


/*******************************************************************/
static void Dialog2CancelButton_CB(void)
/*******************************************************************/
{
  // Dropping the dialog.
  DoneWithDialog2 = TRUE;
  TecGUIDialogDrop(Dialog2Manager);
  TecUtilLockFinish(AddOnID);
}
/*******************************************************************/


/*******************************************************************/
static void Dialog2OkButton_CB(void)
/*******************************************************************/
{
  Boolean_t IsOk;
/*******************************************************************/
  TecGUIDialogDrop(Dialog2Manager);
  // Read the choice of the data set load mode in the radio
  // button box.
  DataSetLoadMode = TecGUIRadioBoxGetToggle(DataSetLoa_RADIO_D2);
  TecGUIDialogDrop(Dialog2Manager);
  DoneWithDialog2 = TRUE;

 /*
  *   IsOk =
  *   TecUtilDialogGetFileNames(SelectFileOption_AllowMultiFileRead,
  *                               &FileNames,
  *                               "Data Files.",
  *                               NULL,"*");
  *                                                           */

  TecUtilLockFinish(AddOnID);
  return;
}
/*******************************************************************/


/*******************************************************************/
static void Dialog2Init_CB(void)
/*******************************************************************/
{
/* Modal Dialogs must call TecUtilLockStart prior to coming */
/* up and then call TecUtilLockFinish when the Ok or Cancel */
/* button is pressed. */
  TecUtilLockStart(AddOnID);
/* <<< Add init code (if necessary) here>>> */
}
/*******************************************************************/


/*******************************************************************/
static void DataSetLoa_RADIO_D2_CB(const LgIndex_t *I)
/*******************************************************************/
{
  TecUtilLockStart(AddOnID);
  TecUtilLockFinish(AddOnID);
}
/*******************************************************************/


/*******************************************************************
 * DIALOG 1
 * 
 *******************************************************************/
/*******************************************************************/
static void Dialog1HelpButton_CB(void)
/*******************************************************************/
{
  TecUtilLockStart(AddOnID);
  TecUtilDialogMessageBox("On-line Help not available for this dialog.",
                          MessageBox_Information);
  TecUtilLockFinish(AddOnID);
}
/*******************************************************************/


/*******************************************************************/
static void Dialog1CancelButton_CB(void)
/*******************************************************************/
{
  TecGUIDialogDrop(Dialog1Manager);
/* Modal Dialogs must call TecUtilLockStart prior to coming */
/* up and then call TecUtilLockFinish when the Ok or Cancel */
/* button is pressed.  Only TecUtilLockFinish is supplied here. */
  TecUtilLockFinish(AddOnID);
}
/*******************************************************************/


/*******************************************************************/
static void Dialog1OkButton_CB(void)
/*******************************************************************/
{
  TecGUIDialogDrop(Dialog1Manager);
  FileLoadMode = TecGUIRadioBoxGetToggle(FileLoadMo_RADIO_D1);
  TecGUIDialogDrop(Dialog1Manager);
  DoneWithDialog1 = TRUE;
  TecUtilLockFinish(AddOnID);
}
/*******************************************************************/


/*******************************************************************/
static void Dialog1Init_CB(void)
/*******************************************************************/
{
/* Modal Dialogs must call TecUtilLockStart prior to coming */
/* up and then call TecUtilLockFinish when the Ok or Cancel */
/* button is pressed. */
  TecUtilLockStart(AddOnID);
/* <<< Add init code (if necessary) here>>> */
}
/*******************************************************************/


/*******************************************************************/
static void FileLoadMo_RADIO_D1_CB(const LgIndex_t *I)
/*******************************************************************/
{
  TecUtilLockStart(AddOnID);
  TecUtilLockFinish(AddOnID);
}
/*******************************************************************/




#include "guibld.c"
