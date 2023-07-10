/************************************************************************/
#include "TECADDON.h"
#include "ADDGLBL.h"
#include "TASSERT.h"
#include "ENGINE.h"

/************************************************************************/
Boolean_t STDCALL ConverterCallback(
/************************************************************************/
          char *DataFName,      /* IN */
          char *TempBinFName,   /* IN */
          char **MessageString) /* OUT (if and only if returning FALSE) */
/************************************************************************/
{

  Boolean_t IsOk = TRUE  ;
  FILE      *f           ;
  Boolean_t DoConversion();
  int n;
  char string[500];

/************************************************************************/
  

  TecUtilLockStart(AddOnID);

  *MessageString = TecUtilStringAlloc(1000,
		               "MessageString for SERAFIN Converter") ;

  /*
   strcpy(string,DataFName);
   TecUtilDialogMessageBox(string,MessageBox_Information);
   */
   /* Open the file DataFName */
   f = fopen(DataFName,"rb") ;
   /*
   strcpy(string," File opened."); 
   TecUtilDialogMessageBox(string,MessageBox_Information);
   */

   IsOk = DoConversion(f,TempBinFName,MessageString);
   fclose(f);


  /*
   * Remove next call when finished...
   */
  
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}
/************************************************************************/



