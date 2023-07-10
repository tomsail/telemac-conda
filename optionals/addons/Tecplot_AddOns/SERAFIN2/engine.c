#include "TECADDON.h"
#include "ADDGLBL.h"
#include "TASSERT.h"
#include "ENGINE.h"





/****************************************************************/
Boolean_t STDCALL ConverterCallback(char *DataFName,      /* IN */
                                    char *TempBinFName,   /* IN */
                                    char **MessageString) /* OUT (if and only if returning FALSE) */
{

/****************************************************************/
  Boolean_t IsOk = TRUE;
  FILE      *f           ;
  Boolean_t DoConversion();
  int n;
 char string[500];
/****************************************************************/
	 

  TecUtilLockStart(AddOnID);
  /*
   * Add code here to open DataFName,
   * read in the data and write out a binary
   * Tecplot datafile to TempBinFName using
   * calls to TecUtilTecxxx functions. If there is
   * a problem, call TecUtilStringAlloc() on
   * MessageString, supply a message describing the
   * problem, and set IsOk to FALSE.
   */

  /*
   * Remove next call when finished...
   */
//  f = fopen(DataFName,"rb") ;
//  IsOk = DoConversion(f,TempBinFName,MessageString);
//  fclose(f);
     
    TRACE("call to doconversion\n");
    IsOk = DoConversion(DataFName,TempBinFName,MessageString);

  
  TecUtilLockFinish(AddOnID);
/****************************************************************/
  return (IsOk);
/****************************************************************/
}



