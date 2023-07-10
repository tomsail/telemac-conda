#include "TECADDON.h"
#include "ADDGLBL.h"
#include "TASSERT.h"
#include "ENGINE.h"





Boolean_t STDCALL ConverterCallback(char *DataFName,      /* IN */
                                    char *TempBinFName,   /* IN */
                                    char **MessageString) /* OUT (if and only if returning FALSE) */
{

  Boolean_t IsOk = TRUE;
  FILE   *f;

  TecUtilLockStart(AddOnID);
 
  f = fopen(DataFName,"rb") ;


  IsOk = DoConversion(f,TempBinFName,MessageString);
  fclose(f);

  
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}



