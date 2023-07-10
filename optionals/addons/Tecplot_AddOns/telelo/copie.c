#include "ADKUTIL.h"
#include <string.h>

void trim( char *str ) 
{
        int i;                                     
        int j=0;                                   
	int len=strlen(str);
        for(i=0;i<len-1;i++)   
	{
          if(str[i]=='\0'||str[i]!=' ') break;
	  j++;
	}
	if(j>0)
	{
	    for(i=0;i<len-j;i++) str[i]=str[i+j];                       
            for(i=len-j;i<len-2;i++) str[i]=' ';                            
	    str[len-1]=0;
	}
        for(i=len-2;(str[i]==' ')&&i>0;i--)     
	{
            str[i]='\0';                           
	}
	return;
}


      VarCorr = (EntIndex_t*)calloc(NbVar,sizeof(EntIndex_t));
      if ( *AppendData == TRUE ) 
      {
	  TRACE("Append data to original data set : \n");
	  TecUtilDataSetGetInfo(NULL,NULL,&NumVars_orig);
	  for(i=0;i<NbVar;i++)VarCorr[i]=0;
	  ivar = 0;
          for(i=1;i<=NumVars_orig;i++)
	  {
              TecUtilVarGetName(i,&VName1);
	      ivar = -1;
	      for(j=1;j<=NbVar;j++)
	      {
		  VName2 = TecUtilStringListGetString(VarNames,j);
		  trim(VName2);
		  if(Str_ustrcmp(VName2,VName1) == 0 )
		  {
		      ivar = j-1;
                      VarCorr[ivar]=i;
		      break;
                      TecUtilStringDealloc(&VName2);    
		  }
                  TecUtilStringDealloc(&VName2);    
	      }
              TecUtilStringDealloc(&VName1);    
	      if(ivar == -1 || VarCorr[ivar] == 0 ) 
	      {
                  strcpy(MessageString,"Different variables in the data sets. Stop.\n");           
		  TecUtilDialogErrMsg(MessageString);
		  return(FALSE);
	      }
	  }
      }
      else
      {
          TRACE("Create data set : \n");
          TecUtilDataSetCreate(title,VarNames,TRUE);
	  for(i=1;i<=NbVar;i++)VarCorr[i-1]=i;
      }
      for(i=0;i<NbVar;i++)
      {
          sprintf(MessageString,"VarCorr[%d]=%d\n",i,VarCorr[i]);
          TRACE(MessageString);
      }
