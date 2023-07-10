#include "TECADDON.h"
#include "ADDGLBL.h"
#include "GUIDEFS.h"
#include <vector>
#include <set>
#include <sstream>
#include <string>
#include "strandeditor.h"

template <class T>
std::string Stringify(T value)
  {
    std::ostringstream o;
    o << value;
    return o.str();
  }


template <class T>
static void LookUpBooleanString(T                 &BooleanValue,
                                const std::string &BooleanString)
{
  static std::set<std::string>  affirmatives;
  static std::set<std::string>  negatives;

  if (affirmatives.size() == 0)
    {
      affirmatives.insert("Yes");
      affirmatives.insert("YES");
      affirmatives.insert("True");
      affirmatives.insert("TRUE");
      affirmatives.insert("On");
      affirmatives.insert("ON");

      negatives.insert("No");
      negatives.insert("NO");
      negatives.insert("False");
      negatives.insert("FALSE");
      negatives.insert("Off");
      negatives.insert("OFF");
    }

  if (affirmatives.find(BooleanString) != affirmatives.end())
    BooleanValue = TRUE;
  else if (negatives.find(BooleanString) != negatives.end())
    BooleanValue = FALSE;
  else
    {
      std::string ErrorMessage(ADDON_NAME": Unrecognized boolean value: '");
      ErrorMessage += BooleanString;
      ErrorMessage += "'. Ignoring....";
      TecUtilDialogErrMsg(ErrorMessage.c_str());
    }
}


static void Tokenize(const std::string         &str,
                     std::vector<std::string>  &tokens,
                     const std::string         &delimiters = " ")
{
    // Skip delimiters at beginning.
    std::string::size_type lastPos = str.find_first_not_of(delimiters, 0);
    // Find first "non-delimiter".
    std::string::size_type pos     = str.find_first_of(delimiters, lastPos);

    while (std::string::npos != pos || std::string::npos != lastPos)
    {
        // Found a token, add it to the vector.
        tokens.push_back(str.substr(lastPos, pos - lastPos));
        // Skip delimiters.  Note the "not_of"
        lastPos = str.find_first_not_of(delimiters, pos);
        // Find next "non-delimiter"
        pos = str.find_first_of(delimiters, lastPos);
    }
}

static void TackOnSetTrailer(std::string  &Command,
                             Boolean_t     OnARun,
                             SetIndex_t    TrailingValue)
{
  if (OnARun)
    {
      Command += '-';
      Command += Stringify(TrailingValue);
    }
  else
    {
      Command += ',';
      Command += Stringify(TrailingValue);
    }
}

void AppendSetString(const Set_pa    Set,
                     Boolean_t       IncludeSquareBrackets,
                     std::string    &SetString)
{
  SetIndex_t LastValue = TECUTILSETNOTMEMBER;
  SetIndex_t LastInstalledValue = TECUTILSETNOTMEMBER;
  SetIndex_t NextValue = TECUTILSETNOTMEMBER;
  Boolean_t  OnARun = FALSE;

  REQUIRE(VALID_REF(Set) || Set == NULL);

  if (Set)
    {
      NextValue = TecUtilSetGetNextMember(Set,NextValue);
      while (NextValue != TECUTILSETNOTMEMBER)
        {
          if (LastValue == TECUTILSETNOTMEMBER)
            {
              // First time through the loop
              LastInstalledValue = NextValue;
              if (IncludeSquareBrackets)
                SetString += '[';
              SetString += Stringify(NextValue);
            }
          else if ((NextValue-LastValue) == 1)
            OnARun = TRUE;
          else 
            {
              if (OnARun)
                TackOnSetTrailer(SetString,TRUE,LastValue);
    
              TackOnSetTrailer(SetString,FALSE,NextValue);
    
              LastInstalledValue = NextValue;
              OnARun = FALSE;
            }
          LastValue = NextValue;
          NextValue = TecUtilSetGetNextMember(Set,NextValue);
        }

      if (LastValue == TECUTILSETNOTMEMBER) // Empty set.
        {
          if (IncludeSquareBrackets)
            SetString += '[';
        }
      else if (LastValue != LastInstalledValue)
        TackOnSetTrailer(SetString,OnARun,LastValue);
    
      if (IncludeSquareBrackets)
        SetString += ']';
    }
  else
    {
      if (IncludeSquareBrackets)
        SetString += "[]";
    }
}


static void RemoveAllWhiteSpace(std::string &inputString)
{
  std::string             whiteSpace          = " \t";
  std::string::size_type  firstWhiteSpace     = inputString.find_first_of(whiteSpace);

  while (firstWhiteSpace != std::string::npos)
    {
      std::string::size_type  firstNonWhiteSpace  = inputString.find_first_not_of(whiteSpace,
                                                                                  firstWhiteSpace); // begin after the first white space
      if (firstNonWhiteSpace != std::string::npos)
        inputString.erase(firstWhiteSpace,                      // position to start erasing
                          firstNonWhiteSpace-firstWhiteSpace);  // number of characters to erase
      else
        inputString.erase(firstWhiteSpace);

      firstWhiteSpace = inputString.find_first_of(whiteSpace);
    }
}


/**
 *  A set must have the form: (s[,s][,s]...)
 *  where s can be one of:  n or n-m
 */
static Boolean_t GetSet(const std::string  &IString,
                        Set_pa             *Set,
                        Boolean_t           HasOuterSquareBrackets)
{
  std::string subString(IString); // Make a copy we can modify
  std::string validChars("0123456789,-");
  if (HasOuterSquareBrackets)
    validChars += "[]";

  RemoveAllWhiteSpace(subString);

  // Make sure we have no invalid characters
  Boolean_t IsOk = subString.find_first_not_of(validChars) == std::string::npos;

  if (IsOk)
    {
      std::vector<std::string>  tokens;

      Tokenize(subString,
               tokens,
               ",");

      *Set = TecUtilSetAlloc(TRUE);
      if (*Set == NULL)
        IsOk = FALSE;

      for (std::vector<std::string>::size_type curToken = 0; 
           curToken < tokens.size() && IsOk;
           curToken++)
        {
          LgIndex_t                 I1        = 0;
          LgIndex_t                 I2        = 0;
          std::string              &curString = tokens.at(curToken);
          std::string::size_type    position  = curString.find('-');

          if (curString.at(0) =='[')
            curString.erase(0,1);

          if (position != std::string::npos)
            {
              I1 = atoi(curString.substr(0, position).c_str());
              I2 = atoi(curString.substr(position+1).c_str());
              IsOk = (I1 > 0 && 
                      I2 > I1);
            }
          else
            {
              I1 = atoi(curString.c_str());
              I2 = I1;
              IsOk = I1 > 0;
            }

          if (IsOk)
            {
              for (LgIndex_t I = I1; I <= I2; I++)
                {
                  if (!TecUtilSetAddMember(*Set,I,TRUE))
                    IsOk = FALSE;
                }
            }
        }
      if (!IsOk && *Set)
        TecUtilSetDealloc(Set);
    }

  return (IsOk);
}



void BuildMacroCommand(const Set_pa  zoneSet,
                       std::string  &macroString)
{
  TecUtilLockStart(AddOnID);

  macroString = "ZoneSet=";
  AppendSetString(zoneSet,
                  FALSE, // IncludeSquareBraces
                  macroString);
  macroString += ';';

  if (GlobalSettings.Data.MultipleZonesPerTimestep)
    {
      macroString += "MultiZonesPerTime=TRUE;";
      macroString += "ZoneGrouping=";
      if (GlobalSettings.Data.ZoneGrouping == ZoneGrouping_ByStrandID)
        macroString += "Strand";
      else
        macroString += "Time";
      macroString += ';';

      macroString += "GroupSize=";
      macroString += Stringify(GlobalSettings.Data.GroupSize);
      macroString += ';';                
    }

  if (GlobalSettings.StrandID.Assign)
    {
      macroString += "AssignStrands=TRUE;";
      macroString += "StrandValue=";
      macroString += Stringify(GlobalSettings.StrandID.Value);
      macroString += ';';
   }

  if (GlobalSettings.SolutionTime.AssignSolutionTime)
    {
      macroString += "AssignSolutionTime=TRUE;";
      macroString += "TimeValue=";
      macroString += Stringify(GlobalSettings.SolutionTime.Value);
      macroString += ';';
      if (GlobalSettings.SolutionTime.Option == TimeOptions_ConstantDelta)
        {
          macroString += "TimeOption=ConstantDelta;";
          macroString += "DeltaValue=";
          macroString += Stringify(GlobalSettings.SolutionTime.Delta);
          macroString += ';';
        }
    }

  if (GlobalSettings.ParentZone.Assign)
    {
      macroString += "AssignParent=TRUE;";
      macroString += "ParentValue=";
      macroString += Stringify(GlobalSettings.ParentZone.Value);
    }

  TecUtilLockFinish(AddOnID);
}

Boolean_t STDCALL MacroCommandCallback(char *MacroCommandString,
                                       char **ErrMsg)
{
  std::string macroCommandString(MacroCommandString);
  std::string errorMessage;
  std::vector<std::string>  tokens;
  Boolean_t IsOk    = TRUE;
  Set_pa    ZoneSet = NULL;

  // Make sure we have a consistent starting point
  InitGlobalSettings();

  Tokenize(macroCommandString,
           tokens,
           ";");
  for (std::vector<std::string>::size_type curToken = 0; 
       curToken < tokens.size() && IsOk;
       curToken++)
    {
      // Break the token into Parameter and Value strings
      std::string::size_type posOfEqual = tokens.at(curToken).find('=');
      // Parameter string is from the first character to the first '='
      std::string param(tokens.at(curToken).substr(0, posOfEqual));
      // Value string is everything after the '='
      std::string value(tokens.at(curToken).substr(posOfEqual+1));

      if (param.find("ZoneSet") != std::string::npos)
        {
          if (!GetSet(value, &ZoneSet, FALSE /* HasOuterSquareBraces */))
            {
              errorMessage += "Bad value for ZoneSet: ";
              errorMessage += value;
              IsOk = FALSE;
            }
        }
      else if (param.find("MultiZonesPerTime") != std::string::npos)
        {
          LookUpBooleanString(GlobalSettings.Data.MultipleZonesPerTimestep,
                              value);
        }
      else if (param.find("ZoneGrouping") != std::string::npos)
        {
          if (value.find("Strand") != std::string::npos)
            GlobalSettings.Data.ZoneGrouping = ZoneGrouping_ByStrandID;
          else if (value.find("Time") != std::string::npos)
            GlobalSettings.Data.ZoneGrouping = ZoneGrouping_ByTimeStep;
          else
            {
              IsOk = FALSE;
              errorMessage = "Unrecognized value for ZoneGrouping parameter: ";
              errorMessage += value;
              errorMessage = ".\nValue must be either \"Strand\" or \"Time\".";
            }
        }
      else if (param.find("GroupSize") != std::string::npos)
        {
          GlobalSettings.Data.GroupSize = atoi(value.c_str());
          if (GlobalSettings.Data.GroupSize < 2)
            {
              IsOk = FALSE;
              errorMessage = "GroupSize must be greater than or equal to 2.";
            }
        }
      else if (param.find("AssignStrands") != std::string::npos)
        {
          LookUpBooleanString(GlobalSettings.StrandID.Assign,
                              value);
        }
      else if (param.find("StrandValue") != std::string::npos)
        {
          GlobalSettings.StrandID.Value = atoi(value.c_str());
          if (GlobalSettings.StrandID.Value < 0)
            {
              IsOk = FALSE;
              errorMessage = "StrandValue must be greater than or equal to 0.";
            }
        }
      else if (param.find("AssignSolutionTime") != std::string::npos)
        {
          LookUpBooleanString(GlobalSettings.SolutionTime.AssignSolutionTime,
                              value);
        }
      else if (param.find("TimeValue") != std::string::npos)
        {
          GlobalSettings.SolutionTime.Value = atof(value.c_str());
        }
      else if (param.find("TimeOption") != std::string::npos)
        {
          if (value.find("SingleValue") != std::string::npos)
            GlobalSettings.SolutionTime.Option = TimeOptions_SingleValue;
          else if (value.find("ConstantDelta") != std::string::npos)
            GlobalSettings.SolutionTime.Option = TimeOptions_ConstantDelta;
          else
            {
              IsOk = FALSE;
              errorMessage = "Unrecognized value for TimeOption parameter: ";
              errorMessage += value;
              errorMessage = ".\nValue must be either \"SingleValue\" or \"ConstantDelta\".";
            }
        }
      else if (param.find("DeltaValue") != std::string::npos)
        {
          GlobalSettings.SolutionTime.Delta = atof(value.c_str());
        }
      else if (param.find("AssignParent") != std::string::npos)
        {
          LookUpBooleanString(GlobalSettings.ParentZone.Assign,
                              value);
        }
      else if (param.find("ParentValue") != std::string::npos)
        {
          GlobalSettings.ParentZone.Value = atoi(value.c_str());
          if (GlobalSettings.ParentZone.Value < 0)
            {
              IsOk = FALSE;
              errorMessage = "ParentValue must be greater than or equal to 0.";
            }
        }
      else
        {
          IsOk = FALSE;
          errorMessage = "Unrecognized parameter: ";
          errorMessage += param;
        }
    }

  if (IsOk)
    {
      RedefineMultipleZones(ZoneSet,
                            GlobalSettings.Data,
                            GlobalSettings.StrandID,
                            GlobalSettings.SolutionTime,
                            GlobalSettings.ParentZone);
    }
  else
    {
      *ErrMsg = TecUtilStringAlloc(errorMessage.length()+1, "Strand Editor macro error message.");
      strcpy(*ErrMsg, errorMessage.c_str());
    }

  if (ZoneSet != NULL)
    TecUtilSetDealloc(&ZoneSet);

  // Make sure the dialog reflects the current settings
  if (TecGUIDialogIsUp(Dialog1Manager))
    UpdateDialog1Values();

  return (IsOk);
}
