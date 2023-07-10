#ifndef MACRO_H_ /* Only include once */
#define MACRO_H_

void BuildMacroCommand(const Set_pa  zoneSet,
                       std::string  &macroString);

Boolean_t STDCALL MacroCommandCallback(char *MacroCommandString,
                                       char **ErrMsg);

#endif /* MACRO_H_ */
