package parse_package is


procedure Parse (COMMAND_LINE : String := "") ;
procedure Parse_Unicode_File (W_Input_String : String);
pragma Wide_Character_Encoding(UTF8);

end Parse_Package;
