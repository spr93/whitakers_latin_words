package Parse_Package is

   pragma Wide_Character_Encoding (UTF8);

   procedure Parse (COMMAND_LINE : String := "");

   procedure Parse_Unicode_File (File_Name_String : String);

   Give_Up : exception;

end Parse_Package;
