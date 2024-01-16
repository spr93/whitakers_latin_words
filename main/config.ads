package CONFIG is

  Output_Adjustment  : constant Integer := 3;
  -- this version skips 2 lines, then prints "=>"
  OUTPUT_SCREEN_SIZE : constant Integer := 24-Output_Adjustment;
  -- assume standard 80x24 for portability and backward compat


  type CONFIGURATION_TYPE is (DEVELOPER_VERSION, USER_VERSION, ONLY_MEANINGS);
  -- SPR:  The ONLY_MEANINGS and DO_ONLY_MEANINGS options are inaccurately named.
  -- They're more like "dictionary mode" because they display the dictionary forms' line + meanings.
  -- But Words has used this terminology for 30 years, so not changing.
  -- Set ONLY_MEANINGS = TRUE and DO_DICTIONARY_FORMS = FALSE to output only meanings lines.

   CONFIGURATION : CONFIGURATION_TYPE := DEVELOPER_VERSION;

   type METHOD_TYPE is (INTERACTIVE, COMMAND_LINE_INPUT, COMMAND_LINE_FILES, NOT_YET_SET);

   METHOD : METHOD_TYPE := NOT_YET_SET;

   type LANGUAGE_TYPE is (LATIN_TO_ENGLISH, ENGLISH_TO_LATIN);

   LANGUAGE : LANGUAGE_TYPE := LATIN_TO_ENGLISH;

  SUPPRESS_PREFACE : Boolean := False;
  -- SPR:  Set to TRUE to build Words an executable that silently accepts line input and
  -- returns only Latin->English results.  Useful as a listening service that other
  -- programs pipe to, especially when used with -rnx parameters.

   type CL_Arguments_Type is
     (READ_ONLY, NO_FILES, NO_EXIT,
      ENGLISH_ONLY, LATIN_ONLY,
      MEANINGS_ONLY);
   type CL_Arguments_Array_Type is array (CL_Arguments_Type) of Boolean;

   Null_CL_Arguments : constant CL_Arguments_Array_Type := (others => False);

   CL_Arguments      : CL_Arguments_Array_Type          := Null_CL_Arguments;

end CONFIG;
