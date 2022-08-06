package CONFIG is

   OUTPUT_SCREEN_SIZE : Integer := 20;

  type CONFIGURATION_TYPE is (DEVELOPER_VERSION, USER_VERSION, ONLY_MEANINGS);
  -- SPR:  The ONLY_MEANINGS and DO_ONLY_MEANINGS options are inaccurately named.
  -- They're more like "dictionary mode" because they display the dictionary forms' line + meanings.
  -- But Words has used this terminology for 30 years without significant user confusion, so no
  -- sense in changing.
  -- Combine ONLY_MEANINGS with DO_DICTIONARY_FORMS = FALSE to get real MEANINGS_ONLY output.

   CONFIGURATION : CONFIGURATION_TYPE := DEVELOPER_VERSION;

   type METHOD_TYPE is (INTERACTIVE, COMMAND_LINE_INPUT, COMMAND_LINE_FILES);

   METHOD : METHOD_TYPE := INTERACTIVE;

   type LANGUAGE_TYPE is (LATIN_TO_ENGLISH, ENGLISH_TO_LATIN);

   LANGUAGE : LANGUAGE_TYPE := LATIN_TO_ENGLISH;

  SUPPRESS_PREFACE : Boolean := False;
  -- SPR:  Set to TRUE to build Words an executable that silently accepts line input and
  -- returns only Latin->English results.  Useful as a listening service that other
  -- programs pipe to when used with -rnx parameters.

   type CL_Arguments_Type is
     (READ_ONLY, NO_FILES, NO_EXIT,
      ENGLISH_ONLY, LATIN_ONLY,
      MEANINGS_ONLY);
   type CL_Arguments_Array_Type is array (CL_Arguments_Type) of Boolean;

   Null_CL_Arguments : constant CL_Arguments_Array_Type := (others => False);

   CL_Arguments      : CL_Arguments_Array_Type := Null_CL_Arguments;

end CONFIG;
