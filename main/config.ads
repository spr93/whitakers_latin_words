package CONFIG is

   
  OUTPUT_SCREEN_SIZE : INTEGER := 20;

  type CONFIGURATION_TYPE is (DEVELOPER_VERSION, USER_VERSION, ONLY_MEANINGS); 
                              
  CONFIGURATION : CONFIGURATION_TYPE := DEVELOPER_VERSION;

  type METHOD_TYPE is (INTERACTIVE, COMMAND_LINE_INPUT, COMMAND_LINE_FILES);

  METHOD : METHOD_TYPE := INTERACTIVE;

  type LANGUAGE_TYPE is (LATIN_TO_ENGLISH, ENGLISH_TO_LATIN);

  LANGUAGE : LANGUAGE_TYPE := LATIN_TO_ENGLISH;

  SUPPRESS_PREFACE : BOOLEAN := FALSE;  
  
  Type CL_Arguments_Type is (READ_ONLY, 
                             NO_FILES,
                             NO_EXIT,

                             ENGLISH_ONLY,      
                             LATIN_ONLY, 
                       
                             MEANINGS_ONLY); 
   
   type CL_Arguments_Array_Type is array(CL_Arguments_Type) of BOOLEAN; 
   
   Null_CL_Arguments : constant CL_Arguments_Array_Type := (others => False);
   
   CL_Arguments : CL_Arguments_Array_Type := Null_CL_Arguments;
   
end CONFIG;
