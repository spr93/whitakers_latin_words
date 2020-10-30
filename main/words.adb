   with Ada.Command_Line;
   with Text_IO; use Text_IO;
   with STRINGS_PACKAGE; use STRINGS_PACKAGE;
   with CONFIG; use CONFIG;
   with WORD_PARAMETERS; use WORD_PARAMETERS;
   with DEVELOPER_PARAMETERS; use DEVELOPER_PARAMETERS;
   with WORD_PACKAGE; use WORD_PACKAGE;
   with PARSE;

   with Ada.Wide_Text_IO; 
   with Ada.Wide_Characters.Handling;
   with Ada.Characters.Conversions;
   with Ada.Interrupts; use Ada.Interrupts;
   with Ada.Interrupts.Names; use Ada.Interrupts.Names;
   with No_Exit_Handler; use No_Exit_Handler; 
   with Ada.Environment_Variables;
   with Ada.Directories;

   procedure WORDS is

      Argument_Offset : INTEGER := 0;  
   begin
      --  The language shift in arguments must take place here
      --  since later parsing of line ignores non-letter characters
      CONFIGURATION := DEVELOPER_VERSION;
     
      -- FIND DATA FILES
      -- If the always-necessary INFLECTS.SEC isn't in the working directory, see if we can find them
      if not Ada.Directories.Exists("INFLECTS.SEC") then
         if Ada.Environment_Variables.Exists("LATINWORDS") then
         Put_Line("LATINWORDS environment variable set; using data files at " & Ada.Environment_Variables.Value("LATINWORDS"));
         Ada.Directories.Set_Directory(Ada.Environment_Variables.Value("LATINWORDS"));
         else
         CHECK_PATH_VARIABLE;
         end if;
      end if;
      -- END FIND WORDS DATA FILES
               
--SIMPLE INTERACTIVE MODE 
   if Ada.Command_Line.ARGUMENT_COUNT = 0  then       --  Simple WORDS
         METHOD := INTERACTIVE;                          --  Interactive
         SUPPRESS_PREFACE := FALSE;
         SET_OUTPUT(STANDARD_OUTPUT);
         INITIALIZE_WORD_PARAMETERS;
         INITIALIZE_DEVELOPER_PARAMETERS;
         INITIALIZE_WORD_PACKAGE;
         PARSE;

 --COMMAND-LINE ARGUMENTS ("new" style to modify interactive mode)
   elsif TRIM(Ada.Command_Line.Argument(1))(1) = '-'  
        then 

        declare 
             Args_Exception : exception;

             begin 

               for I in 1..Ada.Command_Line.Argument_Count loop
             
                 for J in 2..TRIM(Ada.Command_Line.Argument(I))'length loop
                   exit when J > 5; -- max 5 arguments (-E and -L conflict) 
                    case Upper_Case(TRIM(Ada.Command_Line.Argument(I))(J)) is
                    when '-' => exit when J > 2;
                        exit;
                    when 'E' =>  
                       if CL_Arguments(Latin_Only) then raise Args_Exception;
                       else
                           CL_Arguments(ENGLISH_ONLY) := TRUE;
                       end if; 
                    when 'L' =>  
                       if CL_Arguments(English_Only)then raise Args_Exception;
                       else
                           CL_Arguments(LATIN_ONLY) := TRUE;
                       end if;  
                    when 'R' =>  
                       CL_Arguments(READ_ONLY) := TRUE;
                    when 'M' =>  
                       CL_Arguments(MEANINGS_ONLY) := TRUE;
                       CONFIGURATION := ONLY_MEANINGS;      -- re-use this existing setting; nothing more to do to enforce meanings only
                    when 'N' =>  
                       CL_Arguments(NO_FILES) := TRUE;
                    when 'X' => 
                       CL_Arguments(NO_EXIT) := TRUE;
                        pragma Unreserve_All_Interrupts;
                        Attach_Handler(No_Exit_Handler_Access, SIGINT);
                    when '?' | 'H' =>  raise Args_Exception; -- display arguments help and terminate
                    when others => 
                     New_Line;
                     Put_Line("====== UNKNOWN COMMAND-LINE OPTION: " & TRIM(Ada.Command_Line.Argument(I))(J) & " ======");
                     New_Line;
                    end case;  
               end loop; 
               
               exit when I > 5;  -- max 5 arguments (-E and -L conflict) 
            end loop; -- while =< argument_counter'length

            -- Still in the "elsif" statement => we're using the - / -- style arguments => INTERACTIVE MODE STARTUP:
            METHOD := INTERACTIVE;      
            SUPPRESS_PREFACE := FALSE;
            SET_OUTPUT(STANDARD_OUTPUT);
            INITIALIZE_WORD_PARAMETERS;
            INITIALIZE_DEVELOPER_PARAMETERS;
            INITIALIZE_WORD_PACKAGE;
            
            if CL_Arguments(No_Exit) then 
               loop   -- appears to be memory leak if we recursively call parse from within parse exception handler 
               PARSE; -- return => infinite loop avoids more complex exception logic
               end loop;
            else
               Parse;
            return; 
            end if;
      
       exception
         
         when Args_Exception =>     -- Parse may raise NO_EXCEPTION_EXCEPTION; handler is outside the block
            Put_Line("Words operates in two modes when using command-line arguments");
            New_Line;
            Put_Line("[1] NON-INTERACTIVE WORDS:  Fully compatible with classic Words.");
            Put_Line("Usage:");
            Put_Line("words [string of Latin words]");
            Put_Line("   => send results for the Latin words to standard output and exit");
            Put_Line("words [in_file] [out_file]");
            PUT_LINE("   => take Latin words from the in_file and put the results in out_file");
            Put_Line("words " & CHANGE_LANGUAGE_CHARACTER & "e [string of English words]");
            Put_Line("   => returns Latin translation options");
            New_Line;
            Put_Line("[2] MODIFY INTERACTIVE-MODE:  Set limits on interactive mode.");
            Put_Line("Options:");
            Put_Line("-r    READ ONLY:     User cannot change settings, write settings files" );
            Put_Line("                       or direct output to file");    
            Put_Line("-n    NO FILES:      User cannot load a file or direct output to file");  
            Put_Line("-x    NO EXIT:       User cannot exit with two returns or control-C");
            Put_Line("                       NOT A SECURE MODE - only blocks SIGINT");
            Put_Line("                       No effect on suspend (SIGSTP) or kill (SIGTERM)"); 
            Put_Line("                       File system may be readable with " & CHANGE_LANGUAGE_CHARACTER); 
            Put_Line("-l    LATIN ONLY:    User cannot enter English->Latin mode");
            Put_Line("-e    ENGLISH ONLY:  User cannot enter Latin->English mode");
            Put_Line("-m    MEANINGS ONLY: Show only the meanings line (in Latin->English mode)");
            New_Line;
            Put_Line("E.g., words -rnlm limits the user the functionality of a paper dictionary");
            New_Line;
            Put_Line("These options are overrides; non-conflicting settings (WORD.MOD) still apply");
            New_Line;
            Return; 
      end; -- block

else -- NOT entering interactive mode; back to classic Words startup
      
     --   NON-INTERACTIVE MODE STARTUP SUBSEQUENCE:
     SUPPRESS_PREFACE := TRUE;
     INITIALIZE_WORD_PARAMETERS;
     INITIALIZE_DEVELOPER_PARAMETERS;
     INITIALIZE_WORD_PACKAGE;
end if; 

 -- check for change langauge command line option
 if Ada.Command_Line.Argument_Count > 1 
     and then 
       Ada.Command_Line.Argument(1)(1) = CHANGE_LANGUAGE_CHARACTER
        and then 
            Ada.Command_Line.Argument(1)'length > 1
   then 
      CHANGE_LANGUAGE(Trim(Ada.Command_Line.Argument(1))(2));
      Argument_Offset := 1;  -- i.e., start processing at argument 2
   end if; 
      
 --  choose a non-interactive mode: 
 --  when 1 argument     => either a simple Latin word or an input file.
 --  when 2 arguments    => two words in-line
 --  when more arguments => command-line of words   
   if (Ada.Command_Line.Argument_Count - Argument_Offset) in 1..2
     then 
         declare  -- block for I/O and unicode exceptions
          INPUT_NAME   : constant STRING := TRIM(Ada.Command_Line.Argument(1+Argument_Offset));
          W_INPUT : Ada.Wide_Text_IO.File_Type;
          begin
         
          -- Set up the correct input
          -- First try to open the file; if it's invalid name_error is raised
          if WORDS_MODE(DO_UNICODE_INPUT) then
                  Ada.Wide_Text_IO.OPEN(W_INPUT, Ada.Wide_Text_IO.IN_FILE, INPUT_NAME);   
                  Ada.Wide_Text_IO.SET_INPUT(W_INPUT);
               else 
                  OPEN(INPUT, IN_FILE, INPUT_NAME); 
                  SET_INPUT(INPUT);
          end if; 
          
         --set up output
          if (Ada.Command_Line.Argument_Count - Argument_Offset) = 1
             then  --output to screen
               SET_OUTPUT(Standard_Output);  -- text_io standard output   
               
          else --output to file
            WORDS_MODE(DO_ANSI_FORMATTING) := False; 
            --If the outfile exists then append
            begin --handle exception on open
              Open(OUTPUT, APPEND_FILE, TRIM(Ada.Command_Line.Argument(2+Argument_Offset)));
            exception
                  when Name_Error => CREATE(OUTPUT, OUT_FILE, TRIM(Ada.Command_Line.Argument(2+Argument_Offset)));
            end; -- end append/create  
           SET_OUTPUT(OUTPUT);           -- text_io file output
         end if;   
            
         -- start parsing
         if  WORDS_MODE(DO_UNICODE_INPUT) 
         then  
             METHOD := COMMAND_LINE_INPUT; --Reading unicode from file always simulates command-line input
                                           --(repeatedly calls parse)
            while not Ada.Wide_Text_IO.End_Of_File(W_INPUT) loop

                  declare    
                    W_Line : Wide_String := Ada.Wide_Text_IO.Get_Line(W_Input);
 
                    T_Line : String      := Ada.Characters.Conversions.To_String (    
                                            Ada.Wide_Characters.Handling.To_Basic(W_Line));
                  begin  
                  PARSE(Trim(T_Line));   
               end;
             end loop; 

             Ada.Wide_Text_IO.Close(W_INPUT);      
         else
               METHOD := COMMAND_LINE_FILES;
               PARSE;
               Close(Input);
         end if;
         
         SET_INPUT(STANDARD_INPUT);    
         SET_OUTPUT(STANDARD_OUTPUT);
         return; 
      
         exception                 
               when NAME_ERROR  =>                     --  Raised NAME_ERROR therefore
            METHOD := COMMAND_LINE_INPUT;      --  Assume w found word in command line
               when others => 
                   if WORDS_MODE(DO_UNICODE_INPUT) then null;  
                   else raise End_Error; 
                   end if; 
                        Put_Line("I/O ERROR");
                       if Words_MODE(DO_UNICODE_INPUT) then 
                       Put_Line("Unicode input processing is on (DO_UNICODE_INPUT).  Try turning it off.");
                       end if;                     
                    return;
         end; --  file I/O and unicode exception block
    
   else   -- More than 2 arguments (after possible language switch) => translate all words on the command line
       METHOD := COMMAND_LINE_INPUT;
   end If; 

   --  Process words in command line
   if METHOD = COMMAND_LINE_INPUT  then            --  Process words in command line
         for I in (1 + Argument_Offset)..Ada.Command_Line.Argument_Count  loop  --  Assemble input words 
            declare
            INPUT_LINE : String := HEAD(TRIM(Ada.Command_Line.Argument(I)), 250);
            begin 
            PARSE(TRIM(INPUT_LINE));
            end; --block 
         end loop; 
   end if;

end  WORDS;
