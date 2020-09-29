   with Ada.Command_Line;
   with Ada.Text_IO; use Ada.Text_IO;
   with STRINGS_PACKAGE; use STRINGS_PACKAGE;
   with CONFIG; use CONFIG;
   with WORD_PARAMETERS; use WORD_PARAMETERS;
   with DEVELOPER_PARAMETERS; use DEVELOPER_PARAMETERS;
   with WORD_PACKAGE; use WORD_PACKAGE;

with PARSE;
   with Ada.Interrupts; use Ada.Interrupts;
   with Ada.Interrupts.Names; use Ada.Interrupts.Names;
   with No_Exit_Handler; use No_Exit_Handler; 
   with Ada.Environment_Variables;
with Ada.Directories;

   procedure WORDS is
      INPUT_LINE  : STRING(1..250) := (others => ' ');
      ARGUMENTS_START : INTEGER := 1;  
  
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
               
      --The main mode of usage for WORDS is a simple call, followed by screen interaction.
      if Ada.Command_Line.ARGUMENT_COUNT = 0  then      --  Simple WORDS
         METHOD := INTERACTIVE;                          --  Interactive
         SUPPRESS_PREFACE := FALSE;
         SET_OUTPUT(Ada.TEXT_IO.STANDARD_OUTPUT);
         INITIALIZE_WORD_PARAMETERS;
         INITIALIZE_DEVELOPER_PARAMETERS;
         INITIALIZE_WORD_PACKAGE;
         PARSE;
         
      --But there are other, command line options.
      --WORDS may be called with arguments on the same line, 
      --in a number of different modes.
    
   else 

      declare 
             Args_Exception : exception;

             begin 
               If TRIM(Ada.Command_Line.Argument(1))(1) /= '-'  
               then null;
             else 

               for I in 1..Ada.Command_Line.Argument_Count loop
             
                 for J in 2..TRIM (Ada.Command_Line.Argument(I))'length loop
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

            -- Still in the "else" statement => we're using the - / -- style arguments => continue interactive mode startup
            METHOD := INTERACTIVE;      
            SUPPRESS_PREFACE := FALSE;
            SET_OUTPUT(Ada.TEXT_IO.STANDARD_OUTPUT);
            INITIALIZE_WORD_PARAMETERS;
            INITIALIZE_DEVELOPER_PARAMETERS;
            INITIALIZE_WORD_PACKAGE;
            
            if CL_Arguments(No_Exit) then 
               loop   -- appears to be memory leak if we recursively call parse from within parse exception handler 
               PARSE; -- not elegant, but return => infinite loops avoids more complex logic in block exception handler or global handler
               end loop;
               
            else
               Parse;
            end if;
         
            return; -- if parse terminates we have to expressly terminate the program so we don't fall through the old parameters routine

         end if; 
         
       exception
         when Args_Exception: others =>     -- Parse may raise NO_EXCEPTION_EXCEPTION; handler is outside the block
            Put_Line("Words operates in two modes when using command-line arguments");
            New_Line;
            Put_Line("[1] NON-INTERACTIVE WORDS:  Maintains compatibility with classic Words.");
            Put_Line("Usage:");
            Put_Line("words [string of Latin words]");
            Put_Line("   => send results for the Latin words to standard output and exit");
            Put_Line("words [in_file] [out_file]");
            PUT_LINE("   => take Latin words from the in_file and put the results in out_file");
            Put_Line("words " & CHANGE_LANGUAGE_CHARACTER & "e [English word] [OPTIONAL:  part of speech OR second English word]");
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
      

      --  SPR:  Resume Gen. Whitaker's original arguments parsing
         SUPPRESS_PREFACE := TRUE;
         INITIALIZE_WORD_PARAMETERS;
         INITIALIZE_DEVELOPER_PARAMETERS;
         INITIALIZE_WORD_PACKAGE;
      
      --Single parameter, either a simple Latin word or an input file.
      --WORDS amo
      --WORDS infile
      if Ada.Command_Line.ARGUMENT_COUNT = 1  then      --  Input 1 word in-line
         ONE_ARGUMENT:
         declare
            INPUT_NAME  : constant STRING := TRIM(Ada.Command_Line.Argument(1));
         begin
            OPEN(INPUT, IN_FILE, INPUT_NAME); --  Try file name, not raises NAME_ERROR
            METHOD := COMMAND_LINE_FILES;
            SET_INPUT(INPUT);
            SET_OUTPUT(Ada.TEXT_IO.STANDARD_OUTPUT);
            PARSE;          --  No additional arguments, so just go to PARSE now
            exception                  --  Triggers on INPUT
               when NAME_ERROR  =>                   --  Raised NAME_ERROR therefore
                  METHOD := COMMAND_LINE_INPUT;      --  Found word in command line
         end ONE_ARGUMENT;
      
      --With two arguments the options are: inputfile and outputfile,
      --two Latin words, or a language shift to English (Latin being the startup default)
      --and an English  word (with no part of speech).
      --WORDS infile outfile
      --WORDS amo amas
      --WORDS ^e  love
      elsif Ada.Command_Line.ARGUMENT_COUNT = 2  then    --  INPUT and OUTPUT files
         TWO_ARGUMENTS:                                   --  or multiwords in-line
         declare
            INPUT_NAME  : constant STRING := TRIM(Ada.Command_Line.Argument(1));
            OUTPUT_NAME : constant STRING := TRIM(Ada.Command_Line.Argument(2));
         begin
           if INPUT_NAME(1) = CHANGE_LANGUAGE_CHARACTER  then
             if (INPUT_NAME'LENGTH > 1)  then 
                 CHANGE_LANGUAGE(INPUT_NAME(2));
                 ARGUMENTS_START := 2;
                 METHOD := COMMAND_LINE_INPUT;      --  Parse the one word 
              end if; 
            else
               OPEN(INPUT, IN_FILE, INPUT_NAME);
               CREATE(OUTPUT, OUT_FILE, OUTPUT_NAME);
               METHOD := COMMAND_LINE_FILES;
         
               SET_INPUT(INPUT);
               SET_OUTPUT(OUTPUT);
         
               SUPPRESS_PREFACE := TRUE;
               OUTPUT_SCREEN_SIZE := INTEGER'LAST;
               PARSE;           --  No additional arguments, so just go to PARSE now
         
               SET_INPUT(Ada.TEXT_IO.STANDARD_INPUT);    --  Clean up
               SET_OUTPUT(Ada.TEXT_IO.STANDARD_OUTPUT);
               CLOSE(OUTPUT);
            end if;
            exception                  --  Triggers on either INPUT or OUTPUT  !!!
               when NAME_ERROR  =>
                  METHOD := COMMAND_LINE_INPUT;            --  Found words in command line
         
         end TWO_ARGUMENTS;
     
      --With three arguments there could be three Latin words or a language shift
      --and and English word and part of speech.
      --WORDS amo amas amat
      --WORDS ^e love v
      elsif Ada.Command_Line.ARGUMENT_COUNT = 3  then    --  INPUT and OUTPUT files
         THREE_ARGUMENTS:                                   --  or multiwords in-line
         declare
            ARG1 : constant STRING := TRIM(Ada.Command_Line.Argument(1));
            ARG2 : constant STRING := TRIM(Ada.Command_Line.Argument(2));
            ARG3 : constant STRING := TRIM(Ada.Command_Line.Argument(3));
         begin
           if ARG1(1) = CHANGE_LANGUAGE_CHARACTER  then
             if (ARG1'LENGTH > 1)  then 
                 CHANGE_LANGUAGE(ARG1(2));
                 ARGUMENTS_START := 2;
                 METHOD := COMMAND_LINE_INPUT;      --  Parse the one word 
              end if; 
            else
               METHOD := COMMAND_LINE_INPUT;
            end if;
             
         end THREE_ARGUMENTS;
      
      --More than three arguments must all be Latin words.
      --WORDS amo amas amat amamus amatis amant
      else    --  More than three arguments
      
         METHOD := COMMAND_LINE_INPUT;
      end if;
      
      if METHOD = COMMAND_LINE_INPUT  then            --  Process words in command line
         MORE_ARGUMENTS:
         begin
  --Ada.TEXT_IO.PUT_LINE("MORE_ARG  ARG_START = " & INTEGER'IMAGE(ARGUMENTS_START));
           SUPPRESS_PREFACE := TRUE;
            for I in ARGUMENTS_START..Ada.Command_Line.Argument_Count  loop  --  Assemble input words 
               INPUT_LINE := HEAD(TRIM(INPUT_LINE) & " " & Ada.Command_Line.Argument(I), 250);
            end loop;
  --Ada.TEXT_IO.PUT_LINE("To PARSE >" & TRIM(INPUT_LINE));
            PARSE(TRIM(INPUT_LINE));
         end MORE_ARGUMENTS;
      end if;
      end if;
 
   end  WORDS;
