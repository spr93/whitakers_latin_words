with CONFIG;
with Ada.Environment_Variables;

package body LATIN_FILE_NAMES is

  function ADD_FILE_NAME_EXTENSION(NAME, EXTENSION : STRING) return STRING is
  --  This is the version that creates a DOS file name
  --  One that has a name, a '.', and an extension no longer than 3 characters
  --  Arbitarily, we also truncate the NAME to 8 characters
  --  To port to another system, one needs to do this function appropriately
    NAME_LENGTH      : INTEGER := NAME'LENGTH;
    EXTENSION_LENGTH : INTEGER := EXTENSION'LENGTH;
  begin
    if NAME_LENGTH >= 8  then
      NAME_LENGTH  := 8;
    end if;
    if EXTENSION'LENGTH >= 3  then
      EXTENSION_LENGTH  := 3;
    end if;
    return NAME(Name'First..NAME_LENGTH) & '.' & EXTENSION(Extension'First..EXTENSION_LENGTH);
  end ADD_FILE_NAME_EXTENSION;

  function Correct_File (File_Name : in String) return String is
   -- USED ONLY FOR USER-MODIFIABLE FILES.  Initialize_Word_Packae sets the current working directory to the site-wide data/dictionary files directory
   -- so that those files can be quickly opened and closed as necessary throughout execution, without checking and re-checking directories.
   -- Therefore, this function should be called called only when opening the LOCAL dictionary, settings files (WORDS.MDV, WORDS.MOD), and when processing
   -- user-input file names.

    use Ada.Directories;
    --use Ada.Strings.Unbounded;
    use Config;
   begin

    if Current_Directory = Startup_Working_Directory then
        return File_Name; -- nothing to do bc dictionary files/user startup/current working directories are identical.
    elsif CL_ARGUMENTS(No_Files) and then
      ( File_Name = MODE_FULL_NAME or File_Name = MDEV_FULL_NAME or File_Name = ADD_FILE_NAME_EXTENSION (DICTIONARY_FILE_NAME, "LOCAL") )
        then
         -- Enforce the No_Files argument by allowing access only to files in the site-wide data/dictionary files directory
        return File_Name;
    elsif  Cl_Arguments(No_Files) then
         raise Status_Error;
    elsif METHOD = Command_Line_Files -- Trust the shell/operating environment
      then
      Set_Directory(Startup_Working_Directory);
      return File_Name;
    elsif Exists(File_Name) -- We got a valid full path name + file from the user OR we got the name of a file in the site-wide data directory
      then return File_Name;
    else  -- Use or create the user's own in the startup/working directory, which is likely to be the user's HOME directory
      return Compose(Startup_Working_Directory, File_Name);
    end if;

   exception
    when others =>
      return File_Name;
   end Correct_File;

     procedure Find_Data_And_Settings is

      use Ada.Directories;

   begin

      if Ada.Environment_Variables.Exists ("LATINWORDS") then
         Set_Directory (Ada.Environment_Variables.Value ("LATINWORDS"));
      elsif Ada.Environment_Variables.Exists ("LATIN_WORDS") then
         Set_Directory (Ada.Environment_Variables.Value ("LATIN_WORDS"));
      elsif Ada.Directories.Exists (INFLECTIONS_FULL_NAME)
         then return;
      else

         SEARCH_PATH :
         declare

            Path_String : constant String :=
              Ada.Environment_Variables.Value ("PATH");
            Start_Char : Natural := Path_String'First;

         begin

            for End_Char in Path_String'Range loop

               case Path_String (End_Char) is

                  when ':' | ';' | ',' =>
                     if Exists
                         (Compose
                            (Containing_Directory =>
                               Path_String (Start_Char .. (End_Char - 1)),
                             Name => INFLECTIONS_FULL_NAME))
                     then

                        Set_Directory
                          (Path_String (Start_Char .. (End_Char - 1)));

                        exit;
                     elsif End_Char + 1 < Path_String'Last then
                        Start_Char := End_Char + 1;
                     else
                        exit;
                     end if;

                  when ' ' =>
                     if End_Char + 1 < Path_String'Last then
                        Start_Char := End_Char + 1;
                     else
                        exit;
                     end if;

                  when others =>
                     null;

               end case;

               if End_Char = Path_String'Last then
                  if Exists
                      (Compose
                         (Containing_Directory =>
                            Path_String (Start_Char .. End_Char),
                          Name => "INFLECTS.SEC"))
                  then
                     Set_Directory (Path_String (Start_Char .. (End_Char)));
                     exit;
                  else
                     exit;
                  end if;
               end if;

            end loop;

         end SEARCH_PATH; -- block
      end if;

  exception
      when others =>
         null;  -- Check path silently, don't get hung up on permissions errors or nonexistent directories

   end Find_Data_And_Settings;

end LATIN_FILE_NAMES;

