with Ada.Directories;

package LATIN_FILE_NAMES is
  --  The file name conventions are a DOS legacy.

  INFLECTIONS_FULL_NAME     : constant STRING := "INFLECTS.LAT";
  INFLECTIONS_SECTIONS_NAME : constant STRING := "INFLECTS.SEC";

  UNIQUES_FULL_NAME         : constant STRING := "UNIQUES.LAT";
  ADDONS_FULL_NAME          : constant STRING := "ADDONS.LAT";

  --  These files may be created and used by the program
  MODE_FULL_NAME            : constant STRING := "WORD.MOD";
  MDEV_FULL_NAME            : constant STRING := "WORD.MDV";
  OUTPUT_FULL_NAME          : constant STRING := "WORD.OUT";
  UNKNOWNS_FULL_NAME        : constant STRING := "WORD.UNK";

  --  These file names are used with extensions (e.g., GEN, SPE, LOC)
  --  for the various dictionaries
  --  The function ADD_FILE_NAME_EXTENSION below is used to create
  --  a full file name

  DICTIONARY_FILE_NAME  : constant STRING := "DICT";
  DICT_FILE_NAME        : constant STRING := "DICTFILE";
  DICT_LINE_NAME        : constant STRING := "DICTLINE";
  STEM_LIST_NAME        : constant STRING := "STEMLIST";
  STEM_FILE_NAME        : constant STRING := "STEMFILE";
  INDX_FILE_NAME        : constant STRING := "INDXFILE";

  function ADD_FILE_NAME_EXTENSION(NAME, EXTENSION : STRING) return STRING;
  --  This is the function that creates a file name legal for your system
  --  with a FILE_NAME defined above and a program specified extension

  procedure Find_Data_And_Settings;
  function Correct_File (File_Name : in String) return String;


private
  Startup_Working_Directory  : constant STRING := Ada.Directories.Current_Directory;
  --Dictionary_Files_Directory : Ada.Strings.Unbounded.Unbounded_String; -- initialized in Initialize_Word_Package

end LATIN_FILE_NAMES;

