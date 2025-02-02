with TEXT_IO;
with LATIN_FILE_NAMES; use LATIN_FILE_NAMES;

package DEVELOPER_PARAMETERS is

--  This package defines a number of parameters that are used in the program
--  The default values are set in the body, so that they may be changed easily

--  These files are used by the program if requested, but not necessary
--  They are all text files and human readable

--  MDEV_FILE is used by the program to remember values

  MDEV_FILE : TEXT_IO.FILE_TYPE;

  type MDEV_TYPE is (
                      SHOW_DICTIONARY,
                      SHOW_DICTIONARY_LINE,
                      SHOW_DICTIONARY_CODES,
                      DO_PEARSE_CODES,

                      -- DO_ONLY_INITIAL_WORD,
                      -- FOR_WORD_LIST_CHECK,

                      DO_ONLY_FIXES,
                      DO_FIXES_ANYWAY,
                      USE_PREFIXES,
                      USE_SUFFIXES,
                      USE_TACKONS,

                      DO_MEDIEVAL_TRICKS,

                      DO_SYNCOPE,
                      DO_TWO_WORDS,
                      INCLUDE_UNKNOWN_CONTEXT,

                      OMIT_ARCHAIC,
                      OMIT_MEDIEVAL,
                      OMIT_UNCOMMON,

                      PAUSE_IN_SCREEN_OUTPUT,
                    --NO_SCREEN_ACTIVITY,

                      UPDATE_LOCAL_DICTIONARY,
                    --UPDATE_MEANINGS,

                      MINIMIZE_OUTPUT         );

  package MDEV_TYPE_IO is new TEXT_IO.ENUMERATION_IO(MDEV_TYPE);

  type    MDEV_ARRAY   is array (MDEV_TYPE) of BOOLEAN;

  WORDS_MDEV : MDEV_ARRAY;        --  Initialized in body

  START_FILE_CHARACTER               : CHARACTER := '@';
  CHANGE_DEVELOPER_MODES_CHARACTER   : CHARACTER := '!';

  procedure CHANGE_DEVELOPER_MODES;

  procedure UPDATE_LOCAL_DICTIONARY_FILE;

  procedure INITIALIZE_DEVELOPER_PARAMETERS;

end DEVELOPER_PARAMETERS;
