with STRINGS_PACKAGE;  use STRINGS_PACKAGE;
with LATIN_FILE_NAMES; use LATIN_FILE_NAMES;
with CONFIG;           use CONFIG;
with PREFACE;
with Windows_vt100;    -- main/nonwindows placeholder package if non-Windows target
with Unicode_Features; -- main/nonunicode placeholder package if not compiler supported

pragma Elaborate(PREFACE);

package body WORD_PARAMETERS is
   use TEXT_IO;

  type HELP_TYPE is array (NATURAL range <>) of STRING(1..70);
  BLANK_HELP_LINE : constant STRING(1..70) := (others => ' ');
  NO_HELP : constant HELP_TYPE := (2..1 => BLANK_HELP_LINE);

  type REPLY_TYPE is (N, Y);
  package REPLY_TYPE_IO is new TEXT_IO.ENUMERATION_IO(REPLY_TYPE);
  REPLY : array (BOOLEAN) of REPLY_TYPE := (N, Y);
  MODE_OF_REPLY : array (REPLY_TYPE) of BOOLEAN := (FALSE, TRUE);
  BLANK_INPUT : exception;

TRIM_OUTPUT_HELP : constant HELP_TYPE :=  (
   "This option instructs the program to remove from the output list of   ",
   "possible constructs those which are least likely.  There is now a fair",
   "amount of trimming, killing LOC and VOC plus removing Uncommon and    ",
   "non-classical (Archaic/Medieval) when more common results are found   ",
   "and this action is requested (turn it off in MDV (!) parameters).     ",
   "When a TRIM has been done, output is usually followed by asterix (*). ",
   "The asterix may be missing depending on where the TRIM is done.       ",
   "There certainly is no absolute assurence that the items removed are   ",
   "not correct, just that they are statistically less likely.            ",
   "Note that poets are likely to employ unusual words and inflections for",
   "various reasons.  These may be trimmed out if this parameter in on.   ",
   "When in English-Latin mode, TRIM just reduces the output to the top   ",
   "six results, if there are that many. was trimmed and more results are ",
   "available.                                            Default is N(o).",
   "Setting to (Y)es is HIGHLY RECOMMENDED when using Latin-English mode. ");

HAVE_OUTPUT_FILE_HELP : constant HELP_TYPE :=  (
   "This option instructs the program to create a file which can hold the ",
   "output for later study, otherwise the results are just displayed on   ",
   "the screen.  The output file is named " & OUTPUT_FULL_NAME
                                  & (39+OUTPUT_FULL_NAME'LENGTH..70 => ' '),
   "This means that one run will necessarily overwrite a previous run,    ",
   "unless the previous results are renamed or copied to a file of another",
   "name.  This is available if the METHOD is INTERACTIVE, no parameters. ",
   "The default is N(o), since this prevents the program from overwriting ",
   "previous work unintentionally.  Y(es) creates the output file.        " );

WRITE_OUTPUT_TO_FILE_HELP : constant HELP_TYPE :=  (
   "This option instructs the program, when HAVE_OUTPUT_FILE is on, to    ",
   "RE-DIRECT results to the file " & OUTPUT_FULL_NAME
                                  & (31+OUTPUT_FULL_NAME'LENGTH..70 => ' '),
   "This option may be turned on and off during running of the program,   ",
   "thereby capturing only certain desired results.  Only available when  ",
   "HAVE_OUTPUT_FILE is on.                                               ",
   "Note that output is RE-DIRECTED, and therefore not duplicated on      ",
   "screen; if you want a transcript of your session, use 'tee' or a      ",
   "similar utility.                                      Default is N(o).");

WRITE_UNKNOWNS_TO_FILE_HELP : constant HELP_TYPE :=  (
   "This option instructs the program to write all unresolved words to a  ",
   "UNKNOWNS file named " & UNKNOWNS_FULL_NAME
                                & (21+UNKNOWNS_FULL_NAME'LENGTH..70 => ' '),
   "With this option on, the file of unknowns is written, even though     ",
   "the main output contains both known and unknown (unresolved) words.   ",
   "One may wish to save the unknowns for later analysis, testing, or to  ",
   "form the basis for dictionary additions.  When this option is turned  ",
   "on, the UNKNOWNS file is written, destroying any file from a previous ",
   "run.  However, the write may be turned on and off during a single run ",
   "without destroying the information written in that run.               ",
   "This option is for specialized use, so its default is N(o).           ",
   "This does not work in English mode, but may in the future.            " );

-- SPR:  Changed the following two options from default yes to default no because
--       they restrict results based on assumptions about capitalization conventions
--       in the source text as well as supposed user expectations.
--       I don't believe input FORMATTING should ordinarily affect the results'
--       SUBSTANCE; the default results should be consistent and thorough.
--       (The new Unicode handling/macron-stripping feature follows the same principle.)
IGNORE_UNKNOWN_NAMES_HELP : constant HELP_TYPE :=  (
   "This option instructs the program to disregard unknown capitalized    ",
   "words of more than three letters.  This avoids distracting UNKNOWN    ",
   "results when translating texts that contain many proper names.        ",
   "                                                  The default is N(o).");

IGNORE_UNKNOWN_CAPS_HELP : constant HELP_TYPE :=  (
   "This option instructs the program to assume that any all caps word    ",
   "is a proper name (as is common in plays and texts of discussions) or  ",
   "an ad hoc acronym.  This may avoid distracting UNKNOWN results.       ",
   "                                                  The default is N(o).");

DO_COMPOUNDS_HELP : constant HELP_TYPE :=  (
   "This option instructs the program to look ahead for the verb TO_BE (or",
   "iri) when it finds a verb participle, with the expectation of finding ",
   "a compound perfect tense or periphastic.  This option can also be a   ",
   "trimming of the output, in that VPAR that do not fit (not NOM) will be",
   "excluded, possible interpretations are lost.  Default choice is Y(es).",
   "This processing is turned off with the choice of N(o).                " );

DO_FIXES_HELP : constant HELP_TYPE :=  (
   "This option instructs the program, when it is unable to find a proper ",
   "match in the dictionary, to attach various prefixes and suffixes and  ",
   "try again.  This effort is successful in about a quarter of the cases ",
   "which would otherwise give UNKNOWN results, or so it seems in limited ",
   "tests.  For those cases in which a result is produced, about half give",
   "easily interpreted output; many of the rest are etymologically true,  ",
   "but not necessarily obvious; about a tenth give entirely spurious     ",
   "derivations.  The user must proceed with caution.                     ",
   "The default choice is Y(es), since the results are generally useful.  ",
   "This processing can be turned off with the choice of N(o).            " );

DO_TRICKS_HELP : constant HELP_TYPE :=  (
   "This option instructs the program, when it is unable to find a proper ",
   "match in the dictionary, and after various prefixes and suffixes, to  ",
   "try every dirty Latin trick it can think of, mainly common letter     ",
   "replacements like cl -> cul, vul -> vol, ads -> ass, inp -> imp, etc. ",
   "Together these tricks are useful, but may give false positives (>10%).",
   "They provide for recognized varients in classical spelling.  Most of  ",
   "the texts with which this program will be used have been well edited  ",
   "and standardized in spelling.  Now, moreover,  the dictionary is being",
   "populated to such a state that the hit rate on tricks has fallen to a ",
   "low level.  It is very seldom productive, and it is always expensive. ",
   "The only excuse for keeping it as default is that now the dictionary  ",
   "is quite extensive and misses are rare.         Default is now Y(es). ") ;

DO_DICTIONARY_FORMS_HELP : constant HELP_TYPE :=  (
   "This option instructs the program to output a line with the forms     ",
   "normally associated with a dictionary entry (NOM and GEN of a noun,   ",
   "the four principal parts of a verb, M-F-N NOM of an adjective, ...).  ",
   "This occurs when there is other output (i.e., not with UNKNOWNS_ONLY).",
   "The default choice is N(o), but it can be turned on with a Y(es).     " );

SHOW_AGE_HELP : constant HELP_TYPE :=  (
   "This option causes a flag, like '<Late>' to appear for inflection or  ",
   "form in the output.  The AGE indicates when this word/inflection was  ",
   "in use, at least from indications is dictionary citations.  It is     ",
   "just an indication, not controlling, useful when there are choices.   ",
   "No indication means that it is common throughout all periods.         ",
   "The default choice is Y(es), but it can be turned off with a N(o).    " );

SHOW_FREQUENCY_HELP : constant HELP_TYPE :=  (
   "This option causes a flag, like '<rare>' to appear for inflection or  ",
   "form in the output.  The FREQ is indicates the relative usage of the  ",
   "word or inflection, from indications is dictionary citations.  It is  ",
   "just an indication, not controlling, useful when there are choices.   ",
   "No indication means that it is common throughout all periods.         ",
   "The default choice is Y(es), but it can be turned off with a N(o).    " );

DO_EXAMPLES_HELP : constant HELP_TYPE :=  (
   "This option instructs the program to provide examples of usage of the ",
   "cases/tenses/etc. that were constructed.  The default choice is N(o). ",
   "This produces lengthly output and is turned on with the choice Y(es). " );

DO_ONLY_MEANINGS_HELP : constant HELP_TYPE :=  (
   "This option instructs the program to only output the MEANING for a    ",
   "word, and omit the inflection details.  This is primarily used in     ",
   "analyzing new dictionary material, comparing with the existing.       ",
   "However it may be of use for the translator who knows most all of     ",
   "the words and just needs a little reminder for a few.                 ",
   "The default choice is N(o), but it can be turned on with a Y(es).     " );

DO_STEMS_FOR_UNKNOWN_HELP : constant HELP_TYPE :=  (
   "This option instructs the program, when it is unable to find a proper ",
   "match in the dictionary, and after various prefixes and suffixes, to  ",
   "list the dictionary entries around the unknown.  This will likely     ",
   "catch a substantive for which only the ADJ stem appears in dictionary,",
   "an ADJ for which there is only a N stem, etc.    The default is Y(es).");

DO_ARABIC_NUMERALS_HELP : constant HELP_TYPE :=  (
   "This option instructs the program to process Arabic-Hindu numerals as ",
   "words for translation to Roman numerals.  This option applies to both ",
   "English-Latin and Latin-English modes.  If set to N(o), then the      ",
   "program disregards Arabic-Hindu numerals and treats them like spaces. ",
   "spaces.                                          The default is Y(es).");

DO_ANSI_FORMATTING_HELP : constant HELP_TYPE :=  (
   "This option uses standard ANSI tty control characters to format the   ",
   "dictionary output.  The format emphasizes the dictionary and meaning  ",
   "lines, as well as any important usage notes. The goal is to make      ",
   "skimming large output easier.  This parameter has no effect on file   ",
   "output, which is always in plain text.           The default is Y(es).");

DIM_EXAMPLES_TEXT_HELP : constant HELP_TYPE :=  (
   "This option formats the examples line using the ANSI tty control      ",
   "character for 'dimmed' or 'faint' text.  It may make skimming long    ",
   "results faster by keeping the eye focused on the inflections line.    ",
   "However, on some consoles the text may be too difficult to read.  On  ",
   "others, the effect may be to invert the text color rather than dim it.",
   "This parameter has no effect if DO_ANSI_FORMATTING is off.            ",
   "                                                  The default is N(o).");

DO_UNICODE_HELP_TEXT : constant HELP_TYPE :=  (
   "This option allows the program to accept input text with macrons.  It ",
   "takes the input text as Unicode (UTF-8), then converts any accented   ",
   "characters to their basic form before processing.  Words ouptut will  ",
   "still be in plain ASCII (Latin-1). Disable if your input is garbled or",
   "to increase performance when processing large plain-text files.       ",
   "When this parameter is disabled, accented characters will be skipped. ",
   "                                                 The default is Y(es).");

SAVE_PARAMETERS_HELP : constant HELP_TYPE :=  (
   "This option instructs the program, to save the current parameters, as ",
   "just established by the user, in a file WORD.MOD.  If such a file     ",
   "exists, the program will load those parameters at the start.  If no   ",
   "such file can be found in the current subdirectory, the program will  ",
   "start with a default set of parameters.  Since this parameter file is ",
   "human-readable ASCII, it may also be created with a text editor.  If  ",
   "the file found has been improperly created, is in the wrong format, or",
   "otherwise uninterpretable by the program, it will be ignored and the  ",
   "default parameters used, until a proper parameter file in written by  ",
   "the program.  Since one may want to make temporary changes during a   ",
   "run, but revert to the usual set, the default is N(o).                " );

  procedure PUT(HELP : HELP_TYPE) is
  begin
    NEW_LINE;
    for I in HELP'RANGE  loop
      PUT_LINE(HELP(I));
    end loop;
    NEW_LINE;
  end PUT;

  procedure PUT_MODES is
    use MODE_TYPE_IO;
    use REPLY_TYPE_IO;
  begin
    if IS_OPEN(MODE_FILE)  then
      CLOSE(MODE_FILE);
    end if;
    CREATE(MODE_FILE, OUT_FILE, Correct_File(MODE_FULL_NAME));
    for I in WORDS_MODE'RANGE  loop
      PUT(MODE_FILE, I);
      SET_COL(MODE_FILE, 35);
      PUT(MODE_FILE, REPLY(WORDS_MODE(I)));
      NEW_LINE(MODE_FILE);
    end loop;
    CLOSE(MODE_FILE);

    exception
    when others =>
      Put_Line("Error saving WORD.MOD file.  Changes may not have been saved.");
  end PUT_MODES;

  procedure GET_MODES is --(M : out MODE_ARRAY) is
    use MODE_TYPE_IO;
    use REPLY_TYPE_IO;
    MO : MODE_TYPE;
    REP : REPLY_TYPE;
  begin
    OPEN(MODE_FILE, IN_FILE, Correct_File(MODE_FULL_NAME));
    while not END_OF_FILE(MODE_FILE)  loop
      GET(MODE_FILE, MO);
      GET(MODE_FILE, REP);
      WORDS_MODE(MO) := MODE_OF_REPLY(REP);
    end loop;
    CLOSE(MODE_FILE);
    PREFACE.PUT_LINE("MODE_FILE loaded");
  exception
    when NAME_ERROR  =>
      WORDS_MODE := DEFAULT_MODE_ARRAY;

    when Data_Error | End_Error | Layout_Error  =>
      Preface.Format(Inverse);
      PREFACE.PUT("MODE_FILE exists, but empty or corrupted.  Falling back to defaults.");
      Preface.Format(Bold);
      Preface.New_Line;
      PREFACE.PUT_LINE("You can create a replacement file by typing " & CHANGE_PARAMETERS_CHARACTER & " and saving.");
      Preface.Format(Reset);
      WORDS_MODE := DEFAULT_MODE_ARRAY;

    when Status_Error | Use_Error =>
      Preface.Format(Inverse);
      PREFACE.PUT("Access to the existing MODE_FILE was refused.  Falling back to defaults.");
      Preface.Format(Reset);
      Preface.New_Line;
      WORDS_MODE := DEFAULT_MODE_ARRAY;

    when others  =>
      Preface.Format(Inverse);
      PREFACE.PUT("UKNOWN ERROR LOADING MODE_FILE.  Falling back to defaults.");
      Preface.Format(Reset);
      Preface.New_Line;
      WORDS_MODE := DEFAULT_MODE_ARRAY;
  end GET_MODES;

  procedure Check_Compatibility is
  begin
    if ( WORDS_MODE(DO_ONLY_MEANINGS) or CONFIGURATION = ONLY_MEANINGS )
      and then WORDS_MODE(Do_Examples)
    then
      Preface.Put("MODE override: DO_EXAMPLES disabled because meanings-only restriction set");
      WORDS_MODE(DO_EXAMPLES) := False;
      Preface.New_Line;
    end if;

    if WORDS_MODE(WRITE_OUTPUT_TO_FILE) and not WORDS_MODE(HAVE_OUTPUT_FILE) then
         Preface.Put("MODE override: WRITE_OUTPUT_TO_FILE disabled because HAVE_OUTPUT_FILE disabled");
      WORDS_MODE(WRITE_OUTPUT_TO_FILE) := False;
      Preface.New_Line;
    end if;

  end Check_Compatibility;


  procedure INQUIRE(MO : MODE_TYPE; HELP : in HELP_TYPE := NO_HELP) is
    use MODE_TYPE_IO;
    use REPLY_TYPE_IO;
    L1 : STRING(1..100) := (others => ' ');
    LL : NATURAL;
    R  : REPLY_TYPE;
  begin
    PUT(MO);
    PUT(" ?  "); SET_COL(45); PUT("(Currently  ");
    PUT(REPLY(WORDS_MODE(MO))); PUT(" =>");
    GET_LINE(L1, LL);
    if LL /= 0  then
      if TRIM(L1(1..LL)) = ""  then
        PUT_LINE("Blank input, skipping the rest of CHANGE_PARAMETERS");
        raise BLANK_INPUT;
      elsif L1(1) = '?'  then
        PUT(HELP);
        INQUIRE(MO, HELP);
      else
        GET(L1(1..LL), R, LL);
        WORDS_MODE(MO) := MODE_OF_REPLY(R);
      end if;
    end if;
    NEW_LINE;
  end INQUIRE;

  procedure CHANGE_PARAMETERS is
    L1 : STRING(1..100) := (others => ' ');
    LL : NATURAL;
    R  : REPLY_TYPE;

  begin

    PUT_LINE("To set/change parameters reply Y/y or N/n.  Return accepts current value.");
    PUT_LINE("A '?' reply gives information/help on that parameter.  A space skips the rest.");
    NEW_LINE;

  --  Interactive mode - lets you do things on unknown words

  --  You can say it is a noun and then look at the endings
  --  Or look all the endings and guess what part of speech

  --  You can look at the dictionary items that are close to the word
  --  There may be cases in which the stem is found but is not of right part
  --  So maybe the word list is deficient and that root goes also to a ADJ
  --  even if it is listed only for a N.
  --  One can also look for ADV here with ending 'e', etc.

  --  You can look up the word in a paper dictionary (with the help of ending)
  --  And then enter the word into DICT.LOC, so it will hit next time

  --  All unknowns could be recorded in a file for later reference

  --  A '?' gives information (help) about the item in question

  --  One can change the symbol that the main program uses for change and file

  --  One can save the new parameters or let them revert to previous
  --  There should be a basic set of parameters that one can always go to

  --  There should be moods of translation, maybe to switch dictionaries

  --  Maybe to turn on or off pre/suffix
  --  Maybe to allow the user to look at just all the prefixes that match

    INQUIRE(TRIM_OUTPUT, TRIM_OUTPUT_HELP);


    if not CL_Arguments(NO_FILES) then  -- We test for Read_Only before allowing Change_Paramaters
    INQUIRE(HAVE_OUTPUT_FILE, HAVE_OUTPUT_FILE_HELP);

    if IS_OPEN(OUTPUT)  and then not WORDS_MODE(HAVE_OUTPUT_FILE)  then
      CLOSE(OUTPUT);
      WORDS_MODE(WRITE_OUTPUT_TO_FILE) := FALSE;
    end if;
    if not IS_OPEN(OUTPUT) and then WORDS_MODE(HAVE_OUTPUT_FILE)  then
      begin
        CREATE(OUTPUT, OUT_FILE, Correct_File(OUTPUT_FULL_NAME));
      exception
        when others =>
        PUT_LINE("Cannot create WORD.OUT - Check if it is in use elsewhere");
      end;
      end if;

    if WORDS_MODE(HAVE_OUTPUT_FILE) then
      INQUIRE(WRITE_OUTPUT_TO_FILE, WRITE_OUTPUT_TO_FILE_HELP);
    end if;

    INQUIRE(WRITE_UNKNOWNS_TO_FILE, WRITE_UNKNOWNS_TO_FILE_HELP);

    --  If there is an open file then OK
    --  If not open and you now want to start writing to UNKNOWNS, the CREATE
    if not IS_OPEN(UNKNOWNS) and then WORDS_MODE(WRITE_UNKNOWNS_TO_FILE)  then
      begin
        CREATE(UNKNOWNS, OUT_FILE, Correct_File(UNKNOWNS_FULL_NAME));
      exception
        when others =>
          PUT_LINE("Cannot CREATE WORD.UNK");
      end;
      end if;

    end if; -- not CL_Arguments(NO_FILES)

    INQUIRE(IGNORE_UNKNOWN_NAMES, IGNORE_UNKNOWN_NAMES_HELP);

    INQUIRE(IGNORE_UNKNOWN_CAPS, IGNORE_UNKNOWN_CAPS_HELP);

    INQUIRE(DO_COMPOUNDS, DO_COMPOUNDS_HELP);

    INQUIRE(DO_FIXES, DO_FIXES_HELP);

    INQUIRE(DO_TRICKS, DO_TRICKS_HELP);

    if CONFIGURATION /= ONLY_MEANINGS then   -- enclosing CL_Arguments(meanings_only) condition
      INQUIRE(DO_ONLY_MEANINGS, DO_ONLY_MEANINGS_HELP);
      if WORDS_MODE(DO_ONLY_MEANINGS) then
        WORDS_MODE(DO_EXAMPLES) := FALSE;
        else
        INQUIRE(DO_EXAMPLES, DO_EXAMPLES_HELP);
      end if;
    end if;  -- enclosing CL_Arguments(meanings_only) condition

    INQUIRE(DO_DICTIONARY_FORMS, DO_DICTIONARY_FORMS_HELP);

    INQUIRE(DO_STEMS_FOR_UNKNOWN, DO_STEMS_FOR_UNKNOWN_HELP);

    INQUIRE(SHOW_AGE, SHOW_AGE_HELP);

    INQUIRE(SHOW_FREQUENCY, SHOW_FREQUENCY_HELP);

    INQUIRE(DO_ARABIC_NUMERALS, DO_ARABIC_NUMERALS_HELP);

    INQUIRE(DO_ANSI_FORMATTING, DO_ANSI_FORMATTING_HELP);

      if WORDS_MODE(DO_ANSI_FORMATTING)
         and then Windows_Vt100.Is_Windows
      then
         case Windows_Vt100.Enable_Windows_Console_vt100_Codes is
            when True => null;
            when False => Put_Line("INFO:  Terminal unable to enter vt100 mode.  ANSI formatting off.");
            WORDS_MODE(DO_ANSI_FORMATTING) := False;
         end case;
      end if;

       if WORDS_MODE(DO_ANSI_FORMATTING) then
    INQUIRE(DIM_EXAMPLES_TEXT, DIM_EXAMPLES_TEXT_HELP);
    end if;

 if Unicode_Features.Unicode_Function_Available then
    INQUIRE(DO_UNICODE_INPUT, DO_UNICODE_HELP_TEXT);
  end if;

    if not CL_Arguments(NO_FILES) then
    PUT("Do you wish to save this set of parameters? Y or N (Default) ");
    PUT(" =>");
    GET_LINE(L1, LL);
    if LL /= 0  then
      if L1(1) = '?'  then
        PUT(SAVE_PARAMETERS_HELP);
        PUT("Do you wish to save this set of parameters? Y or N (Default) ");
        PUT(" =>");
        GET_LINE(L1, LL);
      end if;
      REPLY_TYPE_IO.GET(L1(1..LL), R, LL);
      if MODE_OF_REPLY(R)  then
        PUT_MODES;
        PUT_LINE("MODE_ARRAY saved in file " & MODE_FULL_NAME);
      end if;
    end if;
    end if; -- not CL_Arguments(NO_FILES)

    NEW_LINE;

  exception
    when BLANK_INPUT  =>
      null;
    when others =>
      PUT_LINE("Bad input - terminating CHANGE_PARAMETERS");

  end CHANGE_PARAMETERS;

  procedure INITIALIZE_WORD_PARAMETERS is
  begin

    GET_MODES; --(WORDS_MODE);

    Check_Compatibility;

  if (WORDS_MODE(HAVE_OUTPUT_FILE))
        and then ( (METHOD = INTERACTIVE) or (METHOD = COMMAND_LINE_INPUT) )
        and then not TEXT_IO.IS_OPEN(OUTPUT)  -- belt+suspenders
     then
     DO_OUTPUT_FILE:
     begin
     TEXT_IO.CREATE(OUTPUT, TEXT_IO.OUT_FILE, Correct_File(OUTPUT_FULL_NAME));
     PREFACE.PUT_LINE("WORD.OUT created");

     exception
      when Text_IO.Status_Error | Text_IO.Use_Error =>
      Preface.Format(Inverse);
      PREFACE.PUT("Could not get exclusive write access to WORD.OUT.  Disabling HAVE_OUTPUT_FILE.");
      Preface.Format(Reset);
      Preface.New_Line;

      when others  =>
      Preface.Format(Inverse);
      PREFACE.PUT("Error setting up WORD.OUT.  Disabling HAVE_OUTPUT_FILE.");
      Preface.Format(Reset);
      Preface.New_Line;
      end DO_OUTPUT_File;
  end if;

    if  WORDS_MODE(WRITE_UNKNOWNS_TO_FILE)
      and then not TEXT_IO.IS_OPEN(UNKNOWNS)
      then
      DO_UNKNOWNS_FILE:
      begin
      TEXT_IO.CREATE(UNKNOWNS, TEXT_IO.Append_File, Correct_File(UNKNOWNS_FULL_NAME));
      PREFACE.PUT_LINE("WORD.UNK created at initialization");
      exception
      when Text_IO.Status_Error | Text_IO.Use_Error =>
      Preface.Format(Inverse);
      PREFACE.PUT("Could not get exclusive write access to WORD.UNK.  Continuing without it.");
      Preface.Format(Reset);
      Preface.New_Line;
      when others  =>
      Preface.Format(Inverse);
      PREFACE.PUT("Error setting up WORD.UNK.  Continuing without it.");
      Preface.Format(Reset);
      Preface.New_Line;
      end DO_UNKNOWNS_FILE;

      end if;
end INITIALIZE_WORD_PARAMETERS;

end WORD_PARAMETERS;
