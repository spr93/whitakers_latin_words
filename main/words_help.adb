with Ada.Text_IO; use Ada.Text_IO;


package body words_help is

   ---------------
   -- SHOW_HELP --
   ---------------

   procedure PUT(HELP : in Main_HELP_TYPE) is
  begin
    NEW_LINE;
    for I in HELP'FIRST..HELP'LAST  loop
      PUT_LINE(HELP(I));
    end loop;
    NEW_LINE;
   end PUT;

   procedure SHOW_HELP (Line : in String) is

      Help_Param : String(1..3) := (others => ' ');
    begin

      if Line'LENGTH >= 3 then
         Help_Param := Line(Line'First..(Line'First + 2));
      end if;

      if    Help_Param = "COD"                          then
            Put(CODES_HELP);
         elsif Help_Param = "SOU"                       then
            Put(SOURCE_HELP);
         elsif Help_Param = "GRA"                       then
            Put(GRAMMAR_HELP);
         elsif Help_Param = "AGE" or Help_Param = "ERA" then
            Put(AGE_HELP);
         elsif Help_Param = "ARE"                       then
            Put(AREA_HELP);
         elsif Help_Param = "GEO"                       then
            Put(GEO_HELP);
         elsif Help_Param = "FRE"                       then
            Put(FREQ_HELP);
         elsif Help_Param = "MEA" or Help_Param = "ABB" then
            Put (MEANING_HELP);
         elsif Help_Param = "PEA"                       then
            Put(PEARSE_HELP);
         Else
            Put(GENERAL_HELP);
     end if;

   exception
      when others => null;

   end SHOW_HELP;

end words_help;
