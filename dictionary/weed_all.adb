with Text_IO;             use Text_IO;
with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
procedure WEED_ALL (W : in out String; POFS : in PART_OF_SPEECH_TYPE) is
   --  In contrast to the Latin phase where the prioritization takes is at
   --  runtime for the English most of the work is done beforehand both the
   --  setting of a priority class for each entry in the scan of DICTLINE and
   --  the WEEDing/TRIMming done herein
   KILL : Boolean := False;
begin

   if W'LENGTH <= 1 then
      --if W(1)  not in  'A'..'Z'  then
      KILL := True;
      --end if;

   else

      if   --  WORDS words
      W = "DECL" or W = "DAT" or W = "ACC" or W = "ABL" or W = "ADJ" or
        W = "AD" or W = "BC" or W = "COMP" or W = "SUPER" or W = "DEMONST" or
        W = "INDEF" or W = "INF" or W = "KLUDGE" or W = "NE" or W = "NW" or
        W = "SE" or W = "SW" or W = "NT" or W = "OT" or W = "PASS" or
        W = "L+S" or
        W = "St" or
        W = "W" or  -- internal W (war) codes e.g., W:[alt meaning]

        W = "OLD"

      then

         KILL := True;
      end if;

--
--           if
--             --  Articles
--           W = "a"    or
--           W = "an"   or
--           W = "the"  or
--           W = "The"  or
--
--             --  Others
--           W = "no"   or
--           W = "w"       -- w/, W.
--
--           then
--
--              KILL := TRUE;
--           end if;
--

      if   --  Fragments
      W = "ad" or W = "de" or W = "bi" or W = "di" or W = "re" or W = "ex"
      then
         KILL := True;
      end if;

      if W = "abb" or     --  Abbreviation
--               --  Number suffixes
--               W = "st"   or      --  1st
--               W = "nd"   or      --  2nd
--               W = "rd"   or      --  3rd
--               W = "th"   or      --  4th

        W = "Cen"          --  e.g., "W. Cen. Gaul"
      then
         KILL := True;
      end if;

      --  Kill abbreviations
      if W (W'LAST) = '.' then
         KILL := True;
      end if;

      --  Kill internal AREA
      if W (W'LAST) = ':' then
         KILL := True;
      end if;

   end if;

   if KILL then
      -- PUT_LINE("KILLED ANY " & W);
      for I in W'RANGE loop
         W (I) := '\';
      end loop;
   end if;

   --PUT_LINE("WEEDed ANY  "  & W & '|' & BOOLEAN'IMAGE(KILL));

end WEED_ALL;
