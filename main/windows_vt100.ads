-------------------------------------
--     NON-WINDOWS PLACEHOLDER     --
-------------------------------------


package windows_vt100 is

   Is_Windows : constant Boolean := False;

   function Enable_Windows_Console_vt100_codes return boolean;
   -- Returns true if vt100 mode was successfully enabled

end windows_vt100;
