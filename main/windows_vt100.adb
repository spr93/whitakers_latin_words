-------------------------------------
--     NON-WINDOWS PLACEHOLDER     --
-------------------------------------


package body windows_vt100 is

function Enable_Windows_Console_Vt100_Codes return boolean is

begin
      pragma Compile_Time_Warning (Standard.True,
             "VERSION FOR NON-WINDOWS TARGETS; see windows/windows_vt100.ads if building for Windows.");

return false;

end;

end windows_vt100;
