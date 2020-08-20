--------------------------------------------------------------------------------
with system.Win32;                      use system.Win32;
with Interfaces.C;                      use Interfaces.C;
With Ada.Text_IO;                       use Ada.Text_IO;
with ada.Characters.Latin_1;
--------------------------------------------------------------------------------

-- This package contains a function to set Windows consoles to accept
-- vt100 formatting codes.
-- Works on Windows 10, both CMD console and PowerShell 5.x+
-- May work on earlier versions of windows in PowerShell

package windows_vt100 is

   function Enable_Windows_Console_vt100_codes return boolean;
   -- Returns true if vt100 mode was successfully enabled

   end windows_vt100;
