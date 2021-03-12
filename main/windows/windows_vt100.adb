--------------------------------------------------------------------------------
with System.Win32;                      use System.Win32;
with Interfaces.C;
--------------------------------------------------------------------------------

package body Windows_vt100 is

function Enable_Windows_Console_vt100_codes return boolean is

   Std_Output_nString : Interfaces.C.unsigned_long := Interfaces.C.unsigned_long'mod(-11);
   Enable_Virtual_Terminal_Processing  : constant := 16#0004#;

   StdOut     : handle;
   StdOutMode : aliased dword;
   Result     : bool;

   function GetStdHandle (Std_nString : Interfaces.C.unsigned_long) return handle;
   pragma Import (stdcall, GetStdHandle, "GetStdHandle");

   function GetConsoleMode (console : handle; mode : access dword) return bool;
   pragma Import (stdcall, GetConsoleMode, "GetConsoleMode");

   function SetConsoleMode (console : handle; mode : dword) return bool;
   pragma Import (stdcall, SetConsoleMode, "SetConsoleMode");

begin

   stdout := GetStdHandle(std_output_nstring);

   Result := GetConsoleMode(stdout , StdOutMode'unchecked_access);
   StdOutMode := StdOutMode or ENABLE_VIRTUAL_TERMINAL_PROCESSING;
   Result := SetConsoleMode (StdOut, StdOutMode);

 --  Put_Line(result'image);  -- DEBUG

   if result /= 0 then return true;
   else return false;
      end if;
end;

end Windows_vt100;
