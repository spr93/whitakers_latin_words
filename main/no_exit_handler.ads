-- Simple null interrupt handler for implementing NO_EXIT option
--
-- This module has some portability issues in non-POSIX environments (i.e., Windows).
-- It works with GNAT for Windows, but neither Janus nor ObjectAda's predefined environments
-- use the same interrupt scheme.
--

with Ada.Interrupts; use Ada.Interrupts;

package No_Exit_Handler is

   protected No_Exit_Handler is
      procedure No_Exit_Catch_Interr with
         Interrupt_Handler;
   end No_Exit_Handler;

   No_Exit_Handler_Access : Parameterless_Handler :=
     No_Exit_Handler.No_Exit_Catch_Interr'Access;

end No_Exit_Handler;
