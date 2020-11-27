-- Simple null interrupt handler for implementing NO_EXIT option

with Ada.Interrupts; use Ada.Interrupts;


package no_exit_handler is

   protected No_Exit_Handler is
      procedure No_Exit_Catch_Interr
      with Interrupt_Handler;
   end No_Exit_Handler;

   No_Exit_Handler_Access : Parameterless_Handler := No_Exit_Handler.No_Exit_Catch_Interr'Access;

end no_Exit_handler;
