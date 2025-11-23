--Process commnication: Ada lab part 3

with Ada.Calendar;
with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
use Ada.Calendar;
use Ada.Text_IO;

procedure comm1 is
   Message : constant String := "Process communication";

   type BufferArray is array (0 .. 9) of Integer;
   Max : constant Integer := 10;

   task buffer is
      -- add your task entries for communication
      entry Put (N : in Integer);
      entry Get (N : out Integer);
      entry Stop;
      entry Allow_Shutdown;
      entry Shutdown;
   end buffer;

   task producer is
      -- add your task entries for communication
   end producer;

   task consumer is
      -- add your task entries for communication
   end consumer;
   task body buffer is
      Message      : constant String := "buffer executing";
      -- change/add your local declarations here
      Data         : BufferArray;
      Head         : Integer := 0;
      Tail         : Integer := 0;
      Count        : Integer := 0;
      Stopping     : Boolean := False;
      Can_Shutdown : Boolean := False;
   begin
      Put_Line (Message);
      loop
         -- add your task code inside this loop
         select
            when(Count < Max) or else Stopping
            =>accept Put (N : in Integer) do
               if not Stopping then
                  Data (Tail) := N;
                  Tail := (Tail + 1) mod Max;
                  Count := Count + 1;
               else
                  -- Ignore values after shutdown
                  null;
               end if;
            end Put;
         or
            when Count > 0
            =>accept Get (N : out Integer) do
               N := Data (Head);
               Head := (Head + 1) mod Max;
               Count := Count - 1;
            end Get;
         or
            when Stopping and then Count = 0
            =>accept Get (N : out Integer) do
               N := -1; --Sentinel value to signal shutdown
            end Get;
         or
            accept Stop do
               Stopping := True;
            end Stop;

         or
            accept Allow_Shutdown do
               Can_Shutdown := True;
            end Allow_Shutdown;
         or
            when Can_Shutdown
            =>accept Shutdown do
               -- just a synchronization point
               null;
            end Shutdown;
         end select;
      end loop;
   end buffer;

   task body producer is
      Message : constant String := "producer executing";
      -- change/add your local declarations here
      subtype Range_0_20 is Integer range 0 .. 20;
      package Rand is new Ada.Numerics.Discrete_Random (Range_0_20);
      G       : Rand.Generator;

      Delay_Times : array (1 .. 5) of Duration := (0.1, 0.15, 0.2, 0.05, 0.12);
      DT          : Integer := 1;
   begin
      Put_Line (Message);
      Rand.Reset (G);
      loop
         -- add your task code inside this loop
         declare
            N : Integer := Rand.Random (G);
         begin
            buffer.Put (N);
            Put_Line ("Producer: sent" & Integer'Image (N));
         end;

         delay Delay_Times (DT);
         DT := (DT mod 5) + 1;

         select
            -- tries to shutdown if allowed
            buffer.Shutdown;
            exit;
         or
            delay 0.0;
         end select;
      end loop;
      Put_Line ("Producer: Terminating");
   end producer;

   task body consumer is
      Message     : constant String := "consumer executing";
      -- change/add your local declarations here
      Sum         : Integer := 0;
      N           : Integer;
      Done        : Boolean := False;
      Delay_Times : array (1 .. 5) of Duration := (0.1, 0.15, 0.2, 0.05, 0.12);
      DT          : Integer := 1;
   begin
      Put_Line (Message);
      Main_Cycle :
      loop
         -- add your task code inside this loop
         buffer.Get (N);

         exit Main_Cycle when N = -1;
         Put_Line ("Consumer: got" & Integer'Image (N));
         if not Done then
            Sum := Sum + N;
            Put_Line ("Consumer: sum =" & Integer'Image (Sum));

            if Sum > 100 then
               Done := True;
               Put_Line ("Consumer: sum exceeded 100, stopping...");
               buffer.Stop;  -- tell buffer to enter stopping mode

            end if;
         end if;
         delay Delay_Times (DT);
         DT := (DT mod 5) + 1;
      end loop Main_Cycle;
      -- add your code to stop executions of other tasks
      Put_Line ("Ending the consumer");
      buffer.Allow_Shutdown;
   exception
      when TASKING_ERROR =>
         Put_Line ("Buffer finished before producer");
         Put_Line ("Ending the consumer");
   end consumer;
begin
   Put_Line (Message);
end comm1;
