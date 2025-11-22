with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Calendar;        use Ada.Calendar;

procedure Realtime_A4 is

   --Named array type for FIFO
   Max : constant := 10;
   type Int_Array is array (1 .. Max) of Integer;

   --Protected FIFO Buffer
   protected FIFO_Buffer is
      entry Put (N : in Integer);
      entry Get (N : out Integer);
      entry Stop;  --Called by consumer to signal termination
   private
      Data     : Int_Array;
      Head     : Integer := 1;
      Tail     : Integer := 1;
      Count    : Integer := 0;
      Stopping : Boolean := False;
   end FIFO_Buffer;

   protected body FIFO_Buffer is

      entry Put (N : in Integer)
        when (Count < Max) or else Stopping is
      begin
         if Stopping then
            --Discard values after termination
            null;
         else
            Data (Tail) := N;
            Tail := (Tail mod Max) + 1;
            Count := Count + 1;
         end if;
      end Put;

      entry Get (N : out Integer)
        when Count > 0 or else Stopping is
      begin
         if Count = 0 and Stopping then
            N := -1;
         else
            N := Data (Head);
            Head := (Head mod Max) + 1;
            Count := Count - 1;
         end if;
      end Get;

      entry Stop when True is
      begin
         Stopping := True;
      end Stop;

   end FIFO_Buffer;

   --Producer 
   task Producer;

   task body Producer is
      subtype Range_0_20 is Integer range 0 .. 20;

      package Rand is new Ada.Numerics.Discrete_Random (Range_0_20);
      G      : Rand.Generator;

      Delay_Times : array (1 .. 5) of Duration := (0.1, 0.15, 0.2, 0.05, 0.12);
      DT_Index    : Natural := 1;

   begin
      Rand.Reset (G);

      loop
         declare
            N : Integer := Rand.Random (G);
         begin
            FIFO_Buffer.Put (N);
            Put_Line ("Producer: sent " & Integer'Image (N));
         end;

         delay Delay_Times (DT_Index);
         DT_Index := (DT_Index mod 5) + 1;

         --Detect termination by zero-time select
         select
            FIFO_Buffer.Stop;
            exit;
         or
            delay 0.0;
         end select;

      end loop;

      Put_Line ("Producer: terminating.");

   exception
      when others =>
         Put_Line ("Producer: unexpected error");
   end Producer;

   --Consumer
   task Consumer;

   task body Consumer is
      Sum : Integer := 0;
      N   : Integer;
      Delay_Times : array (1 .. 4) of Duration := (0.25, 0.15, 0.3, 0.1);
      DT_Index    : Natural := 1;
   begin
      loop
         FIFO_Buffer.Get (N);

         if N = -1 then
            exit;
         end if;

         Put_Line ("Consumer: got " & Integer'Image (N));
         Sum := Sum + N;

         if Sum > 100 then
            Put_Line ("Consumer: sum exceeded 100, stopping system.");
            FIFO_Buffer.Stop;
            exit;
         end if;

         delay Delay_Times (DT_Index);
         DT_Index := (DT_Index mod 4) + 1;
      end loop;

      Put_Line ("Consumer: terminating.");
   end Consumer;


begin
   null;
end Realtime_A4;
