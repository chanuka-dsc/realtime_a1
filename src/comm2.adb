--Protected types: Ada lab part 4

with Ada.Calendar; use Ada.Calendar;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;

procedure comm2 is
    Message: constant String := "Protected Object";
    	type BufferArray is array (0 .. 9) of Integer;
   Max : constant Integer := 10;
        -- protected object declaration
	protected  buffer is
            -- add entries of protected object here 
      entry Put (N : in Integer);
      entry Get (N : out Integer);
      entry Stop;
      entry Shutdown;
      procedure Allow_Shutdown;
	private
            -- add local declarations
      Data : BufferArray;
      Head : Integer := 1;
      Tail : Integer := 1;
      Count : Integer := 0;
      Stopping : Boolean := False;
      Can_Shutdown : Boolean := False;
	end buffer;

	task producer is
		-- add task entries
	end producer;

	task consumer is
                -- add task entries
	end consumer;

	protected body buffer is 
              -- add definitions of protected entries here 
      entry Put(N : in Integer)
         when(Count < Max) or else Stopping is
      begin
         if Stopping then
            null;
         else
            Data(Tail) := N;
            Tail := (Tail + 1) mod Max;
            Count := Count + 1;
         end if;
      end Put;

      entry Get(N : out Integer)
         when (Count > 0) or else Stopping is
      begin
         if Count = 0 and Stopping then
            N := -1; --Sentinel value signals consumer termination
         else
            N := Data(Head);
            Head := (Head + 1) mod Max;
            Count := Count - 1;
         end if;
      end Get;

      entry Stop when True is
      begin
         Stopping := True;
      end Stop;

      entry Shutdown when Can_Shutdown is
      begin
         null;
      end Shutdown;

      procedure Allow_Shutdown is
      begin
         Can_Shutdown := True;
      end Allow_Shutdown;
	end buffer;

   task body producer is 
		Message: constant String := "producer executing";
                -- add local declrations of task here  
      subtype Range_0_20 is Integer range 0..20;
      package Rand is new Ada.Numerics.Discrete_Random (Range_0_20);
      G : Rand.Generator;

      Delay_Times : array(1..5) of Duration := (0.1, 0.15, 0.2, 0.05, 0.12);
      DT : Integer := 1;
	begin
		Put_Line(Message);
      Rand.Reset(G);
		loop
                -- add your task code inside this loop
         declare
            N : Integer := Rand.Random(G);
         begin
            buffer.Put(N);
            Put_Line ("Producer: sent" & Integer'Image(N));
         end;

         delay Delay_Times(DT);
         DT := (DT mod 5) + 1;

         select
            buffer.Shutdown;
            exit;
         or
            delay 0.0;
         end select;     
		end loop;
      Put_Line("Producer: Terminating");
	end producer;

	task body consumer is 
		Message: constant String := "consumer executing";
                -- add local declrations of task here 
      Sum : Integer := 0;
      N : Integer;
      
      Delay_Times : array(1..5) of Duration := (0.1, 0.15, 0.2, 0.05, 0.12);
      DT : Integer := 1;   
	begin
		Put_Line(Message);
		Main_Cycle:
		loop 
                -- add your task code inside this loop   
         buffer.Get(N);

         if N = -1 then
            exit Main_Cycle;
         end if;
         Put_Line ("Consumer: got" & Integer'Image(N));
         Sum := Sum + N;

         if Sum > 100 then
            Put_Line ("Consumer: sum exceeded 100, stopping...");
            buffer.Stop;
            exit Main_Cycle;
         end if;

         delay Delay_Times(DT);
         DT := (DT mod 4) + 1;
		end loop Main_Cycle; 
                -- add your code to stop executions of other tasks     
		Put_Line("Ending the consumer");
      buffer.Allow_Shutdown;
      buffer.Shutdown;
	end consumer;

begin
Put_Line(Message);
end comm2;
