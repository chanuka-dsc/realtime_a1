--  Cyclic scheduler with a watchdog:

with Ada.Calendar;
with Ada.Text_IO;

use Ada.Calendar;
use Ada.Text_IO;

--  add packages to use randam number generator
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;

procedure cyclic_wd is
	Message          : constant String := "Cyclic scheduler with watchdog\n";
	-- change/add your declarations here
	Gen              : Ada.Numerics.Float_Random.Generator;
	R                : Float;
	d                : constant Integer := 1;
	run_f3           : Boolean := True;
	cycle_Start_Time : Time;
	Start_Time       : Time;
	Deadline_Missed  : Boolean := False;
	task Watchdog is
		-- add your task entries for communication
		entry Watch_Start (Time_out : Duration);
		entry Watch_End;

	end Watchdog;

	task body Watchdog is
		Wd_Start_Time : Time;
		Wd_End_Time   : Time;
		Wd_Time_out   : Duration;
	begin
		loop
			-- add your task code inside this loop

			accept Watch_Start (Time_out : Duration) do
				Wd_Start_Time := Clock;
				Wd_Time_out := Time_out;
				Put_Line ("Wd started: ");
			end Watch_Start;
			select
				accept Watch_End do
					Wd_End_Time := Clock;
					Put ("Wd Task Took: ");
					Put_Line (Duration'Image (Wd_End_Time - Wd_Start_Time));
				end Watch_End;
			or
				delay Wd_Time_out;
				Put_Line ("Missed Deadline: ");
				Deadline_Missed := True;
				-- another accept Watch_end to avoid task being blocked in the case of deadline miss
				accept Watch_End do
					Wd_End_Time := Clock;
					Put ("Wd Task Took: ");
					Put_Line (Duration'Image (Wd_End_Time - Wd_Start_Time));
				end Watch_End;
			end select;
		end loop;
	end Watchdog;

	procedure f1 is
		Message : constant String := "f1 executing, time is now";
	begin
		Put (Message);
		Put_Line (Duration'Image (Clock - Start_Time));
	end f1;

	procedure f2 is
		Message : constant String := "f2 executing, time is now";
	begin
		Put (Message);
		Put_Line (Duration'Image (Clock - Start_Time));
	end f2;

	procedure f3 is
		Message : constant String := "f3 executing, time is now";
	begin
		Put (Message);
		Put_Line (Duration'Image (Clock - Start_Time));
		-- add a random delay here
		Ada.Numerics.Float_Random.Reset (Gen);
		R :=
		   Ada.Numerics.Float_Random.Random
		      (Gen); -- random number between 0.0 .. 1.0
		delay Duration (0.1 + 0.7 * R);
	end f3;

begin
	-- Wd Cyclic scheduler start msg
	Put (Message);
	--  pin start of the loop to a whole second
	declare
		Now : constant Time := Clock;
	begin

		Start_Time :=
		   Time_Of
		      (Year (Now),
		       Month (Now),
		       Day (Now),
		       Duration (Integer (Seconds (Now)) + 1));
		delay until Start_Time;
	end;
	loop
		-- change/add your code inside this loop
		cycle_Start_Time := Clock;
		f1;
		f2;
		--  check whether f3 needs to run
		if run_f3 then
			delay until cycle_Start_Time + 0.5;
			-- start watch dog
			Watchdog.Watch_Start (0.5);
			f3;
			-- end watch dog
			Watchdog.Watch_End;
		end if;
		-- flip the f3 run condition
		run_f3 := not run_f3;
		-- If deadline missed resynchronize
		if Deadline_Missed then
			-- wait until the next full second
			declare
				Now : constant Time := Clock;
			begin
				delay until
				   Time_Of
				      (Year (Now),
				       Month (Now),
				       Day (Now),
				       Duration (Integer (Seconds (Now)) + 1));
			end;
			-- reset deadline missed flag
			Deadline_Missed := False;
		else
			-- normal periodic wait
			delay until
			   Time_Of
			      (Year (cycle_Start_Time),
			       Month (cycle_Start_Time),
			       Day (cycle_Start_Time),
			       Duration (Integer (Seconds (cycle_Start_Time)) + d));
		end if;

	end loop;
end cyclic_wd;
