# A1

## Initializing with alr

- cd /path/to/realtime_a1
- alr init --in-place --bin realtime_a1

## Part 1: Cyclic Scheduler

Write a cyclic scheduler that manages three procedures F1, F2, and F3. The execution should satisfy the following constraints:

- F1 shall be executed every second.

- F2 starts when F1 terminates.

- F3 shall execute every other second, starting 0.5 seconds after F1's start.

- The execution times of the functions are not known. However, you can assume:

- F1 and F2 together execute for less than 0.5 seconds.

- F3 does not take more than 0.5 seconds to execute.

Let the functions print an informative message when they execute, e.g.:

F1 executing, time is now: 0.00001
F2 executing, time is now: 0.00004
F3 executing, time is now: 0.50008
F1 executing, time is now: 1.00008
F2 executing, time is now: 1.00010
...

Make sure that the printed time has a resolution of at least 1/1000 sec.

You are given a template that you can already compile and run. From the output, you should see that the start times of F1, F2, and F3 have drifts accumulated over time, which is not allowed in your final solution.

### Your task:

Modify the template in the places indicated so that:

- There is no accumulated drift in the start times of the functions.

- Some jitter is acceptable, but the schedule must not drift further and further away over time.

### Hints (Part 1)

- Use the package Ada.Calendar for access to the clock.

- Use the package Ada.Text_IO for printing messages.

- There is no need for tasking in this part of the lab.

### Questions (Part 1)

1. Explain the difference between relative delay and absolute delay based on your own understanding.

2. Suppose we know the exact execution times of F1, F2, and F3. Is it possible to use relative delay to avoid drift in that case? Why / why not?

## Part 2: Cyclic Scheduler with Watchdogs

Starting from your solution to Part 1:

Modify F3 so that it occasionally takes more than 0.5 seconds to execute.

Augment the cyclic scheduler with a watchdog task to monitor F3's execution time.

### Requirements

The watchdog monitors the execution time of F3.

When F3 exceeds its deadline (0.5 s), the watchdog should immediately print a warning message.

That is, 0.5 s after the start of F3, either:

F3 has finished, or

The watchdog has printed a warning.

The watchdog must let F3 finish, even if it misses its deadline.

The watchdog task should be started at the same time as (or just before) F3 starts executing, and from that point on, it measures the time F3 uses.

When F3 misses its deadline, the cyclic executive should re-synchronize so that:

F1 is started at whole seconds again.

You are provided with a skeleton code file for this part (different from Part 1), which already includes a task declaration. You should:

Examine the code and understand how Ada defines a task.

Modify it in the places indicated by the comments.

Reuse parts of your solution from Part 1 where appropriate.

### Hints (Part 2)

The statements select, accept, and delay in Ada might be useful.

The packages:

Ada.Numerics.Discrete_Random

Ada.Numerics.Float_Random
can be used for creating random numbers (e.g., to make F3 sometimes overrun).

### Questions (Part 2)

How did you synchronize the watchdog task with the start and the end of F3's execution?

Explain how your watchdog task monitors the execution time of F3.

Explain the way you re-synchronize the cyclic executive when F3 misses its deadline.

## Part 3: Process Communication

Create three tasks:

FIFO Buffer Task (for integers)

Acts as a first-in, first-out (FIFO) buffer for integers.

The buffer should block any task that tries to:

insert when the buffer is full (overflow),

remove when the buffer is empty (underflow).

The buffer should be able to store at least 10 integers.

Synchronization and mutual exclusion should be handled inside this buffer task.

### Producer Task

Puts integers in the range 0..20 into the buffer.

Produces values at irregular intervals (e.g., using random delays).

When interacting with the buffer, it should block appropriately if the buffer is full (no busy waiting).

It should print messages describing the integers being produced and sent to the buffer.

### Consumer Task

Pulls integers out of the buffer at irregular intervals.

Summarizes the values it consumes.

When the sum exceeds 100, the consumer should:

Initiate termination of the program.

The information that the buffer and the producer should terminate should be spread using synchronization, not:

using global variables,

having the producer count itself, etc.

You are not allowed to use abort or terminate.

It should print messages describing the integers being taken from the buffer and the current sum.

### Requirements & Constraints (Part 3)

The buffer should not print any messages.

The producer and consumer should print messages with the integers they send/receive.

The producer and consumer should not communicate directly, except for the termination synchronization.

You must not implement semaphores or use Semaphore_Package.

You must not throw away any number produced by the producer, even if the buffer is full at that moment:

The producer should block until the buffer can accept the value.

You may throw away numbers produced after the buffer task receives a termination signal from the consumer.

### Hints (Part 3)

To detect errors, make sure your test run includes situations where:

Both the producer and the consumer get blocked on the buffer at different times.

The statements accept and when might be useful in the buffer task, e.g.:

Conditional accepts depending on whether the buffer is full or empty.

### Question (Part 3)

Show by a concrete scenario that the producer–consumer–buffer program using blocking rendezvous communication can have deadlocks and explain the mistake that can cause it.

## Part 4: Data-Driven Synchronization

Re-implement the FIFO buffer from Part 3 as a protected shared object instead of a separate buffer task.

Keep the same behavior:

FIFO for integers.

Blocking semantics when the buffer is full/empty.

Use Ada protected objects for synchronization and mutual exclusion.

### Question (Part 4)

Does the producer–consumer–buffer program using protected objects suffer from any potential deadlock? Explain why or why not.