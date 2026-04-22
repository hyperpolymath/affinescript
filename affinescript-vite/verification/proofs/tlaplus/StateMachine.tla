--------------------------- MODULE StateMachine ----------------------------
(* SPDX-License-Identifier: PMPL-1.0-or-later                              *)
(* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)                  *)
(*                                                                          *)
(* TLA+ Specification Template: State Machine                               *)
(* Replace with your project's distributed protocol or state machine.       *)
(* Use TLC model checker to verify properties.                              *)
(*                                                                          *)
(* Example: A simple request pipeline with safety properties.               *)
(* Replace States, Init, Next with your project's actual states.            *)
(***************************************************************************)

EXTENDS Naturals, Sequences, FiniteSets

CONSTANTS
    MaxRequests  \* Upper bound on concurrent requests (for model checking)

VARIABLES
    state,       \* Current pipeline state
    processed,   \* Number of processed requests
    queue        \* Request queue

vars == <<state, processed, queue>>

\* Pipeline states — replace with your project's states
States == {"idle", "scanning", "routing", "dispatching", "done", "failed"}

\* Valid transitions — replace with your project's transition rules
ValidTransition(from, to) ==
    \/ from = "idle"        /\ to = "scanning"
    \/ from = "scanning"    /\ to = "routing"
    \/ from = "scanning"    /\ to = "failed"
    \/ from = "routing"     /\ to = "dispatching"
    \/ from = "routing"     /\ to = "failed"
    \/ from = "dispatching" /\ to = "done"
    \/ from = "dispatching" /\ to = "failed"
    \/ from = "done"        /\ to = "idle"
    \/ from = "failed"      /\ to = "idle"

\* Initial state
Init ==
    /\ state = "idle"
    /\ processed = 0
    /\ queue = <<>>

\* Transition action
Transition(newState) ==
    /\ ValidTransition(state, newState)
    /\ state' = newState
    /\ IF newState = "done"
       THEN processed' = processed + 1
       ELSE processed' = processed
    /\ UNCHANGED queue

\* Enqueue a request (only when idle or scanning)
Enqueue ==
    /\ state \in {"idle", "scanning"}
    /\ Len(queue) < MaxRequests
    /\ queue' = Append(queue, "request")
    /\ UNCHANGED <<state, processed>>

\* Next-state relation
Next ==
    \/ \E s \in States : Transition(s)
    \/ Enqueue

\* Fairness: the system must eventually process
Spec == Init /\ [][Next]_vars /\ WF_vars(Next)

\* ---- SAFETY PROPERTIES ----

\* State is always valid
TypeInvariant == state \in States

\* Processed count never decreases (monotonicity)
ProcessedMonotonic == processed >= 0

\* Queue never exceeds max
QueueBounded == Len(queue) <= MaxRequests

\* No impossible transitions (e.g., idle -> done)
NoSkipStates ==
    [][state' # state =>
        ValidTransition(state, state')]_state

\* ---- LIVENESS PROPERTIES ----

\* Every request eventually completes or fails
EventualCompletion == <>(state = "done" \/ state = "failed")

============================================================================
