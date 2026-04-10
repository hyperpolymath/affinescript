// SPDX-License-Identifier: PMPL-1.0-or-later
// End-to-end test: ownership system
// Tests: own/ref/mut modifiers, borrowing

fn takes_owned(x: own String) -> () = ();

fn takes_ref(x: ref Int) -> Int = 42;

fn takes_mut(x: mut Bool) -> () = ();

struct File {
  fd: own Int
}

fn read_file(f: ref File) -> Int = 42;
