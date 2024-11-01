import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type ProcessDown, type Subject}

pub const timeout = 1000

/// The node the oracle is located at, this is hard coded in this example, but
/// instead of "localhost" it could take in an IP and seamlessly work in a real
/// distributed setting.
///
pub fn oracle_node() -> Atom {
  atom.create_from_string("oracle@localhost")
}

/// The atom with the oracle name.
///
pub fn oracle_atom() -> Atom {
  atom.create_from_string("oracle")
}

// -- MESSAGE TYPES ------------------------------------------------------------
// I think this is the most interesting part.
// If we structure the messages as <subject-verb-object>, just by looking at the
// type definition we can get a holistic view of all the ways an actor can
// interact with the outside world.
//
// This is really powerful, since an actor might be interacting with many
// different sources. And _most importantly_ it's compiler enforced!
// - These are all the messages we handle, there's nothing else.
// - We cannot forget to handle one of these messages.
//
// These are all the arcs of an actor's state machine.
//

/// Messages the oracle can receive.
///
pub type OracleMsg {
  SenderAskedSubject(from: Subject(SenderMsg))
  SenderMadeGuess(from: Subject(SenderMsg), number: Int)
  SenderDisconnected(reason: ProcessDown)
  UnknownMessage
}

/// Messages a sender can receive.
///
pub type SenderMsg {
  OracleRepliedToGuess(outcome: Outcome)
  OracleSentSubject(oracle: Subject(OracleMsg))
  OracleDied(reason: ProcessDown)
  OracleKickedYouOutForInactivity
}

/// The outcome of guessing a number.
///
pub type Outcome {
  YouWon
  WrongGuess
  /// This happens if you guessed the correct number but someone was faster than
  /// you and won in your place.
  ///
  SomeoneElseWon
}
