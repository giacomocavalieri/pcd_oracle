import gleam/erlang
import gleam/erlang/node
import gleam/erlang/process.{type Selector, type Subject}
import gleam/int
import gleam/io
import gleam/string
import gleam_community/ansi
import shared.{
  type OracleMsg, type SenderMsg, OracleDied, OracleKickedYouOutForInactivity,
  OracleRepliedToGuess, OracleSentSubject, SenderAskedSubject, SenderMadeGuess,
  SomeoneElseWon, WrongGuess, YouWon,
}
import spinner

pub fn main() {
  // We trap exits so that when the sender process dies (for any reason) we can
  // also stop the BEAM vm and exit the program.
  process.trap_exits(True)

  process.start(sender, True)

  process.new_selector()
  |> process.selecting_trapped_exits(fn(_) { Nil })
  |> process.select_forever
}

fn sender() {
  // Let it crash philosophy at work: if we cannot connect to the oracle node
  // there's no reasonable thing we can do to recover the error so we might as
  // well crash.
  let assert Ok(node) = node.connect(shared.oracle_node())

  // A subject is used to perform well typed message exchange with other
  // processes. A subject describes the type of messages one can send/receive
  // (it's not totally correct, but you could think of it as similar to a typed
  // channel).
  let sender = process.new_subject()

  // We ask the oracle for its subject so we can send him well typed messages.
  // Another example of the "let it crash" philosophy: if, for any reason we
  // cannot receive the oracle's subject, there's no way we can recover from
  // this error so we might as well kill the process.
  // (Of course we could _and should!_ handle this error properly, printing a
  // nice error message, but it's way beyond the scope of this project).
  node.send(node, shared.oracle_atom(), SenderAskedSubject(sender))
  let assert Ok(oracle) = wait_for_subject(sender)

  // A selector is used to receive messages in a well typed way: we can also use
  // it to monitor the oracle so that we can receive an appropriate message when
  // it's node goes down. This is one of the many great built in features of the
  // BEAM.
  let sender_selector =
    process.subject_owner(oracle)
    |> process.monitor_process
    |> process.selecting_process_down(process.new_selector(), _, OracleDied)
    |> process.selecting(sender, fn(message) { message })

  // After all the setup we can start the main game loop.
  sender_loop(sender, sender_selector, oracle)
}

fn wait_for_subject(
  sender: Subject(SenderMsg),
) -> Result(Subject(OracleMsg), Nil) {
  case process.receive(sender, shared.timeout) {
    Error(_) | Ok(OracleRepliedToGuess(_)) | Ok(OracleKickedYouOutForInactivity) ->
      wait_for_subject(sender)
    Ok(OracleSentSubject(subject)) -> Ok(subject)
    Ok(OracleDied(_)) -> Error(Nil)
  }
}

fn sender_loop(
  sender: Subject(SenderMsg),
  sender_selector: Selector(SenderMsg),
  oracle: Subject(OracleMsg),
) -> Nil {
  // The main game loop is super straightforward: we read a number from standard
  // input, send our guess to the oracle and wait for its response.
  case read_number("number guess > ") {
    // In case the user just presses ENTER we end the application.
    GetLineError | EmptyLine -> Nil
    NotANumber -> sender_loop(sender, sender_selector, oracle)
    Number(guess) -> {
      process.send(oracle, SenderMadeGuess(sender, guess))
      wait_for_response(sender, sender_selector, oracle)
    }
  }
}

fn wait_for_response(
  sender: Subject(SenderMsg),
  sender_selector: Selector(SenderMsg),
  oracle: Subject(OracleMsg),
) -> Nil {
  // Start a nice loading spinner to visually show we're waiting for the
  // oracle's response.
  let spinner =
    spinner.new("Waiting for the round to end")
    |> spinner.with_colour(ansi.magenta)
    |> spinner.with_frames(spinner.snake_frames)
    |> spinner.start

  // We wait for a message using the selector we've built outside of the main
  // loop. That is the one used to receive messages from the oracle.
  let result = process.select_forever(sender_selector)
  spinner.stop(spinner)

  case result {
    OracleDied(_) -> io.println("The oracle died")
    OracleKickedYouOutForInactivity ->
      io.println("You were kicked out by the oracle for taking too long")
    OracleSentSubject(_) -> wait_for_response(sender, sender_selector, oracle)
    OracleRepliedToGuess(outcome) -> {
      let msg = case outcome {
        SomeoneElseWon -> ansi.red("too slow!")
        WrongGuess -> ansi.red("wrong guess")
        YouWon -> ansi.green("you won")
      }
      io.println("Oracle sais: " <> msg)
      io.println("Starting a new round...\n")
      sender_loop(sender, sender_selector, oracle)
    }
  }
}

// --- UTILS -------------------------------------------------------------------

type ReadNumberOutcome {
  Number(Int)
  NotANumber
  EmptyLine
  GetLineError
}

fn read_number(prompt: String) -> ReadNumberOutcome {
  case erlang.get_line(prompt) {
    Error(_) -> GetLineError
    Ok("\n") -> EmptyLine
    Ok(line) ->
      case int.parse(string.trim(line)) {
        Error(_) -> NotANumber
        Ok(n) -> Number(n)
      }
  }
}
