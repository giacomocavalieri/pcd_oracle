import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{
  type Pid, type ProcessMonitor, type Selector, type Subject,
}
import gleam/int
import gleam/list
import gleam/set.{type Set}
import logging.{Info}
import shared.{
  type OracleMsg, type SenderMsg, OracleKickedYouOutForInactivity,
  OracleRepliedToGuess, OracleSentSubject, SenderAskedSubject,
  SenderDisconnected, SenderMadeGuess, SomeoneElseWon, UnknownMessage,
  WrongGuess, YouWon,
}

const max_guess = 100

pub fn main() {
  // We use the default configuration of the erlang logger.
  logging.configure()

  // We start the oracle and register its name under the `oracle` atom so that
  // it can be discovered by other processes (even from other BEAM VMs) just by
  // using the same atom.
  let oracle = process.start(oracle, True)
  let assert Ok(_) = process.register(oracle, shared.oracle_atom())

  // After starting the oracle we can put this process to sleep forever and let
  // the oracle do its thing.
  process.sleep_forever()
}

fn oracle() -> Nil {
  // This is the subject the oracle will use to receive messages from senders.
  let oracle = process.new_subject()

  // We create a selector to select all incoming messages, and also perform some
  // decoding to receive messages that might not be well typed.
  // This is fundamental to receive the first `SenderAskedSubject` message since
  // it would otherwise be impossible to type messages across the network!
  // For more on how that is done see the `catch_all_decoder`.
  let selector =
    process.new_selector()
    |> process.selecting(oracle, fn(message) { message })
    |> process.selecting_anything(catch_all_decoder)

  let secret = int.random(max_guess + 1)
  let game_state = new_game(secret)
  logging.log(Info, "Oracle started (secret = " <> int.to_string(secret) <> ")")

  // After preparing the selector we can start the main game loop.
  oracle_loop(selector, oracle, game_state)
}

// --- GAME STATE --------------------------------------------------------------

type GameState {
  GameState(
    number_to_guess: Int,
    /// All the active players in the current round.
    ///
    players: Dict(Pid, #(Subject(SenderMsg), ProcessMonitor)),
    /// All the guesses made in the current round.
    ///
    guesses: Dict(Pid, Int),
    /// A list with the senders in the order in which they made their guess, we
    /// need this to make sure there's only one winner and we can say to all the
    /// others they were too slow.
    ///
    /// The list is in reverse order (first player is last) so that we can
    /// efficiently build it in the main game loop.
    ///
    guesses_order: List(Subject(SenderMsg)),
    /// A set with all the players we still haven't made a guess.
    ///
    needs_to_make_guess: Set(Pid),
  )
}

/// The possible reasons a player might not be allowed to make a guess: it might
/// be an unknown player (meaning it never sent a message asking for the
/// oracle's subject) or it might have already made a guess.
///
type RegisterGuessError {
  AlreadyMadeAGuess
  UnknownPlayer
}

// --- ORACLE LOOP -------------------------------------------------------------
// This is the interesting part: the main game loop of the oracle describing how
// it interact with the outside world.
//

fn oracle_loop(
  selector: Selector(OracleMsg),
  oracle: Subject(OracleMsg),
  game_state: GameState,
) -> Nil {
  case process.select(selector, 5000) {
    // If we don't receive any new message in 5 seconds we end the round (but
    // only if there was at least one player) and kick out all inactive players.
    Error(_) ->
      case dict.is_empty(game_state.players) {
        True -> oracle_loop(selector, oracle, game_state)
        False -> {
          let selector = kick_out_inactive_players(game_state, selector)
          send_outcomes(game_state)
          let game_state = start_new_round(game_state)
          oracle_loop(selector, oracle, game_state)
        }
      }

    // Unknown messages are just ignored.
    Ok(UnknownMessage) -> oracle_loop(selector, oracle, game_state)

    // A sender is joining and asking for our subject to send us messages.
    Ok(SenderAskedSubject(from: sender)) -> {
      logging.log(Info, "New player joined")

      let sender_pid = process.subject_owner(sender)

      // Another cool thing: we get monitoring of the players for free as well.
      // We start monitoring the process so we can be notified if it dies or
      // disconnects.
      // To receive the monitor messages we need to update our selector with the
      // created monitor.
      let monitor = process.monitor_process(sender_pid)
      let selector =
        process.selecting_process_down(selector, monitor, SenderDisconnected)
      let game_state = add_player(game_state, sender, monitor)

      process.send(sender, OracleSentSubject(oracle))
      oracle_loop(selector, oracle, game_state)
    }

    // A sender made a guess, we update the game state and keep looping.
    Ok(SenderMadeGuess(from: sender, number:)) ->
      case register_guess(game_state, sender, number) {
        Ok(game_state) -> oracle_loop(selector, oracle, game_state)
        Error(UnknownPlayer) | Error(AlreadyMadeAGuess) ->
          oracle_loop(selector, oracle, game_state)
      }

    // If a sender disconnects at any time we remove it from the game.
    Ok(SenderDisconnected(process_down)) -> {
      logging.log(Info, "Player disconnected")
      let game_state = remove_player(game_state, process_down.pid)
      oracle_loop(selector, oracle, game_state)
    }
  }
}

fn kick_out_inactive_players(
  game_state: GameState,
  selector: Selector(OracleMsg),
) -> Selector(OracleMsg) {
  // We stop monitoring all processes we deem inactive (that is anyone who still
  // hasn't made a guess at the end of a round).
  let inactives = game_state.needs_to_make_guess
  let #(kicked_out, selector) = {
    let acc = #(0, selector)
    use #(kicked_out, selector) as acc, pid <- set.fold(inactives, from: acc)

    // We use the inactive player's PID to get it's monitor and demonitor it.
    case dict.get(game_state.players, pid) {
      Error(_) -> acc
      Ok(#(player, monitor)) -> {
        let selector = process.deselecting_process_down(selector, monitor)
        process.send(player, OracleKickedYouOutForInactivity)
        #(kicked_out + 1, selector)
      }
    }
  }

  case kicked_out {
    0 -> Nil
    1 -> logging.log(Info, "Kicked out one inactive player")
    n ->
      logging.log(
        Info,
        "Kicked out " <> int.to_string(n) <> " inactive players",
      )
  }

  selector
}

type Winner {
  AlreadyFoundWinner
  NoWinnerYet
}

fn send_outcomes(game_state: GameState) -> Nil {
  do_send_outcomes(
    // The guess_order is built in reverse order to be efficient, so when we
    // send the outcomes we have to go through it in reverse order!
    list.reverse(game_state.guesses_order),
    game_state,
    NoWinnerYet,
  )
}

fn do_send_outcomes(
  order: List(Subject(SenderMsg)),
  game_state: GameState,
  winner: Winner,
) {
  case order {
    [] -> Nil
    [player, ..rest] ->
      case dict.get(game_state.guesses, process.subject_owner(player)) {
        Error(_) -> do_send_outcomes(rest, game_state, winner)
        Ok(guess) -> {
          let outcome = outcome(winner, guess, game_state)
          process.send(player, OracleRepliedToGuess(outcome))
          let winner = case outcome {
            YouWon | SomeoneElseWon -> AlreadyFoundWinner
            WrongGuess -> winner
          }
          do_send_outcomes(rest, game_state, winner)
        }
      }
  }
}

fn outcome(winner, guess, game_state: GameState) {
  case winner, guess == game_state.number_to_guess {
    _, False -> WrongGuess
    NoWinnerYet, True -> YouWon
    AlreadyFoundWinner, True -> SomeoneElseWon
  }
}

fn start_new_round(game_state: GameState) -> GameState {
  let number_to_guess = int.random(max_guess + 1)
  let game_state = advance_round(game_state, number_to_guess)
  logging.log(
    Info,
    "New round (secret = " <> int.to_string(number_to_guess) <> ")",
  )
  game_state
}

@external(erlang, "oracle_ffi", "catch_all_decoder")
fn catch_all_decoder(dynamic: Dynamic) -> OracleMsg

// --- UTILS TO DEAL WITH THE GAME STATE ---------------------------------------
// This is not particularly interesting, it's just utility functions to deal
// with the game state: register guesses, advance to a new round, add new
// players, ...
//

fn new_game(number_to_guess: Int) -> GameState {
  GameState(
    number_to_guess:,
    players: dict.new(),
    guesses: dict.new(),
    guesses_order: [],
    needs_to_make_guess: set.new(),
  )
}

fn advance_round(game_state: GameState, number_to_guess: Int) -> GameState {
  let GameState(
    players:,
    needs_to_make_guess:,
    number_to_guess: _,
    guesses: _,
    guesses_order: _,
  ) = game_state

  let players = set.fold(needs_to_make_guess, players, dict.delete)
  GameState(
    number_to_guess:,
    players:,
    guesses: dict.new(),
    guesses_order: [],
    needs_to_make_guess: set.from_list(dict.keys(players)),
  )
}

fn add_player(
  game_state: GameState,
  player: Subject(SenderMsg),
  monitor: ProcessMonitor,
) -> GameState {
  let player_pid = process.subject_owner(player)
  let GameState(
    players:,
    guesses:,
    needs_to_make_guess:,
    number_to_guess: _,
    guesses_order: _,
  ) = game_state

  GameState(
    ..game_state,
    players: dict.insert(players, player_pid, #(player, monitor)),
    guesses: dict.delete(guesses, player_pid),
    needs_to_make_guess: set.insert(needs_to_make_guess, player_pid),
  )
}

fn remove_player(game_state: GameState, player_pid: Pid) -> GameState {
  let GameState(
    players:,
    guesses:,
    needs_to_make_guess:,
    number_to_guess: _,
    guesses_order: _,
  ) = game_state

  GameState(
    ..game_state,
    players: dict.delete(players, player_pid),
    guesses: dict.delete(guesses, player_pid),
    needs_to_make_guess: set.delete(needs_to_make_guess, player_pid),
  )
}

fn register_guess(
  game_state: GameState,
  player: Subject(SenderMsg),
  guess: Int,
) -> Result(GameState, RegisterGuessError) {
  let player_pid = process.subject_owner(player)
  case dict.has_key(game_state.players, player_pid) {
    False -> Error(UnknownPlayer)
    True ->
      case dict.has_key(game_state.guesses, player_pid) {
        True -> Error(AlreadyMadeAGuess)
        False -> {
          let guesses_order = [player, ..game_state.guesses_order]
          let guesses = dict.insert(game_state.guesses, player_pid, guess)
          let needs_to_make_guess =
            set.delete(game_state.needs_to_make_guess, player_pid)
          let game_state =
            GameState(
              ..game_state,
              guesses:,
              needs_to_make_guess:,
              guesses_order:,
            )

          Ok(game_state)
        }
      }
  }
}
