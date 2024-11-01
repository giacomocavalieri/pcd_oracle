-module(oracle_ffi).
-export([catch_all_decoder/1]).

% This is the only bit that the must be careful about. Since we're distributing
% actors across different nodes it's not possible to guarantee type safety of
% the very first message the oracle receives from an unknown node.
%
% So we need to do a bit of decoding and see if it has the shape we were
% expecting; that is, if it is a `sender_asked_subject` message.
%
catch_all_decoder(Msg) ->
    case Msg of
        {sender_asked_subject, _} -> Msg;
        _ -> unknown_message
    end.
