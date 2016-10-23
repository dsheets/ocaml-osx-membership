(*
 * Copyright (c) 2016 David Sheets <dsheets@docker.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

module type MONAD = sig
  type 'a m
  val return : 'a -> 'a m
  val (>>=) : 'a m -> ('a -> 'b m) -> 'b m
  val fail : exn -> 'a m
  val raise_errno : ?call:string -> ?label:string -> Signed.sint -> 'a m
  val catch : (unit -> 'a m) -> (exn -> 'a m) -> 'a m
end

type uuid

type id =
  | Uid of int
  | Gid of int

val uuid : uuid Ctypes.typ

module type S =
sig
  module M : MONAD
  type 'a m = 'a M.m

  module Uuid : sig
    type t = uuid

    val to_string : t -> string m
    val of_string : string -> t option m
    val of_ptr : unit Ctypes.ptr -> t
  end

  module Id : sig
    type t = id

    val of_uuid : Uuid.t -> t m
    val to_uuid : t -> Uuid.t m
  end
end

module Identity_monad : MONAD with type 'a m = 'a

module Make(M: MONAD)
    (C: Osx_membership_bindings.S with type 'a m = 'a M.m) :
  S with module M = M

include S with module M = Identity_monad
