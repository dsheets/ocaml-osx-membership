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

open Ctypes

module Types = Osx_membership_types.C(Osx_membership_types_detected)

module type MONAD =
sig
  type 'a m
  val return : 'a -> 'a m
  val (>>=) : 'a m -> ('a -> 'b m) -> 'b m
  val fail : exn -> 'a m
  val raise_errno : ?call:string -> ?label:string -> Signed.sint -> 'a m
  val catch : (unit -> 'a m) -> (exn -> 'a m) -> 'a m
end

module type BINDINGS = Osx_membership_bindings.S

type uuid = unit ptr
type id =
  | Uid of int
  | Gid of int

let uuid = ptr void

module type S = sig
  module M : MONAD
  type 'a m = 'a M.m

  module Uuid : sig
    type t = uuid

    val to_string : t -> string m
    val of_string : string -> t option m
    val of_ptr : unit ptr -> t
  end

  module Id : sig
    type t = id

    val of_uuid : Uuid.t -> t m
    val to_uuid : t -> Uuid.t m
  end
end

module Make(M: MONAD)(C: Osx_membership_bindings.S with type 'a m = 'a M.m) =
struct
  open M

  module M = M
  type 'a m = 'a M.m

  (* TODO This might belong in its own lib but it's so small and we
     don't have any other bindings that use UUIDs right now... *)
  module Uuid = struct
    type t = uuid

    let alloc () = to_voidp (allocate_n ~count:16 char)

    let to_string uuid =
      let p = allocate_n ~count:37 char in
      C.uuid_unparse uuid p
      >>= fun ((), _) -> (* errno not set *)
      return (string_from_ptr p ~length:36)

    let of_string s =
      let uuid = alloc () in
      C.uuid_parse s uuid
      >>= fun (r, _) -> (* errno not set *)
      if r < 0
      then return None
      else return (Some uuid)

    let of_ptr p = p
  end

  module Id = struct
    type t = id

    let int_of_uint32 = Unsigned.UInt32.to_int

    let of_uuid uuid =
      let id_p = allocate_n ~count:1 uint32_t in
      let type_p = allocate_n ~count:1 int in
      C.uuid_to_id uuid id_p type_p
      >>= fun (r, errno) ->
      if r < 0
      then
        Uuid.to_string uuid
        >>= fun label ->
        raise_errno ~call:"mbr_uuid_to_id" ~label errno
      else match !@ type_p with
        | x when x = Types.id_type_uid ->
          return (Uid (int_of_uint32 (!@ id_p)))
        | x when x = Types.id_type_gid ->
          return (Gid (int_of_uint32 (!@ id_p)))
        | x ->
          let msg =
            Printf.sprintf "Osx_membership.Id.of_uuid unknown type %d" x
          in
          fail (Failure msg)

    let to_uuid = function
      | Uid uid ->
        let uuid = Uuid.alloc () in
        C.uid_to_uuid (Posix_types.Uid.of_int uid) uuid
        >>= fun (r, errno) ->
        if r < 0
        then
          let label = string_of_int uid in
          raise_errno ~call:"mbr_uid_to_uuid" ~label errno
        else return uuid
      | Gid gid ->
        let uuid = Uuid.alloc () in
        C.gid_to_uuid (Posix_types.Gid.of_int gid) uuid
        >>= fun (r, errno) ->
        if r < 0
        then
          let label = string_of_int gid in
          raise_errno ~call:"mbr_gid_to_uuid" ~label errno
        else return uuid
  end
end

module Identity_monad = struct
  type 'a m = 'a
  let return v = v
  let (>>=) = (|>)
  let fail exn = raise exn
  let raise_errno = Errno_unix.raise_errno
  let catch try_block catch_block =
    try try_block ()
    with exn -> catch_block exn
end

module C = struct
  type 'a m = 'a
  include Osx_membership_bindings.C(Osx_membership_generated)
end
include Make(Identity_monad)(C)
