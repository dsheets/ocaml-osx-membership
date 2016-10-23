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

let (|||) = (lor)

let (??>) flag int = if flag then int else 0

let (??<) field int = field land int <> 0

module Type = Osx_membership_types.C(Osx_membership_types_detected)

module type S = sig
  type 'a m

  val uuid_parse : string -> unit ptr -> (int * Signed.sint) m

  val uuid_unparse : unit ptr -> char ptr -> (unit * Signed.sint) m

  val uuid_to_id :
    unit ptr -> Unsigned.uint32 ptr -> int ptr -> (int * Signed.sint) m

  val gid_to_uuid : Posix_types.gid_t -> unit ptr -> (int * Signed.sint) m

  val uid_to_uuid : Posix_types.uid_t -> unit ptr -> (int * Signed.sint) m
end

module C(F: Cstubs.FOREIGN) = struct

  let uuid_parse = F.(foreign "uuid_parse" (
    string @-> ptr void @-> returning int
  ))

  let uuid_unparse = F.(foreign "uuid_unparse" (
    ptr void @-> ptr char @-> returning void
  ))

  let uuid_to_id = F.(foreign "mbr_uuid_to_id" (
    ptr void @-> ptr Type.id @-> ptr int @-> returning int
  ))

  let gid_to_uuid = F.(foreign "mbr_gid_to_uuid" (
    Posix_types.gid_t @-> ptr void @-> returning int
  ))

  let uid_to_uuid = F.(foreign "mbr_uid_to_uuid" (
    Posix_types.uid_t @-> ptr void @-> returning int
  ))
end
