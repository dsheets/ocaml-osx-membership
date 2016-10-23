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

module Gen = Osx_membership_lwt_generated
module G = Osx_membership_bindings.C(Gen)
module C = struct
  type 'a m = 'a Lwt.t

  let uuid_parse s u = G.(uuid_parse s u).Gen.lwt
  let uuid_unparse u s = G.(uuid_unparse u s).Gen.lwt
  let uuid_to_id u i t = G.(uuid_to_id u i t).Gen.lwt
  let uid_to_uuid u x = G.(uid_to_uuid u x).Gen.lwt
  let gid_to_uuid g x = G.(gid_to_uuid g x).Gen.lwt
end

module Lwt_monad = struct
  include Lwt

  type 'a m = 'a Lwt.t

  let raise_errno ?(call="") ?(label="") errno =
    let errno = Errno.of_code ~host:Errno_unix.host errno in
    fail Errno.(Error { call; label; errno })
end

include Osx_membership.Make(Lwt_monad)(C)
