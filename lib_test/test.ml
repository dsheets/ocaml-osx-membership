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

module Basic = struct
  open Osx_membership

  let uid () =
    let uid = Unix.getuid () in
    let uuid = Id.to_uuid (Id.Uid uid) in
    print_endline (Uuid.to_string uuid);
    match Id.of_uuid uuid with
    | Id.Uid u when u = uid -> ()
    | Id.Uid u -> Alcotest.fail (Printf.sprintf "Got uid %d expected %d" u uid)
    | Id.Gid g ->
      Alcotest.fail (Printf.sprintf "Got gid %d expected uid %d" g uid)

  let gid () =
    let gid = Unix.getgid () in
    let uuid = Id.to_uuid (Id.Gid gid) in
    print_endline (Uuid.to_string uuid);
    match Id.of_uuid uuid with
    | Id.Gid g when g = gid -> ()
    | Id.Gid g -> Alcotest.fail (Printf.sprintf "Got gid %d expected %d" g gid)
    | Id.Uid u ->
      Alcotest.fail (Printf.sprintf "Got uid %d expected gid %d" u gid)

  let tests = [
    "uid", `Quick, uid;
    "gid", `Quick, gid;
  ]
end

let tests = [
  "Basic", Basic.tests;
]

;;
Alcotest.run "OSX membership" tests
