(* A channel represents a single logical thread of communication to 
   and from... anything.  Server messages, IRC channel text, private
   messages, etc all go to different channels.

*)

(* Issue: logging.  Hm. *)
type channel = {
    cName : string;
    mutable cInput : string list;
    mutable cOutput : string list;
    cMutex : Mutex.t;
  };;


let newChannel name = {
    cName = name;
    cInput = [];
    cOutput = [];
    cMutex = Mutex.create ();
  }
;;

let getNewInput queue =
  let i = queue.cInput in
    queue.cInput <- [];
    i
;;

let getNewOutput queue = 
  let i = queue.cOutput in
    queue.cOutput <- [];
    i
;;


let addInput channel message =
  channel.cInput <- message :: channel.cInput
;;

let addOutput channel message =
  channel.cOutput <- message :: channel.cOutput
;;
