(* server.ml
   Each server thing represents a server.
   It is basically metadata, and a bunch of channels.
   It should have functions for all the things a server can do,
   more or less translating into the IRC protocol messages.

*)


open Unix;;

let getMyIP () =
  let myname = gethostname () in
  let mydnsentry = gethostbyname myname in
  let myip = mydnsentry.h_addr_list.(0) in
    myip
;;

let getIP h =
  let mydnsentry = gethostbyname h in
  let myip = mydnsentry.h_addr_list.(0) in
    myip
;;







type server = {
    sName : string;
    mutable sNick : string;
    sUserHost : string;
    mutable sUserName : string;

    sHostname : string;
    sPort : int;
    sSocket : file_descr;

    mutable sChannels : Channel.channel list;
  };;


let newServer hostname port nick username =

  let s = {
      sName = hostname;
      sNick = nick;
      sUserHost = gethostname();
      sUserName = username;
      
      sHostname = hostname;
      sPort = port;
      sSocket = socket PF_INET SOCK_STREAM 0;

      sChannels = [(Channel.newChannel hostname)];
    } in    
    s
;;

let sendLine s line =
  let oc = Unix.out_channel_of_descr s.sSocket in
    (*print_endline ("+++" ^ line);*)
    output_string oc (line ^ "\r\n");
    flush oc;
;;

let getLine s =
  let ic = Unix.in_channel_of_descr s.sSocket in
    input_line ic;
;;



let loginServer s =
  let destip = getIP s.sHostname in
    connect s.sSocket (ADDR_INET( destip, s.sPort ));

  let userString = Printf.sprintf "USER %s %s %s %s\r\n"
    s.sNick s.sUserHost s.sName s.sUserName in

    sendLine s userString;
;;

let logoutServer s msg =
  sendLine s ("QUIT " ^ msg);
  shutdown s.sSocket SHUTDOWN_ALL;
;;

(* Gets all the messages extant, parses 'em,
   and sticks 'em into the channel queues. *)
let receiveMessages s =
  ()
;;

