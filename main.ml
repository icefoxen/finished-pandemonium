open Server;;

let dicecap = 200;;

let list2str lst =
   List.fold_left (fun accm s -> accm ^ " " ^ s) "" lst
;;

let parseLine line =
   let re = Str.regexp "[\r\n ]" in
   Str.split re line
;;

let parseUsername namestring =
   let re = Str.regexp ":.*!" in
   if Str.string_match re namestring 0 then
      let s = Str.matched_string namestring in
      let s = Str.string_after s 1 in
      Str.string_before s ((String.length s) - 1)
   else (
      print_endline ("+++Username " ^ namestring ^ " is unparsable?");
      raise (Failure( "parseUsername" ))
   )
;;

let sendPrivmsg message dest server =
   let msg = Printf.sprintf "PRIVMSG %s %s\n" dest message in
   Printf.printf "***Sending: %s\n" msg;
   sendLine server msg
;;

let rollDice numdice who =
   let numdice = int_of_string numdice in
   if numdice > dicecap then
      "Too many dice.  Stop abusing me!"
   else
      let rec diceHelper num accm =
         if num <= 0 then
            accm
   else
      diceHelper (num - 1) (((Random.int 10) + 1) :: accm)
   in
   let rolls = diceHelper numdice [] in
   let rollstext = List.fold_left 
   (fun x y -> x ^ " " ^ (string_of_int y)) "" rolls in
   let successes = List.fold_left
   (fun total itm -> if itm = 10 then total + 2
         else if itm >= 7 then total + 1
   else total) 0 rolls in
   Printf.sprintf "Rolling %d dice for %s, %d successes:   %s"
   numdice who successes rollstext
;;

let rollDamage numdice who =
   let numdice = int_of_string numdice in
   if numdice > dicecap then
      "Too many dice.  Stop abusing me!"
   else
      let rec diceHelper num accm =
         if num <= 0 then
            accm
   else
      diceHelper (num - 1) (((Random.int 10) + 1) :: accm)
   in
   let rolls = diceHelper numdice [] in
   let rollstext = List.fold_left 
   (fun x y -> x ^ " " ^ (string_of_int y)) "" rolls in
   let successes = List.fold_left
   (fun total itm -> if itm >= 7 then total + 1 else total) 0 rolls in
   Printf.sprintf "Rolling %d dice for %s, %d successes:   %s"
   numdice who successes rollstext
;;


let handleCommand message server =
   let from = parseUsername (List.nth message 0)
   and dest = List.nth message 2
   and actualMessage = List.tl (List.tl (List.tl message)) in
   let m1 = List.hd actualMessage in
   try
      if (m1 = ":!dice") || (m1 = ":!roll") then
         let dicemsg = rollDice (List.nth actualMessage 1) from in
         sendPrivmsg dicemsg dest server;
      else if m1 = ":!dmg" then
         let dicemsg = rollDamage (List.nth actualMessage 1) from in
         sendPrivmsg dicemsg dest server;
      else
         print_endline m1;
   with
   Failure( "nth" ) -> ()
   | Failure( "int_of_string" ) -> 
         sendPrivmsg "That's not a number!  Fiend!" dest server;
   (*Printf.printf "Message from: %s to %s: %s\n" from dest 
   (List.hd actualMessage);
      List.iter print_endline message
      *)
;;

let handleMessage message server =
   try
      if (List.nth message 0) = "PING" then (
         let pongmessage = "PONG " ^ server.sHostname in
         (*Printf.printf "Ping recieved, sending %s\n" pongmessage; *)
         sendLine server pongmessage;
      ) else if (List.nth message 1) = "PRIVMSG" then (
         handleCommand message server
      ) else
         Printf.printf "***Unknown message: %s\n" (list2str message)
   with
   Failure( "nth" ) -> Printf.printf "Error: Unknown message"
   | Failure( "parseUserName" ) -> Printf.printf "Error: Username not parsed?"
;;

let main server channel = 
   Random.self_init ();
   let s = Server.newServer server 6667 "DiceBot" "Dr.JoeBotnik" in

   print_endline "***Connecting to server...";
   loginServer s;

   print_endline (getLine s);
   print_endline (getLine s);
   print_endline (getLine s);
   print_endline (getLine s);

   Unix.sleep 1;

   print_endline "***Done.  Okay, joining channel...";
   sendLine s "NICK DiceBot";
   sendLine s ("JOIN #" ^ channel);
   print_endline "***Done.  Listening for messages.";

   while true do
      let line = getLine s in
      let message = parseLine line in
      handleMessage message s;
   done
;;


let _ = 
   if (Array.length Sys.argv) > 2 then
      main Sys.argv.(1) Sys.argv.(2)
      else
         Printf.printf "Usage: %s <server> <channel>\n\n" Sys.argv.(0)
;;
