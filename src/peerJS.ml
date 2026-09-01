open Ezjs_min

type peer_cs_options = {
  key: string option;
  host: string option;
  port: int option;
  ping_internal: int option;
  path: string option;
  secure: bool option;
  debug: int option;
} [@@deriving jsoo {camel}]

type connect_options = {
  label: string option;
  metadata: Unsafe.any option;
  serialization: string option;
  reliable: bool option;
} [@@deriving jsoo {remove_undefined}]

class type data_channel = object
end

class type peer_connection = object
end

type _ data_event =
  | Data : Unsafe.any data_event
  | Open : unit data_event
  | Close : unit data_event
  | Error : Unsafe.any data_event

class type data_connection = object
  method data_channel: data_channel t readonly_prop
  method label: js_string t readonly_prop
  method metadata: Unsafe.any optdef readonly_prop
  method open_: bool t readonly_prop
  method peerConnection: peer_connection t readonly_prop
  method peer: js_string t readonly_prop
  method reliable: bool t readonly_prop
  method serialization: js_string t readonly_prop
  method type_: js_string t readonly_prop
  method bufferSize: number t readonly_prop
  method send: Unsafe.any -> unit meth
  method close: unit meth
  method on: js_string t -> (Unsafe.any -> unit) callback -> unit meth
  method once: js_string t -> (Unsafe.any -> unit) callback -> unit meth
end

class type call_options = object
  method metadata: Unsafe.any optdef readonly_prop
  method sdpTransform: (js_string t -> js_string t) callback readonly_prop
end

class type stream = object
end

class type media_connection = object
end

type _ peer_event =
  | Open : string peer_event
  | Connection : data_connection t peer_event
  | Call : media_connection t peer_event
  | Close : unit peer_event
  | Disconnected : unit peer_event
  | Error : Unsafe.any peer_event

class type peer = object
  method id: js_string t opt readonly_prop
  method connections: Unsafe.any Table.t readonly_prop
  method open_: bool t readonly_prop
  method disconnected: bool t readonly_prop
  method destroyed: bool t readonly_prop
  method connect: js_string t -> connect_options_jsoo t optdef -> data_connection t meth
  method call: js_string t -> stream t -> call_options t optdef -> media_connection t meth
  method on: js_string t -> (Unsafe.any -> unit) callback -> unit meth
  method once: js_string t -> (Unsafe.any -> unit) callback -> unit meth
  method disconnect: unit meth
  method reconnect: unit meth
  method destroy: unit meth
end

type peer_cs = (js_string t optdef -> peer_cs_options_jsoo t optdef -> peer t) constr

let peer ?id ?options () =
  let id = optdef string id in
  let options = optdef peer_cs_options_to_jsoo options in
  let cs: peer_cs = Unsafe.global##._Peer in
  new%js cs id options

let connect ?options (peer: peer t) id =
  let id = string id in
  let options = optdef connect_options_to_jsoo options in
  peer##connect id options

let call ?options (peer: peer t) id (stream: stream t) =
  let id = string id in
  let options = Optdef.option options in
  peer##call id stream options

let peer_event: type a. a peer_event -> string * (Unsafe.any -> a) = function
  | Open -> "open", (fun x -> to_string (Unsafe.coerce x))
  | Connection -> "connection", (fun x -> (Unsafe.coerce x : data_connection t))
  | Call -> "call", (fun x -> (Unsafe.coerce x : media_connection t))
  | Close -> "close", ignore
  | Disconnected -> "disconnected", ignore
  | Error -> "error", Fun.id

let peer_listen : type a. (peer t) -> a peer_event -> (a -> unit) -> unit = fun peer ev cb ->
  let ev, f = peer_event ev in
  peer##on (string ev) (wrap_callback (fun x -> cb (f x)))

let peer_once : type a. (peer t) -> a peer_event -> (a -> unit) -> unit = fun peer ev cb ->
  let ev, f = peer_event ev in
  peer##once (string ev) (wrap_callback (fun x -> cb (f x)))

let disconnect (peer: peer t) = peer##disconnect
let reconnect (peer: peer t) = peer##reconnect
let destroy (peer: peer t) = peer##destroy

let send (conn: data_connection t) data =
  conn##send (Unsafe.inject data)

let close (conn: data_connection t) = conn##close

let data_event: type a. a data_event -> string * (Unsafe.any -> a) = function
  | Data -> "data", Fun.id
  | Open -> "open", ignore
  | Close -> "close", ignore
  | Error -> "error", Fun.id

let data_listen : type a. (data_connection t) -> a data_event -> (a -> unit) -> unit = fun conn ev cb ->
  let ev, f = data_event ev in
  conn##on (string ev) (wrap_callback (fun x -> cb (f x)))

let data_once : type a. (data_connection t) -> a data_event -> (a -> unit) -> unit = fun conn ev cb ->
  let ev, f = data_event ev in
  conn##once (string ev) (wrap_callback (fun x -> cb (f x)))
