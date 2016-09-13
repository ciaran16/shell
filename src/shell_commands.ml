module String_map = struct
  include Map.Make (String)

  let add_all_keys ks v t =
    ks |> List.fold_left (fun t k -> t |> add k v) t

  let safe_find s t = try Some (t |> find s) with Not_found -> None
end

let rec get n = function
  | [] -> None
  | hd::tl -> if n <= 0 then Some hd else tl |> get (n - 1)

module Arg = struct
  type info = {
    doc : string;
    predict : (string -> string list) option;
  }

  module Opt = struct
    type t = {
      names : string list;
      is_flag : bool;
      info : info;
    }

    let create names ~is_flag info = {
      names;
      is_flag;
      info
    }

    let names {names; _} = names

    let is_flag {is_flag; _} = is_flag

    let info {info; _} = info
  end

  module Pos = struct
    type t = {
      info : info
    }

    let create info = {info}

    let info {info; _} = info
  end

  type arg = [`Opt of Opt.t | `Pos of Pos.t]

  let info = function
  | `Opt opt -> Opt.info opt
  | `Pos pos -> Pos.info pos

  let doc arg = (info arg).doc

  let predict prefix arg =
    match (info arg).predict with
    | None -> []
    | Some f -> f prefix

  type 'a t = [
    | `Opt_value of Opt.t * (string option -> 'a)
    | `Pos_value of Pos.t * (string option -> 'a)
    | `Const of 'a
  ]

  let flag ?(doc="") names : bool t =
    let opt = Opt.create names ~is_flag:true {doc; predict = None} in
    `Opt_value (opt, fun o -> o <> None)

  let opt ?(doc="") ?predict ?(default="") names : string t =
    let opt = Opt.create names ~is_flag:false {doc; predict} in
    `Opt_value (opt, function None -> default | Some s -> s)

  let pos ?(doc="") ?predict ?(default="") () : string t =
    let pos = Pos.create {doc; predict} in
    `Pos_value (pos, function None -> default | Some s -> s)

  let pos_required ?(doc="") ?predict () : string t =
    let pos = Pos.create {doc; predict} in
    `Pos_value (pos, function None -> "" | Some s -> s)
end

module Values = struct
  type t = {
    opt_values : string String_map.t;
    pos_values : string list;
  }

  let empty = {
    opt_values = String_map.empty;
    pos_values = [];
  }

  let opt_name opt =
    match Arg.Opt.names opt with
    | [] -> ""
    | hd::_ -> hd

  let opt opt {opt_values; _} =
    opt_values |> String_map.safe_find (opt_name opt)

  let pos_index i {pos_values; _} =
    pos_values |> get i

  let prepend_opt opt v ({opt_values; _} as t) =
    if opt_values |> String_map.mem (opt_name opt) then t
    else
      let opt_values = opt_values |> String_map.add (opt_name opt) v in
      {t with opt_values}

  let prepend_pos_value v ({pos_values; _} as t) =
    let pos_values = v :: pos_values in
    {t with pos_values}
end

module Command = struct
  type 'a t = {
    name : string;
    opt_map : Arg.Opt.t String_map.t;
    pos_list : Arg.Pos.t list;
    doc : string;
    execute : Values.t -> 'a;
    should_exit : bool;
  }

  type full = unit Lwt.t t

  let create ?(doc="") name f = {
    name;
    opt_map = String_map.empty;
    pos_list = [];
    doc;
    execute = (fun _values -> f);
    should_exit = false;
  }

  let name {name; _} = name

  let rename name t = {t with name}

  let ($) (({opt_map; pos_list; execute; _} as t)) = function
    | `Opt_value (opt, value_f) ->
      let opt_map =
        opt_map |> String_map.add_all_keys (Arg.Opt.names opt) opt
      in
      let execute values =
        values |> Values.opt opt |> value_f |> execute values
      in
      {t with opt_map; execute}
    | `Pos_value (pos, value_f) ->
      let execute values =
        let i = List.length pos_list in
        values |> Values.pos_index i |> value_f |> execute values
      in
      let pos_list = pos_list @ [pos] in
      {t with pos_list; execute}
    | `Const v ->
      let execute values = execute values v in
      {t with execute}

  let const v = `Const v

  let map f ({execute; _} as t) =
    let execute values = execute values |> f in
    {t with execute}

  let exit = {(create "exit" Lwt.return_unit) with should_exit = true}

  let eval value_pairs {execute; _} =
    let values = List.fold_right (fun (arg, v) values ->
        match arg with
        | `Opt opt -> values |> Values.prepend_opt opt v
        | `Pos _ -> values |> Values.prepend_pos_value v
      ) value_pairs Values.empty
    in
    execute values

  let doc {doc; _} = doc

  let lookup_pos i {pos_list; _} = pos_list |> get i

  let lookup_opt name {opt_map; _} = opt_map |> String_map.safe_find name

  let opt_bindings {opt_map; _} = opt_map |> String_map.bindings

  let should_exit {should_exit; _} = should_exit
end

module Map = struct
  type t = unit Lwt.t Command.t String_map.t

  let add command t = t |> String_map.add (Command.name command) command

  let empty = String_map.empty |> add Command.exit

  let get name t = t |> String_map.safe_find name

  let bindings t = t |> String_map.bindings
end
