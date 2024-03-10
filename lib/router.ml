open Core
open Base.Poly

let rewrite_path p =
  let components = String.split ~on:'/' p in
  let named_params =
    List.filter_map ~f:(String.chop_prefix ~prefix:"s") components
  in
  let max_anon =
    List.fold ~init:(-1) named_params ~f:(fun i s ->
        try Scanf.sscanf s "anon%d" (fun d -> if d > i then d else i)
        with Scanf.Scan_failure _ -> i)
  in
  let parse_component s =
    match String.chop_prefix ~prefix:":" s with
    | Some c -> `Named c
    | None -> if s = "*" || s = "**" then `Anon else `Match
  in
  let mk_param n =
    Spec.(make_parameter_object ~name:n ~in_:Path ~required:true ())
    |> Json_schema.Helpers.obj
  in
  List.fold
    ~init:([], [], max_anon + 1)
    components
    ~f:(fun (cs, ps, anon_count) c ->
      match parse_component c with
      | `Named c -> (("{" ^ c ^ "}") :: cs, mk_param c :: ps, anon_count)
      | `Anon ->
        let name = sprintf "anon%d" anon_count in
        (("{" ^ name ^ "}") :: cs, mk_param name :: ps, anon_count + 1)
      | `Match -> (c :: cs, ps, anon_count))
  |> function
  | cs, ps, _ -> (String.concat ~sep:"/" (List.rev cs), ps)

let merge_parameters orig add =
  let same_param (p1 : Spec.parameter_object Json_schema.or_ref)
      (p2 : Spec.parameter_object Json_schema.or_ref) =
    let open Json_schema in
    let open Spec in
    match (p1, p2) with
    | Ref r1, Ref r2 -> r1 = r2
    | Obj p1, Obj p2 -> p1.name = p2.name
    | _ -> false
  in
  List.fold_left ~init:orig add ~f:(fun orig p ->
      match List.find ~f:(same_param p) orig with
      | Some _ -> orig
      | None -> p :: orig)

module type Config_Type = sig
  type app
  type route
  type handler

  val json_path : string
  val doc_path : string
  val json_route : string -> route
  val doc_route : string -> route
  val get : string -> handler -> route
  val post : string -> handler -> route
  val delete : string -> handler -> route
  val put : string -> handler -> route
  val options : string -> handler -> route
  val head : string -> handler -> route
  val patch : string -> handler -> route
  val build_routes : route list -> app
end

module Make (Config : Config_Type) = struct
  type t = {
    spec : Spec.t;
    routes : Config.route list;
    schemas : string list;
    operation_ids : string list;
  }

  let empty =
    {
      spec =
        Spec.make ~openapi:"3.0.0"
          ~info:(Spec.make_info_object ~title:"Application" ~version:"0.1" ())
          ~paths:[] ();
      routes = [];
      schemas = [];
      operation_ids = [];
    }

  let title t r =
    { r with spec = { r.spec with info = { r.spec.info with title = t } } }

  let description d r =
    {
      r with
      spec = { r.spec with info = { r.spec.info with description = Some d } };
    }

  let terms_of_service t r =
    {
      r with
      spec =
        { r.spec with info = { r.spec.info with terms_of_service = Some t } };
    }

  let contact c r =
    {
      r with
      spec = { r.spec with info = { r.spec.info with contact = Some c } };
    }

  let license l r =
    {
      r with
      spec = { r.spec with info = { r.spec.info with license = Some l } };
    }

  let version v r =
    { r with spec = { r.spec with info = { r.spec.info with version = v } } }

  let validate_schemas schema a =
    if List.mem ~equal:( = ) a.schemas schema then
      failwith ("Attempted to register duplicate schema name: " ^ schema)
    else { a with schemas = schema :: a.schemas }

  let schema n s a =
    let a = validate_schemas n a in
    let open Spec in
    Option.value ~default:(make_components_object ()) a.spec.components
    |> (fun cs ->
         Option.value ~default:[] cs.schemas |> fun ss ->
         { cs with schemas = Some ((n, s) :: ss) })
    |> Option.return
    |> fun cs -> { a with spec = { a.spec with components = cs } }

  let response n r a =
    let open Spec in
    Option.value ~default:(make_components_object ()) a.spec.components
    |> (fun cs ->
         Option.value ~default:[] cs.responses |> fun rs ->
         { cs with responses = Some ((n, r) :: rs) })
    |> Option.return
    |> fun cs -> { a with spec = { a.spec with components = cs } }

  let parameter n p a =
    let open Spec in
    Option.value ~default:(make_components_object ()) a.spec.components
    |> (fun cs ->
         Option.value ~default:[] cs.parameters |> fun ps ->
         { cs with parameters = Some ((n, p) :: ps) })
    |> Option.return
    |> fun cs -> { a with spec = { a.spec with components = cs } }

  let example n ex a =
    let open Spec in
    Option.value ~default:(make_components_object ()) a.spec.components
    |> (fun cs ->
         Option.value ~default:[] cs.examples |> fun ss ->
         { cs with examples = Some ((n, ex) :: ss) })
    |> Option.return
    |> fun cs -> { a with spec = { a.spec with components = cs } }

  let request_body n r a =
    let open Spec in
    Option.value ~default:(make_components_object ()) a.spec.components
    |> (fun cs ->
         Option.value ~default:[] cs.request_bodies |> fun ss ->
         { cs with request_bodies = Some ((n, r) :: ss) })
    |> Option.return
    |> fun cs -> { a with spec = { a.spec with components = cs } }

  let header n h a =
    let open Spec in
    Option.value ~default:(make_components_object ()) a.spec.components
    |> (fun cs ->
         Option.value ~default:[] cs.headers |> fun ss ->
         { cs with headers = Some ((n, h) :: ss) })
    |> Option.return
    |> fun cs -> { a with spec = { a.spec with components = cs } }

  let security_scheme n s a =
    let open Spec in
    Option.value ~default:(make_components_object ()) a.spec.components
    |> (fun cs ->
         Option.value ~default:[] cs.security_schemes |> fun ss ->
         { cs with security_schemes = Some ((n, s) :: ss) })
    |> Option.return
    |> fun cs -> { a with spec = { a.spec with components = cs } }

  let link n l a =
    let open Spec in
    Option.value ~default:(make_components_object ()) a.spec.components
    |> (fun cs ->
         Option.value ~default:[] cs.links |> fun ss ->
         { cs with links = Some ((n, l) :: ss) })
    |> Option.return
    |> fun cs -> { a with spec = { a.spec with components = cs } }

  let callback n c a =
    let open Spec in
    Option.value ~default:(make_components_object ()) a.spec.components
    |> (fun cs ->
         Option.value ~default:[] cs.callbacks |> fun ss ->
         { cs with callbacks = Some ((n, c) :: ss) })
    |> Option.return
    |> fun cs -> { a with spec = { a.spec with components = cs } }

  let schema_ref x = "#/components/schemas/" ^ x |> Json_schema.Helpers.ref
  let response_ref x = "#/components/responses/" ^ x |> Json_schema.Helpers.ref

  let parameter_ref x =
    "#/components/parameters/" ^ x |> Json_schema.Helpers.ref

  let example_ref x = "#/components/examples/" ^ x |> Json_schema.Helpers.ref

  let request_body_ref x =
    "#/components/requestBodies/" ^ x |> Json_schema.Helpers.ref

  let header_ref x = "#/components/headers/" ^ x |> Json_schema.Helpers.ref

  let security_scheme_ref x =
    "#/components/securitySchemes/" ^ x |> Json_schema.Helpers.ref

  let link_ref x = "#/components/link/" ^ x |> Json_schema.Helpers.ref
  let callback_ref x = "#/components/callbacks/" ^ x |> Json_schema.Helpers.ref

  let validate_operation_id id a =
    Option.fold ~init:a.operation_ids
      ~f:(fun ids id ->
        if List.mem ~equal:( = ) a.operation_ids id then
          failwith ("Attempted to register duplicate operation id: " ^ id)
        else id :: ids)
      id

  let validate_path ~path ~method_ (operation : Spec.operation_object option) =
    if operation |> Option.is_some then
      failwith ("Attempted to overwrite operation: " ^ path ^ " " ^ method_)

  let get ?tags ?summary ?description ?external_docs ?operation_id
      ?(parameters = []) ?request_body ?(responses = []) ?callbacks ?deprecated
      ?security ?servers path handler a =
    let orig_path = path in
    let path, pathps = rewrite_path path in
    let operation_ids = validate_operation_id operation_id a in
    let p =
      List.Assoc.find ~equal:( = ) a.spec.paths path
      |> Option.value ~default:(Spec.make_path_object ())
    in
    validate_path ~path ~method_:"GET" p.get;
    let p =
      {
        p with
        get =
          Some
            (Spec.make_operation_object ?tags ?summary ?description
               ?external_docs ?operation_id
               ~parameters:(merge_parameters parameters pathps)
               ?request_body ~responses ?callbacks ?deprecated ?security
               ?servers ());
      }
    in
    let paths = List.Assoc.add ~equal:( = ) a.spec.paths path p in
    {
      spec = { a.spec with paths };
      routes = a.routes @ [Config.get orig_path handler];
      operation_ids;
      schemas = a.schemas;
    }

  let post ?tags ?summary ?description ?external_docs ?operation_id
      ?(parameters = []) ?request_body ?(responses = []) ?callbacks ?deprecated
      ?security ?servers path handler a =
    let operation_ids = validate_operation_id operation_id a in
    let orig_path = path in
    let path, pathps = rewrite_path path in
    let p =
      List.Assoc.find ~equal:( = ) a.spec.paths path
      |> Option.value ~default:(Spec.make_path_object ())
    in
    validate_path ~path ~method_:"POST" p.post;
    let p =
      {
        p with
        post =
          Some
            (Spec.make_operation_object ?tags ?summary ?description
               ?external_docs ?operation_id
               ~parameters:(merge_parameters parameters pathps)
               ?request_body ~responses ?callbacks ?deprecated ?security
               ?servers ());
      }
    in
    let paths = List.Assoc.add ~equal:( = ) a.spec.paths path p in
    {
      spec = { a.spec with paths };
      routes = a.routes @ [Config.post orig_path handler];
      operation_ids;
      schemas = a.schemas;
    }

  let delete ?tags ?summary ?description ?external_docs ?operation_id
      ?(parameters = []) ?request_body ?(responses = []) ?callbacks ?deprecated
      ?security ?servers path handler a =
    let orig_path = path in
    let path, pathps = rewrite_path path in
    let p =
      List.Assoc.find ~equal:( = ) a.spec.paths path
      |> Option.value ~default:(Spec.make_path_object ())
    in
    let operation_ids = validate_operation_id operation_id a in
    validate_path ~path ~method_:"DELET" p.delete;
    let p =
      {
        p with
        delete =
          Some
            (Spec.make_operation_object ?tags ?summary ?description
               ?external_docs ?operation_id
               ~parameters:(merge_parameters parameters pathps)
               ?request_body ~responses ?callbacks ?deprecated ?security
               ?servers ());
      }
    in
    let paths = List.Assoc.add ~equal:( = ) a.spec.paths path p in
    {
      spec = { a.spec with paths };
      routes = a.routes @ [Config.delete orig_path handler];
      operation_ids;
      schemas = a.schemas;
    }

  let put ?tags ?summary ?description ?external_docs ?operation_id
      ?(parameters = []) ?request_body ?(responses = []) ?callbacks ?deprecated
      ?security ?servers path handler a =
    let orig_path = path in
    let path, pathps = rewrite_path path in
    let operation_ids = validate_operation_id operation_id a in
    let p =
      List.Assoc.find ~equal:( = ) a.spec.paths path
      |> Option.value ~default:(Spec.make_path_object ())
    in
    validate_path ~path ~method_:"PUT" p.put;
    let p =
      {
        p with
        put =
          Some
            (Spec.make_operation_object ?tags ?summary ?description
               ?external_docs ?operation_id
               ~parameters:(merge_parameters parameters pathps)
               ?request_body ~responses ?callbacks ?deprecated ?security
               ?servers ());
      }
    in
    let paths = List.Assoc.add ~equal:( = ) a.spec.paths path p in
    {
      spec = { a.spec with paths };
      routes = a.routes @ [Config.put orig_path handler];
      operation_ids;
      schemas = a.schemas;
    }

  let options ?tags ?summary ?description ?external_docs ?operation_id
      ?(parameters = []) ?request_body ?(responses = []) ?callbacks ?deprecated
      ?security ?servers path handler a =
    let orig_path = path in
    let path, pathps = rewrite_path path in
    let operation_ids = validate_operation_id operation_id a in
    let p =
      List.Assoc.find ~equal:( = ) a.spec.paths path
      |> Option.value ~default:(Spec.make_path_object ())
    in
    validate_path ~path ~method_:"OPTIONS" p.options;
    let p =
      {
        p with
        options =
          Some
            (Spec.make_operation_object ?tags ?summary ?description
               ?external_docs ?operation_id
               ~parameters:(merge_parameters parameters pathps)
               ?request_body ~responses ?callbacks ?deprecated ?security
               ?servers ());
      }
    in
    let paths = List.Assoc.add ~equal:( = ) a.spec.paths path p in
    {
      spec = { a.spec with paths };
      routes = a.routes @ [Config.options orig_path handler];
      operation_ids;
      schemas = a.schemas;
    }

  let head ?tags ?summary ?description ?external_docs ?operation_id
      ?(parameters = []) ?request_body ?(responses = []) ?callbacks ?deprecated
      ?security ?servers path handler a =
    let operation_ids = validate_operation_id operation_id a in
    let orig_path = path in
    let path, pathps = rewrite_path path in
    let p =
      List.Assoc.find ~equal:( = ) a.spec.paths path
      |> Option.value ~default:(Spec.make_path_object ())
    in
    validate_path ~path ~method_:"HEAD" p.head;
    let p =
      {
        p with
        head =
          Some
            (Spec.make_operation_object ?tags ?summary ?description
               ?external_docs ?operation_id
               ~parameters:(merge_parameters parameters pathps)
               ?request_body ~responses ?callbacks ?deprecated ?security
               ?servers ());
      }
    in
    let paths = List.Assoc.add ~equal:( = ) a.spec.paths path p in
    {
      spec = { a.spec with paths };
      routes = a.routes @ [Config.head orig_path handler];
      operation_ids;
      schemas = a.schemas;
    }

  let patch ?tags ?summary ?description ?external_docs ?operation_id
      ?(parameters = []) ?request_body ?(responses = []) ?callbacks ?deprecated
      ?security ?servers path handler a =
    let operation_ids = validate_operation_id operation_id a in
    let orig_path = path in
    let path, pathps = rewrite_path path in
    let p =
      List.Assoc.find ~equal:( = ) a.spec.paths path
      |> Option.value ~default:(Spec.make_path_object ())
    in
    validate_path ~path ~method_:"PATCH" p.patch;
    let p =
      {
        p with
        patch =
          Some
            (Spec.make_operation_object ?tags ?summary ?description
               ?external_docs ?operation_id
               ~parameters:(merge_parameters parameters pathps)
               ?request_body ~responses ?callbacks ?deprecated ?security
               ?servers ());
      }
    in
    let paths = List.Assoc.add ~equal:( = ) a.spec.paths path p in
    {
      spec = { a.spec with paths };
      routes = a.routes @ [Config.patch orig_path handler];
      operation_ids;
      schemas = a.schemas;
    }

  let build ?(version = "3") router =
    router.routes
    @ [
        Config.json_route (Spec.yojson_of_t router.spec |> Yojson.Safe.to_string);
        Config.doc_route
          (Printf.sprintf
             {|<html>
  <head>
    <title>Swagger %s UI</title>
    <link
      rel="stylesheet"
      href="https://cdn.jsdelivr.net/npm/swagger-ui-dist@%s/swagger-ui.css"
    />
    <link
      rel="shortcut icon"
      href="https://fastapi.tiangolo.com/img/favicon.png"
    />
  </head>
  <body>
    <div id="swagger-ui"></div>
    <script src="https://cdn.jsdelivr.net/npm/swagger-ui-dist@%s/swagger-ui-bundle.js"></script>
    <script>
      const ui = SwaggerUIBundle({
        url: "%s",
        oauth2RedirectUrl: window.location.origin + "%s/oauth2-redirect",
        dom_id: "#swagger-ui",
        presets: [
          SwaggerUIBundle.presets.apis,
          SwaggerUIBundle.SwaggerUIStandalonePreset,
        ],
        layout: "BaseLayout",
        deepLinking: true,
        showExtensions: true,
        showCommonExtensions: true,
      });
    </script>
  </body>
</html>
|}
             router.spec.info.title version version Config.json_path
             Config.doc_path);
      ]
    |> Config.build_routes
end
