open Odoc_unit

let packages ~dirs ~extra_paths ~remap ~gen_indices (pkgs : Packages.t list) :
    t list =
  let { odoc_dir; odocl_dir; index_dir; mld_dir = _ } = dirs in
  (* [module_of_hash] Maps a hash to the corresponding [Package.t], library name and
     [Packages.modulety]. [lib_dirs] maps a library name to the odoc dir containing its
     odoc files. *)
  let extra_libs_paths = extra_paths.Voodoo.libs in
  let extra_pkg_paths = extra_paths.Voodoo.pkgs in

  let module_of_hash, lib_dirs =
    let open Packages in
    let h = Util.StringMap.empty in
    let lds = extra_libs_paths in
    List.fold_left
      (fun (h, lds) pkg ->
        List.fold_left
          (fun (h, lds) lib ->
            let h' =
              List.fold_left
                (fun h mod_ ->
                  Util.StringMap.add mod_.m_intf.mif_hash (pkg, lib, mod_) h)
                h lib.modules
            in
            let lib_dir = lib_dir pkg lib in
            let lds' = Util.StringMap.add lib.lib_name lib_dir lds in
            (h', lds'))
          (h, lds) pkg.libraries)
      (h, lds) pkgs
  in
  let pkg_paths =
    List.fold_left
      (fun acc pkg -> Util.StringMap.add pkg.Packages.name (doc_dir pkg) acc)
      extra_pkg_paths pkgs
  in

  let dash_p pkgname path = (pkgname, path) in

  let dash_l lib_name =
    match Util.StringMap.find_opt lib_name lib_dirs with
    | Some dir -> [ (lib_name, dir) ]
    | None ->
        Logs.debug (fun m -> m "Library %s not found" lib_name);
        []
  in
  let base_args pkg lib_deps : Pkg_args.t =
    let own_page = dash_p pkg.Packages.name (doc_dir pkg) in
    let own_libs = List.concat_map dash_l (Util.StringSet.to_list lib_deps) in
    Pkg_args.v ~pages:[ own_page ] ~libs:own_libs ~odoc_dir ~odocl_dir
  in
  let args_of_config config : Pkg_args.t =
    let { Global_config.deps = { packages; libraries } } = config in
    let pages_rel =
      List.filter_map
        (fun pkgname ->
          match Util.StringMap.find_opt pkgname pkg_paths with
          | None ->
              Logs.debug (fun m -> m "Package '%s' not found" pkgname);
              None
          | Some path -> Some (dash_p pkgname path))
        packages
    in
    let libs_rel = List.concat_map dash_l libraries in
    Pkg_args.v ~pages:pages_rel ~libs:libs_rel ~odoc_dir ~odocl_dir
  in
  let args_of =
    let cache = Hashtbl.create 10 in
    fun pkg lib_deps : Pkg_args.t ->
      match Hashtbl.find_opt cache (pkg, lib_deps) with
      | Some res -> res
      | None ->
          let result =
            Pkg_args.combine (base_args pkg lib_deps)
              (args_of_config pkg.Packages.config)
          in
          Hashtbl.add cache (pkg, lib_deps) result;
          result
  in

  let index_of pkg =
    let roots = [ Fpath.( // ) odocl_dir (doc_dir pkg) ] in
    let output_file = Fpath.(index_dir / pkg.name / Odoc.index_filename) in
    let pkg_dir = doc_dir pkg in
    let sidebar =
      let output_file = Fpath.(index_dir / pkg.name / Odoc.sidebar_filename) in
      { output_file; json = false; pkg_dir }
    in
    {
      roots;
      output_file;
      json = false;
      search_dir = doc_dir pkg;
      sidebar = Some sidebar;
    }
  in

  let make_unit ~name ~kind ~rel_dir ~input_file ~pkg ~lib_deps ~enable_warnings
      ~to_output : _ unit =
    let to_output = to_output || not remap in
    (* If we haven't got active remapping, we output everything *)
    let ( // ) = Fpath.( // ) in
    let ( / ) = Fpath.( / ) in
    let pkg_args = args_of pkg lib_deps in
    let parent_id = rel_dir |> Odoc.Id.of_fpath in
    let odoc_file =
      odoc_dir // rel_dir / (String.uncapitalize_ascii name ^ ".odoc")
    in
    (* odoc will uncapitalise the output filename *)
    let odocl_file =
      odocl_dir // rel_dir / (String.uncapitalize_ascii name ^ ".odocl")
    in
    {
      output_dir = odoc_dir;
      pkgname = Some pkg.Packages.name;
      pkg_args;
      parent_id;
      input_file;
      odoc_file;
      odocl_file;
      kind;
      to_output;
      enable_warnings;
      index = Some (index_of pkg);
    }
  in
  let missing = ref Util.StringSet.empty in

  let rec build_deps deps =
    List.filter_map
      (fun (_name, hash) ->
        match Util.StringMap.find_opt hash module_of_hash with
        | None ->
            missing := Util.StringSet.add hash !missing;
            None
        | Some (pkg, lib, mod_) ->
            let lib_deps = Util.StringSet.add lib.lib_name lib.lib_deps in
            let result = of_intf mod_.m_hidden pkg lib lib_deps mod_.m_intf in
            Some result)
      deps
  and of_intf =
    (* Memoize (using the hash as the key) the creation of interface units, to
       avoid creating them twice *)
    let intf_cache : (string, intf unit) Hashtbl.t = Hashtbl.create 10 in
    fun hidden pkg (lib : Packages.libty) lib_deps (intf : Packages.intf) :
        intf unit ->
      let do_ () : intf unit =
        let rel_dir = lib_dir pkg lib in
        let kind =
          let deps = build_deps intf.mif_deps in
          let kind = `Intf { hidden; hash = intf.mif_hash; deps } in
          kind
        in
        let name = intf.mif_path |> Fpath.rem_ext |> Fpath.basename in
        make_unit ~name ~kind ~rel_dir ~input_file:intf.mif_path ~pkg ~lib_deps
          ~enable_warnings:pkg.selected ~to_output:pkg.selected
      in
      match Hashtbl.find_opt intf_cache intf.mif_hash with
      | Some unit -> unit
      | None ->
          let unit = do_ () in
          Hashtbl.add intf_cache intf.mif_hash unit;
          unit
  in
  let of_impl pkg lib lib_deps (impl : Packages.impl) : impl unit option =
    match impl.mip_src_info with
    | None -> None
    | Some { src_path } ->
        let rel_dir = lib_dir pkg lib in
        let kind =
          let src_name = Fpath.filename src_path in
          let src_id =
            Fpath.(src_lib_dir pkg lib / src_name) |> Odoc.Id.of_fpath
          in
          `Impl { src_id; src_path }
        in
        let name =
          impl.mip_path |> Fpath.rem_ext |> Fpath.basename
          |> String.uncapitalize_ascii |> ( ^ ) "impl-"
        in
        let unit =
          make_unit ~name ~kind ~rel_dir ~input_file:impl.mip_path ~pkg
            ~lib_deps ~enable_warnings:pkg.selected ~to_output:pkg.selected
        in
        Some unit
  in

  let of_module pkg (lib : Packages.libty) lib_deps (m : Packages.modulety) :
      t list =
    let i :> t = of_intf m.m_hidden pkg lib lib_deps m.m_intf in
    let m :> t list =
      Option.bind m.m_impl (of_impl pkg lib lib_deps) |> Option.to_list
    in
    i :: m
  in
  let of_lib pkg (lib : Packages.libty) =
    let lib_deps = Util.StringSet.add lib.lib_name lib.lib_deps in
    let lib_deps =
      List.fold_left
        (fun acc lib -> Util.StringSet.add lib.Packages.lib_name acc)
        lib_deps pkg.Packages.libraries
    in
    let index = index_of pkg in
    let units = List.concat_map (of_module pkg lib lib_deps) lib.modules in
    if remap && not pkg.selected then units
    else
      let landing_page :> t = Landing_pages.library ~dirs ~pkg ~index lib in
      landing_page :: units
  in
  let of_mld pkg (mld : Packages.mld) : mld unit list =
    let open Fpath in
    let { Packages.mld_path; mld_rel_path } = mld in
    let rel_dir = doc_dir pkg // Fpath.parent mld_rel_path |> Fpath.normalize in
    let kind = `Mld in
    let name = mld_path |> Fpath.rem_ext |> Fpath.basename |> ( ^ ) "page-" in
    let lib_deps =
      pkg.libraries
      |> List.map (fun lib -> lib.Packages.lib_name)
      |> Util.StringSet.of_list
    in
    let unit =
      make_unit ~name ~kind ~rel_dir ~input_file:mld_path ~pkg ~lib_deps
        ~enable_warnings:pkg.selected ~to_output:pkg.selected
    in
    [ unit ]
  in
  let of_md pkg (md : Fpath.t) : md unit list =
    let ext = Fpath.get_ext md in
    match ext with
    | ".md" ->
        let rel_dir = pkg_dir pkg in
        let kind = `Md in
        let name = md |> Fpath.rem_ext |> Fpath.basename |> ( ^ ) "page-" in
        let lib_deps = Util.StringSet.empty in
        let unit =
          make_unit ~name ~kind ~rel_dir ~input_file:md ~pkg ~lib_deps
            ~enable_warnings:pkg.selected ~to_output:pkg.selected
        in
        [ unit ]
    | _ ->
        Logs.debug (fun m -> m "Skipping non-markdown doc file %a" Fpath.pp md);
        []
  in
  let of_asset pkg (asset : Packages.asset) : asset unit list =
    let open Fpath in
    let { Packages.asset_path; asset_rel_path } = asset in
    let rel_dir =
      doc_dir pkg // Fpath.parent asset_rel_path |> Fpath.normalize
    in
    let kind = `Asset in
    let unit =
      let name = asset_path |> Fpath.basename |> ( ^ ) "asset-" in
      make_unit ~name ~kind ~rel_dir ~input_file:asset_path ~pkg
        ~lib_deps:Util.StringSet.empty ~enable_warnings:false ~to_output:true
    in
    [ unit ]
  in

  let of_package (pkg : Packages.t) : t list =
    let lib_units :> t list list = List.map (of_lib pkg) pkg.libraries in
    let mld_units :> t list list = List.map (of_mld pkg) pkg.mlds in
    let asset_units :> t list list = List.map (of_asset pkg) pkg.assets in
    let md_units :> t list list = List.map (of_md pkg) pkg.other_docs in
    let pkg_index :> t list =
      let has_index_page =
        List.exists
          (fun mld ->
            Fpath.equal
              (Fpath.normalize mld.Packages.mld_rel_path)
              (Fpath.normalize (Fpath.v "./index.mld")))
          pkg.mlds
      in
      if has_index_page || (remap && not pkg.selected) then []
      else
        let index = index_of pkg in
        [ Landing_pages.package ~dirs ~pkg ~index ]
    in
    let src_index :> t list =
      if remap && not pkg.selected then []
      else if
        (* Some library has a module which has an implementation which has a source *)
        List.exists
          (fun lib ->
            List.exists
              (fun m ->
                match m.Packages.m_impl with
                | Some { mip_src_info = Some _; _ } -> true
                | _ -> false)
              lib.Packages.modules)
          pkg.libraries
      then
        let index = index_of pkg in
        [ Landing_pages.src ~dirs ~pkg ~index ]
      else []
    in
    List.concat
      ((pkg_index :: src_index :: lib_units)
      @ mld_units @ asset_units @ md_units)
  in
  if gen_indices then
    let gen_indices :> t = Landing_pages.package_list ~dirs ~remap pkgs in
    gen_indices :: List.concat_map of_package pkgs
  else List.concat_map of_package pkgs
