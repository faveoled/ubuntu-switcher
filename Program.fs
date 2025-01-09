open Fable.Core
open Fable.Core.PyInterop
open Fable.Python.Builtins

let write_file (path: string) (content: string): unit =
    let file = builtins.``open``(path, OpenTextMode.Write)
    let res = file.write(content)
    file?close()
    printf $"Wrote file {path}"
    ()

let read_file (path: string): string =
    let file = builtins.``open``(path, OpenTextMode.ReadText)
    let content = file.read()
    file?close()
    content
    
let file_exists (path: string): bool =
   try
      read_file path |> ignore
      true
   with
      | ex when ex.ToString().Contains("Errno 2") -> false
      | other -> raise other


[<ImportAll("subprocess")>]
[<Emit("subprocess.run(args = $1,  capture_output = True, text = True).stdout")>]
let run_process (cmd: list<string>): string = nativeOnly

let run_stdout_lst (cmd: list<string>): string =
    printfn "%s" ("running command: " + String.concat " " cmd)
    run_process cmd

let run_stdout_nooutput (cmd: string): unit =
    run_stdout_lst (cmd.Split(" ") |> Array.toList) |> ignore

let run_stdout (cmd: string): string =
    match run_stdout_lst (cmd.Split(" ") |> Array.toList) with
    | "" -> failwith $"no output for command: {cmd}"
    | non_empty -> non_empty
    
    
    
let get_manifest_link (flavour: string) (version: string) : string =
    match flavour with
    | "ubuntu" -> $"https://releases.ubuntu.com/noble/ubuntu-{version}-desktop-amd64.manifest"
    | "kubuntu" -> $"https://cdimage.ubuntu.com/kubuntu/releases/{version}/release/kubuntu-{version}-desktop-amd64.manifest"
    | "xubuntu" -> $"https://nl.archive.ubuntu.com/ubuntu-cdimage-xubuntu/releases/{version}/release/xubuntu-{version}-desktop-amd64.manifest"
    | "ubuntu-mate" -> $"https://cdimage.ubuntu.com/ubuntu-mate/releases/{version}/release/ubuntu-mate-{version}-desktop-amd64.manifest"
    | _ -> failwith $"unrecognized ubuntu flavour: {flavour}"
   

let substring_before_first (str:string) (part:string): string =
    let index = str.IndexOf part
    if index < 0 then
        failwith $"part {part} not found in string {str}"
    else
        str.Substring(0, index)

let substring_after_first (str: string) (part: string): string =
    let index = str.IndexOf part
    if index < 0 then
        failwith $"part {part} not found in string {str}"
    else
        str.Substring(index + part.Length)

let substring_after_last (str: string) (part: string): string =
    let lastIndex = str.LastIndexOf part
    match lastIndex with
    | -1 -> failwith $"part {part} not found in string {str}"
    | _  -> str.Substring(lastIndex + part.Length)

let sorted_set_str (set: Set<string>): string =
    set
    |> Seq.sort
    |> Seq.map (fun s -> s + " ")
    |> Seq.reduce (fun acc s -> acc + s)
    |> _.TrimEnd(' ')

let is_in_skiplist (pkg_name: string): bool =
    pkg_name.StartsWith("linux-headers-") ||
    pkg_name.StartsWith("linux-image-") ||
    pkg_name.StartsWith("linux-modules-") ||
    pkg_name.StartsWith("linux-modules-extra") ||
    pkg_name.StartsWith("linux-tools-")

[<EntryPoint>]
let main args =
    let target_flavour = args[0]
    printfn $"target chosen: {target_flavour}"
    let all_installed = run_stdout "apt list --installed"
    let installed_packages: Set<string> =
        all_installed.Split "\n"
        |> Array.filter (fun i -> i.Contains("/"))
        |> Array.map (fun i -> substring_before_first i "/")
        |> Array.filter (fun pkg -> not $ is_in_skiplist pkg)
        |> Set.ofArray
        
    let release_data = read_file "/etc/lsb-release"
    let current_release_opt = 
        release_data.Split("\n")
            |> Array.filter _.Contains("DISTRIB_ID")
            |> Array.map (fun s -> substring_after_first s "=")
            |> Array.map _.ToLower()
            |> Array.tryHead

    let release_data = read_file "/etc/os-release"
    let current_version_opt = 
        release_data.Split("\n")
            |> Array.filter _.Contains("VERSION=")
            |> Array.map (fun s -> substring_after_first s "=")
            |> Array.map (fun s -> s.Replace("\"", ""))
            |> Array.map (fun s -> substring_before_first s " ")
            |> Array.tryHead
    
    let current_flavour = 
        match current_release_opt with
        | Some a -> a
        | None -> failwith "distribution id not found"

    let current_version = 
        match current_version_opt with
        | Some a -> a
        | None -> failwith "current version not found"
         
    let curr_url = get_manifest_link current_flavour current_version
    let target_url = get_manifest_link target_flavour current_version
    
    if (not $ file_exists (substring_after_last curr_url "/"))
    then
        run_stdout_nooutput $"wget {curr_url}"

    if (not $ file_exists (substring_after_last target_url "/"))
    then
        run_stdout_nooutput $"wget {target_url}"
    
    let curr_filename = substring_after_last curr_url "/"
    let target_filename = substring_after_last target_url "/"
    
    let curr_flavour_pkgs: Set<string> =
        (read_file curr_filename).Split("\n")
        |> Array.filter (fun i -> i.Trim().Length > 0)
        |> Array.filter (fun i -> not $ i.StartsWith("snap:"))
        |> Array.map (fun i -> substring_before_first i "\t")
        |> Array.map (fun i ->
            match i.Contains(":") with
            | true -> substring_before_first i ":"
            | false -> i
        )
        |> Array.filter (fun pkg -> not $ is_in_skiplist pkg)
        |> Set.ofArray
    
    let target_flavour_pkgs: Set<string> =
        (read_file target_filename).Split("\n")
        |> Array.filter (fun i -> i.Trim().Length > 0)
        |> Array.filter (fun i -> not $ i.StartsWith("snap:"))
        |> Array.map (fun i -> substring_before_first i "\t")
        |> Array.map (fun i ->
            match i.Contains(":") with
            | true -> substring_before_first i ":"
            | false -> i
        )        
        |> Array.filter (fun pkg -> not $ is_in_skiplist pkg)
        |> Set.ofArray
    
    let my_added_pkgs: Set<string> =
        Set.difference (installed_packages) (curr_flavour_pkgs)

    let to_install_new_flavour_pkgs: Set<string> =
        Set.difference (Set.union my_added_pkgs target_flavour_pkgs) (installed_packages)
    
    let to_delete_old_flavour_pkgs: Set<string> =
        Set.difference (installed_packages) (Set.union my_added_pkgs target_flavour_pkgs)
       
    printfn $"Remove user-installed packages (optional):"
    write_file "01_remove_user_pkgs.sh" $"sudo apt remove {sorted_set_str my_added_pkgs}"
    
    printfn $"Add {target_flavour} flavour:"
    write_file $"02_add_flavour_{target_flavour}.sh" $"sudo apt install {sorted_set_str to_install_new_flavour_pkgs}"

    printfn $"Remove {current_flavour} flavour:"
    write_file $"03_remove_flavour_{current_flavour}.sh" $"sudo apt purge {sorted_set_str to_delete_old_flavour_pkgs}"

    // Return 0. This indicates success.
    0
