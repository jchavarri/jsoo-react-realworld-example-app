module AnonymousOnly = struct
  let%component make ~user ~children =
    match user with
    | Some _ -> React.null
    | None -> React.Fragment.make ~children ()
end

module AuthenticatedOnly = struct
  let%component make ~user ~children =
    match user with
    | None -> React.null
    | Some _ -> React.Fragment.make ~children ()
end
