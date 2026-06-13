(** The browser bridge for the generated PWA registration (see {!Pwa}): the update-available state
    as a Fur signal, and the user-confirmed apply. Browser-only (link into the JS bundle).

    {[ (* render an "update available — reload?" affordance off the signal *)
       if Fur.get (Pwa_client.update_available ()) then
         <button onClick=(fun _ -> Pwa_client.apply_update ())>"Reload to update"</button> ]} *)

(** Flips to [true] once a NEW build's service worker is installed and waiting. Render the "update
    available — reload?" affordance off this. *)
val update_available : unit -> bool Fur.signal

(** Swap to the waiting worker and reload the page. Call on user confirmation only — a silent
    mid-session swap could mix bundle versions. A no-op when nothing is waiting. *)
val apply_update : unit -> unit
