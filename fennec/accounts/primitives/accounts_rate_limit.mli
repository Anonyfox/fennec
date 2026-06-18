(** Brute-force throttle for the account auth methods ([login], [createUser]). A token bucket per
    (rule, key): each holds up to [max_attempts] tokens, refilled at [max_attempts / window]
    tokens/sec, and one attempt spends one token. Mirrors Meteor's [DDPRateLimiter] defaults — 5
    attempts / 10 s — but keys on BOTH the client IP and the login selector (account), so neither a
    single IP spraying many accounts nor many IPs hammering one account can grind the password path.

    This is deliberately distinct from {!Rate_limit} (the HTTP middleware): that keys only on the
    socket peer and throttles the WebSocket {e upgrade}, whereas every login [method] travels over one
    long-lived DDP connection — so the middleware never sees individual attempts. It also cannot key
    on the account selector. Thread-safe across the server's worker domains; a gated GC sweep evicts
    fully-recovered buckets so the table can't grow one entry per spoofed key forever.

    Pairs with the account-enumeration timing defense in [login_with_password_completion]: throttling
    keys on the {e selector string} (not on whether a user exists), so a missing and a present account
    are rate-limited identically — the throttle is not an existence oracle. *)

type t

(** A single rule: [max_attempts] tokens that refill fully over [window] seconds. *)
type limit

(** [limit ~max_attempts ~window] is [max_attempts] attempts per [window] seconds. Raises
    [Invalid_argument] if [max_attempts <= 0] or [window <= 0.]. *)
val limit : max_attempts:int -> window:float -> limit

(** Meteor's default for the auth methods: 5 attempts per 10 seconds. *)
val default_login : limit

(** Default signup throttle: 5 createUser calls per 10 seconds. *)
val default_create_user : limit

(** [make ?now ?enabled ?login ?create_user ()] builds the limiter. [enabled] (default [true]) gates
    the whole thing — [false] makes every check pass (an escape hatch / "off" switch). [login] and
    [create_user] default to {!default_login} / {!default_create_user}. [now] overrides the clock
    (for tests). *)
val make :
  ?now:(unit -> float) ->
  ?enabled:bool ->
  ?login:limit ->
  ?create_user:limit ->
  unit ->
  t

(** [login_allowed t ~ip ~account] charges one login attempt against the per-account bucket and, when
    [ip] is known, the per-IP bucket. [Ok ()] spends one token from each; [Error retry_after] (seconds,
    [>= 1]) means a bucket was empty and {e nothing} was spent — a throttled attempt does not deplete
    the other dimension's budget. [account] is any stable key for the selector (e.g. ["email:ada@x"]). *)
val login_allowed : t -> ip:string option -> account:string -> (unit, int) result

(** [create_user_allowed t ~ip] charges one signup against the per-IP bucket (a shared fail-closed
    bucket when [ip] is unknown). [Error retry_after] when throttled. *)
val create_user_allowed : t -> ip:string option -> (unit, int) result
