(* accounts_base.ml — the Accounts engine, now a THIN GLUE that re-exports the cohesive engine
   modules in dependency order. Its public surface is unchanged: accounts.ml does [include
   Accounts_base] and satisfies accounts.mli, so the carve is invisible to the 4 consumers.

   The engine, by responsibility (each layer opens only the ones below it):
     Accounts_types            data model + module aliases + Assigns keys + trivial helpers
     Accounts_secrets          CSPRNG/hashing/constant-eq, the signed-session codec, the MFA seal
     Accounts_identity_bridge  external_identity + per-provider identity glue + service factories
     Accounts_runtime          make/configure/hooks/audit + the strategy->audit/mechanism maps
     Accounts_request          the cookie-verifying paw + require_* guards + can/can_in
     Accounts_lifecycle        user/password/email/role/MFA-enrollment/org/identity-link writes
     Accounts_login            session issue + the login_with_* family + step-up + sessions
     Accounts_http             the *_paw routes + SCIM provisioning + JSON/BSON helpers *)

include Accounts_types
include Accounts_secrets
include Accounts_identity_bridge
include Accounts_runtime
include Accounts_request
include Accounts_lifecycle
include Accounts_login
include Accounts_http
