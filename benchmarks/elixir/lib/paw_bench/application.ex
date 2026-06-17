defmodule PawBench.Application do
  # Boots Bandit (the pure-Elixir HTTP server) serving the Plug router. The BEAM uses all
  # schedulers (= cores) by default, so this is multicore out of the box.
  use Application

  @impl true
  def start(_type, _args) do
    port = String.to_integer(System.get_env("PORT") || "8080")
    children = [{Bandit, plug: PawBench.Router, scheme: :http, ip: {127, 0, 0, 1}, port: port}]
    Supervisor.start_link(children, strategy: :one_for_one, name: PawBench.Supervisor)
  end
end
