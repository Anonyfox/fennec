defmodule PawBench.MixProject do
  use Mix.Project

  def project do
    [app: :paw_bench, version: "0.0.0", elixir: "~> 1.15", deps: deps()]
  end

  def application do
    [mod: {PawBench.Application, []}, extra_applications: [:logger]]
  end

  # Plug = the composable building block; Bandit = the modern pure-Elixir HTTP server.
  defp deps do
    [{:plug, "~> 1.16"}, {:bandit, "~> 1.5"}]
  end
end
