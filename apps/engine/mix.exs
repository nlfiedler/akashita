defmodule AkashitaEngine.Mixfile do
  use Mix.Project

  def project do
    [
      app: :engine,
      version: "0.1.0",
      build_path: "../../_build",
      config_path: "../../config/config.exs",
      deps_path: "../../deps",
      lockfile: "../../mix.lock",
      elixir: "~> 1.5",
      start_permanent: Mix.env == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {AkashitaEngine.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:inugami, github: "nlfiedler/inugami", tag: "1.0.0"},
      {:ulid, "~> 0.1.0"},
      {:temp, "~> 0.4.3", only: [:test]}
    ]
  end
end
