defmodule WebappPhoenix.MixProject do
  use Mix.Project

  def project do
    [
      app: :webapp_phoenix,
      version: "0.1.0",
      elixir: "~> 1.14",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      aliases: aliases(),
      deps: deps()
    ]
  end

  # Configuration for the OTP application.
  #
  # Type `mix help compile.app` for more information.
  def application do
    [
      mod: {WebappPhoenix.Application, []},
      extra_applications: [:logger, :runtime_tools, :os_mon, :observer, :wx, :runtime_tools]
    ]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  # Specifies your project dependencies.
  #
  # Type `mix help deps` for examples and options.
  defp deps do
    [
      {:phoenix, "~> 1.7.21"},
      {:phoenix_html, "~> 4.1"},
      {:phoenix_live_reload, "~> 1.2", only: :dev},
      {:phoenix_live_view, "~> 1.0"},
      {:floki, ">= 0.30.0", only: :test},
      {:phoenix_live_dashboard, "~> 0.8.3"},
      {:esbuild, "~> 0.8", runtime: Mix.env() == :dev},
      {:tailwind, "~> 0.2.0", runtime: Mix.env() == :dev},
      {:heroicons,
       github: "tailwindlabs/heroicons",
       tag: "v2.1.1",
       sparse: "optimized",
       app: false,
       compile: false,
       depth: 1},
      {:swoosh, "~> 1.5"},
      {:finch, "~> 0.13"},
      {:telemetry_metrics, "~> 1.0"},
      {:telemetry_poller, "~> 1.0"},
      {:gettext, "~> 0.26"},
      {:jason, "~> 1.4"},
      {:dns_cluster, "~> 0.1.1"},
      {:bandit, "~> 1.5"},
      {:req, "~> 0.5.0"},
      {:sweet_xml, "~> 0.7.5"},
      {:mime, "~> 2.0"},
      {:mail, "~> 0.4"},
      {:stream_gzip, "~>0.4"},
      {:zstream, "~> 0.6"},
      {:exldap, "~> 0.6"},
      {:oauth2, "~> 2.1"},
      {:joken, "~> 2.6"}
    ]
  end

  # Aliases are shortcuts or tasks specific to the current project.
  # For example, to install project dependencies and perform other setup tasks, run:
  #
  #     $ mix setup
  #
  # See the documentation for `Mix` for more info on aliases.
  defp aliases do
    [
      setup: ["deps.get", "assets.setup", "assets.build"],
      "npm.install": ["cmd cd assets && npm install"],
      "assets.setup": ["npm.install", "tailwind.install --if-missing", "esbuild.install --if-missing"],
      "assets.build": ["tailwind webapp_phoenix", "esbuild webapp_phoenix"],
      "assets.deploy": [
        "tailwind webapp_phoenix --minify",
        "esbuild webapp_phoenix --minify",
        "phx.digest"
      ]
    ]
  end
end
