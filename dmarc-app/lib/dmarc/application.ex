defmodule Dmarc.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      DmarcWeb.Telemetry,
      Dmarc.Repo,
      {DNSCluster, query: Application.get_env(:dmarc, :dns_cluster_query) || :ignore},
      {Phoenix.PubSub, name: Dmarc.PubSub},
      # Start the Finch HTTP client for sending emails
      {Finch, name: Dmarc.Finch},
      # Start a worker by calling: Dmarc.Worker.start_link(arg)
      # {Dmarc.Worker, arg},
      # Start to serve requests, typically the last entry
      DmarcWeb.Endpoint
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Dmarc.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    DmarcWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
