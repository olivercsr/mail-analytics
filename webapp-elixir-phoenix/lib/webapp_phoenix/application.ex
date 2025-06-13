defmodule WebappPhoenix.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      WebappPhoenixWeb.Telemetry,
      {DNSCluster, query: Application.get_env(:webapp_phoenix, :dns_cluster_query) || :ignore},
      {Phoenix.PubSub, name: WebappPhoenix.PubSub},
      # Start the Finch HTTP client for sending emails
      {Finch, name: WebappPhoenix.Finch},
      # Start a worker by calling: WebappPhoenix.Worker.start_link(arg)
      # {WebappPhoenix.Worker, arg},
      # Start to serve requests, typically the last entry
      WebappPhoenixWeb.Endpoint,
      {Ingress.MailDecoder,
        name: MailDecoder,
        # basepath: "./mails",
        # maildestpath: "mails/done",
        # attachmentspath: "attachments/pending",
      },
      {Ingress.FileCollector,
        name: FileCollector,
        interval_seconds: 10,
        basepath: "./mails",
        srcpath: "new",
        destpath: "done",
        action: &Ingress.MailDecoder.decode(MailDecoder, &1)
      }
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: WebappPhoenix.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    WebappPhoenixWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
