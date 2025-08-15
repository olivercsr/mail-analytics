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

      {Db.ExistDb,
        name: Db.ExistDb,
        config: Application.get_env(:webapp_phoenix, Db.ExistDb.Config)
      },

      {Ingress.DmarcImporter,
        name: DmarcImporter,
      },
      Supervisor.child_spec({Ingress.FileCollector,
        name: DmarcFileCollector,
        config: Application.get_env(:webapp_phoenix, DmarcFileCollector),
        action: &Ingress.DmarcImporter.import_file(DmarcImporter, &1, &2, &3)
      }, id: :dmarc_file_collector),
      Supervisor.child_spec({Ingress.FileCollector,
        name: DmarcFilePendingChecker,
        config: Application.get_env(:webapp_phoenix, DmarcFilePendingChecker),
        action: fn _ -> :ok end  # TODO: implement
      }, id: :dmarc_file_pending_checker),

      {Ingress.AttachmentDecoder,
        name: AttachmentDecoder,
        basepath: Application.get_env(:webapp_phoenix, :mail_folder),
        dmarcreportsdir: "dmarc/new",
      },
      Supervisor.child_spec({Ingress.FileCollector,
        name: AttachmentFileCollector,
        config: Application.get_env(:webapp_phoenix, AttachmentFileCollector),
        action: &Ingress.AttachmentDecoder.decode(AttachmentDecoder, &1, &2, &3)
      }, id: :attachment_file_collector),
      Supervisor.child_spec({Ingress.FileCollector,
        name: AttachmentFilePendingChecker,
        config: Application.get_env(:webapp_phoenix, AttachmentFilePendingChecker),
        action: fn _ -> :ok end
      }, id: :attachment_file_pending_checker),

      {Ingress.MailDecoder,
        name: MailDecoder,
        basepath: Application.get_env(:webapp_phoenix, :mail_folder),
        attachmentsdir: "attachments/new",
      },
      Supervisor.child_spec({Ingress.FileCollector,
        name: MailFileCollector,
        config: Application.get_env(:webapp_phoenix, MailFileCollector),
        action: &Ingress.MailDecoder.decode(MailDecoder, &1, &2, &3)
      }, id: :mail_file_collector),
      Supervisor.child_spec({Ingress.FileCollector,
        name: MailFilePendingChecker,
        config: Application.get_env(:webapp_phoenix, MailFilePendingChecker),
        action: fn _ -> :ok end
      }, id: :mail_file_pending_checker),
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: WebappPhoenix.Supervisor]
    Supervisor.start_link(children, opts)

    # Finch http connection pool setup:
    Finch.start_link(
      name: DmarcFinchPool,
      pools: %{
        default: [
          size: 100,
          count: 10,
          pool_max_idle_time: 60_000
        ]
      }
    )
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    WebappPhoenixWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
