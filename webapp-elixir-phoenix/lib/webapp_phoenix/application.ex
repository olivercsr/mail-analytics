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
        interval_seconds: 17,
        minfileage: 1,
        basepath: "./mails",
        newpath: "dmarc/new",
        pendingpath: "dmarc/pending",
        donepath: "dmarc/done",
        action: &Ingress.DmarcImporter.import_file(DmarcImporter, &1, &2, &3)
      }, id: :dmarc_importer),
      Supervisor.child_spec({Ingress.FileCollector,
        name: DmarcFilePendingChecker,
        interval_seconds: 51,
        minfileage: 5,
        basepath: "./mails",
        newpath: "dmarc/pending",
        pendingpath: "dmarc/new",
        donepath: "dmarc/new",
        action: fn _ -> :ok end
      }, id: :dmarc_file_pending_checker),

      {Ingress.AttachmentDecoder,
        name: AttachmentDecoder,
        basepath: "./mails",
        dmarcreportsdir: "dmarc/new",
      },
      Supervisor.child_spec({Ingress.FileCollector,
        name: AttachmentFileCollector,
        interval_seconds: 13,
        minfileage: 1,
        basepath: "./mails",
        newpath: "attachments/new",
        pendingpath: "attachments/pending",
        donepath: "attachments/done",
        action: &Ingress.AttachmentDecoder.decode(AttachmentDecoder, &1, &2, &3)
      }, id: :attachment_file_collector),
      Supervisor.child_spec({Ingress.FileCollector,
        name: AttachmentFilePendingChecker,
        interval_seconds: 53,
        minfileage: 5,
        basepath: "./mails",
        newpath: "attachments/pending",
        pendingpath: "attachments/new",
        donepath: "attachments/new",
        action: fn _ -> :ok end
      }, id: :attachment_file_pending_checker),

      {Ingress.MailDecoder,
        name: MailDecoder,
        basepath: "./mails",
        attachmentsdir: "attachments/new",
      },
      Supervisor.child_spec({Ingress.FileCollector,
        name: MailFileCollector,
        interval_seconds: 11,
        minfileage: 1,
        basepath: "./mails",
        newpath: "mails/new",
        pendingpath: "mails/pending",
        donepath: "mails/done",
        action: &Ingress.MailDecoder.decode(MailDecoder, &1, &2, &3)
      }, id: :mail_file_collector),
      Supervisor.child_spec({Ingress.FileCollector,
        name: MailFilePendingChecker,
        interval_seconds: 57,
        minfileage: 5,
        basepath: "./mails",
        newpath: "mails/pending",
        pendingpath: "mails/new",
        donepath: "mails/new",
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
