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
      DmarcWeb.Endpoint,

      {Task.Supervisor,
        name: TaskSupervisor
      },

      {Db.ExistDb,
        name: Db.ExistDb,
        config: Application.get_env(:dmarc, Db.ExistDb.Config)
      },

      Supervisor.child_spec({Ingress.FileCollector,
        name: DmarcFileCollector,
        config: Application.get_env(:dmarc, DmarcFileCollector),
        action: &Task.Supervisor.start_child(
          TaskSupervisor,
          fn -> Ingress.DmarcImporter.import_file(
            &1, &2, &3
          ) end
        )
      }, id: :dmarc_file_collector),
      Supervisor.child_spec({Ingress.FileCollector,
        name: DmarcFilePendingChecker,
        config: Application.get_env(:dmarc, DmarcFilePendingChecker),
        action: fn _, _, _ -> :ok end
      }, id: :dmarc_file_pending_checker),

      Supervisor.child_spec({Ingress.FileCollector,
        name: AttachmentFileCollector,
        config: Application.get_env(:dmarc, AttachmentFileCollector),
        action: &Task.Supervisor.start_child(
          TaskSupervisor,
          fn -> Ingress.AttachmentDecoder.decode(
            Application.get_env(:dmarc, AttachmentDecoder),
            &1, &2, &3
          ) end
        )
      }, id: :attachment_file_collector),
      Supervisor.child_spec({Ingress.FileCollector,
        name: AttachmentFilePendingChecker,
        config: Application.get_env(:dmarc, AttachmentFilePendingChecker),
        action: fn _, _, _ -> :ok end
      }, id: :attachment_file_pending_checker),

      Supervisor.child_spec({Ingress.FileCollector,
        name: MailFileCollector,
        config: Application.get_env(:dmarc, MailFileCollector),
        action: &Task.Supervisor.start_child(
          TaskSupervisor,
          fn -> Ingress.MailDecoder.decode(
            Application.get_env(:dmarc, MailDecoder),
            &1, &2, &3
          ) end
        )
      }, id: :mail_file_collector),
      Supervisor.child_spec({Ingress.FileCollector,
        name: MailFilePendingChecker,
        config: Application.get_env(:dmarc, MailFilePendingChecker),
        action: fn _, _, _ -> :ok end
      }, id: :mail_file_pending_checker),
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Dmarc.Supervisor]
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
    DmarcWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
