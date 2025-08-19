import Config

# config/runtime.exs is executed for all environments, including
# during releases. It is executed after compilation and before the
# system starts, so it is typically used to load production configuration
# and secrets from environment variables or elsewhere. Do not define
# any compile-time configuration in here, as it won't be applied.
# The block below contains prod specific runtime configuration.

# ## Using releases
#
# If you use `mix release`, you need to explicitly enable the server
# by passing the PHX_SERVER=true when you start it:
#
#     PHX_SERVER=true bin/webapp_phoenix start
#
# Alternatively, you can use `mix phx.gen.release` to generate a `bin/server`
# script that automatically sets the env var above.
if System.get_env("PHX_SERVER") do
  config :webapp_phoenix, WebappPhoenixWeb.Endpoint, server: true
end

if config_env() == :prod do
  # The secret key base is used to sign/encrypt cookies and other secrets.
  # A default value is used in config/dev.exs and config/test.exs but you
  # want to use a different value for prod and you most likely don't want
  # to check this value into version control, so we use an environment
  # variable instead.
  secret_key_base =
    System.get_env("SECRET_KEY_BASE") ||
      raise """
      environment variable SECRET_KEY_BASE is missing.
      You can generate one by calling: mix phx.gen.secret
      """

  host = System.get_env("PHX_HOST") || "example.com"
  port = String.to_integer(System.get_env("PORT") || "4000")

  config :webapp_phoenix, :dns_cluster_query, System.get_env("DNS_CLUSTER_QUERY")

  config :webapp_phoenix, WebappPhoenixWeb.Endpoint,
    url: [host: host, port: 443, scheme: "https"],
    http: [
      # Enable IPv6 and bind on all interfaces.
      # Set it to  {0, 0, 0, 0, 0, 0, 0, 1} for local network only access.
      # See the documentation on https://hexdocs.pm/bandit/Bandit.html#t:options/0
      # for details about using IPv6 vs IPv4 and loopback vs public addresses.
      ip: {0, 0, 0, 0, 0, 0, 0, 0},
      port: port
    ],
    secret_key_base: secret_key_base

  # ## SSL Support
  #
  # To get SSL working, you will need to add the `https` key
  # to your endpoint configuration:
  #
  #     config :webapp_phoenix, WebappPhoenixWeb.Endpoint,
  #       https: [
  #         ...,
  #         port: 443,
  #         cipher_suite: :strong,
  #         keyfile: System.get_env("SOME_APP_SSL_KEY_PATH"),
  #         certfile: System.get_env("SOME_APP_SSL_CERT_PATH")
  #       ]
  #
  # The `cipher_suite` is set to `:strong` to support only the
  # latest and more secure SSL ciphers. This means old browsers
  # and clients may not be supported. You can set it to
  # `:compatible` for wider support.
  #
  # `:keyfile` and `:certfile` expect an absolute path to the key
  # and cert in disk or a relative path inside priv, for example
  # "priv/ssl/server.key". For all supported SSL configuration
  # options, see https://hexdocs.pm/plug/Plug.SSL.html#configure/1
  #
  # We also recommend setting `force_ssl` in your config/prod.exs,
  # ensuring no data is ever sent via http, always redirecting to https:
  #
  #     config :webapp_phoenix, WebappPhoenixWeb.Endpoint,
  #       force_ssl: [hsts: true]
  #
  # Check `Plug.SSL` for all available options in `force_ssl`.

  # ## Configuring the mailer
  #
  # In production you need to configure the mailer to use a different adapter.
  # Also, you may need to configure the Swoosh API client of your choice if you
  # are not using SMTP. Here is an example of the configuration:
  #
  #     config :webapp_phoenix, WebappPhoenix.Mailer,
  #       adapter: Swoosh.Adapters.Mailgun,
  #       api_key: System.get_env("MAILGUN_API_KEY"),
  #       domain: System.get_env("MAILGUN_DOMAIN")
  #
  # For this example you need include a HTTP client required by Swoosh API client.
  # Swoosh supports Hackney and Finch out of the box:
  #
  #     config :swoosh, :api_client, Swoosh.ApiClient.Hackney
  #
  # See https://hexdocs.pm/swoosh/Swoosh.html#module-installation for details.
end

config :webapp_phoenix, :env, config_env()

mail_folder = System.get_env("MAIL_FOLDER") || "./mails"

config :webapp_phoenix,
  auth_cookie: (System.get_env("AUTH_COOKIE") || "x-dmarc-session") |> String.trim(),
  mail_folder: mail_folder

config :webapp_phoenix, Db.ExistDb.Config, %Db.ExistDb.Config{
  base_url: System.get_env("EXISTDB_URL"),
  user: System.get_env("EXISTDB_USER"),
  password: System.get_env("EXISTDB_PASSWORD")
}

# OAuth providers
config :webapp_phoenix, Google,
 client_id: System.get_env("GOOGLE_CLIENT_ID"),
 client_secret: System.get_env("GOOGLE_CLIENT_SECRET"),
 redirect_uri: System.get_env("GOOGLE_REDIRECT_URI")

#config :webapp_phoenix, GitHub,
#  client_id: System.get_env("GITHUB_CLIENT_ID"),
#  client_secret: System.get_env("GITHUB_CLIENT_SECRET"),
#  redirect_uri: System.get_env("GITHUB_REDIRECT_URI")

config :webapp_phoenix, KanIdm,
  client_id: System.get_env("KANIDM_CLIENT_ID"),
  client_secret: System.get_env("KANIDM_CLIENT_SECRET"),
  redirect_uri: System.get_env("KANIDM_REDIRECT_URI")

config :joken, :default_signer,
  signer_alg: "HS512",
  key_octet: System.get_env("JWT_SIGNKEY")

config :webapp_phoenix, DmarcFileCollector, %Ingress.FileCollector.Config{
  interval_seconds: 41,
  # interval_seconds: 21,
  minfileage: 1,
  basepath: mail_folder,
  newpath: "dmarc/new",
  pendingpath: "dmarc/pending",
  donepath: "dmarc/done",
}
config :webapp_phoenix, DmarcFilePendingChecker, %Ingress.FileCollector.Config{
  interval_seconds: 61,
  minfileage: 30,
  # minfileage: 2,
  basepath: mail_folder,
  newpath: "dmarc/pending",
  pendingpath: "dmarc/new",
  donepath: "dmarc/new",
}

config :webapp_phoenix, AttachmentFileCollector, %Ingress.FileCollector.Config{
  interval_seconds: 37,
  # interval_seconds: 17,
  minfileage: 1,
  basepath: mail_folder,
  newpath: "attachments/new",
  pendingpath: "attachments/pending",
  donepath: "attachments/done",
}
config :webapp_phoenix, AttachmentFilePendingChecker, %Ingress.FileCollector.Config{
  interval_seconds: 59,
  minfileage: 30,
  # minfileage: 2,
  basepath: mail_folder,
  newpath: "attachments/pending",
  pendingpath: "attachments/new",
  donepath: "attachments/new",
}

config :webapp_phoenix, MailFileCollector, %Ingress.FileCollector.Config{
  interval_seconds: 31,
  # interval_seconds: 11,
  minfileage: 1,
  basepath: mail_folder,
  newpath: "mails/new",
  pendingpath: "mails/pending",
  donepath: "mails/done",
}
config :webapp_phoenix, MailFilePendingChecker, %Ingress.FileCollector.Config{
  interval_seconds: 53,
  minfileage: 30,
  # minfileage: 2,
  basepath: mail_folder,
  newpath: "mails/pending",
  pendingpath: "mails/new",
  donepath: "mails/new",
}

config :webapp_phoenix, MailDecoder, %Ingress.MailDecoder.Config{
  basepath: mail_folder,
  attachmentsdir: "attachments/new",
}

