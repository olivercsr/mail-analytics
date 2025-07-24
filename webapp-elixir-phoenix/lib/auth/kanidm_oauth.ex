defmodule Auth.KanIdmOAuth do
  use OAuth2.Strategy
  # alias OAuth2.Strategy.AuthCode

  # Public API

  defp config do
    [
      strategy: __MODULE__,
      site: "http://dmarc.localhost:4000",
      authorize_url: "https://idm.localhost:8443/ui/oauth2",
      token_url: "https://idm.localhost:8443/oauth2/token"
    ]
  end

  def client do
    Application.get_env(:webapp_phoenix, KanIdm)
      |> Keyword.merge(config())
      |> OAuth2.Client.new()
      |> OAuth2.Client.put_serializer("application/json", Jason)
  end

  def authorize_url!(params \\ []) do
    OAuth2.Client.authorize_url!(client(), params)
  end

  def get_token!(params \\ [], headers \\ [], opts \\ []) do
    allOpts = Keyword.merge([ssl: [verify: :verify_none]], opts)
    OAuth2.Client.get_token!(client(), params, headers, allOpts)
  end

  def get_user!(client) do
    IO.puts("uuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuu")
    r = OAuth2.Client.get(client, "http://idm.localhost:8443/oauth2/token/introspect")
    IO.inspect(r)
    OAuth2.Client.get(client, "https://idm.localhost:8443/oauth2/openid/dmarc-client/userinfo")
  end

  # Strategy callbacks

  def authorize_url(client, params) do
    OAuth2.Strategy.AuthCode.authorize_url(client, params)
  end

  def get_token(client, params, headers) do
    client
      |> put_header("accept", "application/json")
      |> OAuth2.Strategy.AuthCode.get_token(params, headers)
  end
end

