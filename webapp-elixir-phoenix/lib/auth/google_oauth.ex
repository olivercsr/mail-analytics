defmodule Auth.GoogleOAuth do
  use OAuth2.Strategy
  # alias OAuth2.Strategy.AuthCode

  # Public API

  defp config do
    [
      strategy: __MODULE__,
      site: "https://accounts.google.com",
      authorize_url: "https://accounts.google.com/o/oauth2/v2/auth",
      token_url: "https://oauth2.googleapis.com/token"
    ]
  end

  def client do
    Application.get_env(:webapp_phoenix, Google)
      |> Keyword.merge(config())
      |> OAuth2.Client.new()
      |> OAuth2.Client.put_serializer("application/json", Jason)
  end

  def authorize_url!(params \\ []) do
    OAuth2.Client.authorize_url!(client(), params)
  end

  def get_tokenx(params \\ [], headers \\ [], opts \\ []) do
    # allOpts = Keyword.merge([ssl: [verify: :verify_none]], opts)
    OAuth2.Client.get_token(client(), params, headers, opts)
  end

  def get_user!(client) do
    %{body: user} = OAuth2.Client.get!(client, "https://www.googleapis.com/oauth2/v3/userinfo")

    %{
      email: user["email"],
      avatar: user["picture"],
      email_verified: user["email_verified"],
      sub: user["sub"]
    }
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

