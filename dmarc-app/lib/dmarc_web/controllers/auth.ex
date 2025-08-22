defmodule DmarcWeb.AuthController do
  require Logger

  use DmarcWeb, :controller

  defp authorize_url!("kanidm") do
    Auth.KanIdmOAuth.authorize_url!(state: "abc", scope: "openid profile email")
  end

  defp authorize_url!("google") do
    Auth.GoogleOAuth.authorize_url!(scope: "openid https://www.googleapis.com/auth/userinfo.email https://www.googleapis.com/auth/userinfo.profile")
  end

  defp authorize_url!(provider) do
    raise("No matching OAuth provider found for #{provider}")
  end

  defp get_token("kanidm", code) do
    Auth.KanIdmOAuth.get_tokenx(state: "abc", code: code)
  end

  defp get_token("google", code) do
    Auth.GoogleOAuth.get_tokenx(code: code)
  end

  defp get_token(provider, _) do
    {:error, "No matching OAuth provider found for #{provider}"}
  end

  defp get_user!("kanidm", client) do
    Auth.KanIdmOAuth.get_user!(client)
  end

  defp get_user!("google", client) do
    Auth.GoogleOAuth.get_user!(client)
  end

  def index(conn, %{"provider" => provider}) do
    url = authorize_url!(provider)

    Logger.debug([module: __MODULE__, message: "index", provider: provider, authorizationUrl: url])

    redirect(conn, external: url)
  end

  def callback(conn, %{"provider" => provider, "code" => code}) do
    jwtcookie = Application.get_env(:dmarc, :auth_cookie)

    Logger.debug([
      module: __MODULE__,
      message: "callback",
      jwt_cookie: jwtcookie,
      provider: provider,
      code: "...#{String.slice(code, -3..-1//1)}"
    ])

    client = case get_token(provider, code) do
      {:ok, client} -> client
      {:error, reason} -> raise("Could not get token: #{inspect reason}")
    end
    user = get_user!(provider, client)

    IO.puts("====================================================== got token")
    IO.inspect(client)
    IO.inspect(client.token.expires_at)
    IO.inspect(user)

    # TODO: implement:
    #   (see README.md for detailed steps)
    #   - verify (id) token

    ptoken = Phoenix.Token.encrypt(DmarcWeb.Endpoint, "thesecret", %{:foo => "dingens"})
    IO.inspect(ptoken)

    case Auth.Jwt.generate_and_sign(%{
      "exp" => client.token.expires_at,
      "provider" => provider,
      "sub" => user["sub"],
      "email" => user["email"],
    }) do
      {:ok, token, claims} -> (
        Logger.info([
          module: __MODULE__,
          message: "generated JWT",
          jwt: token,
          claims: claims,
        ])
        conn
        |> put_resp_cookie(jwtcookie, token, http_only: true, secure: true, same_site: "lax")
        |> redirect(to: "/")
      )
      _ -> conn
        |> assign(:failed_logins, (conn.assigns[:failed_logins] || 0) + 1)
        |> redirect(to: "/login")
    end
  end
end

