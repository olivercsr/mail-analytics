defmodule WebappPhoenixWeb.AuthController do
  require Logger

  use WebappPhoenixWeb, :controller

  defp authorize_url!("kanidm") do
    Auth.KanIdmOAuth.authorize_url!(state: "abc", scope: "openid profile email")
  end

  defp authorize_url!("google") do
    Auth.GoogleOAuth.authorize_url!(scope: "openid https://www.googleapis.com/auth/userinfo.email https://www.googleapis.com/auth/userinfo.profile")
  end

  defp authorize_url!(provider) do
    raise("No matching OAuth provider found for #{provider}")
  end

  defp get_token!("kanidm", code) do
    Auth.KanIdmOAuth.get_token!(state: "abc", code: code)
  end

  defp get_token!("google", code) do
    Auth.GoogleOAuth.get_token!(code: code)
  end

  defp get_token!(provider, _) do
    raise("No matching OAuth provider found for #{provider}")
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
    Logger.debug([module: __MODULE__, message: "callback", provider: provider, code: "...#{String.slice(code, -3..-1//1)}"])

    client = get_token!(provider, code)
    user = get_user!(provider, client)

    IO.puts("====================================================== got token")
    IO.inspect(client)
    IO.inspect(user)

    # TODO: implement:
    #   (see README.md for detailed steps)
    #   - verify (id) token
    #   - create & sign JWT with relevant information
    #   - set JWT as cookie
    #   - on each request, check JWT
    #   - if appropriate, redirect to login

    token = Auth.Jwt.generate_and_sign()
    IO.puts("========================= our token =======================")
    IO.inspect(token)

    redirect(conn, to: "/")
  end

  def testtoken(conn, _params) do
    {:ok, token, claims} = Auth.Jwt.generate_and_sign(%{"sub" => "abcdef"})
    verificationResult = Auth.Jwt.verify_and_validate(token)
    IO.puts("========================= our token =======================")
    IO.inspect(token)
    IO.inspect(claims)
    IO.puts("========================= token verification result =======================")
    IO.inspect(verificationResult)

    resp(conn, 200, "all ok")
  end
end

