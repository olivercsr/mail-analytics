defmodule WebappPhoenixWeb.AuthController do
  use WebappPhoenixWeb, :controller

  defp authorize_url!("kanidm") do
    Auth.KanIdmOAuth.authorize_url!(state: "abc", scope: "openid profile email")
  end

  defp authorize_url!("google") do
    Auth.GoogleOAuth.authorize_url!(scope: "https://www.googleapis.com/auth/userinfo.email")
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
    redirect(conn, external: authorize_url!(provider))
  end

  def callback(conn, %{"provider" => provider, "code" => code}) do
    client = get_token!(provider, code)
    user = get_user!(provider, client)

    IO.puts("====================================================== got token")
    IO.inspect(client)
    IO.inspect(user)

    # token = generate_and_sign_jwt!()

    redirect(conn, to: "/")
  end
end

