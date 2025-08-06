defmodule WebappPhoenixWeb.Plugs.AuthJwt do
  import Plug.Conn
  require Config
  require Logger

  def init(default), do: default

  defp extract_jwt_from_conn(conn, jwtcookie) do
    case get_cookies(conn)[jwtcookie] do
      jwt when is_binary(jwt) and jwt != ""  -> {:ok, jwt}
      cookieval -> (
        Logger.info([
          module: __MODULE__,
          message: "Could not extract Auth JWT from cookies",
          jwtcookiename: jwtcookie,
          jwtcookievalue: cookieval,
        ])
        nil
      )
    end
  end

  defp verify_jwt(jwt) do
    case Auth.Jwt.verify_and_validate(jwt) do
      {:ok, verificationResult} -> {:ok, verificationResult}
      result -> (
        Logger.info([
          module: __MODULE__,
          message: "Could not verify JWT",
          jwt: jwt,
          verificationResult: result,
        ])
        nil
      )
    end
  end

  defp get_authuser_from_verify(verificationResult) do
    case verificationResult["email"] do
      email when is_binary(email) and email != "" -> {:ok, email}
      value -> (
        Logger.info([
          module: __MODULE__,
          message: "Could not get email from JWT content",
          verificationResult: verificationResult,
          email: value,
        ])
        nil
      )
    end
  end

  defp get_authuser(conn, httpheader) do
    with {:ok, jwt} <- extract_jwt_from_conn(conn, httpheader),
      {:ok, verificationInfo} <- verify_jwt(jwt),
      {:ok, authuser} <- get_authuser_from_verify(verificationInfo)
    do
      IO.puts("================================= jwt")
      IO.inspect(jwt)

      if is_binary(authuser) and authuser != "" do
        {:ok, authuser}
      end
    end
  end

  def call(conn, _params) do
    jwtcookie = Application.get_env(:webapp_phoenix, :auth_cookie)

    Logger.info([
      module: __MODULE__,
      message: "verifying jwt auth",
      jwt_cookie: jwtcookie
    ])

    case get_authuser(conn, jwtcookie) do
      {:ok, authuser} -> (
        assign(conn, :authuser, authuser)
        Logger.info([module: __MODULE__, message: "jwt", authuser: authuser])
        conn
      )
      _ -> resp(conn, 401, "unauthenticated") |> halt()
    end
  end
end


