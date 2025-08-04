defmodule WebappPhoenixWeb.Plugs.AuthJwt do
  import Plug.Conn
  require Config
  require Logger

  def init(default), do: default

  defp extract_jwt_from_conn(conn) do
    case get_cookies(conn) do
      %{"x-dmarc-session" => jwt} when is_binary(jwt) and jwt != ""  -> {:ok, jwt}
      _ -> nil
    end
  end

  defp verify_jwt(jwt) do
    case Auth.Jwt.verify_and_validate(jwt) do
      {:ok, verificationResult} -> {:ok, verificationResult}
      _ -> nil
    end
  end

  defp get_authuser_from_verify(verificationResult) do
    case verificationResult["email"] do
      email when is_binary(email) and email != "" -> {:ok, email}
      _ -> nil
    end
  end

  defp get_authuser(conn) do
    with {:ok, jwt} <- extract_jwt_from_conn(conn),
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

  def call(conn, _args) do
    case get_authuser(conn) do
      {:ok, authuser} -> assign(conn, :authuser, authuser); 
      Logger.info([module: __MODULE__, message: "jwt", authuser: authuser]);
        conn
        _ -> resp(conn, 401, "unauthenticated") |> halt()
    end
  end
end


