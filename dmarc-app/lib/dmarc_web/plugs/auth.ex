defmodule DmarcWeb.Plugs.Auth do
  import Plug.Conn
  require Config
  require Logger

  def init(default), do: default

  defp get_authuser_from_env() do
    (Application.get_env(:dmarc, :dev_authuser, "") || "") |> String.trim()
  end

  defp get_authuser_from_header(conn, authuser_header) do
    [authuser] = conn
      |> Plug.Conn.get_req_header(authuser_header) # TODO: lookup mail addresses
      |> Enum.map(&String.trim/1)

    authuser
  end

  defp get_authuser(conn, authuser_header) do
    case Application.get_env(:dmarc, :env, :unknown) do
      :dev -> case get_authuser_from_env() do
        authuser when is_binary(authuser) and authuser != "" -> authuser
        _ -> get_authuser_from_header(conn, authuser_header)
      end
      _ -> get_authuser_from_header(conn, authuser_header)
    end
  end

  def call(conn, authuser_header) do
    # IO.puts("========================================= conn")
    # IO.inspect(conn)

    authuser = case get_authuser(conn, authuser_header) do
      authuser when is_binary(authuser) and authuser != "" -> authuser
    end

    Logger.info([module: __MODULE__, message: "authuser", authuser: authuser])

    put_session(conn, "authuser", authuser)
    # assign(conn, :authuser, authuser)
  end
end

