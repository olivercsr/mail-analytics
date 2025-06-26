defmodule WebappPhoenixWeb.QueryController do
  use WebappPhoenixWeb, :controller

  # require Req
  # import SweetXml

  def count(conn, %{"start" => startts, "end" => endts} = _params) do
    # response = Req.get!("http://localhost:8080/exist/rest/dmarc")
    # data = response.body
    # IO.inspect(IEx.Info.info(data))
    # IO.inspect(data)
    # xml_data = response.body |> xpath(~x"/")
    # IO.inspect(xml_data)

    #results = WebappPhoenix.ExistDb.query("query_count")
    #IO.puts("results: #{results}")

    # TODO: handle parsing errors?
    {startts_int, ""} = Integer.parse(startts)
    {endts_int, ""} = Integer.parse(endts)
    # existdb_config = Application.get_env(:webapp_phoenix, Db.ExistDb)
    # [tenant] = Plug.Conn.get_req_header(conn, "remote-user") # TODO: lookup mail addresses
    #   |> Enum.map(&String.trim/1)
    tenant = conn.assigns.authuser
    {:ok, results} = Db.ExistDb.query(Db.ExistDb,
      tenant,
      "query_count",
      %{wantedBegin: startts_int, wantedEnd: endts_int}
    )

    conn
      |> render(:count, tenant: tenant, startts: startts, endts: endts, results: results)
  end
end

