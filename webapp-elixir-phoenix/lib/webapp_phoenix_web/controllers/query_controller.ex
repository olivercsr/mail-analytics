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

    case WebappPhoenix.ExistDb.query("query_count") do
      {:ok, result} -> IO.inspect(result)
      {:error, error} -> IO.inspect(error)
    end

    render(conn, :count, startts: startts, endts: endts)
  end
end

