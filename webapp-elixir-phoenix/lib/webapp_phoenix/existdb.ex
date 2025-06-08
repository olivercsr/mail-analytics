defmodule WebappPhoenix.ExistDb do

  require Logger
  require Finch
  require Req
  import SweetXml

  def query(query_name) do
    query = EEx.eval_file("priv/xqueries/#{query_name}.eex", [query_name: query_name])
    xml_query = EEx.eval_file("priv/xqueries/container.eex", [query: query, variables: [
      [key: "wantedBegin", type: "integer", value: 1715689600],
      [key: "wantedEnd", type: "integer", value: 1742974400]
    ]])

    Logger.info(EEx.eval_string("query_name: <%= query_name %>", [query_name: query_name]))
    Logger.info("query: #{xml_query}")

    req = Req.new(url: "http://localhost:8080/exist/rest/dmarc", headers: %{"content-type" => ["text/xml"]}, body: xml_query)
    # IO.inspect(req)

    Finch.start_link(
      name: MyFinch,
      pools: %{
        default: [
          size: 100,
          count: 10,
          pool_max_idle_time: 60_000
        ]
      }
    )
    case Req.post(req, finch: MyFinch) do
      {:ok, result} when result.status >= 200 and result.status < 300 -> 
        {:ok, result.body |> xpath(~x"/")}
      {:ok, result} ->
        {:error, "unsuccessful http response code: #{result.status} - #{result.body}"}
      {:error, exception} ->
        {:error, exception}
    end
  end
end

