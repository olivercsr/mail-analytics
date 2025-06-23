defmodule Util.ExistDb do

  require Logger
  require Finch
  require Req
  import SweetXml

  def _type_of_value(value) do
    # IO.inspect(value)
    cond do
      is_integer(value) -> "integer"
      is_binary(value) -> "string"
    end
  end

  def query(query_name, variables) do
    query = EEx.eval_file("priv/xqueries/#{query_name}.eex", [query_name: query_name])
    variables = Enum.map(variables, fn {k, v} -> [key: k, type: _type_of_value(v), value: v] end)
    xml_query = EEx.eval_file("priv/xqueries/container.eex", [query: query, variables: variables])
    # [
    #   [key: "wantedBegin", type: "integer", value: 1715689600],
    #   [key: "wantedEnd", type: "integer", value: 1742974400]
    # ]

    Logger.info(EEx.eval_string("query_name: <%= query_name %>", [query_name: query_name]))
    Logger.info("query: #{xml_query}")

    req = Req.new(
      url: "http://localhost:8080/exist/rest/dmarc",
      headers: %{"content-type" => ["application/xml"]},
      body: xml_query
    )
    # IO.inspect(req)

    case Req.post(req, finch: DmarcFinchPool) do
      {:ok, result} when result.status >= 200 and result.status < 300 -> 
        {:ok, result.body |> xpath(~x"/")}
      {:ok, result} ->
        {:error, "unsuccessful http response code: #{result.status} - #{result.body}"}
      {:error, exception} ->
        {:error, exception}
    end
  end

  def store(filename, report_stream) do
    # TODO: make this entire request-making logic generic:
    req = Req.new(
      # TODO: url-encode filename?
      url: "http://localhost:8080/exist/rest/dmarc/#{filename}",
      auth: {:basic, %{user: "admin", password: ""}},
      headers: %{"content-type" => ["application/xml"]},
      body: report_stream
    )
    case Req.put(req, finch: DmarcFinchPool) do
      {:ok, result} when result.status >= 200 and result.status < 300 ->
        {:ok, result.body}
    end
  end
end

