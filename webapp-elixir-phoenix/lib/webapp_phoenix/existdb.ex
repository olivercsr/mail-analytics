defmodule WebappPhoenix.ExistDb do

  require Logger
  require Req
  import SweetXml

  def query(query_name) do
    query = EEx.eval_file("priv/xqueries/#{query_name}.eex", [query_name: query_name])
    xml_query = EEx.eval_file("priv/xqueries/container.eex", [query: query, variables: [
      [key: "wantedBegin", type: "integer", value: 123]
    ]])

    data = Req.get!("http://localhost:8080/exist/rest/dmarc").body |> xpath(~x"/")
    IO.inspect(data)

    Logger.info(EEx.eval_string("query_name: <%= query_name %>", [query_name: query_name]))
    Logger.info("query: #{xml_query}")

    1234
  end
end

