defmodule WebappPhoenix.ExistDb do

  require Logger

  def query(query_name) do
    query = EEx.eval_file("priv/xqueries/query_count.eex", [query_name: query_name])
    Logger.info(EEx.eval_string("query_name: <%= query_name %>", [query_name: query_name]))
    Logger.info("query: #{query}")

    1234
  end
end

