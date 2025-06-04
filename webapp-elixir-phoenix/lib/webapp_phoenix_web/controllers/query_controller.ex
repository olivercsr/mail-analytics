defmodule WebappPhoenixWeb.QueryController do
  use WebappPhoenixWeb, :controller

  def count(conn, %{"start" => startts, "end" => endts} = _params) do
    # The home page is often custom made,
    # so skip the default app layout.
    results = WebappPhoenix.ExistDb.query("query_count")
    IO.puts("results: #{results}")

    render(conn, :count, startts: startts, endts: endts)
  end
end

