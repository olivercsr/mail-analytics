defmodule WebappPhoenixWeb.QueryController do
  use WebappPhoenixWeb, :controller

  def count(conn, %{"start" => startts, "end" => endts} = _params) do
    # The home page is often custom made,
    # so skip the default app layout.
    render(conn, :count, startts: startts, endts: endts)
  end
end

