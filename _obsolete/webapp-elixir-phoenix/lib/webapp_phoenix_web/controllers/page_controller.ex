defmodule WebappPhoenixWeb.PageController do
  use WebappPhoenixWeb, :controller

  def home(conn, _params) do
    # NOTE: The home page is often custom made, so skip the default app layout.
    conn
    |> put_root_layout(html: {WebappPhoenixWeb.Layouts, :home_root})
    |> render(:home, layout: false)
  end
end
