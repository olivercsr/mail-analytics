defmodule DmarcWeb.PageController do
  use DmarcWeb, :controller

  def home(conn, _params) do
    # The home page is often custom made,
    # so skip the default app layout.
    conn
    |> put_root_layout(html: {DmarcWeb.Layouts, :home_root})
    |> render(:home, layout: false)
  end
end
