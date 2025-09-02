defmodule WebappPhoenixWeb.LoginController do
  use WebappPhoenixWeb, :controller

  def index(conn, _params) do
    conn
    |> put_root_layout(html: {WebappPhoenixWeb.Layouts, :home_root})
    |> render(:login, layout: false)
  end
end

