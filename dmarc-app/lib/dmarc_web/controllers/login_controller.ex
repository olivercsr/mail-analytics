defmodule DmarcWeb.LoginController do
  use DmarcWeb, :controller

  def index(conn, _params) do
    conn
    |> put_root_layout(html: {DmarcWeb.Layouts, :home_root})
    |> render(:login, layout: false)
  end
end

